open Syntax
open Closure
open Printf

let read_from_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let rec get_ctx decls nlam =
  match decls with
    [] -> [], nlam
  | Decl (v, _) :: tl -> let tl, nnlam = get_ctx tl (nlam + 1) in
    (v, Forall([], TVar nlam)) :: tl, nnlam
  | _ :: tl -> get_ctx tl nlam

let rec def_ctx decls context types nd nlam texpr tc ist mn tp =
  match decls with
    [] -> context, types, nd, texpr, tc, ist, mn, tp, nlam
  | Decl(v, body) :: tl ->
    let s, t, nlam, e = Types.infer body context nlam in
    let n_ctx  = (Types.subst_context s context) in
    let n_ctx = List.remove_assoc v n_ctx in
    let n_ctx = (v, Types.gen n_ctx t) :: n_ctx in
    def_ctx tl n_ctx types nd nlam (texpr @ [TyDecl (v, e, t)]) tc ist mn
      (tp ^ (sprintf "Value _%s;\n" v))
  | TDef t :: tl ->
    let v =
      match snd (List.hd t) with
        Forall (_, t) -> Utils.get_t_n t
    in
    let n, _ = List.split t in
    let s    = "  enum {" ^
               (List.fold_left (fun x y -> x ^ ", __" ^ y)
                  ("DUMB" ^ (string_of_int nlam))
                  n ^ "} _" ^ v ^ ";") in
    let rec com_t l s1 s2 s3 =
      match l with
        [] -> s1, s2, s3
      | (n, Forall(_, t)) :: tl ->
        let n1, n2, n3 =
          match t with
            TOth _ ->
            Printf.sprintf
              "Value make_%s() {
          Value n;
          n.t = %s;
          n._%s = __%s;
          n.has_cell = 0;
          return (n);
}\n" n (String.uppercase_ascii v) v n,
            Printf.sprintf "_%s = make_%s();\n" n n,
            Printf.sprintf "Value _%s;\n" n
          | _ ->
            Printf.sprintf
              "Value make_%s(Value *env, Value cell, int len) {
          Value n;
          n.t = %s;
          n._%s = __%s;
          n.cell = alloc(1);
          *(n.cell) = cell;
          n.has_cell = 1;
          return (n);
}\n" n (String.uppercase_ascii v) v n,
            Printf.sprintf "_%s = make_closure(make_%s, NULL, 0);\n" n n,
            Printf.sprintf "Value _%s;\n" n
        in
        com_t tl (s1 ^ n1) (s2 ^ n2) (s3 ^ n3)
      in
      let fn, m, t' = com_t t "" "" "" in
      let ntc       = ",\n  " ^ (String.uppercase_ascii v) in
      let nin       = Printf.sprintf
          "case %s :
      if (l1._%s != l2._%s)
        return (make_int(0));
      else if (l1.has_cell)
          return(intern_eq(*(l1.cell), *(l2.cell)));
    break;"
          (String.uppercase_ascii v) v v
      in
      def_ctx tl (context @ t) (types ^ s) (nd ^ fn) (nlam + 1)
        texpr (tc ^ ntc) (ist ^ nin) (mn ^ m) (tp ^ t')

let rec compile_module m nlam ctx c1 c2 c3 c4 c5 c6 =
  match m with
    []       -> c1, c2, c3, c4, c5, c6, nlam, ctx
  | hd :: tl ->
    let s =
      read_from_file ("lib/" ^ (String.lowercase_ascii hd) ^ ".oc") in
    let t, _ = Lexer.lexer s 0 0 in
    let t, _ = List.split t in
    let p, _ = Parser.parse_tops t in
    let c, n = get_ctx p nlam in
    let rec compile_funs d fs b n =
      match d with
        [] -> fs, b, n
      | TyDecl (v, bd, _) :: tl ->
        let fn, nf, nb, n, _ =
          closure_to_c (to_closure (deB bd ("", 1))) n "tenv" in
        let f = sprintf "Value _%s;\n" v in
        compile_funs tl (fs ^ f ^ nf) (b ^ nb ^ (sprintf "_%s = %s;\n" v fn)) n
    in
    let c, t, n, e, lt, i, m, tp, nlam = def_ctx p (ctx @ c) "" "" n [] "" "" "" "" in
    let f, b, nlam = compile_funs e tp m nlam in
    compile_module tl nlam c (c1 ^ tp ^ f) (c2 ^ m ^ b) (c3 ^ lt)
      (c4 ^ t) (c5 ^ i) (c6 ^ n)

let compile f =
  let s    = read_from_file f    in
  let t, _ = Lexer.lexer s 0 0   in
  let t, _ = List.split t        in
  let f, m = Parser.parse_tops t in
  let c, n = get_ctx f 0 in
  let c1, c2, c3, c4, c5, c6, nlam, ctx =
    compile_module (["stdlib"; "list"; "char"; "pair"] @ m) n (Types.initial_ctx @ c)
      "" "" "" "" "" "" in
  let _, t, n, e, lt, i, m, tp, nlam =
    def_ctx f ctx "" "" nlam [] "" "" "" "" in
  let oc = open_out "out.c"      in
  fprintf oc "%s\n" (decls_to_c e (c1 ^ tp) (c2 ^ m) nlam);
  close_out oc;
  let oc = open_out "core.h"     in
  Core.core oc (c3 ^ lt) (c4 ^ t) (c5 ^ i) (c6 ^ n)
