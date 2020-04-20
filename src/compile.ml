open Syntax

let read_from_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let compile f =
  let rec def_ctx decls context types nd nlam texpr tc ist mn tp =
    match decls with
      [] -> context, types, nd, texpr, tc, ist, mn, tp
    | Decl(v, body) :: tl ->
      let tmp_ctx    = context @ [v, Forall([], TVar 0)] in
      let s, t, n, e = Types.infer body tmp_ctx 1        in
      let n_ctx  =
      match v with
          "main" ->
          (* The main function should be of type int -> int *)
          let s2 = Types.unify t (TFun(TOth "int", TOth "int")) in
          (Types.subst_context (Types.compose_subst s s2) context) @
          [v, Forall([], (TFun(TOth "int", TOth "int")))]
        | _ ->
          (Types.subst_context s context) @ [v, Types.gen context t]
      in
      def_ctx tl n_ctx types nd n (texpr @ [TyDecl (v, e, t)]) tc ist mn tp
    | TDef t :: tl ->
      let v =
        match snd (List.hd t) with
          Forall (_, t) -> Utils.get_t_n t
      in
      let n, _ = List.split t in
      let s    = "  enum {" ^
                 (List.fold_left (fun x y -> x ^ ", _" ^ y)
                    ("DUMB" ^ (string_of_int nlam))
                    (List.map String.uppercase_ascii n)) ^ "} _" ^ v ^ ";" in
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
          n._%s = _%s;
          return (n);
}\n" n v (String.uppercase_ascii n), Printf.sprintf "_%s = make_%s();" n n,
            Printf.sprintf "Value _%s;\n" n
          | _ ->
            Printf.sprintf
              "Value make_%s(Value *env, Value cell, int len) {
          Value n;
          n._%s = _%s;
          n.cell = malloc (sizeof(Value));
          *(n.cell) = cell;
          return (n);
}\n" n v (String.uppercase_ascii n),
            Printf.sprintf "_%s = make_closure(make_%s, NULL, 0);" n n,
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
    break;"
          (String.uppercase_ascii v) v v
      in
      def_ctx tl (context @ t) (types ^ s) (nd ^ fn) (nlam + 1)
        texpr (tc ^ ntc) (ist ^ nin) (mn ^ m) (tp ^ t')
  in
  let s    = read_from_file f    in
  let t, _ = Lexer.lexer s 0 0   in
  let t, _ = List.split t        in
  let f    = Parser.parse_tops t in
  let c, t, n, e, lt, i, m, tp = def_ctx f Types.initial_ctx "" "" 0 [] "" "" "" "" in
  Utils.print_context c;
  let oc = open_out "out.c"      in
  Printf.fprintf oc "%s\n" (Closure.decls_to_c e tp m 0);
  close_out oc;
  let oc = open_out "core.h"     in
  Core.core oc lt t i n
