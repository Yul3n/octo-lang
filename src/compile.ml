open Syntax

let read_from_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let compile f =
  let rec def_ctx decls context types nd nlam texpr =
    match decls with
      [] -> context, types, nd, texpr
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
      def_ctx tl n_ctx types nd n (texpr @ [TyDecl (v, e, t)])
    | TDef t :: tl ->
      let v =
        match snd (List.hd t) with
          Forall ([], (TOth v)) -> v
        | _ -> raise (Invalid_argument "Shouldn't happend")
      in
      let n, _ = List.split t in
      let s    = "  enum {" ^
                 (List.fold_left (fun x y -> x ^ ", _" ^ y)
                    "DUMB" (List.map String.uppercase_ascii n)) ^
                 "} _" ^ v ^ ";" in
      let fn =
        Printf.sprintf
          "Value make_%s(int val){
          Value n;
          n._%s = val;
          return (n);
}" v v
      in
      def_ctx tl (context @ t) (types ^ s) (nd ^ fn) nlam texpr
  in
  let s          = read_from_file f        in
  let t, _       = Lexer.lexer s 0 0       in
  let t, _       = List.split t            in
  let f          = Parser.parse_tops t     in
  let c, t, n, e = def_ctx f Types.initial_ctx "" "" 0 [] in
  Utils.print_context c;
  let oc = open_out "out.c"      in
  Printf.fprintf oc "%s\n" (Closure.decls_to_c e "" "" 0 c);
  close_out oc;
  let oc = open_out "core.h"     in
  Core.core oc t n
