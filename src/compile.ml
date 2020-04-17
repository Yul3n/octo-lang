open Syntax

let read_from_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let compile f =
  let rec def_ctx decls context types nd nlam texpr tc ist =
    match decls with
      [] -> context, types, nd, texpr, tc, ist
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
      def_ctx tl n_ctx types nd n (texpr @ [TyDecl (v, e, t)]) tc ist
    | TDef t :: tl ->
      let rec get_t_n t =
        match t with
          TOth v     -> v
        | TFun(_, t) -> get_t_n t
        | _          -> raise (Invalid_argument "Shouldn't happend")
      in
      let v =
        match snd (List.hd t) with
          Forall (_, t) -> get_t_n t
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
      let ntc = ",\n  " ^ (String.uppercase_ascii v) in
      let nin = Printf.sprintf
          "case %s :
    for (int i = 0; i < l2.list.length; i ++)
      if ((*(l1.list.list + i))._%s != (*(l2.list.list + i))._%s)
        return (make_int(0));
    break;"
          (String.uppercase_ascii v) v v
      in
      def_ctx tl (context @ t) (types ^ s) (nd ^ fn) nlam texpr (tc ^ ntc) (ist ^ nin)
  in
  let s                 = read_from_file f    in
  let t, _              = Lexer.lexer s 0 0   in
  let t, _              = List.split t        in
  let f                 = Parser.parse_tops t in
  let c, t, n, e, lt, i = def_ctx f Types.initial_ctx "" "" 0 [] "" "" in
  Utils.print_context c;
  let oc = open_out "out.c"      in
  Printf.fprintf oc "%s\n" (Closure.decls_to_c e "" "" 0 c);
  close_out oc;
  let oc = open_out "core.h"     in
  Core.core oc lt t i n
