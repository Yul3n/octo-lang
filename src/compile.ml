open Syntax

let read_from_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let compile f =
  let rec def_ctx decls context =
    match decls with
      [] -> context
    | Decl(v, body) :: tl ->
      let s, t, _ = Types.infer body context 0 in
      let n_ctx      = (Types.subst_context s context) @ [v, Types.gen context t] in
      def_ctx tl n_ctx
  in
  let s    = read_from_file f    in
  let t, _ = Lexer.lexer s 0 0   in
  let f    = Parser.parse_tops t in
  let ctx  = def_ctx f []        in
  Utils.print_context ctx;
  Closure.decls_to_c f "" "" 0
