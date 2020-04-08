exception Syntax_error of string

type token
  = WHERE
  | BACKSLASH
  | LPARENT 
  | RPARENT
  | PLUS 
  | MINUS
  | TIMES
  | DIVIDE
  | ARROW
  | EQUAL
  | BLOCK of token list
  | IDENT of string
  | INT   of int

let rec lexer input pos linum rel_pos act_ident =
  let catch pos chr =
    try
      match String.get input pos with
        c when c = chr -> true
      | _              -> false
  with _ -> false
  in
  let unexpected_char linum pos chr =
    raise (Syntax_error ("Unexpected character: '" ^ (String.make 1 chr) ^ "', line " ^
                         (string_of_int linum) ^ ", character " ^ string_of_int pos))
  in

  let is_digit chr = (Char.code('0') <= Char.code chr) &&
                     (Char.code('9') >= Char.code chr)
  in
  let is_alpha chr = (Char.code('A') <= Char.code chr) &&
                     (Char.code('Z') >= Char.code chr) ||
                     (Char.code('a') <= Char.code chr) &&
                     (Char.code('z') >= Char.code chr)
  in
  let is_ident chr = is_alpha chr || is_digit chr || chr = '_' in
  let rec parse_f f str pos =
    match pos with
      len when len = String.length str -> ""
    | _ -> match String.get str pos with
        chr when f chr -> (String.make 1 chr) ^ parse_f f str (pos + 1)
      | _ -> ""
  in
  try
    match String.get input pos with
      '\n' -> let ident = String.length (parse_f ((=) ' ') input (pos + 1)) in
      begin
        match ident with
          len when len = act_ident ->
          lexer input (pos + 1 + ident) (linum + 1) ident len
        | len when len < act_ident -> [], pos + len + 1
        | len when len > act_ident ->
          let block, fpos1 = lexer input (pos + ident + 1) (linum + 1) ident len   in
          print_int fpos1;
          let toks, fpos2  = lexer input (fpos1) (linum + 1) ident act_ident in
          BLOCK(block) :: toks, fpos2
        | _ -> unexpected_char linum pos ' '
      end
    | ' '
    | '\t' -> lexer input (pos + 1) linum (rel_pos + 1) act_ident
    | '+'  -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      PLUS :: toks, fpos
    | '='  -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      EQUAL :: toks, fpos
    | '*'  -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      TIMES :: toks, fpos
    | '/'  -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      DIVIDE :: toks, fpos
    | '\\' -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      BACKSLASH :: toks, fpos
    | '('  -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      LPARENT :: toks, fpos
    | ')'  -> let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
      RPARENT :: toks, fpos
    | '-'  ->
      begin
        match catch (pos + 1) '>' with
          true  ->
          let toks, fpos = lexer input (pos + 2) linum (rel_pos + 1) act_ident in
          ARROW :: toks, fpos
        | false ->
          let toks, fpos = lexer input (pos + 1) linum (rel_pos + 1) act_ident in
          MINUS :: toks, fpos
      end
    | n when is_digit n ->
      let num = parse_f is_digit input pos in
      let len = String.length num          in
      let toks, fpos = lexer input (pos + len) linum (rel_pos + 1) act_ident in
      INT (int_of_string num) :: toks, fpos
    | a when is_alpha a ->
      let ide = parse_f is_ident input pos in
      let len = String.length ide          in
      let toks, fpos = lexer input (pos + len) linum (rel_pos + 1) act_ident in
      begin
        match ide with
          "where" -> WHERE
        | _       ->
          IDENT ide
      end :: toks, fpos
    | c -> unexpected_char linum (rel_pos) c
  with Invalid_argument _ -> [], pos

let string_of_token token =
  match token with
    PLUS  -> "+"
  | MINUS -> "-"
  | EQUAL -> "="
  | TIMES -> "*"
  | _ -> raise (Syntax_error "Not implemented")
