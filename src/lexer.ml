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
  | IDENT of string
  | INT   of int

let rec lexer input pos linum rel_pos =
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
      '\n' -> lexer input (pos + 1) (linum + 1) 0
    | ' '
    | '\t' -> lexer input (pos + 1) linum (rel_pos + 1)
    | '+'  -> PLUS      :: lexer input (pos + 1) linum (rel_pos + 1)
    | '='  -> EQUAL     :: lexer input (pos + 1) linum (rel_pos + 1)
    | '*'  -> TIMES     :: lexer input (pos + 1) linum (rel_pos + 1)
    | '/'  -> DIVIDE    :: lexer input (pos + 1) linum (rel_pos + 1)
    | '\\' -> BACKSLASH :: lexer input (pos + 1) linum (rel_pos + 1)
    | '('  -> LPARENT   :: lexer input (pos + 1) linum (rel_pos + 1)
    | ')'  -> RPARENT   :: lexer input (pos + 1) linum (rel_pos + 1)
    | '-'  ->
      begin
        match catch (pos + 1) '>' with
          true  -> ARROW :: lexer input (pos + 2) linum (rel_pos + 2)
        | false -> MINUS :: lexer input (pos + 1) linum (rel_pos + 1)
      end
    | n when is_digit n ->
      let num = parse_f is_digit input pos in
      let len = String.length num          in
      INT (int_of_string num) :: lexer input (pos + len) linum (rel_pos + len)
    | a when is_alpha a ->
      let ide = parse_f is_ident input pos in
      let len = String.length ide          in
      begin
        match ide with
          "where" -> WHERE
        | _       ->
          IDENT ide
      end :: lexer input (pos + len) linum (rel_pos + len)
    | c -> unexpected_char linum (rel_pos) c
  with Invalid_argument _ -> []

let string_of_token token =
  match token with
    PLUS  -> "+"
  | MINUS -> "-"
  | EQUAL -> "="
  | TIMES -> "*"
  | _ -> raise (Syntax_error "Not implemented")
