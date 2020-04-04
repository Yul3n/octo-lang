exception Syntax_error of string

type token
  = IF
  | WHERE
  | BACKSLASH
  | LPARENT 
  | RPARENT
  | PLUS 
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ARROW
  | DOT
  | CONS
  | UNION
  | COMMA
  | LBRACKET
  | RBRACKET
  | EQUAL
  | IDENT of string
  | FLOAT of float
  | INT   of int 



let rec lexer input pos linum rel_pos =
  let catch pos chr =
    try
      match String.get input pos with
        c when c = chr -> true
      | _              -> false
  with _ -> false
  in
  let is_digit chr = (Char.code('0') <= Char.code chr) && (Char.code('9') >= Char.code chr)
  in
  let is_alpha chr = (Char.code('A') <= Char.code chr) && (Char.code('Z') >= Char.code chr)
                     || (Char.code('a') <= Char.code chr) && (Char.code('z') >= Char.code chr)
  in
  let is_ident chr = is_alpha chr || is_digit chr || chr = '_'
  in
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
    | '.'  -> DOT       :: lexer input (pos + 1) linum (rel_pos + 1)
    | '\\' -> BACKSLASH :: lexer input (pos + 1) linum (rel_pos + 1)
    | '('  -> LPARENT   :: lexer input (pos + 1) linum (rel_pos + 1)
    | ')'  -> RPARENT   :: lexer input (pos + 1) linum (rel_pos + 1)
    | ']'  -> LBRACKET  :: lexer input (pos + 1) linum (rel_pos + 1)
    | '['  -> RBRACKET  :: lexer input (pos + 1) linum (rel_pos + 1)
    | ','  -> COMMA     :: lexer input (pos + 1) linum (rel_pos + 1)
    | '*'  -> TIMES     :: lexer input (pos + 1) linum (rel_pos + 1)
    | '@'  -> UNION     :: lexer input (pos + 1) linum (rel_pos + 1)
    | '-'  ->
      begin
        match catch (pos + 1) '>' with
          true  -> ARROW :: lexer input (pos + 2) linum (rel_pos + 2)
        | false -> MINUS :: lexer input (pos + 1) linum (rel_pos + 1)
      end
    | ':'  ->
      begin
        match catch (pos + 1) ':' with
          true  -> ARROW :: lexer input (pos + 2) linum (rel_pos + 2)
        | false -> raise (Syntax_error ("Unexpected character ':', line " ^
                                        (string_of_int linum) ^ ", character " ^
                                        string_of_int(rel_pos)))
      end
    | n when is_digit n ->

  with _ -> [] 
