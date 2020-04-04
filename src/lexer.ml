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
  | EQUAL
  | IDENT of string
  | FLOAT of float
  | INT   of int 

let rec lexer input pos =
  try
    match String.get input pos with
      '+'  -> PLUS      :: lexer input (pos + 1)
    | '='  -> EQUAL     :: lexer input (pos + 1)
    | '.'  -> DOT       :: lexer input (pos + 1)
    | '\\' -> BACKSLASH :: lexer input (pos + 1)
    | '('  -> LPARENT   :: lexer input (pos + 1)
    | ')'  -> RPARENT   :: lexer input (pos + 1)
    | ']'  -> LBRACKET  :: lexer input (pos + 1)
    | '['  -> RBRACKET  :: lexer input (pos + 1)
    | ','  -> COMMA     :: lexer input (pos + 1)
  with _ -> [] 
