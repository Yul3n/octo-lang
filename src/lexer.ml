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
  | COMMA
  | LBRACKET
  | RBRACKET
  | CONS
  | APPEND
  | WILDCARD
  | IDENT  of string
  | FLOAT  of float
  | INT    of int
  | STRING of string 

let rec lexer input pos =
  match String.length input with
    len when len = pos -> []
  | _ -> 
