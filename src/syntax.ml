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
  | UNDER
  | BLOCK of (token * int) list
  | IDENT of string
  | INT   of int

type op
  = Plus
  | Minus
  | Times
  | Divide

type expr
  = Var    of string
  | Lambda of string * expr
  | App    of expr * expr
  | Binop  of expr * op * expr
  | Num    of int
  | IndVar of int

type fun_decl
  = Decl of string * expr

type expr_t
  = TInt
  | TVar of int
  | TFun of expr_t * expr_t

type scheme
  = Forall of int list * expr_t
