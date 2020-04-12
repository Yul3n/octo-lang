type token
  = WHERE
  | BACKSLASH
  | PIPE
  | TYPE
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
  | NUM   of int

type op
  = Plus
  | Minus
  | Times
  | Divide

type expr_t
  = TVar of int
  | TFun of expr_t * expr_t
  | TOth of string

type expr
  = Var    of string
  | Lambda of string * expr
  | App    of expr * expr
  | Binop  of expr * op * expr
  | Num    of int
  | IndVar of int
  | TDef   of (string * expr_t) list

type fun_decl
  = Decl of string * expr

type scheme
  = Forall of int list * expr_t
