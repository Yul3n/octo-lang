type op
  = Equal
  | Plus
  | Minus
  | Times
  | Divide
  | Mod

type expr
  = Var    of string
  | Lambda of string * expr
  | App    of expr * expr
  | Where  of expr * string * expr
  | Binop  of expr * op * expr
  | Num    of int

type fun_decl
  = Decl of string * expr

type expr_t
  = TInt
  | TVar of int
  | TFun of expr_t * expr_t

type scheme
  = Forall of int list * expr_t
