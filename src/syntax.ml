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
  | If     of expr * expr * expr
  | Binop  of expr * op * expr
  | Num    of int
  | Mu     of string * expr
