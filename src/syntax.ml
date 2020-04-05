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
