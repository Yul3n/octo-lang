type expr_t
  = TInt
  | TVar of int
  | TFun of expr_t * expr_t

type scheme = Forall of int list * expr_t
