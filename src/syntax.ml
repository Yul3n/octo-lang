type token
  = WHERE
  | CASE
  | OF
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
  | MINDE of string
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
  | Case   of (expr * expr) list

type scheme
  = Forall of int list * expr_t

type fun_decl
  = Decl of string * expr
  | TDef of (string * scheme) list

type typed_expr
  = TyLambda of string * typed_expr * expr_t
  | TyVar    of string * expr_t
  | TyApp    of typed_expr * typed_expr * expr_t
  | TyIndVar of int * expr_t
  | TyNum    of int * expr_t
  | TyBinop  of typed_expr * op * typed_expr * expr_t
  | TyCase   of (typed_expr * typed_expr) list * expr_t
