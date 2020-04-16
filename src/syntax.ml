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
  | CONS
  | COMMA
  | LBRACKET
  | RBRACKET
  | AT
  | EXCLAM
  | BLOCK of (token * int) list
  | IDENT of string
  | MINDE of string
  | NUM   of int

type op
  = Plus
  | Minus
  | Times
  | Divide
  | Cons
  | Union
  | Elem

type expr_t
  = TVar  of int
  | TFun  of expr_t * expr_t
  | TOth  of string
  | TList of expr_t

type expr
  = Var    of string
  | Lambda of string * expr
  | App    of expr * expr
  | Num    of int
  | Case   of (expr * expr) list
  | List   of expr list

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
  | TyCase   of (typed_expr * typed_expr) list * expr_t
  | TyList   of typed_expr list * expr_t

type typed_decl
  = TyDecl of string * typed_expr * expr_t
