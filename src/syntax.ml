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
  | DDIVIDE
  | AT
  | MOD
  | EXCLAM
  | OPEN
  | WHEN
  | AND
  | STR   of string
  | CHAR  of char
  | BLOCK of (token * int) list
  | IDENT of string
  | MINDE of string
  | NUM   of float

type expr_t
  = TVar  of int
  | TFun  of expr_t * expr_t
  | TOth  of string
  | TList of expr_t
  | TPair of expr_t * expr_t

type expr
  = Var    of string
  | Lambda of string * expr
  | App    of expr * expr
  | Num    of float
  | Case   of (expr * expr) list
  | List   of expr list
  | Pair   of expr * expr
  | Char   of char

type scheme
  = Forall of int list * expr_t

type fun_decl
  = Decl of string * expr
  | TDef of (string * scheme) list
  | And  of fun_decl

type typed_expr
  = TyLambda of string * typed_expr * expr_t
  | TyVar    of string * expr_t
  | TyApp    of typed_expr * typed_expr * expr_t
  | TyIndVar of int * expr_t
  | TyNum    of float * expr_t
  | TyCase   of (typed_expr * typed_expr) list * expr_t
  | TyList   of typed_expr list * expr_t
  | TyPair   of typed_expr * typed_expr * expr_t
  | TyChar   of char * expr_t

type typed_decl
  = TyDecl of string * typed_expr * expr_t
