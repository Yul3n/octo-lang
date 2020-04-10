open Syntax

let pr = "int *tenv = malloc((sizeof(env) + 1) * sizeof(int));
     int len = sizeof (env);
     memcpy (tenv + 1, env, len);
     *tenv = n.n.value;"

type closure
  = CloVar  of int
  | CloNum  of int
  | Closure of int list * closure
  | ClosApp of closure * closure
  | CloGVar of string

exception Error of string

let rec free e s =
  match e with
    Var _       
  | Num _                -> []
  | App(l, r)
  | Binop(l, _, r)       -> free l s @ free r s
  | IndVar n when n >= s -> [n]
  | IndVar _             -> []
  | Lambda(_, body)      -> free body (s + 1)

let rec deB e (v, n) =
  match e with
    Var v2 when v = v2 -> IndVar n
  | Var _ as v         -> v
  | Lambda (x, body)   -> Lambda ("", deB (deB body (v, n + 1)) (x, 1))
  | App (l, r)         -> App (deB l (v, n), deB r (v, n))
  | IndVar _ as n      -> n
  | Binop(l, o, r)     -> Binop (deB l (v, n), o, deB r (v, n))
  | Num _ as n         -> n

let rec to_closure expr =
  match expr with
    Num n           -> CloNum n
  | Var n           -> CloGVar n
  | IndVar n        -> CloVar n
  | Binop (l, o, r) ->
    let f =
      match o with
        Plus   -> "+"
      | Minus  -> "-"
      | Times  -> "*"
      | Divide -> "/"
    in
    ClosApp (ClosApp (CloGVar f, to_closure l), to_closure r)
  | Lambda(_, b)    -> Closure (free expr 1, to_closure b)
  | App (l, r)      -> ClosApp (to_closure l, to_closure r)

let rec closure_to_c clo nlam =
  match clo with
    CloNum n         -> Printf.sprintf "make_int(%d)" n, "", nlam
  | CloVar n         ->
    begin
      match n with
        1 -> "n"
      | n -> Printf.sprintf "*(env + %d)" (n - 2)
    end, "", nlam
  | ClosApp (f, arg) ->
    let s1, nf, nlam = closure_to_c f nlam   in
    let s2, na, nlam = closure_to_c arg nlam in
    Printf.sprintf "%s.clo.lam(tenv, %s)" s1 s2, nf ^ na, nlam
  | CloGVar v        ->
    let fn =
      match v with
        "+" -> "sum"
      | "-" -> "dif"
      | "*" -> "tim"
      | "/" -> "div"
      | _   -> v
    in
    Printf.sprintf "make_closure(%s, NULL, 0)" fn, "", nlam
  | Closure (_, body) ->
    let cbody, nf, nnlam = closure_to_c body (nlam + 1) in
    Printf.sprintf "make_closure(__lam%d, tenv, sizeof(tenv))" nlam, nf ^ (Printf.sprintf "
     Value __lam%d(int *env, Value n) {
     %s
     return %s;}" nlam pr cbody), nnlam

let rec decls_to_c decls funs body nlam =
  match decls with
    [] ->
    "#include \"core.h\"
#include <stdlib.h>\n" ^
    funs ^
    "\nint main (int argc, char* argv[]) {
         int *tenv = NULL;
         Value n = make_int(atoi(argv[1]));
         return (" ^
    body ^
    ").n.value;\n}\n"
  | hd :: tl ->
    begin
      match hd with
        Decl (v, b) when v = "main"->
        let body, nf, nlam = closure_to_c (to_closure (deB b ("", 1))) nlam in
        decls_to_c tl (funs ^ nf) body nlam
      | Decl (v, b) ->
        let fn, nf, nlam = closure_to_c (to_closure (deB b ("", 1))) nlam in
        let f =
          Printf.sprintf "Value %s(int *env, Value n) {%s return %s;}" v pr fn
        in
        decls_to_c tl (funs ^ nf ^ f) body nlam
    end

let compile f =
  let s = Compile.read_from_file f in
  let _ = Compile.compile s in
  let t, _ = Lexer.lexer s 0 1 0 0  in
  let f = Parser.parse_tops t in
  decls_to_c f "" "" 0
