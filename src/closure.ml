open Syntax

let pr = "Value *tenv = malloc((len + 1) * sizeof(Value));
     memcpy (tenv + 1, env, len);
     *tenv = n;\n"

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
    Num n           -> CloNum  n
  | Var n           -> CloGVar n
  | IndVar n        -> CloVar  n
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

let rec closure_to_c clo nlam env =
  match clo with
    CloNum n         -> Printf.sprintf "make_int(%d)" n, "", "", nlam, env
  | CloVar n         ->
    begin
      match n with
        1 -> "n"
      | n -> Printf.sprintf "*(env + %d)" (n - 2)
    end, "", "", nlam, env
  | ClosApp (f, arg) ->
    let s1, nf, p1, nlam, nv = closure_to_c f nlam env in
    let n = Printf.sprintf "l%d" nlam in
    let s2, na, p2, nlam, v  = closure_to_c arg nlam (n ^ ".clo.env") in
    n, nf ^ na, p1 ^ (Printf.sprintf "%s = %s.clo.lam(%s, %s, len + 1);\n" n s1 nv s2)
                ^ p2 , nlam + 1, v
  | CloGVar v        ->
    let fn =
      match v with
        "+" -> "sum"
      | "-" -> "dif"
      | "*" -> "tim"
      | "/" -> "div"
      | _   -> v
    in
    Printf.sprintf "%s" fn, "", "", nlam, env
  | Closure (_, body) ->
    let n = Printf.sprintf "l%d" nlam in
    let cbody, nf, c, nnlam, v = closure_to_c body (nlam + 1) "tenv" in
    n, nf ^ (Printf.sprintf "Value __lam%d(Value *env, Value n, int len) {
     %s" nlam pr) ^ c ^ "\nreturn " ^ cbody ^";}\n",
    (Printf.sprintf "%s = make_closure (__lam%d, %s, len + 1);\n" n nlam env),
    nnlam, v

let rec decls_to_c decls funs body nlam =
  match decls with
    [] ->
    let rec range = function -1 -> [] | n -> n :: range (n - 1) in
    let s = List.fold_left (fun x y -> x ^ (Printf.sprintf "Value l%d;\n" y))
        "" (range (nlam - 1)) in
    "#include \"core.h\"
#include <stdlib.h>\n" ^ s ^
    funs ^
    "\nint main (int argc, char* argv[]) {
        Value *tenv = malloc(sizeof(Value));
        Value n = make_int(atoi(argv[1]));
        *tenv = n;
        int len = 0;\n" ^
    body ^
    "}\n"
  | hd :: tl ->
    begin
      match hd with
        Decl (v, b) when v = "main"->
        let nbody, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
            nlam "tenv"
        in
        decls_to_c tl (funs ^ nf) (body ^ b ^ "\n        return(" ^ nbody ^ ").n.value;\n") nlam
      | Decl (v, b) ->
        let fn, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
            nlam "tenv"
        in
        let f =
          Printf.sprintf "Value %s;\n" v
        in
        decls_to_c tl (funs ^ nf ^ f) (body ^ b ^ (Printf.sprintf "%s = %s;\n" v fn))  nlam
    end

let compile f =
  let s = Compile.read_from_file f in
  let _ = Compile.compile s in
  let t, _ = Lexer.lexer s 0 1 0 0  in
  let f = Parser.parse_tops t in
  decls_to_c f "" "" 0
