open Syntax

let pr = "
        Value *tenv = malloc((len + 1) * sizeof(Value));
        memcpy (tenv + 1, env, len * sizeof(Value));
        *tenv = n;\n"

type closure
  = CloVar  of int
  | CloNum  of int
  | Closure of int list * closure
  | ClosApp of closure * closure
  | CloGVar of string
  | CloCase of (closure * closure) list

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
  | Case c               -> let _, exps = List.split c in
    List.fold_left (@) [] (List.map (fun x -> free x s) exps)

let rec deB e (v, n) =
  match e with
    Var v2 when v = v2 -> IndVar n
  | Var _ as v         -> v
  | Lambda (x, body)   -> Lambda ("", deB (deB body (v, n + 1)) (x, 1))
  | App (l, r)         -> App (deB l (v, n), deB r (v, n))
  | Num _ as n         -> n
  | IndVar _ as n      -> n
  | Binop(l, o, r)     -> Binop (deB l (v, n), o, deB r (v, n))
  | Case c             -> Case (Utils.snd_map (fun x -> deB x (v, n)) c)

let rec to_closure expr =
  match expr with
    Num n           -> CloNum  n
  | Var n           -> CloGVar ("_" ^ n)
  | IndVar n        -> CloVar  n
  | Binop (l, o, r) ->
    let f =
      match o with
        Plus   -> "suml"
      | Minus  -> "difl"
      | Times  -> "timl"
      | Divide -> "divl"
    in
    ClosApp (ClosApp (CloGVar f, to_closure l), to_closure r)
  | Lambda(_, b)    -> Closure (free expr 1, to_closure b)
  | App (l, r)      -> ClosApp (to_closure l, to_closure r)
  | _ -> raise Not_found

let rec closure_to_c clo nlam env ctx =
  match clo with
    CloNum n         -> Printf.sprintf "make_int(%d)" n, "", "", nlam, env
  | CloVar n         ->
    begin
      match n with
        1 -> "n"
      | n -> Printf.sprintf "(*(env + %d))" (n - 2)
    end, "", "", nlam, env
  | ClosApp (f, arg) ->
    let s1, nf, p1, nlam, nv = closure_to_c f nlam env ctx in
    let n = Printf.sprintf "l%d" nlam in
    let s2, na, p2, nlam, v  = closure_to_c arg nlam (n ^ ".clo.env") ctx in
    n, nf ^ na, p1 ^ (Printf.sprintf "%s = %s.clo.lam(%s, %s, len + 1);\n" n s1 nv s2)
                ^ p2 , nlam + 1, v
  | CloGVar v        ->
    begin
      match Char.code (String.get v 1) with
        c when c >= (Char.code 'a') -> v
      | _ ->
        match List.assoc (String.sub v 1 ((String.length v) - 1)) ctx with
          Forall ([], TOth v2) -> "make_" ^ v2 ^ "(" ^ (String.uppercase_ascii v) ^ ")"
        | _                   -> raise (Invalid_argument "shouldn't happened")
    end, "", "", nlam, env
  | Closure (_, body) ->
    let n = Printf.sprintf "l%d" nlam in
    let cbody, nf, c, nnlam, _ = closure_to_c body (nlam + 1) "tenv" ctx in
    n, nf ^ (Printf.sprintf "Value __lam%d(Value *env, Value n, int len) {
     %s" nlam pr) ^ c ^ "return " ^ cbody ^";}\n",
    (Printf.sprintf "%s = make_closure (__lam%d, %s, len + 1);\n" n nlam env),
    nnlam, env
  | _ -> raise Not_found

let rec decls_to_c decls funs body nlam ctx =
  match decls with
    [] ->
    let rec range = function -1 -> [] | n -> n :: range (n - 1) in
    let s = List.fold_left (fun x y -> x ^ (Printf.sprintf "Value l%d;\n" y))
        "" (range (nlam - 1)) in
    "#include \"core.h\"\n#include <stdlib.h>\nValue suml;\nValue difl;\n" ^ s ^
    funs ^
    "\nint main (int argc, char* argv[]) {
        Value *tenv = malloc(sizeof(Value));
        Value n = make_int(atoi(argv[1]));
        *tenv = n;
        int len = 0;
        difl = make_closure(dif, NULL, 0);
        suml = make_closure(sum, NULL, 0);\n" ^
    body ^
    "}\n"
  | hd :: tl ->
    match hd with
      Decl (v, b) when v = "main"->
      let nbody, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
          nlam "tenv" ctx
      in
      decls_to_c tl (funs ^ nf) (body ^ b ^ "\nfree (tenv);\nreturn(" ^
                                 nbody ^ ").clo.lam(NULL, n, 0).n.value;\n") nlam ctx
    | Decl (v, b) ->
      let fn, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
          nlam "tenv" ctx
      in
      let f = Printf.sprintf "Value _%s;\n" v in
      decls_to_c tl (funs ^ nf ^ f) (body ^ b ^ (Printf.sprintf "_%s = %s;\n" v fn)) nlam ctx
    | _           ->
      decls_to_c tl funs body nlam ctx
