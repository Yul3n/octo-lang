open Syntax

exception Error of string

let rec free e s =
  match e with
    Var _       
  | Num _                -> []
  | App(l, r)
  | Binop(l, _, r)       -> free l s @ free r s
  | IndVar n when n >= s -> [n]
  | IndVar _            -> []
  | Lambda(_, body)     -> free body (s + 1)

let rec deB e (v, n) =
  match e with
    Var v2 when v = v2 -> IndVar n
  | Var _ as v         -> v
  | Lambda (x, body)   -> Lambda ("", deB (deB body (v, n + 1)) (x, 1))
  | App (l, r)         -> App (deB l (v, n), deB r (v, n))
  | IndVar _ as n      -> n
  | Binop(l, o, r)     -> Binop (deB l (v, n), o, deB r (v, n))
  | Num _ as n         -> n

let rec to_closure expr ctx =
  match expr with
    Num n         -> CloNum n
  | Var _         -> raise (Error "Free variable.")
  | _             -> raise Not_found
