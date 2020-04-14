open Syntax

let pr = "
        Value *tenv = malloc((len + 1) * sizeof(Value));
        memcpy (tenv + 1, env, len * sizeof(Value));
        *tenv = n;\n"

type closure
  = CloVar  of int * expr_t
  | CloNum  of int * expr_t
  | Closure of int list * closure * expr_t
  | CloApp of closure * closure * expr_t
  | CloGVar of string * expr_t
  | CloCase of (closure * closure) list * expr_t

exception Error of string

let rec free e s =
  match e with
    TyVar _
  | TyNum _ -> []
  | TyApp(l, r, _)
  | TyBinop(l, _, r, _) -> free l s @ free r s
  | TyIndVar (n, _) when n >= s -> [n]
  | TyIndVar _ -> []
  | TyLambda(_, body, _) -> free body (s + 1)
  | TyCase (c, _) -> let _, exps = List.split c in
    List.fold_left (@) [] (List.map (fun x -> free x s) exps)

let rec deB e (v, n) =
  match e with
    TyVar (v2, t) when v = v2 -> TyIndVar (n, t)
  | TyVar _ as v              -> v
  | TyLambda (x, body, t)     -> TyLambda ("", deB (deB body (v, n + 1)) (x, 1), t)
  | TyApp (l, r, t)           -> TyApp (deB l (v, n), deB r (v, n), t)
  | TyNum _ as n              -> n
  | TyIndVar _ as n           -> n
  | TyBinop(l, o, r, t)       -> TyBinop (deB l (v, n), o, deB r (v, n), t)
  | TyCase (c, t)             -> TyCase (Utils.snd_map (fun x -> deB x (v, n + 1)) c, t)

let rec to_closure expr =
  match expr with
    TyNum (n, t)         -> CloNum (n, t)
  | TyVar (n, t)         -> CloGVar ("_" ^ n, t)
  | TyIndVar(n, t)       -> CloVar (n, t)
  | TyBinop (l, o, r, t) ->
    let f =
      match o with
        Plus   -> "suml"
      | Minus  -> "difl"
      | Times  -> "timl"
      | Divide -> "divl"
    in
    CloApp (CloApp (CloGVar (f, (TFun (TFun (TOth "int", TOth "int"), TOth "int"))),
                      to_closure l, (TFun (TOth "int", TOth "int"))), to_closure r, t)
  | TyLambda(_, b, t)   -> Closure (free expr 1, to_closure b, t)
  | TyApp (l, r, t)     -> CloApp (to_closure l, to_closure r, t)
  | TyCase (c, t)       -> let p, e = List.split c in
    let cp = List.map to_closure p in
    let ce = List.map to_closure e in
    CloCase (List.combine cp ce, t)

let rec closure_to_c clo nlam env ctx =
  match clo with
    CloNum (n, _) -> Printf.sprintf "make_int(%d)" n, "", "", nlam, env
  | CloVar (n, _) ->
    begin
      match n with
        1 -> "n"
      | n -> Printf.sprintf "(*(env + %d))" (n - 2)
    end, "", "", nlam, env
  | CloApp (f, arg, t) ->
    let s1, nf, p1, nlam, nv = closure_to_c f nlam env ctx in
    let n = Printf.sprintf "l%d" nlam in
    let s2, na, p2, nlam, v =
      match t with
        TFun (_, _) -> closure_to_c arg (nlam + 1)  (n ^ ".clo.env") ctx
      | _ -> closure_to_c arg (nlam + 1) env ctx
    in
    begin
    match f with
      CloGVar _ ->
      n, nf ^ na, p1 ^  p2 ^ (Printf.sprintf "%s = %s.clo.lam(tenv, %s, len + 1);\n" n s1 s2) ,
    nlam, v
    | _ -> n, nf ^ na, p1 ^  p2 ^ (Printf.sprintf "%s = %s.clo.lam(%s, %s, len + 1);\n" n s1 nv s2) ,
    nlam, v
           end
  | CloGVar (v, _) ->
    begin
      match Char.code (String.get v 1) with
        c when c >= (Char.code 'a') -> v
      | _ ->
        match List.assoc (String.sub v 1 ((String.length v) - 1)) ctx with
          Forall ([], TOth v2) -> "make_" ^ v2 ^ "(" ^ (String.uppercase_ascii v) ^ ")"
        | _                   -> raise (Invalid_argument "shouldn't happened")
    end, "", "", nlam, env
  | Closure (_, body, _) ->
    let n = Printf.sprintf "l%d" nlam in
    let cbody, nf, c, nnlam, _ = closure_to_c body (nlam + 1) "tenv" ctx in
    n, nf ^ (Printf.sprintf "Value __lam%d(Value *env, Value n, int len) {
     %s" nlam pr) ^ c ^ "return " ^ cbody ^";}\n",
    (Printf.sprintf "%s = make_closure (__lam%d, %s, len + 1);\n" n nlam env),
    nnlam, env
  | CloCase (c, t) ->
    let type_to_c t =
      match t with
        TFun(TOth v, _) -> "._" ^ v
      | _      -> raise (Error "invalid pattern matching")
    in
    let n = Printf.sprintf "l%d" nlam in
    let case_to_c p nlam =
      match p with
        []           -> "", "", "", nlam, []
      | (f, s) :: tl ->
        let nbody, nf, p2, nlam, _ = closure_to_c s (nlam + 1) env ctx in
        let p3, nlam, nf2, p4 =
          match f with
          CloGVar (v, _) when (Char.code (String.get v 1) >= Char.code 'a') ->
          Printf.sprintf "else{\n%s = %s;\n}" n nbody, nlam, "", ""
        | _ ->
          let np, nf2, p4, nlam, _ = closure_to_c f (nlam) env ctx in
          (Printf.sprintf "if ((%s%s) == ((*tenv)%s)) {\n%s = %s;\n}\n"
             np (type_to_c t) (type_to_c t) n nbody), nlam + 1, nf2, p4
        in
        p3, nf ^ nf2, p2 ^ p4, nlam, tl
    in
    let rec cases_to_c n nf p nlam l =
      match l with
        [] -> n, nf, p, nlam, env
      | l  ->
        let n2, nf2, p2, nlam, tl = case_to_c l nlam in
        cases_to_c (n ^ n2) (nf ^ nf2) (p ^ p2) nlam tl
    in
    let b, nf, p, nlam, _ = cases_to_c "" "" "" nlam c in
    let f =
      Printf.sprintf
        "Value __lam%d(Value *env, Value n, int len) {
        %s
        %s
        return %s;
}" nlam pr b n
    in
    Printf.sprintf "make_closure(__lam%d,tenv, len + 1)" nlam, nf ^ f, p, nlam + 1, env

let rec decls_to_c decls funs body nlam ctx =
  match decls with
    [] ->
    let rec range = function -1 -> [] | n -> n :: range (n - 1) in
    let s = List.fold_left (fun x y -> x ^ (Printf.sprintf "Value l%d;\n" y))
        "" (range (nlam - 1)) in
    "#include \"core.h\"\n#include <stdlib.h>\n#include <stdio.h>\nValue suml;\nValue difl;
Value divl;\nValue timl;\n" ^ s ^
    funs ^
    "\nint main (int argc, char* argv[]) {
        Value *tenv = malloc(sizeof(Value));
        Value n = make_int(atoi(argv[1]));
        *tenv = n;
        int len = 0;
        difl = make_closure(dif, NULL, 0);
        divl = make_closure(dv, NULL, 0);
        timl = make_closure(tim, NULL, 0);
        suml = make_closure(sum, NULL, 0);\n" ^
    body ^
    "}\n"
  | hd :: tl ->
    match hd with
      TyDecl (v, b, _) when v = "main"->
      let nbody, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
          nlam "tenv" ctx
      in
      decls_to_c tl (funs ^ nf) (body ^ b ^ "\nfree (tenv);\nprintf(\"%d\\n\"," ^
                                 nbody ^ ".clo.lam(NULL, n, 0)._int);\n return 0;") nlam ctx
    | TyDecl (v, b, _) ->
      let fn, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
          nlam "tenv" ctx
      in
      let f = Printf.sprintf "Value _%s;\n" v in
      decls_to_c tl (funs ^ nf ^ f) (body ^ b ^ (Printf.sprintf "_%s = %s;\n" v fn)) nlam ctx
