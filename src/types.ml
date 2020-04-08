open Syntax
open Utils

exception Error of string

type expr_t
  = TInt
  | TVar of int
  | TFun of expr_t * expr_t

type scheme = Forall of int list * expr_t

(* Return the list of all free type variables in the type t. *)
let rec ftv t =
  match t with
    TInt        -> []
  | TVar v      -> [v]
  | TFun (l, r) -> (ftv l) @ (ftv r)

let ftv_sch (Forall(v, t)) =
  List.filter (fun x -> List.exists ((<>) x) v) (ftv t)

let ftv_env env =
  List.flatten (List.map ftv_sch (List.map snd env))

(* Apply the substitution subst to the type ty and return it. *)
let rec app_subst subst ty =
  match ty with
    TVar var            ->
    let rec findvar subst =
      match subst with
        []                               -> TVar var
      | (nvar, nty) :: _ when nvar = var -> nty
      | _ :: tl                          -> findvar tl
    in
    findvar subst
  | TInt                -> TInt
  | TFun (ltype, rtype) -> TFun ((app_subst subst ltype), (app_subst subst rtype))

(* Apply a substitution to a scheme by ignoring bound variables. *)
let subst_scheme subst (Forall(vars, ty)) =
  let rec delete_bound vars subst =
    match subst with
      []              -> []
    | (nvar, ty) :: tl ->
      begin
        match List.find_opt ((=) nvar) vars with
          None   -> (nvar, ty) :: (delete_bound vars tl)
        | Some _ -> delete_bound vars tl
      end
  in
  Forall (vars, (app_subst (delete_bound vars subst) ty))

(* Apply a substitution to a context by applying it to every scheme in the
   context. *)
let subst_context subst context =
  snd_map (subst_scheme subst) context

(* Apply the substitution 1 to the 2. *)
let compose_subst subst1 subst2 =
  (snd_map (app_subst subst1) subst2) @ subst1

let inst scheme nvar =
  let Forall(vars, t) = scheme in
  let rec fresh l nvars =
    match l with
      []      -> []
    | _ :: tl -> TVar nvars :: fresh tl (nvars + 1)
  in
  let new_vars = fresh vars nvar in
  let subst = List.combine vars new_vars in
  app_subst subst t

let gen env t =
  let e    = ftv_env env in

  let vars =
    match e with
      [] -> ftv t
    | _  -> List.filter (fun x -> List.exists ((<>) x) e) (ftv t)
  in
  Forall (vars, t)

let rec unify t1 t2 =
  let bind var t =
    match var with
      v when TVar v = t                            -> []
    | v when List.find_opt ((=) v) (ftv t) <> None ->
      raise (Error "Occurs check failed: infinite datatype.")
    | _                                            -> [var, t]
  in
  match t1, t2 with
    TInt, TInt                   -> []
  | TFun (l1, r1), TFun (l2, r2) ->
    let sub1 = unify l1 l2 in
    let sub2 = unify (app_subst sub1 r1) (app_subst sub1 r2) in
    compose_subst sub1 sub2
  | t, TVar v | TVar v, t        -> bind v t
  | _, _                         -> raise (Error "Can't unify types")

let rec infer expr context nvar =
  match expr with
    Lambda (var, body)       ->
    let var_t        = TVar nvar                              in
    let tmp_ctx      = (var, (Forall ([], var_t))) :: context in
    let sub, body_t,nvar = infer body tmp_ctx (nvar + 1)     in
    sub, TFun ((app_subst sub var_t), body_t), nvar
  | Num _                   -> [], TInt, nvar
  | Where (body, var, expr) ->
    let sub1, expr_t, nvar = infer expr context nvar in
    let tmp_ctx      = (var, (Forall ([], expr_t))) :: context in
    let sub2, body_t, nvar = infer body (subst_context sub1 tmp_ctx) nvar in
    (compose_subst sub2 sub1), body_t, nvar
  | Var var                 ->
    begin
      match fst_lookup var context with
        None     -> raise (Error ("Use of an unbound variable:" ^ var))
      | Some sch -> let Forall(l,_) = sch in
        [], (inst sch nvar), (nvar + List.length l)
    end
  | App (fn, arg)           ->
    let res_t             = TVar nvar in
    let sub1, fun_t, nvar = infer fn context (nvar + 1) in
    let sub2, arg_t, nvar = infer arg (subst_context sub1 context) nvar in
    let sub3              = unify (app_subst sub2 fun_t) (TFun (arg_t, res_t)) in
    let fsub              = compose_subst sub3 (compose_subst sub2 sub1) in
    fsub, (app_subst sub3 res_t), nvar
  | Binop (lval, _, rval)   ->
    let ls1, lt, nvar = infer lval context nvar in
    let ls2           = unify lt TInt           in
    let ls3           = compose_subst ls2 ls1   in
    let rs1, rt, nvar = infer rval (subst_context ls3 context) nvar in
    let rs2           = unify rt TInt           in
    compose_subst rs2 (compose_subst rs1 ls3), TInt, nvar
