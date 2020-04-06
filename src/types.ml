open Syntax
open Utils

type expr_t
  = TInt
  | TVar of int
  | TFun of expr_t * expr_t

type scheme = Forall of int list * expr_t

let rec app_subst subst ty =
  match ty with
    TVar var ->
    let rec findvar subst =
      match subst with
        []                               -> TVar var
      | (nvar, nty) :: _ when nvar = var -> nty
      | _ :: tl                          -> findvar tl
    in
    findvar subst
  | TInt -> TInt
  | TFun (ltype, rtype) -> TFun ((app_subst subst ltype), (app_subst subst rtype))

let subst_scheme subst scheme =
  let Forall(vars, ty) = scheme in
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

let subst_context subst context =
  snd_map (subst_scheme subst) context

let compose_subst subst1 subst2 =
  (snd_map (app_subst subst1) subst2) @ subst1

let rec infer expr context nvar =
  match expr with
    Lambda (var, body)       ->
    let var_t        = TVar nvar                              in
    let tmp_ctx      = (var, (Forall ([], var_t))) :: context in
    let nsub, body_t,nvar = infer body tmp_ctx (nvar + 1)     in
    nsub, TFun ((app_subst nsub var_t), body_t), nvar
  | Num _                   -> [], TInt, nvar
  | Where (body, var, expr) ->
    let nsub1, expr_t, nvar = infer expr context nvar in
    let tmp_ctx      = (var, (Forall ([], expr_t))) :: context in
    let nsub2, body_t, nvar = infer body (subst_context nsub1 tmp_ctx) nvar in
    (compose_subst nsub1 nsub2), body_t, nvar
  | _     -> raise Not_found
