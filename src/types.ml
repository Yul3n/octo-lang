open Syntax

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
  let vars, ty = scheme in
  let rec delete_bound vars subst =
    match subst with
      []              -> []
    | (nvar, ty) :: tl ->
      begin
        match List.find_opt ((=) nvar) vars with
          None   -> (nvar, ty)
        | Some _ -> []
      end :: (delete_bound vars tl)
  in
  Forall (vars, (app_subst (delete_bound vars subst) ty))
 
let rec infer expr context nvar =
  match expr with
    Lambda (var, body) ->
    let var_t        = TVar nvar                              in
    let tmp_ctx      = (var, (Forall ([], var_t))) :: context in
    let nsub, body_t = infer body tmp_ctx (nvar + 1)          in
    nsub, TFun ((app_subst nsub var_t), body_t)
  | Num _ -> [], TInt
  |
