open Syntax
open Utils

exception Type_error of string

(* Return the list of all free type variables in the type t. *)
let rec ftv t =
  match t with
    TOth  _     -> []
  | TVar v      -> [v]
  | TFun (l, r) -> (ftv l) @ (ftv r)
  | TList (t)   -> ftv t

let ftv_sch (Forall(v, t)) =
  List.filter (fun x -> List.exists ((<>) x) v) (ftv t)

let ftv_env env =
  List.flatten (List.map ftv_sch (List.map snd env))

(* Apply the substitution subst to the type ty and return it. *)
let rec app_subst subst ty =
  match ty with
    TVar var      ->
    let rec findvar subst =
      match subst with
        []                               -> TVar var
      | (nvar, nty) :: _ when nvar = var -> nty
      | _ :: tl                          -> findvar tl
    in
    findvar subst
  | TOth v        -> TOth v
  | TFun (lt, rt) -> TFun ((app_subst subst lt), (app_subst subst rt))
  | TList t       -> TList (app_subst subst t)

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

let rec chain_compose l =
  match l with
    [] -> []
  | hd :: tl -> compose_subst hd (chain_compose tl)

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
  Forall (List.sort_uniq compare vars, t)

let rec unify t1 t2 =
  let bind var t =
    match var with
      v when TVar v = t -> []
    | v when List.mem v (ftv t) = true ->
      raise (Type_error ("Occurs check failed: infinite datatype, can't unify : " ^
               (string_of_type t1) ^ " and " ^ (string_of_type t2)))
    | _                                -> [var, t]
  in
  match t1, t2 with
    TOth _, TOth _ -> []
  | TFun (l1, r1), TFun (l2, r2) ->
    let sub1 = unify l1 l2 in
    let sub2 = unify (app_subst sub1 r1) (app_subst sub1 r2) in
    compose_subst sub1 sub2
  | t, TVar v | TVar v, t -> bind v t
  | TList t1, TList t2 -> unify t1 t2
  | t1, t2 ->
    raise (Type_error ("Can't unify types: " ^ (string_of_type t1) ^ " and "
                       ^ (string_of_type t2)))

let initial_ctx =
  (* Mathematical operators are of type : int -> int -> int *)
  let op_sch = Forall([], TFun(TOth "int", TFun(TOth "int", TOth "int"))) in
  ["suml", op_sch;
   "difl", op_sch;
   "timl", op_sch;
   "divl", op_sch;
   (* Forall a, the type of cons is a -> a list -> a list *)
   "conl", Forall([0], TFun(TVar 0, TFun(TList (TVar 0), TList (TVar 0))));
   (* Forall a, the type of union is a list -> a list -> a list *)
   "unil", Forall([0], TFun(TList(TVar 0), TFun(TList (TVar 0), TList (TVar 0))));
   (* Forall a, the type of the indexing operator is a list -> int -> a *)
   "indl", Forall([0], TFun(TList(TVar 0), TFun(TOth "int", TVar 0)));
   (* Forall a, the type of head is a list -> a *)
   "head", Forall([0], TFun(TList(TVar 0), TVar 0));
   (* Forall a, the type of tail is a list -> a list *)
   "tail", Forall([0], TFun(TList(TVar 0), TList(TVar 0)))]

let rec unify_lst lst nvar t ctx subst exprs =
      match lst with
        []       -> t, nvar, subst, exprs
      | hd :: tl ->
        let tmpctx          = subst_context subst ctx in
        let s1, tt, nvar, e = infer hd tmpctx nvar    in
        let s2              = unify tt t              in
        let sf              = compose_subst s2 s1     in
        unify_lst tl nvar (app_subst sf t) ctx (compose_subst subst sf) (exprs @ [e])

and infer expr context nvar =
  match expr with
    Lambda (var, body) ->
    let var_t                = TVar nvar                              in
    let tmp_ctx              = (var, (Forall ([], var_t))) :: context in
    let s, body_t, nvar, b   = infer body tmp_ctx (nvar + 1)          in
    let t                    = TFun ((app_subst s var_t), body_t)     in
    s, t, nvar, TyLambda (var, b, t)
  | Num n -> [], TOth "int", nvar, TyNum (n, TOth "int")
  | Var var ->
    begin
      match List.assoc_opt var context with
        None     -> raise (Type_error ("Use of an unbound variable:" ^ var))
      | Some sch -> let Forall(l, _) = sch in
        let t = (inst sch nvar) in
        [], t, (nvar + List.length l), TyVar (var, t)
    end
  | App (fn, arg) ->
    let res_t              = TVar nvar in
    let s1, fun_t, nvar, l = infer fn context (nvar + 1) in
    let s2, arg_t, nvar, r = infer arg (subst_context s1 context) nvar in
    let s3                 = unify (app_subst s2 fun_t) (TFun (arg_t, res_t)) in
    let fs                 = compose_subst s3 (compose_subst s2 s1) in
    let t                  = (app_subst s3 res_t) in
    fs, t, nvar, TyApp (l, r, t)
  | Case cases ->
    let patterns, exprs = List.split cases in
    let pt, nvar, s1, p = unify_lst patterns (nvar + 1) (TVar nvar) context [] [] in
    let tmp_ctx         = subst_context s1 context in
    let et, nvar, s2, e = unify_lst exprs (nvar + 1) (TVar nvar) tmp_ctx [] [] in
    let sf              = compose_subst s2 s1 in
    let t               = TFun (app_subst s1 pt, app_subst s2 et) in
    sf, t, nvar, TyCase(List.combine p e, t)
  | List l ->
    let t, nvar, s, e = unify_lst l (nvar + 1) (TVar nvar) context [] [] in
    s, TList t, nvar, TyList(e, TList t)
