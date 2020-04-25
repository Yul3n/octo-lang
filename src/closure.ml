open Syntax
open Printf

let pr =
"   Value *tenv = malloc((len + 1) * sizeof(Value));
        memcpy (tenv + 1, env, len * sizeof(Value));
        *tenv = n;\n"

exception Error of string

type closure
  = CloVar  of int * expr_t
  | CloNum  of float * expr_t
  | Closure of int list * closure * expr_t
  | CloApp  of closure * closure * expr_t
  | CloGVar of string * expr_t
  | CloCase of (closure * closure) list * expr_t
  | CloList of closure list * expr_t
  | CloPair of closure * closure * expr_t
  | CloChar of char * expr_t

let rec free e s =
  match e with
    TyVar _
  | TyNum _
  | TyChar _ -> []
  | TyApp(l, r, _) -> free l s @ free r s
  | TyIndVar (n, _) when n >= s -> [n]
  | TyIndVar _ -> []
  | TyLambda(_, body, _) -> free body (s + 1)
  | TyCase (c, _) -> let ps, es = List.split c in
    let freelst = fun y -> List.fold_left (@) [] (List.map (fun x -> free x s) y) in
    (freelst ps) @ freelst es
  | TyList (l, _) -> List.fold_left (@) [] (List.map (fun x -> free x s) l)
  | TyPair (l, r, _) -> free l s @ free r s

let rec deB e (v, n) =
  match e with
    TyVar (v2, t) when v = v2 -> TyIndVar (n, t)
  | TyVar _ as v              -> v
  | TyLambda (x, body, t)     -> TyLambda ("", deB (deB body (x, 1)) (v, n + 1) , t)
  | TyApp (l, r, t)           -> TyApp (deB l (v, n), deB r (v, n), t)
  | TyNum _ as n              -> n
  | TyChar _ as n             -> n
  | TyIndVar _ as n           -> n
  | TyCase (c, t)             -> let p, e = List.split c in
    let m = fun y -> (List.map (fun x -> deB x (v, n + 1)) y) in
    let cc = List.combine (m p) (m e) in
    TyCase (cc, t)
  | TyList (l, t)             -> let nl = List.map (fun x -> deB x (v, n)) l in
    TyList (nl, t)
  | TyPair (l, r, t)           -> TyPair (deB l (v, n), deB r (v, n), t)

let rec to_closure expr =
  match expr with
    TyNum (n, t)       -> CloNum (n, t)
  | TyVar (n, t) when
      (String.get n ((String.length n) - 1)) = '@'
                       -> CloGVar (n, t)
  | TyVar (n, t )      -> CloGVar ("_" ^ n, t)
  | TyIndVar (n, t)    -> CloVar (n, t)
  | TyLambda (_, b, t) -> Closure (free expr 1, to_closure b, t)
  | TyApp (l, r, t)    -> CloApp (to_closure l, to_closure r, t)
  | TyCase (c, t)      -> let p, e = List.split c in
    let cp = List.map to_closure p in
    let ce = List.map to_closure e in
    CloCase (List.combine cp ce, t)
  | TyList (l, t)      -> CloList (List.map to_closure l, t)
  | TyPair (l, r, t)   -> CloPair (to_closure l, to_closure r, t)
  | TyChar (c, t)      -> CloChar (c, t)

let rec closure_to_c clo nlam env  =
  match clo with
    CloNum (n, _) -> sprintf "make_int(%f)" n, "", "", nlam, env
  | CloChar (c, _) -> sprintf "make_char('%c')" c, "", "", nlam, env
  | CloVar (n, _) ->
    begin
      (* If the index is not one, the variable is in the closure's context. *)
      match n with
        1 -> "n"
      | n -> sprintf "(*(env + %d))" (n - 2)
    end, "", "", nlam, env
  | CloApp (f, arg, _) ->
    let s1, nf, p1, nlam, _ = closure_to_c f (nlam + 1) env  in
    let n = sprintf "l%d" nlam in
    let s2, na, p2, nlam, v = closure_to_c arg (nlam + 1) env
    in
    let nv =
      match f with
        CloGVar (_,_) -> "tenv"
      | _             -> s1 ^ ".clo.env"
    in
    n, nf ^ na, p2 ^ p1 ^
                (sprintf "Value %s = %s.clo.lam(%s, %s, len + 1);\n"
                   n s1 nv s2), nlam, v
  | CloGVar (v, _) -> List.hd (String.split_on_char '@' v), "", "", nlam, env
  | Closure (_, body, _) ->
    let n = sprintf "l%d" nlam in
    let cbody, nf, c, nnlam, _ = closure_to_c body (nlam + 1) "tenv"  in
    n, nf ^ (sprintf "Value __lam%d(Value *env, Value n, int len) {
     %s%sfree(tenv);\nreturn(%s);\n}\n" nlam pr c cbody),
    (sprintf "Value %s = make_closure (__lam%d, %s, len + 1);\n" n nlam env),
    nnlam, env
  | CloCase (c, t) ->
    let case_to_c p nlam =
      let equ_to_c l r = sprintf "(intern_eq(%s, %s))._float" l r in
      let rec pattern_to_c e nlam =
        match e with
          CloGVar (v, _) when (Char.code (String.get v 1) >= Char.code 'a') -> "1", nlam, "", ""
        | CloApp (CloApp((CloGVar ("lor@", _)), l, _), r, _) ->
          let l, nlam, lf, lp = pattern_to_c l nlam in
          let r, nlam, rf, rp = pattern_to_c r nlam in
          l ^ "||" ^ r, nlam, lf ^ rf, lp ^ rp
        | CloApp (CloApp((CloGVar ("land@", _)), l, _), r, _) ->
          let l, nlam, lf, lp    = pattern_to_c l nlam in
          let r, rf, rp, nlam, _ = closure_to_c r nlam env in
          l ^ "&&" ^ (equ_to_c r "_True"), nlam, lf ^ rf, lp ^ rp
        | _ ->
          let np, nf2, p4, nlam, _ = closure_to_c e nlam env in
          equ_to_c np "(*(tenv))", nlam, nf2, p4
      in
      match p with
        []           -> "", "", "", nlam, []
      | (f, s) :: tl ->
        let nbody, nf, p2, nlam, _ = closure_to_c s (nlam + 1) env in
        let prelude, postlude =
            match t with
              TFun(TList _, _) ->
              let rec min_len e =
                match e with
                  (CloApp(CloApp(CloGVar ("conl@", _), _, _), l, _)) -> 1 + min_len l
                | _ -> 0
              in
              sprintf "if ((*(tenv)).list.length >= %d){" (min_len f), "}"
            | _ -> "", ""
          in
        let p3, nlam, nf2, p4 =
          let np, nlam, nf2, p4 = pattern_to_c f nlam in
          (sprintf "if (%s) {\n%s\nfree(tenv);\nreturn %s;\n}\n"
             np p2 nbody), nlam + 1, nf2, p4
        in
        prelude ^ p4 ^ p3 ^ postlude, nf ^ nf2, "", nlam, tl
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
      sprintf
        "Value __lam%d(Value *env, Value n, int len) {
        %s
        %s
        %s
        puts(\"Non-exhaustive pattern-matching\");
        exit(1);
}\n" nlam pr p b
    in
    sprintf "l%d" nlam, nf ^ f,
    sprintf "Value l%d = make_closure(__lam%d,tenv, len + 1);" nlam nlam, nlam + 1, env
  | CloList (clo, _) ->
    let lp = sprintf "l%d" nlam in
    let rec l_to_c nlam pos =
      function
        []       ->  "", "", nlam
      | hd :: tl -> let b, f, p, nlam, _ = closure_to_c hd nlam env  in
        let nf, np, nlam = l_to_c nlam (pos + 1) tl in
        let pl = sprintf "*(%s + %d) = %s;\n" lp pos b in
        f ^ nf, p ^ np ^ pl, nlam
    in
    let f, p, nlam = l_to_c nlam 0 clo in
    (sprintf "make_list(%s, %d)" lp (List.length clo)), f,
    (sprintf "Value *%s = malloc (%d * sizeof(Value));\n" lp
       (List.length clo)) ^ p, nlam + 1, env
  | CloPair (l, r, _) ->
    let l, lf, lp, nlam, _ = closure_to_c l nlam env in
    let r, rf, rp, nlam, _ = closure_to_c r nlam env in
    sprintf "make_pair(%s, %s)" l r, lf ^ rf, lp ^ rp, nlam, env

let rec decls_to_c decls funs body nlam  =
  match decls with
    [] ->
    "#include \"core.h\"\n#include \"lib/base.h\"\n#include <stdlib.h>\n#include <stdio.h>\n" ^
    funs ^
    "\nint main (int argc, char* argv[]) {
        if (argc == 1){
            puts(\"Error: the program has to be called with an argument.\");
            exit(1);
        }
        base_init();
        int len = 0;
        \n" ^
    body ^
    "\n}\n"
  | hd :: tl ->
    match hd with
      TyDecl (v, b, t) when v = "main"->
      let get =
        begin
          match t with
            TFun (TOth "float", _) ->
            "int num;
        if (sscanf(argv[1], \"%d\", &num) != 1) {
            puts(\"Error: the input should be a number.\");
            exit (1);

        }
        Value n = make_int(num);\n"
          | _ -> "Value n = c_str_to_octo_str(*(argv + 1));"
        end ^ "Value *tenv = malloc(sizeof(Value));
        *tenv = n;"
      in
      let print = sprintf "print_value(%s.clo.lam(NULL, n, 0));" in
      let nbody, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
          nlam "tenv"
      in
      decls_to_c tl (funs ^ nf) (get ^ body ^ b ^
                                 "\nfree (tenv);\n" ^ (print nbody)
                                 ^ "\n return 0;") nlam
    | TyDecl (v, b, _) ->
      let fn, nf, b, nlam, _ = closure_to_c (to_closure (deB b ("", 1)))
          nlam "tenv"
      in
      let f = sprintf "Value _%s;\n" v in
      decls_to_c tl (funs ^ f ^ nf ) (body ^ b ^ (sprintf "_%s = %s;\n" v fn)) nlam
