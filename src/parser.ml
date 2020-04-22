open Syntax
open Lexer

let parse_error str =
  raise (Syntax_error ("Parser error: " ^ str))

let rec reduce exprs =
    match exprs with
      []               -> raise (Syntax_error "empty tokens")
    | fst :: snd :: tl -> reduce (App(fst, snd) :: tl)
    | hd :: []         -> hd

let rec parse_args tokens =
  match tokens with
    []            -> [], tokens
  | IDENT v :: tl ->
    let vars, tok = (parse_args tl) in
    v :: vars, tok
  | UNDER :: tl   ->
    let vars, tok = (parse_args tl) in
    "" :: vars, tok
  | _             -> [], tokens

let rec wrap_lam vals expr =
  match vals with
    []       -> expr
  | hd :: tl -> wrap_lam tl (Lambda (hd, expr))

let rec parse_expr tokens exprs is_math =
  let binop l op r =
    App(App(Var op, l), r)
  in
  let rec parse_equ tokens body =
    match tokens with
      IDENT v :: tl ->
      begin
        let vars, tl = parse_args tl in
        match tl with
          EQUAL :: tl ->
          let e, tl  = parse_expr tl [] false in
          parse_equ tl (App (Lambda(v, body), wrap_lam (List.rev vars) (reduce e)))
        | tok :: _ -> parse_error ("Unexpected token: " ^ (Utils.string_of_token tok))
        | []       -> parse_error ("Declaration without a body.")
      end
    | []            -> body, []
    | tok :: _      -> parse_error ("Unexpected token: " ^ (Utils.string_of_token tok))
  in
  let rec parse_mul tokens lval =
    let nval, ntl =
      match tokens with
        TIMES :: tl  -> let rval, tl = parse_expr tl [] false in
        binop lval "timl@" (reduce rval), tl
      | DIVIDE :: tl -> let rval, tl = parse_expr tl [] true in
        binop lval "divl@" (reduce rval), tl
      | MOD :: tl -> let rval, tl = parse_expr tl [] true in
        binop lval "modl@" (reduce rval), tl
      | tl           -> lval, tl
    in
    match ntl with
      TIMES :: _ | DIVIDE :: _ ->
      parse_mul ntl nval
    | _                        -> nval, ntl
  in
  let parse_add tokens lval =
    let nval, ntl =
      match tokens with
        PLUS :: tl  -> let mul_val, tl = parse_expr tl [] true in
        let rval, ntl   = parse_mul tl (reduce mul_val) in
        binop lval "suml@" rval, ntl
      | MINUS :: tl -> let mul_val, tl = parse_expr tl [] true in
        let rval, ntl   = parse_mul tl (reduce mul_val) in
        binop lval "difl@" rval, ntl
      | tl           -> lval, tl
    in
    match ntl with
      TIMES :: _ | DIVIDE :: _ ->
      parse_mul ntl nval
    | _                        -> nval, ntl
  in
  let rec parse_cons tokens =
    let e, tl = parse_expr tokens [] true in
    let l, tl = parse_add tl (reduce e)   in
    match tl with
      CONS :: tl -> let r, tl = parse_cons tl in
      binop l "conl@" r, tl
    | tl         -> l, tl
  in
  let rec parse_union tokens =
    let l, tl = parse_cons tokens in
    match tl with
      AT :: tl -> let r, tl = parse_union tl in
      binop l "unil@" r, tl
    | tl         -> l, tl
  in
  let e, tl =
    match tokens with
    [] -> exprs, []
  | BACKSLASH :: tl ->
    let vars, tl = parse_args tl in
    begin
      match tl with
        ARROW :: tl ->
        let body, tl = parse_expr tl [] false in
        exprs @ [wrap_lam (List.rev vars) (reduce body)], tl
      | tok :: _    -> parse_error ("Unexpected token :" ^ (Utils.string_of_token tok))
      | []          -> parse_error ("Lambda function without body")
    end
  | MINDE var :: tl
  | IDENT var :: tl -> exprs @ [Var var], tl
  | CONS :: tl -> let r, tl = parse_cons tl in
    (Utils.firsts exprs) @ [binop (Utils.last exprs) "conl@" r], tl
  | AT :: tl -> let r, tl = parse_union tl in
    (Utils.firsts exprs) @ [binop (Utils.last exprs) "unil@" r], tl
  | NUM num :: tl -> exprs @ [Num num], tl
  | PLUS :: _ | MINUS :: _ ->
    let lst = Utils.last exprs           in
    let expr, ntl = parse_add tokens lst in
    (Utils.firsts exprs) @ [expr], ntl
  | TIMES :: _ | DIVIDE :: _ | MOD :: _ ->
    let lst       = Utils.last exprs     in
    let expr, ntl = parse_mul tokens lst in
    (Utils.firsts exprs) @ [expr], ntl
  | LPARENT :: tl ->
    (* Parses the token stream until a right parentheses is reached.*)
    let rec parse_rparent tokens exprs =
      match tokens with
        RPARENT :: tl -> reduce exprs, tl
      | []            -> parse_error "Opened parenthesis without a closing one."
      | tl            ->
        let nexpr, ntl = parse_expr tl exprs false in
        parse_rparent ntl nexpr
    in
    let expr, ntl = parse_rparent tl [] in
    exprs @ [expr], ntl
  | WHERE :: BLOCK(bl) :: tl ->
    let b, _ = List.split bl      in
    let lst  = Utils.last exprs   in
    let fsts = Utils.firsts exprs in
    fsts @ [fst (parse_equ b lst)], tl
  | WHERE :: tl ->
    let parse_sequ tokens body =
    match tokens with
      IDENT v :: tl ->
      begin
        let vars, tl = parse_args tl in
        match tl with
          EQUAL :: tl ->
          let e, tl  = parse_expr tl [] false in
          App(Lambda(v, body), wrap_lam (List.rev vars) (reduce e)), tl
        | tok :: _ -> parse_error ("Unexpected token: " ^ (Utils.string_of_token tok))
        | []       -> parse_error ("Declaration without a body.")
      end
    | []            -> body, []
    | tok :: _      -> parse_error ("Unexpected token: " ^ (Utils.string_of_token tok))
  in
    let lst   = Utils.last exprs   in
    let w, tl = parse_sequ tl lst  in
    let fsts  = Utils.firsts exprs in
    fsts @ [w], tl
  | CASE :: tl ->
    let v = "aaa@@@" in
    let e = Var v in
    let te, tl = parse_expr tl [] false in
    begin
      match tl with
        OF :: BLOCK bl :: tl ->
        let parse_case tokens =
          let rec parse_pattern env e body =
            match e with
              Var v2 when Char.code (String.get v2 0) >= Char.code 'a' ->
              env, (App (Lambda (v2, body), env))
            | App (App (Var "conl@", l), r) ->
              let l2, b2 = parse_pattern (App(Var "head", env)) l body in
              let r2, b3 = parse_pattern (App(Var "tail", env)) r b2   in
              App(App(Var "conl@", l2), r2), b3
            | Pair (l, r) ->
              let l2, b2 = parse_pattern (App(Var "fst", env)) l body in
              let r2, b3 = parse_pattern (App(Var "snd", env)) r b2   in
              Pair (l2, r2), b3
            | App (Var v, t)
              when Char.code (String.get v 0) < Char.code 'a' ->
              let t, b = parse_pattern (App (Var "get_b@", env)) t body in
              App (Var v, t), b
            | _ -> e, body
          in
          let rec parse_arr tokens exprs =
            match tokens with
              ARROW :: _ -> reduce exprs, tokens
            | []         -> parse_error "Invalid pattern matching"
            | PIPE :: tl ->
              let e, tl = parse_expr tl [] false in
              parse_arr tl [App (App (Var "lor@", reduce exprs), reduce e)]
            | tl         ->
              let e, tl = parse_expr tl exprs false in
              parse_arr tl e
          in
          let p, tl = parse_arr tokens [] in
          let e2, tl =
            match tl with
              ARROW :: tl -> parse_expr tl [] false
            | _ -> parse_error "Invalid pattern matching"
          in
          let p, b = parse_pattern e p (reduce e2) in
          p, b, tl
        in
        let rec parse_cases tokens =
          match tokens with
            [] -> []
          | l  -> let p, e, tl = parse_case l in
            (p, e) :: parse_cases tl
        in
        let b, _ = List.split bl in
        exprs @ [App(Lambda (v, App(Case(parse_cases b), e)), reduce te)], tl
      | _ -> parse_error "Invalid pattern matching"
    end
  | BLOCK bl :: tl ->
    let b, _ = List.split bl in
    exprs @ [parse_all b []], tl
  | EXCLAM :: tl ->
    let l, tl = parse_expr tl [] false in
    (Utils.firsts exprs) @ [binop (Utils.last exprs) "indl@" (reduce l)], tl
  | LBRACKET :: _ ->
    let rec parse_list tokens =
      match tokens with
        []             -> parse_error "Invalid list declaration"
      | LBRACKET :: RBRACKET :: tl -> [], tl
      | RBRACKET :: tl -> [], tl
      | COMMA :: tl
      | LBRACKET :: tl ->
        let rec parse_elem tokens exprs =
          match tokens with
            []            -> parse_error "Invalid list declaration"
          | COMMA :: _
          | RBRACKET :: _ -> reduce exprs, tokens
          | _             -> let e, t = parse_expr tokens exprs false in
            parse_elem t e
        in
        let e, tl = parse_elem tl [] in
        let l, tl = parse_list tl    in
        e :: l, tl
      | t :: _ -> parse_error ("Unexpected token: " ^ Utils.string_of_token t)
    in
    let l, tl = parse_list tokens in
    exprs @ [List l], tl
  | COMMA :: tl ->
    let rec parse_elem tokens exprs =
      match tokens with
        []           -> parse_error "Invalid pair declaration"
      | COMMA :: _
      | RPARENT :: _ -> reduce exprs, tokens
      | _            -> let e, tl = parse_expr tokens exprs false in
        parse_elem tl e
    in
    let l     = Utils.last exprs in
    let r, tl = parse_elem tl [] in
    (Utils.firsts exprs) @ [Pair (l, r)], tl
  | STR s :: tl -> let l = Utils.string_to_char_list s in
    let l = List.map (fun x -> Char x) l in
    exprs @ [List l], tl
  | CHAR c :: tl -> exprs @ [Char c], tl
  | tok :: _ -> parse_error ("Unexpected token: " ^ (Utils.string_of_token tok))
  in
  match is_math with
    false ->
    begin
      match tl with
        PLUS   :: _
      | MINUS  :: _
      | TIMES  :: _
      | DIVIDE :: _
      | CONS   :: _
      | AT     :: _
      | EXCLAM :: _
      | WHERE  :: _ -> parse_expr tl e false
      | _           -> e, tl
    end
  | true ->
    match tl with
      EXCLAM :: _ -> parse_expr tl e false
    | _           -> e, tl

and parse_all tokens exprs =
  match tokens with
    [] -> reduce exprs
  | tk ->
    let nexpr, ntl = parse_expr tk exprs false in
    parse_all ntl nexpr

let rec parse_tops tokens =
  match tokens with
    []            -> [], []
  | IDENT v :: tl ->
    begin
      match tl with
        IDENT _ :: _
      | EQUAL :: _
      | UNDER :: _ ->
        begin
          let vars, tl = parse_args tl in
          match tl with
            EQUAL :: tl ->
            let e, tl = parse_expr tl [] false in
            let e     = Decl (v, wrap_lam (List.rev vars) (reduce e)) in
            let n, m  = parse_tops tl in
            e :: n, m
          | _                      -> parse_error "Expected a function declaration"
        end
      | _ ->
        let rec parse_cf tokens v v2 =
          match tokens with
            [] -> [], [], v2
          | IDENT v2 :: tl when v = v2 ->
            let v2 =
              match tl with
                IDENT v :: _ -> v
              | _            -> v2
            in
            let p, tl    = parse_expr tl [] false in
            let vars, tl = parse_args tl          in
            begin
              match tl with
                EQUAL :: tl ->
                let e, t      = parse_expr tl [] false in
                let l, tl, v2 = parse_cf t v v2        in
                (reduce p, wrap_lam (List.rev vars) (reduce e)) :: l, tl, v2
              | _ -> parse_error "Invalid function declaration"
            end
          | t -> [], t, v2
        in
        let c, tl, v2 = parse_cf tokens v "" in
        let e = Decl (v, Lambda(v2, App(Case c, Var v2))) in
        let n, m = parse_tops tl in
        e :: n, m
    end
  | TYPE :: IDENT v :: EQUAL :: BLOCK(bl) :: tl
  | TYPE :: IDENT v :: BLOCK ((EQUAL, _) :: bl) :: tl ->
    let rec parse_type tokens t =
      let b, tl =
        match tokens with
          MINDE v :: tl ->
          begin
            match tl with
              []
            | PIPE :: _ -> (v, Forall([], t)), tl
            | _ ->
              let rec parse_multype ?(t=None) tokens =
                match tokens with
                  IDENT v :: ([])
                | IDENT v :: PIPE :: _ ->
                  begin
                    match t with
                      None   -> TOth v
                    | Some t -> TPair(t, TOth v)
                  end, List.tl tokens
                | IDENT v :: tl ->
                  begin
                    match t with
                      None   -> parse_multype tl ~t:(Some(TOth v))
                    | Some t -> parse_multype tl ~t:(Some(TPair(t, TOth v)))
                  end
                | tok :: _ -> parse_error
                                ("Unexpected token: " ^ (Utils.string_of_token tok))
                | [] -> parse_error "Empty type declaration."
              in
              let t2, tl = parse_multype tl in
              (v, Forall([], TFun(t2, t))), tl
          end
        | tok :: _      -> parse_error ("Unexpected token: " ^ (Utils.string_of_token tok))
        | []            -> parse_error "Empty type declaration."
      in
      match tl with
        PIPE :: tl -> let n, tl = (parse_type tl t) in
        b :: n, tl
      | _ -> [b], tl
    in
    let b, _     = List.split bl in
    let types, t = parse_type b (TOth v) in
    begin
      match t with
        [] -> let e = TDef types in
        let n, m = parse_tops tl in
        e :: n, m
      | _  -> parse_error "Invalid type declaration."
    end
  | OPEN :: IDENT v :: tl ->
    let n, m = parse_tops tl in
    n, v :: m
  | _             -> parse_error "Expected a function declaration"
