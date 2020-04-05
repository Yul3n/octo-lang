open Syntax
open Lexer

let parse_error str =
  raise (Syntax_error ("Parser error: unexpected token:" ^ str))

let rec reduce exprs =
    match exprs with
      []               -> raise (Syntax_error "empty tokens")
    | fst :: snd :: tl -> reduce (App(fst, snd) :: tl)
    | hd :: []         -> hd

let rec parser tokens exprs =
  let rec parse_mul tokens lval =
    let nval, ntl =
      match tokens with
        TIMES :: tl  -> let rval, tl = parser tl [] in
        Binop (lval, Times, (reduce rval)), tl
      | DIVIDE :: tl -> let rval, tl = parser tl [] in
        Binop (lval, Divide, reduce(rval)), tl
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
        PLUS :: tl  -> let mul_val, tl = parser tl []  in
        let rval, ntl   = parse_mul tl (reduce mul_val) in
        Binop (lval, Plus, rval), ntl
      | MINUS :: tl -> let mul_val, tl = parser tl []  in
        let rval, ntl   = parse_mul tl (reduce mul_val) in
        Binop (lval, Minus, rval), ntl
      | tl           -> lval, tl
    in
    match ntl with
      TIMES :: _ | DIVIDE :: _ ->
      parse_mul ntl nval
    | _                          -> nval, ntl
  in
  match tokens with
    [] -> exprs, []
  | BACKSLASH :: tl ->
    begin
      match tl with
        IDENT ident :: tl ->
        begin
          match tl with
            ARROW :: tl ->
            let body, tl = parser tl [] in
            exprs @ [Lambda (ident, (reduce body))], tl
          | tok :: _    -> parse_error (string_of_token tok)
          | []          -> parse_error "Lambda function without a body."
        end
      | tok :: _          -> parse_error (string_of_token tok)
      | []                -> parse_error "Lambda function without a body."
    end
  | IDENT var :: tl -> exprs @ [Var var], tl
  | INT num :: tl   -> exprs @ [Num num], tl
  | PLUS :: _ | MINUS :: _ ->
    let lst = Utils.last exprs           in
    let expr, ntl = parse_add tokens lst in
    (Utils.firsts exprs) @ [expr], ntl
  | TIMES :: _ | DIVIDE :: _ ->
    let lst       = Utils.last exprs     in
    let expr, ntl = parse_mul tokens lst in
    (Utils.firsts exprs) @ [expr], ntl
  | LPARENT :: tl ->
    (* Parses the token stream until a right parentheses is reached.*)
    let rec parse_rparent tokens exprs =
      match tokens with
        RPARENT :: tl -> reduce exprs, tl
      | tl            ->
        let nexpr, ntl = parser tl exprs in
        parse_rparent ntl nexpr
    in
    let expr, ntl = parse_rparent tl [] in
    exprs @ [expr], ntl
  | _ -> parse_error "Unimplemented"

let rec parse_all tokens exprs =
  match tokens with
    [] -> reduce exprs
  | tk ->
    let nexpr, ntl = parser tk exprs in
    parse_all ntl nexpr
