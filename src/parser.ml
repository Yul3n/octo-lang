open Syntax
open Lexer

let parse_error str =
  raise (Syntax_error ("Parser error: unexpected token:" ^ str))

let rec parser tokens exprs =
  let rec parse_mul tokens lval =
    let nval, ntl =
      match tokens with
        TIMES :: tl  -> let rval, tl = parser tl [] in
        Binop (lval, Times, rval), tl
      | DIVIDE :: tl -> let rval, tl = parser tl [] in
        Binop (lval, Divide, rval), tl
      | tl           -> lval, tl
    in
    match ntl with
      TIMES :: _ | DIVIDE :: _ ->
      parse_mul ntl nval
    | _                          -> nval, ntl
  in
  let parse_add tokens lval =
    let nval, ntl =
      match tokens with
        PLUS :: tl  -> let mul_val, tl = parser tl []  in
        let rval, ntl   = parse_mul tl mul_val in
        Binop (lval, Plus, rval), ntl
      | MINUS :: tl -> let mul_val, tl = parser tl []  in
        let rval, ntl   = parse_mul tl mul_val in
        Binop (lval, Minus, rval), ntl
      | tl           -> lval, tl
    in
    match ntl with
      TIMES :: _ | DIVIDE :: _ ->
      parse_mul ntl nval
    | _                          -> nval, ntl
  in
  match tokens with
    [] ->
    let rec reduce exprs =
      match exprs with
        []               -> raise (Syntax_error "empty tokens")
      | hd :: []         -> hd
      | fst :: snd :: tl -> reduce (App(fst, snd) :: tl)
    in
    (reduce exprs), []
  | BACKSLASH :: tl ->
    begin
      match tl with
        IDENT ident :: tl ->
        begin
          match tl with
            ARROW :: tl ->
            let body, tl = parser tl [] in
            Lambda (ident, body), tl
          | tok :: _    -> parse_error (string_of_token tok)
          | []          -> parse_error "Lambda function without a body."
        end
      | tok :: _          -> parse_error (string_of_token tok)
      | []                -> parse_error "Lambda function without a body."
    end
  | IDENT var :: tl -> Var var, tl
  | INT num :: tl   -> Num num, tl
  | PLUS :: _ | MINUS :: _ ->
    begin
      match exprs with
        _ :: lst :: [] ->
        parse_add tokens lst
      | lst :: [] ->
        parse_add tokens lst
      | _ -> parse_error "Operation function without a body."
    end
  | _ -> parse_error "Unimplemented"
