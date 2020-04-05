open Syntax
open Lexer

let parse_error str =
  raise (Syntax_error ("Parser error: unexpected token:" ^ str))

let rec parser tokens exprs =
  let rec parse_factor tokens lval =
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
      parse_factor ntl nval
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
