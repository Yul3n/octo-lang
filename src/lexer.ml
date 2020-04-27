open Syntax

exception Syntax_error of string

let rec lexer input pos act_ident =
  let unexpected_char pos chr =
    let l, r = Utils.get_pos input 1 0 0 pos in
    raise (Syntax_error ("Unexpected character: '" ^ (String.make 1 chr) ^ "', line " ^
                         (string_of_int l) ^ ", character " ^ string_of_int (r - 1)))
  in
  let is_digit chr = ((Char.code('0') <= Char.code chr) &&
                      (Char.code('9') >= Char.code chr)) ||
                     (chr = '.') || (chr = '-')
  in
  let is_alpha chr = (Char.code('A') <= Char.code chr) &&
                     (Char.code('Z') >= Char.code chr) ||
                     (Char.code('a') <= Char.code chr) &&
                     (Char.code('z') >= Char.code chr)
  in
  let is_ident chr = is_alpha chr || is_digit chr || chr = '_' || chr = '\'' in
  let rec parse_f f str pos =
    match pos with
      len when len = String.length str -> ""
    | _ -> match String.get str pos with
        chr when f chr -> (String.make 1 chr) ^ parse_f f str (pos + 1)
      | _ -> ""
  in
  let slex l t =
    let toks, fpos = lexer input (pos + l) act_ident in
    (t, pos) :: toks, fpos
  in
  try
    match String.get input pos with
      '\n' -> let indent = String.length (parse_f ((=) ' ') input (pos + 1)) in
      begin
        match indent with
          len when len = act_ident ->
          lexer input (pos + 1 + indent) len
        | len when len < act_ident -> if (act_ident - indent) > 2 then [], pos else [], pos + len + 1
        | len when len > act_ident ->
          let block, fpos1 = lexer input (pos + indent + 1) len   in
          let toks, fpos2  = lexer input (fpos1) act_ident in
          (BLOCK(block), pos) :: toks, fpos2
        | _ -> unexpected_char 0 'f' (* Won't happen *)
      end
    | ' '
    | '\t' -> lexer input (pos + 1) act_ident
    | '+'  ->  let c = String.get input (pos + 1) in
      begin
        match c with
          n when is_digit n ->
          let num = parse_f is_digit input pos in
          let len = String.length num          in
          slex len (NUM (float_of_string num))
        | _ -> slex 1 PLUS
      end
    | '='  -> slex 1 EQUAL
    | '*'  -> slex 1 TIMES
    | '/'  ->
      begin
        match String.get input (pos + 1) with
          '/' -> slex 2 DDIVIDE
        | _   -> slex 1 DIVIDE
      end
    | '\\' -> slex 1 BACKSLASH
    | '_'  -> slex 1 UNDER
    | '('  -> slex 1 LPARENT
    | ')'  -> slex 1 RPARENT
    | '|'  -> slex 1 PIPE
    | '@'  -> slex 1 AT
    | '['  -> slex 1 LBRACKET
    | ']'  -> slex 1 RBRACKET
    | '!'  -> slex 1 EXCLAM
    | '>'  -> slex 1 GRT
    | ','  -> slex 1 COMMA
    | '%'  -> slex 1 MOD
    | '\'' -> let c = String.get input (pos + 1) in
      begin
        match String.get input (pos + 2) with
          '\'' -> slex 3 (CHAR c)
        | c    -> unexpected_char (pos + 2) c
      end
    | ':'  ->
      begin
        match String.get input (pos + 1) with
          ':' -> slex 2 CONS
        | _   -> unexpected_char pos ':'
      end
    | '-'  ->
      begin
        match String.get input (pos + 1) with
          '>' -> slex 2 ARROW
        | '-' -> let l = String.length (parse_f ((<>) '\n') input (pos + 2)) in
          lexer input (pos + 2 + l) act_ident
        | n when is_digit n ->
          let num = parse_f is_digit input pos in
          let len = String.length num          in
          slex len (NUM (float_of_string num))
        | _   -> slex 1 MINUS
      end
    | n when is_digit n ->
      let num = parse_f is_digit input pos in
      let len = String.length num          in
      slex len (NUM (float_of_string num))
    | a when is_alpha a ->
      let ide = parse_f is_ident input pos in
      let len = String.length ide          in
      let tok =
          match ide with
            "where"  -> WHERE
          | "type"   -> TYPE
          | "case"   -> CASE
          | "of"     -> OF
          | "open"   -> OPEN
          | "when"   -> WHEN
          | "infixl" -> INFXL
          | v        ->
            if (Char.code (String.get v 0) >= Char.code('a'))
            then IDENT ide
            else MINDE ide
      in
      slex len tok
    | '"' ->
      let str = parse_f ((<>) '"') input (pos + 1) in
      let len = String.length str                  in
      slex (len + 2) (STR str)
    | c -> unexpected_char pos c
  with Invalid_argument _ -> [], pos
