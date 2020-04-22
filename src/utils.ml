open Syntax

let last l =
  match l with
    _ :: lst :: [] -> lst
  | lst :: []      -> lst
  | _              -> raise (Invalid_argument
                               "Can't determine the last element of an empty list.")

let rec firsts l =
  match l with
    []      
  | _ :: []  -> []
  | hd :: tl -> hd :: firsts tl

let rec snd_map f l =
  match l with
    []               -> []
  | (fst, snd) :: tl -> (fst, f snd) :: snd_map f tl

let rec get_t_n t =
  match t with
    TOth v     -> v
  | TFun(_, t) -> get_t_n t
  | _          -> raise (Invalid_argument "Shouldn't happend")

let rec string_to_char_list s =
  match s with
    "" -> []
  | s  -> (String.get s 0) ::
          string_to_char_list (String.sub s 1 ((String.length s) - 1))

(* Printing and error reporting functions *)

let rec string_of_token token =
  match token with
    PLUS      -> "+"
  | MINUS     -> "-"
  | EQUAL     -> "="
  | TIMES     -> "*"
  | DIVIDE    -> "/"
  | MOD       -> "%"
  | ARROW     -> "->"
  | PIPE      -> "|"
  | TYPE      -> "type"
  | MINDE v
  | IDENT v   -> v
  | WHERE     -> "where"
  | CASE      -> "case"
  | OF        -> "of"
  | EXTERN    -> "extern"
  | BACKSLASH -> "\\"
  | LPARENT   -> "("
  | RPARENT   -> ")"
  | LBRACKET  -> "["
  | RBRACKET  -> "]"
  | AT        -> "@"
  | COMMA     -> ","
  | CONS      -> "::"
  | UNDER     -> "_"
  | EXCLAM    -> "!"
  | OPEN      -> "open"
  | CHAR c    -> "'" ^ (String.make 1 c) ^ "'"
  | STR s     -> "\"" ^ s ^ "\""
  | NUM n     -> string_of_float n
  | BLOCK b   -> let b, _ = List.split b in
    List.fold_left (^) "" (List.map string_of_token b)

let to_greek c =
      Printf.sprintf "\206%c" (Char.chr @@ Char.code c - Char.code 'A' + 145)

let rec string_of_type t =
  match t with
    TVar v       -> to_greek (Char.chr (Char.code 'a' + v))
  | TFun (f, t)  -> "(" ^ (string_of_type f) ^ " ――→ " ^ (string_of_type t) ^ ")"
  | TOth v       -> v
  | TList t      -> "(" ^ string_of_type t ^ " list)"
  | TPair (l, r) -> (string_of_type l) ^ " * " ^ (string_of_type r)

let print_scheme (Forall(v, t)) =
  begin
    match v with
      [] -> ()
    | _  ->
      print_string "∀";
      List.iter (fun x -> print_string (to_greek (Char.chr (Char.code 'a' + x)))) v;
      print_string "."
  end;
  print_string (string_of_type t)

let print_context ctx =
  let print_ctx_elem (name, sch) =
    Printf.printf "%s: " name;
    print_scheme sch
  in
  List.iter (fun x -> print_ctx_elem x; print_newline()) ctx

let rec get_pos input l r pos mpos =
      match pos with
        n when n > mpos -> l, r
      | _ ->
        match String.get input pos with
          '\n' -> get_pos input (l + 1) 0 (pos + 1) mpos
        | _    -> get_pos input l (r + 1) (pos + 1) mpos
