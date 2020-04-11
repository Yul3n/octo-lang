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


(*
 * Printing and error reporting functions
 *)

let rec string_of_token token =
  match token with
    PLUS      -> "+"
  | MINUS     -> "-"
  | EQUAL     -> "="
  | TIMES     -> "*"
  | DIVIDE    -> "/"
  | ARROW     -> "->"
  | IDENT v   -> v
  | WHERE     -> "where"
  | BACKSLASH -> "\\"
  | LPARENT   -> "("
  | RPARENT   -> ")"
  | INT n     -> string_of_int n
  | BLOCK b   -> List.fold_left (^) "" (List.map string_of_token b)

let to_greek c =
      Printf.sprintf "\206%c" (Char.chr @@ Char.code c - Char.code 'A' + 145)

let rec string_of_type t =
    match t with
      TInt        -> "int"
    | TVar v      -> to_greek (Char.chr (Char.code 'a' + v))
    | TFun (f, t) -> "(" ^ (string_of_type f) ^ " ――→ " ^ (string_of_type t) ^ ")"

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
    Printf.printf "//%s: " name;
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
