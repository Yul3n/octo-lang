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

let rec assoc_all l key =
  match l with
    []                        -> []
  | (k, v) :: tl when k = key -> v :: assoc_all tl key
  | _ :: tl                   -> assoc_all tl key


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
  | PIPE      -> "|"
  | TYPE      -> "type"
  | MINDE v
  | IDENT v   -> v
  | WHERE     -> "where"
  | BACKSLASH -> "\\"
  | LPARENT   -> "("
  | RPARENT   -> ")"
  | UNDER     -> "_"
  | NUM n     -> string_of_int n
  | BLOCK b   -> let b, _ = List.split b in
    List.fold_left (^) "" (List.map string_of_token b)

let to_greek c =
      Printf.sprintf "\206%c" (Char.chr @@ Char.code c - Char.code 'A' + 145)

let rec string_of_type t =
  match t with
    TVar v      -> to_greek (Char.chr (Char.code 'a' + v))
  | TFun (f, t) -> "(" ^ (string_of_type f) ^ " ――→ " ^ (string_of_type t) ^ ")"
  | TOth v      -> v

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
