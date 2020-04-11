open Lexer
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
 * Printing and error reporting errors
 *)

let string_of_token token =
  match token with
    PLUS  -> "+"
  | MINUS -> "-"
  | EQUAL -> "="
  | TIMES -> "*"
  | _ -> raise (Syntax_error "Not implemented")

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
