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
