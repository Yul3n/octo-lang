open Utils

let _ =
  print_context (compile (read_from_file (Sys.argv.(1))))
