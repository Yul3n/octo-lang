let read_from_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  let str = Bytes.unsafe_to_string s in
  Lexer.lexer str 0 1 0 0
