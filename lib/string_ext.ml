let to_snake_case s =
  let buf = Buffer.create (String.length s + 4) in
  String.iteri
    (fun i c ->
      match (i, c) with
      | _, '-' -> Buffer.add_char buf '_'
      | 0, 'A' .. 'Z' -> Buffer.add_char buf (Char.lowercase_ascii c)
      | _, 'A' .. 'Z' ->
          Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
      | _ -> Buffer.add_char buf c)
    s;
  Buffer.contents buf
