let pluralize str =
  let without_last = String.sub str 0 (String.length str - 1) in
  let is_vowel = function 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false in
  let ends_with_consonant_y =
    String.length str >= 3
    && String.ends_with ~suffix:"y" str
    && not (is_vowel str.[String.length str - 2])
  in
  if
    [ "s"; "sh"; "ch"; "x"; "z" ]
    |> List.exists (fun suf -> String.ends_with ~suffix:suf str)
  then str ^ "es"
  else if ends_with_consonant_y then without_last ^ "ies"
  else str ^ "s"

let to_snake_case str =
  let buf = Buffer.create (String.length str + 4) in
  String.iteri
    (fun i c ->
      match (i, c) with
      | 0, '_' -> ()
      | _, '-' -> Buffer.add_char buf '_'
      | 0, 'A' .. 'Z' -> Buffer.add_char buf (Char.lowercase_ascii c)
      | _, 'A' .. 'Z' ->
          Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
      | _ -> Buffer.add_char buf c)
    str;
  Buffer.contents buf
