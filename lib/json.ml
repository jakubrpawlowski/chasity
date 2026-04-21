(* Minimal JSON AST and pretty-printer — no external library *)

open Fun_ext

type t =
  | Str of string
  | Int of int
  | Float of float
  | Bool of bool
  | Arr of t list
  | Obj of (string * t) list

let json_escape s =
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let rec to_json_string depth = function
  | Str s -> Printf.sprintf "\"%s\"" (json_escape s)
  | Int n -> string_of_int n
  | Float f -> Printf.sprintf "%g" f
  | Bool b -> if b then "true" else "false"
  | Arr [] -> "[]"
  | Arr items ->
      let pad = String.make ((depth + 1) * 2) ' ' in
      let inner =
        List.map (fun item -> pad ^ to_json_string (depth + 1) item) items
      in
      Printf.sprintf "[\n%s\n%s]"
        (String.concat ",\n" inner)
        (String.make (depth * 2) ' ')
  | Obj [] -> "{}"
  | Obj pairs ->
      let pad = String.make ((depth + 1) * 2) ' ' in
      let inner =
        List.map
          (fun (k, v) ->
            Printf.sprintf "%s\"%s\": %s" pad (json_escape k)
              (to_json_string (depth + 1) v))
          pairs
      in
      Printf.sprintf "{\n%s\n%s}"
        (String.concat ",\n" inner)
        (String.make (depth * 2) ' ')

let to_string = to_json_string 0 >> fun s -> s ^ "\n"
