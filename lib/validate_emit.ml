(* Validation annotation generation: maps SHACL constraints to protovalidate field options *)

open Fun_ext

let string_constraints (prop : Shacl.property_shape) =
  List.filter_map Fun.id
    [
      Option.map (Printf.sprintf "pattern: \"%s\"") prop.pattern;
      Option.map (Printf.sprintf "min_len: %d") prop.min_length;
      Option.map (Printf.sprintf "max_len: %d") prop.max_length;
    ]

let numeric_constraints ~proto_type (prop : Shacl.property_shape) =
  let fmt_val =
    match proto_type with
    | "float" | "double" -> Printf.sprintf "%g"
    | _ -> Float.to_int >> Printf.sprintf "%d"
  in
  List.filter_map Fun.id
    [
      Option.map (fmt_val >> Printf.sprintf "gte: %s") prop.min_inclusive;
      Option.map (fmt_val >> Printf.sprintf "lte: %s") prop.max_inclusive;
      Option.map (fmt_val >> Printf.sprintf "gt: %s") prop.min_exclusive;
      Option.map (fmt_val >> Printf.sprintf "lt: %s") prop.max_exclusive;
    ]

let is_repeated (prop : Shacl.property_shape) =
  match prop.max_count with Some 1 -> false | _ -> true

let type_constraint_block ~proto_type (prop : Shacl.property_shape) =
  match proto_type with
  | "string" ->
      let cs = string_constraints prop in
      if cs = [] then None else Some ("string", String.concat ", " cs)
  | "int32" | "int64" | "uint64" | "float" | "double" ->
      let cs = numeric_constraints ~proto_type prop in
      if cs = [] then None else Some (proto_type, String.concat ", " cs)
  | _ -> None

let min_items_of (prop : Shacl.property_shape) =
  match prop.min_count with Some n when n >= 1 -> Some n | _ -> None

let collect_options ~proto_type (prop : Shacl.property_shape) =
  let type_block = type_constraint_block ~proto_type prop in
  if is_repeated prop then
    let items_part =
      Option.map
        (fun (ty, cs) -> Printf.sprintf "items: {%s: {%s}}" ty cs)
        type_block
    in
    let min_part =
      Option.map (Printf.sprintf "min_items: %d") (min_items_of prop)
    in
    let parts = List.filter_map Fun.id [ min_part; items_part ] in
    if parts = [] then []
    else
      [
        Printf.sprintf "(buf.validate.field).repeated = {%s}"
          (String.concat ", " parts);
      ]
  else
    match type_block with
    | Some (ty, cs) -> [ Printf.sprintf "(buf.validate.field).%s = {%s}" ty cs ]
    | None -> []

let emit_field_options ~proto_type prop =
  match collect_options ~proto_type prop with
  | [] -> ""
  | opts -> Printf.sprintf " [%s]" (String.concat ", " opts)

let has_constraints ~proto_type prop = collect_options ~proto_type prop <> []

let has_fractional_int_constraints ~proto_type (prop : Shacl.property_shape) =
  match proto_type with
  | "int32" | "int64" | "uint64" ->
      let values =
        List.filter_map Fun.id
          [
            prop.min_inclusive;
            prop.max_inclusive;
            prop.min_exclusive;
            prop.max_exclusive;
          ]
      in
      List.exists (Float.is_integer >> not) values
  | _ -> false
