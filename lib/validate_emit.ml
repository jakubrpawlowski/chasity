(* Validation annotation generation: maps SHACL constraints to protovalidate field options *)

let string_constraints (prop : Shacl.property_shape) =
  List.filter_map Fun.id
    [
      Option.map (fun p -> Printf.sprintf "pattern: \"%s\"" p) prop.pattern;
      Option.map (fun n -> Printf.sprintf "min_len: %d" n) prop.min_length;
      Option.map (fun n -> Printf.sprintf "max_len: %d" n) prop.max_length;
    ]

let numeric_constraints proto_type (prop : Shacl.property_shape) =
  let fmt_val =
    match proto_type with
    | "float" | "double" -> Printf.sprintf "%g"
    | _ -> fun v -> Printf.sprintf "%d" (Float.to_int v)
  in
  List.filter_map Fun.id
    [
      Option.map
        (fun n -> Printf.sprintf "gte: %s" (fmt_val n))
        prop.min_inclusive;
      Option.map
        (fun n -> Printf.sprintf "lte: %s" (fmt_val n))
        prop.max_inclusive;
      Option.map
        (fun n -> Printf.sprintf "gt: %s" (fmt_val n))
        prop.min_exclusive;
      Option.map
        (fun n -> Printf.sprintf "lt: %s" (fmt_val n))
        prop.max_exclusive;
    ]

let is_repeated (prop : Shacl.property_shape) =
  match prop.max_count with Some 1 -> false | _ -> true

let collect_options proto_type (prop : Shacl.property_shape) =
  let type_options =
    match proto_type with
    | "string" ->
        let cs = string_constraints prop in
        if cs = [] then []
        else
          [
            Printf.sprintf "(buf.validate.field).string = {%s}"
              (String.concat ", " cs);
          ]
    | "int32" | "int64" | "uint64" | "float" | "double" ->
        let cs = numeric_constraints proto_type prop in
        if cs = [] then []
        else
          [
            Printf.sprintf "(buf.validate.field).%s = {%s}" proto_type
              (String.concat ", " cs);
          ]
    | _ -> []
  in
  let repeated_options =
    if is_repeated prop then
      match prop.min_count with
      | Some n when n >= 1 ->
          [ Printf.sprintf "(buf.validate.field).repeated.min_items = %d" n ]
      | _ -> []
    else []
  in
  type_options @ repeated_options

let emit_field_options proto_type prop =
  match collect_options proto_type prop with
  | [] -> ""
  | opts -> Printf.sprintf " [%s]" (String.concat ", " opts)

let has_constraints proto_type prop = collect_options proto_type prop <> []

let has_fractional_int_constraints proto_type (prop : Shacl.property_shape) =
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
      List.exists (fun v -> not (Float.is_integer v)) values
  | _ -> false
