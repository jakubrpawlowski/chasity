(* Code generation: emits .proto files from SHACL shapes (IR) *)

type error = Unsupported_datatype of Shacl.iri

let proto_type_of_datatype (Shacl.Iri iri) =
  match iri with
  | "http://www.w3.org/2001/XMLSchema#string"
  | "http://www.w3.org/2001/XMLSchema#anyURI"
  | "http://www.w3.org/2001/XMLSchema#date"
  | "http://www.w3.org/2001/XMLSchema#time"
  | "http://www.w3.org/2001/XMLSchema#duration" ->
      Ok "string"
  | "http://www.w3.org/2001/XMLSchema#integer"
  | "http://www.w3.org/2001/XMLSchema#long" ->
      Ok "int64"
  | "http://www.w3.org/2001/XMLSchema#int" -> Ok "int32"
  | "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" -> Ok "uint64"
  | "http://www.w3.org/2001/XMLSchema#float" -> Ok "float"
  | "http://www.w3.org/2001/XMLSchema#double"
  | "http://www.w3.org/2001/XMLSchema#decimal" ->
      Ok "double"
  | "http://www.w3.org/2001/XMLSchema#boolean" -> Ok "bool"
  | "http://www.w3.org/2001/XMLSchema#base64Binary" -> Ok "bytes"
  | "http://www.w3.org/2001/XMLSchema#dateTime" ->
      Ok "google.protobuf.Timestamp"
  | _ -> Error (Unsupported_datatype (Shacl.Iri iri))

type cardinality = Required | Optional | Repeated

let cardinality_of_property (prop : Shacl.property_shape) =
  match prop.max_count with
  | Some 1 -> (
      match prop.min_count with Some n when n >= 1 -> Required | _ -> Optional)
  | _ -> Repeated

let local_name_of_iri (Shacl.Iri iri) =
  let after_hash =
    match String.rindex_opt iri '#' with Some i -> i + 1 | None -> 0
  in
  let after_slash =
    match String.rindex_opt iri '/' with Some i -> i + 1 | None -> 0
  in
  let start = max after_hash after_slash in
  String.sub iri start (String.length iri - start)

let enum_type_name path = String.capitalize_ascii (local_name_of_iri path)

let enum_value_name ~prefix value =
  String.uppercase_ascii prefix ^ "_" ^ String.uppercase_ascii value

let snake_case s =
  let buf = Buffer.create (String.length s + 4) in
  String.iteri
    (fun i c ->
      if i > 0 && c >= 'A' && c <= 'Z' then Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    s;
  Buffer.contents buf

let field_name path = snake_case (local_name_of_iri path)

let proto_type_of_property (prop : Shacl.property_shape) =
  if prop.in_ <> [] then Ok (enum_type_name prop.path)
  else
    match prop.class_ with
    | Some class_iri -> Ok (local_name_of_iri class_iri)
    | None -> (
        match prop.datatype with
        | Some dt -> proto_type_of_datatype dt
        | None -> Error (Unsupported_datatype prop.path))

let sort_by_order pairs =
  List.sort
    (fun ((a : Shacl.property_shape), _) ((b : Shacl.property_shape), _) ->
      match (a.order, b.order) with
      | Some order_a, Some order_b -> compare order_a order_b
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0)
    pairs

let resolve_properties props =
  let errors, resolved =
    List.fold_left
      (fun (errs, res) (prop : Shacl.property_shape) ->
        match proto_type_of_property prop with
        | Ok type_name -> (errs, (prop, type_name) :: res)
        | Error e -> (e :: errs, res))
      ([], []) props
  in
  match errors with
  | [] -> Ok (List.rev resolved)
  | errs -> Error (List.rev errs)

let emit_field type_name (prop : Shacl.property_shape) field_num =
  let label =
    match cardinality_of_property prop with
    | Required -> ""
    | Optional -> "optional "
    | Repeated -> "repeated "
  in
  let comment =
    match (prop.name, prop.description) with
    | Some n, Some d -> Printf.sprintf "  // %s - %s\n" n d
    | Some n, None -> Printf.sprintf "  // %s\n" n
    | None, Some d -> Printf.sprintf "  // %s\n" d
    | None, None -> ""
  in
  comment
  ^ Printf.sprintf "  %s%s %s = %d;\n" label type_name (field_name prop.path)
      field_num

let emit_enum (prop : Shacl.property_shape) =
  let type_name = enum_type_name prop.path in
  let prefix = local_name_of_iri prop.path in
  let values =
    List.mapi
      (fun i v ->
        Printf.sprintf "  %s = %d;\n" (enum_value_name ~prefix v) (i + 1))
      prop.in_
  in
  String.concat ""
    ([
       Printf.sprintf "enum %s {\n" type_name;
       Printf.sprintf "  %s = 0;\n" (enum_value_name ~prefix "UNSPECIFIED");
     ]
    @ values @ [ "}\n" ])

let emit_proto ~package (shape : Shacl.node_shape) =
  match resolve_properties shape.properties with
  | Error errs -> Error errs
  | Ok resolved ->
      let sorted = sort_by_order resolved in
      let message_name = local_name_of_iri shape.target_class in
      let needs_timestamp =
        List.exists
          (fun (_, type_name) -> type_name = "google.protobuf.Timestamp")
          sorted
      in
      let header =
        Printf.sprintf "syntax = \"proto3\";\n\npackage %s;\n\n" package
      in
      let imports =
        if needs_timestamp then
          "import \"google/protobuf/timestamp.proto\";\n\n"
        else ""
      in
      let enums =
        List.filter_map
          (fun ((prop : Shacl.property_shape), _) ->
            if prop.in_ <> [] then Some (emit_enum prop ^ "\n") else None)
          sorted
      in
      let fields =
        List.mapi
          (fun i (prop, type_name) -> emit_field type_name prop (i + 1))
          sorted
      in
      Ok
        (String.concat ""
           ([ header; imports ] @ enums
           @ [ Printf.sprintf "message %s {\n" message_name ]
           @ fields @ [ "}\n" ]))
