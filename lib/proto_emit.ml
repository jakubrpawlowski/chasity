(* Code generation: emits .proto files from SHACL shapes (IR) *)

type error = Unsupported_datatype of Iri.t | Fractional_constraint of Iri.t

let proto_type_of_datatype (Iri.Iri iri) =
  match iri with
  | "http://www.w3.org/2001/XMLSchema#anyURI"
  | "http://www.w3.org/2001/XMLSchema#date"
  | "http://www.w3.org/2001/XMLSchema#duration"
  | "http://www.w3.org/2001/XMLSchema#gMonthDay"
  | "http://www.w3.org/2001/XMLSchema#gYear"
  | "http://www.w3.org/2001/XMLSchema#string"
  | "http://www.w3.org/2001/XMLSchema#time" ->
      Ok "string"
  | "http://www.w3.org/2001/XMLSchema#base64Binary" -> Ok "bytes"
  | "http://www.w3.org/2001/XMLSchema#boolean" -> Ok "bool"
  | "http://www.w3.org/2001/XMLSchema#dateTime" ->
      Ok "google.protobuf.Timestamp"
  | "http://www.w3.org/2001/XMLSchema#decimal"
  | "http://www.w3.org/2001/XMLSchema#double" ->
      Ok "double"
  | "http://www.w3.org/2001/XMLSchema#float" -> Ok "float"
  | "http://www.w3.org/2001/XMLSchema#int" -> Ok "int32"
  | "http://www.w3.org/2001/XMLSchema#integer"
  | "http://www.w3.org/2001/XMLSchema#long" ->
      Ok "int64"
  | "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" -> Ok "uint64"
  | _ -> Error (Unsupported_datatype (Iri.Iri iri))

type cardinality = Required | Optional | Repeated

let cardinality_of_property (prop : Shacl.property_shape) =
  match prop.max_count with
  | Some 1 -> (
      match prop.min_count with Some n when n >= 1 -> Required | _ -> Optional)
  | _ -> Repeated

let local_name_of_iri (Iri.Iri iri) =
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

type resolved_type =
  | Simple of string
  | Oneof of (string * string) list (* (type_name, variant_suffix) pairs *)

let proto_type_of_property (prop : Shacl.property_shape) =
  if prop.in_ <> [] then Ok (Simple (enum_type_name prop.path))
  else if prop.or_ <> [] then
    let variants =
      List.map
        (fun iri ->
          let name = local_name_of_iri iri in
          (name, snake_case name))
        prop.or_
    in
    Ok (Oneof variants)
  else
    match prop.class_ with
    | Some class_iri -> Ok (Simple (local_name_of_iri class_iri))
    | None -> (
        match prop.datatype with
        | Some dt -> proto_type_of_datatype dt |> Result.map (fun s -> Simple s)
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

let emit_comment (prop : Shacl.property_shape) =
  match (prop.name, prop.description) with
  | Some n, Some d -> Printf.sprintf "  // %s - %s\n" n d
  | Some n, None -> Printf.sprintf "  // %s\n" n
  | None, Some d -> Printf.sprintf "  // %s\n" d
  | None, None -> ""

let field_width = function
  | Simple _ -> 1
  | Oneof variants -> List.length variants

let assign_field_numbers sorted =
  let _, assignments =
    List.fold_left
      (fun (num, acc) (prop, resolved) ->
        (num + field_width resolved, (prop, resolved, num) :: acc))
      (1, []) sorted
  in
  List.rev assignments

let emit_field resolved_type (prop : Shacl.property_shape) field_num =
  match resolved_type with
  | Simple type_name ->
      let label =
        match cardinality_of_property prop with
        | Required -> ""
        | Optional -> "optional "
        | Repeated -> "repeated "
      in
      let options = Validate_emit.emit_field_options type_name prop in
      emit_comment prop
      ^ Printf.sprintf "  %s%s %s = %d%s;\n" label type_name
          (field_name prop.path) field_num options
  | Oneof variants ->
      let base = field_name prop.path in
      let inner =
        List.mapi
          (fun i (type_name, suffix) ->
            Printf.sprintf "    %s %s_%s = %d;\n" type_name base suffix
              (field_num + i))
          variants
      in
      emit_comment prop
      ^ Printf.sprintf "  oneof %s {\n%s  }\n" base (String.concat "" inner)

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

let emit_message (shape : Shacl.node_shape) sorted =
  let message_name = local_name_of_iri shape.target_class in
  let fields =
    assign_field_numbers sorted
    |> List.map (fun (prop, resolved, num) -> emit_field resolved prop num)
  in
  String.concat ""
    ([ Printf.sprintf "message %s {\n" message_name ] @ fields @ [ "}\n" ])

(* Topological sort: dependencies (referenced shapes) come before referencing shapes *)
let sort_shapes (shapes : Shacl.node_shape list) =
  let shapes_arr = Array.of_list shapes in
  let n = Array.length shapes_arr in
  let iri_to_idx = Hashtbl.create (n * 2) in
  Array.iteri
    (fun i (s : Shacl.node_shape) ->
      let (Iri.Iri si) = s.iri in
      let (Iri.Iri ci) = s.target_class in
      Hashtbl.replace iri_to_idx si i;
      Hashtbl.replace iri_to_idx ci i)
    shapes_arr;
  let refs_of (s : Shacl.node_shape) =
    List.concat_map
      (fun (p : Shacl.property_shape) ->
        (match p.node with
        | Some iri -> [ iri ]
        | None -> ( match p.class_ with Some iri -> [ iri ] | None -> []))
        @ p.or_)
      s.properties
  in
  let visited = Array.make n false in
  let result = ref [] in
  let rec visit i =
    if not visited.(i) then (
      visited.(i) <- true;
      List.iter
        (fun (Iri.Iri iri) ->
          match Hashtbl.find_opt iri_to_idx iri with
          | Some j when j <> i -> visit j
          | _ -> ())
        (refs_of shapes_arr.(i));
      result := shapes_arr.(i) :: !result)
  in
  for i = 0 to n - 1 do
    visit i
  done;
  List.rev !result

let resolve_shape (shape : Shacl.node_shape) =
  match resolve_properties shape.properties with
  | Error errs -> Error errs
  | Ok resolved ->
      let bad =
        List.filter_map
          (fun ((prop : Shacl.property_shape), r) ->
            match r with
            | Simple t when Validate_emit.has_fractional_int_constraints t prop
              ->
                Some (Fractional_constraint prop.path)
            | _ -> None)
          resolved
      in
      if bad <> [] then Error bad else Ok (shape, sort_by_order resolved)

let emit_proto ~package ?(imports = []) (shapes : Shacl.node_shape list) =
  let sorted_shapes = sort_shapes shapes in
  let results = List.map resolve_shape sorted_shapes in
  let all_errors =
    List.concat_map (function Error e -> e | Ok _ -> []) results
  in
  if all_errors <> [] then Error all_errors
  else
    let resolved =
      List.filter_map (function Ok x -> Some x | Error _ -> None) results
    in
    let needs_timestamp =
      List.exists
        (fun (_, sorted) ->
          List.exists
            (fun (_, r) -> r = Simple "google.protobuf.Timestamp")
            sorted)
        resolved
    in
    let needs_validate =
      List.exists
        (fun (_, sorted) ->
          List.exists
            (fun ((p : Shacl.property_shape), r) ->
              match r with
              | Simple t -> Validate_emit.has_constraints t p
              | Oneof _ -> false)
            sorted)
        resolved
    in
    let header =
      Printf.sprintf "syntax = \"proto3\";\n\npackage %s;\n\n" package
    in
    let well_known =
      List.filter_map Fun.id
        [
          (if needs_validate then
             Some "import \"buf/validate/validate.proto\";\n"
           else None);
          (if needs_timestamp then
             Some "import \"google/protobuf/timestamp.proto\";\n"
           else None);
        ]
    in
    let extra =
      List.map (fun path -> Printf.sprintf "import \"%s\";\n" path) imports
    in
    let all_imports = well_known @ extra in
    let imports_str =
      match all_imports with [] -> "" | lines -> String.concat "" lines ^ "\n"
    in
    let enums =
      List.concat_map
        (fun (_, sorted) ->
          List.filter_map
            (fun ((prop : Shacl.property_shape), _) ->
              if prop.in_ <> [] then Some (emit_enum prop ^ "\n") else None)
            sorted)
        resolved
    in
    let messages =
      List.map (fun (shape, sorted) -> emit_message shape sorted) resolved
    in
    Ok
      (String.concat ""
         ([ header; imports_str ] @ enums @ [ String.concat "\n" messages ]))
