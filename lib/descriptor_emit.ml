(* Code generation: emits JSON entity descriptors from SHACL shapes (IR) *)

open Fun_ext

(* Field kind classification from SHACL property constraints *)
type field_kind =
  | Literal
  | Value_object
  | Sub_entity
  | Uri_ref
  | Repeated_uri
  | Enum
  | Oneof

let classify_field (prop : Shacl.property_shape) =
  if prop.in_ <> [] then Enum
  else if prop.or_ <> [] then Oneof
  else
    match prop.node with
    | Some _ -> (
        match prop.max_count with Some 1 -> Value_object | _ -> Sub_entity)
    | None -> (
        match prop.class_ with
        | Some _ -> (
            match prop.max_count with Some 1 -> Uri_ref | _ -> Repeated_uri)
        | None -> Literal)

let field_kind_to_string = function
  | Literal -> "literal"
  | Value_object -> "value_object"
  | Sub_entity -> "sub_entity"
  | Uri_ref -> "uri_ref"
  | Repeated_uri -> "repeated_uri"
  | Enum -> "enum"
  | Oneof -> "oneof"

let is_required (prop : Shacl.property_shape) =
  match prop.min_count with Some n when n >= 1 -> true | _ -> false

let is_repeated (prop : Shacl.property_shape) =
  match prop.max_count with Some 1 -> false | _ -> true

let field_name = Iri.to_local_name >> String_ext.to_snake_case
let iri_string (Iri.Iri s) = s

let json_number f =
  if Float.is_integer f then Json.Int (Float.to_int f) else Json.Float f

let sort_properties =
  List.sort (fun (a : Shacl.property_shape) (b : Shacl.property_shape) ->
      match (a.order, b.order) with
      | Some oa, Some ob -> compare oa ob
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0)

let validation_of_property (prop : Shacl.property_shape) =
  let pairs =
    List.filter_map Fun.id
      [
        Option.map (fun s -> ("pattern", Json.Str s)) prop.pattern;
        Option.map (fun n -> ("min_length", Json.Int n)) prop.min_length;
        Option.map (fun n -> ("max_length", Json.Int n)) prop.max_length;
        Option.map
          (fun f -> ("min_inclusive", json_number f))
          prop.min_inclusive;
        Option.map
          (fun f -> ("max_inclusive", json_number f))
          prop.max_inclusive;
        Option.map
          (fun f -> ("min_exclusive", json_number f))
          prop.min_exclusive;
        Option.map
          (fun f -> ("max_exclusive", json_number f))
          prop.max_exclusive;
      ]
  in
  match pairs with [] -> None | _ -> Some (Json.Obj pairs)

let field_descriptor (prop : Shacl.property_shape) =
  let kind = classify_field prop in
  Json.Obj
    ([
       ("name", Json.Str (field_name prop.path));
       ("predicate", Json.Str (iri_string prop.path));
       ("kind", Json.Str (field_kind_to_string kind));
     ]
    @ (match kind with
      | Literal ->
          Option.to_list
            (Option.map
               (fun dt -> ("datatype", Json.Str (iri_string dt)))
               prop.datatype)
      | Value_object | Sub_entity | Uri_ref | Repeated_uri ->
          Option.to_list
            (Option.map
               (fun cls -> ("reference", Json.Str (Iri.to_local_name cls)))
               prop.class_)
      | Enum ->
          [ ("values", Json.Arr (List.map (fun v -> Json.Str v) prop.in_)) ]
      | Oneof ->
          [
            ( "alternatives",
              Json.Arr
                (List.map
                   (fun iri -> Json.Str (Iri.to_local_name iri))
                   prop.or_) );
          ])
    @ [
        ("required", Json.Bool (is_required prop));
        ("repeated", Json.Bool (is_repeated prop));
      ]
    @
    match validation_of_property prop with
    | Some v -> [ ("validation", v) ]
    | None -> [])

let emit_descriptor (shape : Shacl.node_shape) =
  Json.Obj
    [
      ("entity_name", Json.Str (Iri.to_local_name shape.target_class));
      ("rdf_type", Json.Str (iri_string shape.target_class));
      ("shape_iri", Json.Str (iri_string shape.iri));
      ( "fields",
        Json.Arr
          (shape.properties |> sort_properties |> List.map field_descriptor) );
    ]
  |> Json.to_string
