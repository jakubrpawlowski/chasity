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
