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
