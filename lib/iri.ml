type t = Iri of string

(* In RDF, an IRI like http://example.org/Person has two parts:
   - Namespace: http://example.org/
   - Local name: Person (the part after the last / or #) *)
let to_local_name (Iri iri) =
  let after_hash =
    match String.rindex_opt iri '#' with Some i -> i + 1 | None -> 0
  in
  let after_slash =
    match String.rindex_opt iri '/' with Some i -> i + 1 | None -> 0
  in
  let start = max after_hash after_slash in
  String.sub iri start (String.length iri - start)
