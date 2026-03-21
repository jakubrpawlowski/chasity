(* RDF helpers: namespace prefixes and list traversal *)

let sh s = Term.Iri ("http://www.w3.org/ns/shacl#" ^ s)
let rdf s = Term.Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ^ s)

let rec collect_rdf_list store term =
  let nil = rdf "nil" in
  if Term.compare term nil = 0 then []
  else
    let pairs = Triple_store.find_subject term store in
    match Triple_store.find_object (rdf "first") pairs with
    | None -> []
    | Some first ->
        let rest =
          match Triple_store.find_object (rdf "rest") pairs with
          | Some next -> collect_rdf_list store next
          | None -> []
        in
        first :: rest
