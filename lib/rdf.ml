(* RDF helpers: namespace prefixes, term conversion, list traversal *)

let sh s = Ntriples.Term.Iri ("http://www.w3.org/ns/shacl#" ^ s)
let rdf s = Ntriples.Term.Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ^ s)
let iri_of_term = function Ntriples.Term.Iri s -> Some (Iri.Iri s) | _ -> None

let int_of_term = function
  | Ntriples.Term.Literal { value; _ } -> int_of_string_opt value
  | _ -> None

let float_of_term = function
  | Ntriples.Term.Literal { value; _ } -> float_of_string_opt value
  | _ -> None

let string_of_term = function
  | Ntriples.Term.Literal { value; _ } -> Some value
  | _ -> None

let rec collect_rdf_list store term =
  let nil = rdf "nil" in
  if Ntriples.Term.compare term nil = 0 then []
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
