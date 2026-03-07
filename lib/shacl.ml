type iri = Iri of string

type property_shape = {
  path : iri;
  datatype : iri option;
  min_count : int option;
  max_count : int option;
  pattern : string option;
}

type node_shape = {
  iri : iri;
  target_class : iri;
  properties : property_shape list;
}

let sh s = Ntriples.Term.Iri ("http://www.w3.org/ns/shacl#" ^ s)
let rdf s = Ntriples.Term.Iri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" ^ s)

let find_object predicate pairs =
  List.find_map
    (fun (pred, obj) ->
      if Ntriples.Term.compare pred predicate = 0 then Some obj else None)
    pairs

let find_all_objects predicate pairs =
  List.filter_map
    (fun (pred, obj) ->
      if Ntriples.Term.compare pred predicate = 0 then Some obj else None)
    pairs

let iri_of_term = function Ntriples.Term.Iri s -> Some (Iri s) | _ -> None

let int_of_term = function
  | Ntriples.Term.Literal { value; _ } -> int_of_string_opt value
  | _ -> None

let string_of_term = function
  | Ntriples.Term.Literal { value; _ } -> Some value
  | _ -> None

let extract_property_shape store prop_term =
  let pairs = Triple_store.find_subject prop_term store in
  match find_object (sh "path") pairs |> Option_ext.flat_map iri_of_term with
  | None -> None
  | Some path ->
      Some
        {
          path;
          datatype =
            find_object (sh "datatype") pairs |> Option_ext.flat_map iri_of_term;
          min_count =
            find_object (sh "minCount") pairs |> Option_ext.flat_map int_of_term;
          max_count =
            find_object (sh "maxCount") pairs |> Option_ext.flat_map int_of_term;
          pattern =
            find_object (sh "pattern") pairs
            |> Option_ext.flat_map string_of_term;
        }

let extract_node_shapes store =
  let node_shape_subjects =
    Triple_store.find_by_predicate (rdf "type") store
    |> List.filter_map (fun (subject, obj) ->
           if Ntriples.Term.compare obj (sh "NodeShape") = 0 then Some subject
           else None)
  in
  List.filter_map
    (fun subject ->
      let pairs = Triple_store.find_subject subject store in
      match
        ( iri_of_term subject,
          find_object (sh "targetClass") pairs
          |> Option_ext.flat_map iri_of_term )
      with
      | Some iri, Some target_class ->
          let prop_terms = find_all_objects (sh "property") pairs in
          let properties =
            List.filter_map (extract_property_shape store) prop_terms
          in
          Some { iri; target_class; properties }
      | _ -> None)
    node_shape_subjects
