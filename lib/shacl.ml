(* Semantic analysis: extracts typed SHACL shapes (IR) from the triple store (AST) *)

type property_shape = {
  path : Iri.t;
  datatype : Iri.t option;
  min_count : int option;
  max_count : int option;
  pattern : string option;
  class_ : Iri.t option;
  node : Iri.t option;
  in_ : string list;
  or_ : Iri.t list;
  min_length : int option;
  max_length : int option;
  min_inclusive : float option;
  max_inclusive : float option;
  min_exclusive : float option;
  max_exclusive : float option;
  name : string option;
  description : string option;
  order : int option;
}

type node_shape = {
  iri : Iri.t;
  target_class : Iri.t;
  properties : property_shape list;
}

(* Topological sort: dependencies (referenced shapes) come before referencing shapes *)
let sort_shapes (shapes : node_shape list) =
  let shapes_arr = Array.of_list shapes in
  let n = Array.length shapes_arr in
  let iri_to_idx = Hashtbl.create (n * 2) in
  Array.iteri
    (fun i (s : node_shape) ->
      let (Iri.Iri si) = s.iri in
      let (Iri.Iri ci) = s.target_class in
      Hashtbl.replace iri_to_idx si i;
      Hashtbl.replace iri_to_idx ci i)
    shapes_arr;
  let refs_of (s : node_shape) =
    List.concat_map
      (fun (p : property_shape) ->
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

let extract_property_shape store prop_term =
  let pairs = Triple_store.find_subject prop_term store in
  match
    Triple_store.find_object (Rdf.sh "path") pairs
    |> Option_ext.flat_map Term.to_iri
  with
  | None -> None
  | Some path ->
      Some
        {
          path;
          datatype =
            Triple_store.find_object (Rdf.sh "datatype") pairs
            |> Option_ext.flat_map Term.to_iri;
          min_count =
            Triple_store.find_object (Rdf.sh "minCount") pairs
            |> Option_ext.flat_map Term.to_int;
          max_count =
            Triple_store.find_object (Rdf.sh "maxCount") pairs
            |> Option_ext.flat_map Term.to_int;
          pattern =
            Triple_store.find_object (Rdf.sh "pattern") pairs
            |> Option_ext.flat_map Term.to_string;
          class_ =
            Triple_store.find_object (Rdf.sh "class") pairs
            |> Option_ext.flat_map Term.to_iri;
          node =
            Triple_store.find_object (Rdf.sh "node") pairs
            |> Option_ext.flat_map Term.to_iri;
          in_ =
            (match Triple_store.find_object (Rdf.sh "in") pairs with
            | Some list_head ->
                List.filter_map
                  (fun term ->
                    match term with
                    | Term.Iri _ ->
                        Term.to_iri term |> Option.map Iri.to_local_name
                    | _ -> Term.to_string term)
                  (Rdf.collect_rdf_list store list_head)
            | None -> []);
          or_ =
            (match Triple_store.find_object (Rdf.sh "or") pairs with
            | Some list_head ->
                List.filter_map
                  (fun term ->
                    let shape_pairs = Triple_store.find_subject term store in
                    Triple_store.find_object (Rdf.sh "class") shape_pairs
                    |> Option_ext.flat_map Term.to_iri)
                  (Rdf.collect_rdf_list store list_head)
            | None -> []);
          min_length =
            Triple_store.find_object (Rdf.sh "minLength") pairs
            |> Option_ext.flat_map Term.to_int;
          max_length =
            Triple_store.find_object (Rdf.sh "maxLength") pairs
            |> Option_ext.flat_map Term.to_int;
          min_inclusive =
            Triple_store.find_object (Rdf.sh "minInclusive") pairs
            |> Option_ext.flat_map Term.to_float;
          max_inclusive =
            Triple_store.find_object (Rdf.sh "maxInclusive") pairs
            |> Option_ext.flat_map Term.to_float;
          min_exclusive =
            Triple_store.find_object (Rdf.sh "minExclusive") pairs
            |> Option_ext.flat_map Term.to_float;
          max_exclusive =
            Triple_store.find_object (Rdf.sh "maxExclusive") pairs
            |> Option_ext.flat_map Term.to_float;
          name =
            Triple_store.find_object (Rdf.sh "name") pairs
            |> Option_ext.flat_map Term.to_string;
          description =
            Triple_store.find_object (Rdf.sh "description") pairs
            |> Option_ext.flat_map Term.to_string;
          order =
            Triple_store.find_object (Rdf.sh "order") pairs
            |> Option_ext.flat_map Term.to_int;
        }

let extract_node_shapes store =
  let node_shape_subjects =
    Triple_store.find_by_predicate (Rdf.rdf "type") store
    |> List.filter_map (fun (subject, obj) ->
           if Term.compare obj (Rdf.sh "NodeShape") = 0 then Some subject
           else None)
  in
  List.filter_map
    (fun subject ->
      let pairs = Triple_store.find_subject subject store in
      match
        ( Term.to_iri subject,
          Triple_store.find_object (Rdf.sh "targetClass") pairs
          |> Option_ext.flat_map Term.to_iri )
      with
      | Some iri, Some target_class ->
          let prop_terms =
            Triple_store.find_all_objects (Rdf.sh "property") pairs
          in
          let properties =
            List.filter_map (extract_property_shape store) prop_terms
          in
          Some { iri; target_class; properties }
      | _ -> None)
    node_shape_subjects
