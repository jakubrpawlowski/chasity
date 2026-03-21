(* Linking phase: resolves cross-file shape references into proto import paths *)

type error = Duplicate_iri of { iri : string; file1 : string; file2 : string }

type warning =
  | Unresolved_reference of string
  | Node_class_mismatch of { node : string; class_ : string }

type file_group = {
  source : string;
  shapes : Shacl.node_shape list;
  imports : string list;
  warnings : warning list;
}

module StringMap = Map.Make (String)

let build_registry (file_shapes : (string * Shacl.node_shape list) list) =
  let register source shape_iri (Iri.Iri s) reg =
    match StringMap.find_opt s reg with
    | Some (existing_source, existing_shape_iri)
      when existing_shape_iri <> shape_iri ->
        Error
          (Duplicate_iri { iri = s; file1 = existing_source; file2 = source })
    | _ -> Ok (StringMap.add s (source, shape_iri) reg)
  in
  let register_shape source (shape : Shacl.node_shape) reg =
    Ok reg
    |> Result_ext.flat_map (register source shape.iri shape.iri)
    |> Result_ext.flat_map (register source shape.iri shape.target_class)
  in
  List.fold_left
    (fun reg (source, shapes) ->
      List.fold_left
        (fun reg shape -> Result_ext.flat_map (register_shape source shape) reg)
        reg shapes)
    (Ok StringMap.empty) file_shapes

let import_path ~package source =
  let base = Filename.chop_extension (Filename.basename source) in
  let package_dir = String.split_on_char '.' package |> String.concat "/" in
  Printf.sprintf "%s/%s.proto" package_dir (Proto_emit.snake_case base)

let resolve_refs ~package ~source registry (shape : Shacl.node_shape) =
  let check_iri iri =
    let (Iri.Iri s) = iri in
    match StringMap.find_opt s registry with
    | Some (ref_source, _) when ref_source <> source ->
        ([ import_path ~package ref_source ], [])
    | Some _ -> ([], [])
    | None -> ([], [ Unresolved_reference s ])
  in
  let merge (acc_imports, acc_warnings) (new_imports, new_warnings) =
    (acc_imports @ new_imports, acc_warnings @ new_warnings)
  in
  List.fold_left
    (fun acc (prop : Shacl.property_shape) ->
      let acc =
        match (prop.node, prop.class_) with
        | Some node_iri, Some class_iri ->
            let acc = merge acc (check_iri node_iri) in
            let acc = merge acc (check_iri class_iri) in
            let (Iri.Iri n) = node_iri in
            let (Iri.Iri c) = class_iri in
            let node_source = StringMap.find_opt n registry in
            let class_source = StringMap.find_opt c registry in
            if node_source <> class_source then
              merge acc ([], [ Node_class_mismatch { node = n; class_ = c } ])
            else acc
        | Some iri, None -> merge acc (check_iri iri)
        | None, Some iri -> merge acc (check_iri iri)
        | None, None -> acc
      in
      List.fold_left (fun acc iri -> merge acc (check_iri iri)) acc prop.or_)
    ([], []) shape.properties

let resolve_file registry ~package (source, shapes) =
  let imports, warnings =
    List.fold_left
      (fun (imports, warnings) shape ->
        let new_imports, new_warnings =
          resolve_refs ~package ~source registry shape
        in
        (imports @ new_imports, warnings @ new_warnings))
      ([], []) shapes
  in
  let imports = List.sort_uniq String.compare imports in
  { source; shapes; imports; warnings }

let resolve ~package (file_shapes : (string * Shacl.node_shape list) list) =
  build_registry file_shapes
  |> Result_ext.flat_map (fun registry ->
         Ok (List.map (resolve_file registry ~package) file_shapes))
