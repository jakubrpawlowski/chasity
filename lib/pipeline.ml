(* Pipeline: orchestrates .ttl files through parse → extract → resolve → emit *)

type group_result = {
  source : string;
  warnings : Resolve.warning list;
  proto : (string, Proto_emit.error list) result;
  service : string;
}

type error = Parse_errors of string list | Resolve_error of Resolve.error

let parse_file file =
  match Ntriples.from_file (Path file) with
  | Error (Riot_failed { path = Path p; exit_code }) ->
      Error (Printf.sprintf "%s: riot failed (exit %d)" p exit_code)
  | Ok triples ->
      triples
      |> Triple_store.of_triples
      |> Shacl.extract_node_shapes
      |> fun shapes -> Ok (file, shapes)

let emit_group ~package (group : Resolve.file_group) =
  {
    source = group.source;
    warnings = group.warnings;
    proto = Proto_emit.emit_proto ~package ~imports:group.imports group.shapes;
    service =
      group.shapes
      |> List.map (fun (s : Shacl.node_shape) ->
          Iri.to_local_name s.target_class)
      |> Service_emit.emit_service_proto ~package
           ~entity_import:(Resolve.import_path ~package group.source);
  }

let compile ~package files =
  let parsed, errors =
    files
    |> List.partition_map (fun f ->
        match parse_file f with Ok x -> Left x | Error e -> Right e)
  in
  match errors with
  | _ :: _ -> Error (Parse_errors errors)
  | [] ->
      Resolve.resolve ~package parsed
      |> Result.map_error (fun e -> Resolve_error e)
      |> Result.map (List.map (emit_group ~package))
