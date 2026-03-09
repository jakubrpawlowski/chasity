open Cmdliner

let generate_cmd =
  let doc = "Generate .proto files from SHACL shapes." in
  let shapes =
    let doc = "Path to SHACL shapes file or directory of .ttl files." in
    Arg.(required & opt (some string) None & info [ "shapes" ] ~doc)
  in
  let out =
    let doc = "Output directory for generated .proto files." in
    Arg.(value & opt dir "." & info [ "out" ] ~doc)
  in
  let package =
    let doc = "Proto package name (e.g. mycompany.api.v1)." in
    Arg.(required & opt (some string) None & info [ "package" ] ~doc)
  in
  let collect_ttl_files path =
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "%s: not found" path)
    else if Sys.is_directory path then
      Ok
        (Sys.readdir path |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".ttl")
        |> List.map (Filename.concat path))
    else Ok [ path ]
  in
  let process_shape ~package out file shape =
    match Chasity_lib.Proto_emit.emit_proto ~package shape with
    | Error errs ->
        List.map
          (fun (Chasity_lib.Proto_emit.Unsupported_datatype (Iri iri)) ->
            Printf.sprintf "%s: unsupported datatype %s" file iri)
          errs
    | Ok proto ->
        let name =
          Chasity_lib.Proto_emit.local_name_of_iri shape.target_class
        in
        let package_dir =
          String.split_on_char '.' package |> String.concat Filename.dir_sep
        in
        let dir = Filename.concat out package_dir in
        let rec mkdir_p path =
          if not (Sys.file_exists path) then (
            mkdir_p (Filename.dirname path);
            Sys.mkdir path 0o755)
        in
        mkdir_p dir;
        let out_path =
          Filename.concat dir (Chasity_lib.Proto_emit.snake_case name ^ ".proto")
        in
        let oc = open_out out_path in
        output_string oc proto;
        close_out oc;
        []
  in
  let process_file ~package out file =
    match Chasity_lib.Ntriples.from_file (Path file) with
    | Error (Riot_failed { path = Path p; exit_code }) ->
        [ Printf.sprintf "%s: riot failed (exit %d)" p exit_code ]
    | Ok triples ->
        let store = Chasity_lib.Triple_store.of_triples triples in
        let shapes = Chasity_lib.Shacl.extract_node_shapes store in
        List.concat_map (process_shape ~package out file) shapes
  in
  let run shapes out package =
    match collect_ttl_files shapes with
    | Error msg -> `Error (false, msg)
    | Ok files -> (
        match List.concat_map (process_file ~package out) files with
        | [] -> `Ok ()
        | failed ->
            List.iter (Fmt.epr "chasity: %s@.") failed;
            `Error (false, "some files failed"))
  in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const run $ shapes $ out $ package))

let main_cmd =
  let doc = "SHACL to Protobuf transpiler." in
  let info = Cmd.info "chasity" ~version:"0.1.0" ~doc in
  Cmd.group info [ generate_cmd ]

let () = exit (Cmd.eval main_cmd)
