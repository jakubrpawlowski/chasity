open Cmdliner

let color_warn fmt =
  Fmt.epr ("chasity: %a: " ^^ fmt ^^ "@.") Fmt.(styled `Yellow string) "warning"

let color_error fmt =
  Fmt.epr ("chasity: %a: " ^^ fmt ^^ "@.") Fmt.(styled `Red string) "error"

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
  let write_group ~package out (group : Chasity_lib.Pipeline.group_result) =
    group.warnings
    |> List.iter (function
         | Chasity_lib.Resolve.Unresolved_reference iri ->
             color_warn "unresolved reference %s" iri
         | Chasity_lib.Resolve.Node_class_mismatch { node; class_ } ->
             color_warn "sh:node %s and sh:class %s resolve to different files"
               node class_);
    match group.proto with
    | Error errs ->
        errs
        |> List.map (function
             | Chasity_lib.Proto_emit.Unsupported_datatype (Iri iri) ->
                 Printf.sprintf "%s: unsupported datatype %s" group.source iri
             | Chasity_lib.Proto_emit.Fractional_constraint (Iri iri) ->
                 Printf.sprintf "%s: fractional constraint on integer field %s"
                   group.source iri)
    | Ok proto -> (
        let base =
          group.source |> Filename.basename |> Filename.chop_extension
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
          Filename.concat dir
            (Chasity_lib.String_ext.to_snake_case base ^ ".proto")
        in
        let oc = open_out out_path in
        output_string oc proto;
        close_out oc;
        match
          Sys.command
            (Printf.sprintf "buf format -w %s" (Filename.quote out_path))
        with
        | 0 -> []
        | code ->
            [ Printf.sprintf "%s: buf format failed (exit %d)" out_path code ])
  in
  let run shapes out package =
    (* Enable ANSI colors on stderr if TTY — must run before color_warn/color_error *)
    Fmt_tty.setup_std_outputs ();
    match collect_ttl_files shapes with
    | Error msg -> `Error (false, msg)
    | Ok files -> (
        match Chasity_lib.Pipeline.compile ~package files with
        | Error (Parse_errors errs) ->
            List.iter (color_error "%s") errs;
            `Error (false, "some files failed to parse")
        | Error (Resolve_error (Duplicate_iri { iri; file1; file2 })) ->
            color_error "duplicate target class %s (%s and %s)" iri file1 file2;
            `Error (false, "duplicate definitions")
        | Ok groups -> (
            match List.concat_map (write_group ~package out) groups with
            | [] -> `Ok ()
            | failed ->
                List.iter (color_error "%s") failed;
                `Error (false, "some files failed")))
  in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const run $ shapes $ out $ package))

let main_cmd =
  let doc = "SHACL to Protobuf transpiler." in
  let info = Cmd.info "chasity" ~version:"0.2.0" ~doc in
  Cmd.group info [ generate_cmd ]

let () = exit (Cmd.eval main_cmd)
