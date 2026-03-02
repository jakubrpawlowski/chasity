open Cmdliner

let generate_cmd =
  let doc = "Generate .proto files from SHACL shapes." in
  let shapes =
    let doc = "Path to SHACL shapes file (.ttl)." in
    Arg.(required & opt (some file) None & info [ "shapes" ] ~doc)
  in
  let out =
    let doc = "Output directory for generated .proto files." in
    Arg.(value & opt dir "." & info [ "out" ] ~doc)
  in
  let run shapes _out =
    match Chasity_lib.Ntriples.from_file (Path shapes) with
    | Ok triples ->
        List.iter
          (fun t -> Fmt.pr "%a@." Chasity_lib.Ntriples.pp_triple t)
          triples;
        `Ok ()
    | Error (Riot_failed { path = Path p; exit_code }) ->
        Fmt.epr "riot failed on %s (exit %d)@." p exit_code;
        `Error (false, "riot failed")
  in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const run $ shapes $ out))

let main_cmd =
  let doc = "SHACL to Protobuf transpiler." in
  let info = Cmd.info "chasity" ~version:"0.1.0" ~doc in
  Cmd.group info [ generate_cmd ]

let () = exit (Cmd.eval main_cmd)
