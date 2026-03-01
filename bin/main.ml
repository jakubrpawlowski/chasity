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
  let run shapes out =
    Fmt.pr "generate: shapes=%s out=%s@." shapes out;
    `Ok ()
  in
  let info = Cmd.info "generate" ~doc in
  Cmd.v info Term.(ret (const run $ shapes $ out))

let main_cmd =
  let doc = "SHACL to Protobuf transpiler." in
  let info = Cmd.info "chasity" ~version:"0.1.0" ~doc in
  Cmd.group info [ generate_cmd ]

let () = exit (Cmd.eval main_cmd)
