let () =
  Alcotest.run "chasity"
    [ Test_ntriples.suite; Test_shacl.suite; Test_proto_emit.suite ]
