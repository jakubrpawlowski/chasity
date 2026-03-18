let () =
  Alcotest.run "chasity"
    [
      Test_ntriples.suite;
      Test_shacl.suite;
      Test_proto_emit.suite;
      Test_validate_emit.suite;
      Test_resolve.suite;
    ]
