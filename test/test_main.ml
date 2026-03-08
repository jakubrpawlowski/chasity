let test_parse_person () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/person.ttl") with
  | Ok triples -> Alcotest.(check int) "triple count" 36 (List.length triples)
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code

let test_extract_person_shape () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/person.ttl") with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples ->
      let store = Chasity_lib.Triple_store.of_triples triples in
      let shapes = Chasity_lib.Shacl.extract_node_shapes store in
      Alcotest.(check int) "shape count" 1 (List.length shapes);
      let shape = List.hd shapes in
      Alcotest.(check string)
        "target class" "http://schema.org/Person"
        (let (Iri s) = shape.target_class in
         s);
      Alcotest.(check int) "property count" 6 (List.length shape.properties);
      let gender =
        List.find
          (fun (p : Chasity_lib.Shacl.property_shape) ->
            let (Iri s) = p.path in
            s = "http://schema.org/gender")
          shape.properties
      in
      Alcotest.(check (list string))
        "gender enum values" [ "male"; "female" ] gender.in_

let test_datatype_mappings () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/all_types.ttl") with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples ->
      let store = Chasity_lib.Triple_store.of_triples triples in
      let shapes = Chasity_lib.Shacl.extract_node_shapes store in
      let shape = List.hd shapes in
      let expected =
        [
          ("http://example.org/stringField", "string");
          ("http://example.org/anyURIField", "string");
          ("http://example.org/dateField", "string");
          ("http://example.org/timeField", "string");
          ("http://example.org/durationField", "string");
          ("http://example.org/integerField", "int64");
          ("http://example.org/longField", "int64");
          ("http://example.org/intField", "int32");
          ("http://example.org/nonNegativeIntegerField", "uint64");
          ("http://example.org/floatField", "float");
          ("http://example.org/doubleField", "double");
          ("http://example.org/decimalField", "double");
          ("http://example.org/booleanField", "bool");
          ("http://example.org/base64BinaryField", "bytes");
          ("http://example.org/dateTimeField", "google.protobuf.Timestamp");
        ]
      in
      List.iter
        (fun (path_iri, expected_proto) ->
          let prop =
            List.find
              (fun (p : Chasity_lib.Shacl.property_shape) ->
                let (Iri s) = p.path in
                s = path_iri)
              shape.properties
          in
          match prop.datatype with
          | None -> Alcotest.failf "no datatype for %s" path_iri
          | Some dt -> (
              match Chasity_lib.Proto_emit.proto_type_of_datatype dt with
              | Ok proto_type ->
                  Alcotest.(check string) path_iri expected_proto proto_type
              | Error _ -> Alcotest.failf "unsupported datatype for %s" path_iri
              ))
        expected

let () =
  Alcotest.run "chasity"
    [
      ( "ntriples",
        [ Alcotest.test_case "parse person.ttl" `Quick test_parse_person ] );
      ( "shacl",
        [
          Alcotest.test_case "extract PersonShape" `Quick
            test_extract_person_shape;
        ] );
      ( "proto_emit",
        [ Alcotest.test_case "datatype mappings" `Quick test_datatype_mappings ]
      );
    ]
