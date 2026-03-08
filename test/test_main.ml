let test_parse_person () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/person.ttl") with
  | Ok triples -> Alcotest.(check int) "triple count" 32 (List.length triples)
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
      Alcotest.(check int) "property count" 5 (List.length shape.properties);
      let gender =
        List.find
          (fun (p : Chasity_lib.Shacl.property_shape) ->
            let (Iri s) = p.path in
            s = "http://schema.org/gender")
          shape.properties
      in
      Alcotest.(check (list string))
        "gender enum values" [ "male"; "female" ] gender.in_

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
    ]
