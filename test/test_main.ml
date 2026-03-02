let test_parse_person () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/person.ttl") with
  | Ok triples -> Alcotest.(check int) "triple count" 15 (List.length triples)
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code

let () =
  Alcotest.run "chasity"
    [
      ( "ntriples",
        [ Alcotest.test_case "parse person.ttl" `Quick test_parse_person ] );
    ]
