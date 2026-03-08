let test_parse_person () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/person.ttl") with
  | Ok triples -> Alcotest.(check int) "triple count" 51 (List.length triples)
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
      Alcotest.(check int) "property count" 7 (List.length shape.properties);
      let find path_iri =
        List.find
          (fun (p : Chasity_lib.Shacl.property_shape) ->
            let (Iri s) = p.path in
            s = path_iri)
          shape.properties
      in
      let iri_str = Option.map (fun (Chasity_lib.Shacl.Iri s) -> s) in
      (* name: datatype, minCount, minLength, maxLength, name, description, order *)
      let name = find "http://schema.org/name" in
      Alcotest.(check (option string))
        "name datatype" (Some "http://www.w3.org/2001/XMLSchema#string")
        (iri_str name.datatype);
      Alcotest.(check (option int)) "name minCount" (Some 1) name.min_count;
      Alcotest.(check (option int)) "name minLength" (Some 1) name.min_length;
      Alcotest.(check (option int)) "name maxLength" (Some 200) name.max_length;
      Alcotest.(check (option string))
        "name sh:name" (Some "Full name") name.name;
      Alcotest.(check (option string))
        "name sh:description" (Some "The person's full legal name")
        name.description;
      Alcotest.(check (option int)) "name order" (Some 1) name.order;
      (* email: datatype, minCount, pattern *)
      let email = find "http://schema.org/email" in
      Alcotest.(check (option int)) "email minCount" (Some 1) email.min_count;
      Alcotest.(check (option string))
        "email pattern" (Some "^.+@.+\\\\..+$") email.pattern;
      Alcotest.(check (option int)) "email order" (Some 6) email.order;
      (* birthDateTime: datatype, maxCount *)
      let birth = find "http://schema.org/birthDateTime" in
      Alcotest.(check (option string))
        "birthDateTime datatype"
        (Some "http://www.w3.org/2001/XMLSchema#dateTime")
        (iri_str birth.datatype);
      Alcotest.(check (option int))
        "birthDateTime maxCount" (Some 1) birth.max_count;
      Alcotest.(check (option int)) "birthDateTime order" (Some 3) birth.order;
      (* heightCm: datatype, maxCount, minInclusive, maxInclusive *)
      let height = find "http://schema.org/heightCm" in
      Alcotest.(check (option int))
        "heightCm maxCount" (Some 1) height.max_count;
      Alcotest.(check (option int))
        "heightCm minInclusive" (Some 140) height.min_inclusive;
      Alcotest.(check (option int))
        "heightCm maxInclusive" (Some 210) height.max_inclusive;
      Alcotest.(check (option int)) "heightCm order" (Some 4) height.order;
      (* weightLbs: datatype, maxCount, minExclusive, maxExclusive *)
      let weight = find "http://schema.org/weightLbs" in
      Alcotest.(check (option int))
        "weightLbs maxCount" (Some 1) weight.max_count;
      Alcotest.(check (option int))
        "weightLbs minExclusive" (Some 80) weight.min_exclusive;
      Alcotest.(check (option int))
        "weightLbs maxExclusive" (Some 500) weight.max_exclusive;
      Alcotest.(check (option int)) "weightLbs order" (Some 5) weight.order;
      (* gender: in_, maxCount *)
      let gender = find "http://schema.org/gender" in
      Alcotest.(check (list string))
        "gender enum values" [ "male"; "female" ] gender.in_;
      Alcotest.(check (option int)) "gender maxCount" (Some 1) gender.max_count;
      Alcotest.(check (option int)) "gender order" (Some 2) gender.order;
      (* employer: class_, node, maxCount *)
      let employer = find "http://schema.org/employer" in
      Alcotest.(check (option string))
        "employer class" (Some "http://schema.org/Organization")
        (iri_str employer.class_);
      Alcotest.(check (option string))
        "employer node" (Some "http://schema.org/OrganizationShape")
        (iri_str employer.node);
      Alcotest.(check (option int))
        "employer maxCount" (Some 1) employer.max_count;
      Alcotest.(check (option int)) "employer order" (Some 7) employer.order

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

let test_unsupported_datatype () =
  let iri = Chasity_lib.Shacl.Iri "http://example.org/madeUpType" in
  match Chasity_lib.Proto_emit.proto_type_of_datatype iri with
  | Error (Unsupported_datatype _) -> ()
  | Ok proto_type -> Alcotest.failf "expected error but got %s" proto_type

let test_cardinality () =
  let open Chasity_lib.Proto_emit in
  let card_testable =
    Alcotest.testable
      (fun fmt c ->
        Fmt.string fmt
          (match c with
          | Required -> "Required"
          | Optional -> "Optional"
          | Repeated -> "Repeated"))
      ( = )
  in
  let make ?min_count ?max_count () : Chasity_lib.Shacl.property_shape =
    {
      path = Iri "test";
      datatype = None;
      min_count;
      max_count;
      pattern = None;
      class_ = None;
      node = None;
      in_ = [];
      min_length = None;
      max_length = None;
      min_inclusive = None;
      max_inclusive = None;
      min_exclusive = None;
      max_exclusive = None;
      name = None;
      description = None;
      order = None;
    }
  in
  Alcotest.(check card_testable)
    "minCount=1 maxCount=1" Required
    (cardinality_of_property (make ~min_count:1 ~max_count:1 ()));
  Alcotest.(check card_testable)
    "maxCount=1 no minCount" Optional
    (cardinality_of_property (make ~max_count:1 ()));
  Alcotest.(check card_testable)
    "no maxCount" Repeated
    (cardinality_of_property (make ()));
  Alcotest.(check card_testable)
    "minCount=1 no maxCount" Repeated
    (cardinality_of_property (make ~min_count:1 ()))

let test_local_name_of_iri () =
  let open Chasity_lib in
  Alcotest.(check string)
    "slash IRI" "Organization"
    (Proto_emit.local_name_of_iri (Shacl.Iri "http://schema.org/Organization"));
  Alcotest.(check string)
    "hash IRI" "string"
    (Proto_emit.local_name_of_iri
       (Shacl.Iri "http://www.w3.org/2001/XMLSchema#string"))

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
        [
          Alcotest.test_case "datatype mappings" `Quick test_datatype_mappings;
          Alcotest.test_case "unsupported datatype" `Quick
            test_unsupported_datatype;
          Alcotest.test_case "cardinality" `Quick test_cardinality;
          Alcotest.test_case "local_name_of_iri" `Quick test_local_name_of_iri;
        ] );
    ]
