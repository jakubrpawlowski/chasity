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

let test_enum_mapping () =
  let open Chasity_lib in
  Alcotest.(check string)
    "enum type name" "Gender"
    (Proto_emit.enum_type_name (Shacl.Iri "http://schema.org/gender"));
  Alcotest.(check string)
    "enum value" "GENDER_MALE"
    (Proto_emit.enum_value_name ~prefix:"gender" "male");
  Alcotest.(check string)
    "enum value" "GENDER_FEMALE"
    (Proto_emit.enum_value_name ~prefix:"gender" "female")

let suite =
  ( "proto_emit",
    [
      Alcotest.test_case "datatype mappings" `Quick test_datatype_mappings;
      Alcotest.test_case "unsupported datatype" `Quick test_unsupported_datatype;
      Alcotest.test_case "cardinality" `Quick test_cardinality;
      Alcotest.test_case "local_name_of_iri" `Quick test_local_name_of_iri;
      Alcotest.test_case "enum mapping" `Quick test_enum_mapping;
    ] )
