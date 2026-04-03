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
  let iri = Chasity_lib.Iri.Iri "http://example.org/madeUpType" in
  match Chasity_lib.Proto_emit.proto_type_of_datatype iri with
  | Error (Unsupported_datatype _) -> ()
  | Error (Fractional_constraint _) ->
      Alcotest.fail
        "expected Unsupported_datatype but got Fractional_constraint"
  | Error (Node_without_class _) ->
      Alcotest.fail "expected Unsupported_datatype but got Node_without_class"
  | Ok proto_type -> Alcotest.failf "expected error but got %s" proto_type

let make_prop ?(path = "test") ?min_count ?max_count ?order () :
    Chasity_lib.Shacl.property_shape =
  {
    path = Iri path;
    datatype = None;
    min_count;
    max_count;
    pattern = None;
    class_ = None;
    node = None;
    in_ = [];
    or_ = [];
    min_length = None;
    max_length = None;
    min_inclusive = None;
    max_inclusive = None;
    min_exclusive = None;
    max_exclusive = None;
    name = None;
    description = None;
    order;
  }

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
  Alcotest.(check card_testable)
    "minCount=1 maxCount=1" Required
    (cardinality_of_property (make_prop ~min_count:1 ~max_count:1 ()));
  Alcotest.(check card_testable)
    "maxCount=1 no minCount" Optional
    (cardinality_of_property (make_prop ~max_count:1 ()));
  Alcotest.(check card_testable)
    "no maxCount" Repeated
    (cardinality_of_property (make_prop ()));
  Alcotest.(check card_testable)
    "minCount=1 no maxCount" Repeated
    (cardinality_of_property (make_prop ~min_count:1 ()))

let test_sort_by_order () =
  let open Chasity_lib.Proto_emit in
  let a = make_prop ~path:"a" ~order:3 () in
  let b = make_prop ~path:"b" () in
  let c = make_prop ~path:"c" ~order:1 () in
  let d = make_prop ~path:"d" () in
  let e = make_prop ~path:"e" ~order:2 () in
  let sorted =
    sort_by_order
      [
        (a, "string"); (b, "string"); (c, "string"); (d, "string"); (e, "string");
      ]
  in
  let paths =
    List.map
      (fun ((p : Chasity_lib.Shacl.property_shape), _) ->
        let (Iri s) = p.path in
        s)
      sorted
  in
  Alcotest.(check (list string))
    "ordered first, then unordered"
    [ "c"; "e"; "a"; "b"; "d" ]
    paths

let make_shape ?(props = []) iri cls : Chasity_lib.Shacl.node_shape =
  { iri = Iri iri; target_class = Iri cls; properties = props }

let test_sort_shapes () =
  let open Chasity_lib in
  (* A references B via sh:class, B references C via sh:class, D references B via sh:node *)
  let c = make_shape "s:C" "c:C" in
  let b =
    make_shape
      ~props:
        [
          (make_prop ~path:"refC" ()
          |> fun p -> { p with class_ = Some (Iri.Iri "c:C") });
        ]
      "s:B" "c:B"
  in
  let a =
    make_shape
      ~props:
        [
          (make_prop ~path:"refB" ()
          |> fun p -> { p with class_ = Some (Iri.Iri "c:B") });
        ]
      "s:A" "c:A"
  in
  let d =
    make_shape
      ~props:
        [
          (make_prop ~path:"refB" ()
          |> fun p -> { p with node = Some (Iri.Iri "s:B") });
        ]
      "s:D" "c:D"
  in
  let sorted = Shacl.sort_shapes [ a; d; b; c ] in
  let iris =
    List.map
      (fun (s : Shacl.node_shape) ->
        let (Iri.Iri i) = s.iri in
        i)
      sorted
  in
  (* C first (leaf), then B (depends on C), then A and D (both depend on B) *)
  Alcotest.(check (list string))
    "C before B before A and D"
    [ "s:C"; "s:B"; "s:A"; "s:D" ]
    iris

let test_to_local_name () =
  let open Chasity_lib in
  Alcotest.(check string)
    "slash IRI" "Organization"
    (Iri.to_local_name (Iri.Iri "http://schema.org/Organization"));
  Alcotest.(check string)
    "hash IRI" "string"
    (Iri.to_local_name (Iri.Iri "http://www.w3.org/2001/XMLSchema#string"))

let test_enum_mapping () =
  let open Chasity_lib in
  Alcotest.(check string)
    "enum type name" "Gender"
    (Proto_emit.enum_type_name (Iri.Iri "http://schema.org/gender"));
  Alcotest.(check string)
    "enum value" "GENDER_MALE"
    (Proto_emit.enum_value_name ~prefix:"gender" "male");
  Alcotest.(check string)
    "enum value" "GENDER_FEMALE"
    (Proto_emit.enum_value_name ~prefix:"gender" "female")

let test_snake_case () =
  let open Chasity_lib.String_ext in
  Alcotest.(check string)
    "camelCase" "birth_date_time"
    (to_snake_case "birthDateTime");
  Alcotest.(check string) "already snake" "name" (to_snake_case "name");
  Alcotest.(check string) "leading upper" "person" (to_snake_case "Person")

let test_emit_proto () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/person.ttl") with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples -> (
      let store = Chasity_lib.Triple_store.of_triples triples in
      let shapes = Chasity_lib.Shacl.extract_node_shapes store in
      let shape = List.hd shapes in
      match Chasity_lib.Proto_emit.emit_proto ~package:"test.v1" [ shape ] with
      | Error _ -> Alcotest.fail "emit_proto returned errors"
      | Ok proto ->
          let expected =
            String.concat "\n"
              [
                "syntax = \"proto3\";";
                "";
                "package test.v1;";
                "";
                "import \"buf/validate/validate.proto\";";
                "import \"google/protobuf/timestamp.proto\";";
                "";
                "enum Gender {";
                "  GENDER_UNSPECIFIED = 0;";
                "  GENDER_MALE = 1;";
                "  GENDER_FEMALE = 2;";
                "}";
                "";
                "message Person {";
                "  // Full name - The person's full legal name";
                "  repeated string name = 1 [(buf.validate.field).repeated = \
                 {min_items: 1, items: {string: {min_len: 1, max_len: 200}}}];";
                "  optional Gender gender = 2;";
                "  optional google.protobuf.Timestamp birth_date_time = 3;";
                "  optional int64 height_cm = 4 [(buf.validate.field).int64 = \
                 {gte: 140, lte: 210}];";
                "  optional int64 weight_lbs = 5 [(buf.validate.field).int64 = \
                 {gt: 80, lt: 500}];";
                "  repeated string email = 6 [(buf.validate.field).repeated = \
                 {min_items: 1, items: {string: {pattern: \
                 \"^.+@.+\\\\..+$\"}}}];";
                "  optional Organization employer = 7;";
                "}";
                "";
              ]
          in
          Alcotest.(check string) "proto output" expected proto)

let test_emit_or_shape () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/or_shape.ttl") with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples -> (
      let store = Chasity_lib.Triple_store.of_triples triples in
      let shapes = Chasity_lib.Shacl.extract_node_shapes store in
      let shape = List.hd shapes in
      match Chasity_lib.Proto_emit.emit_proto ~package:"test.v1" [ shape ] with
      | Error _ -> Alcotest.fail "emit_proto returned errors"
      | Ok proto ->
          let expected =
            String.concat "\n"
              [
                "syntax = \"proto3\";";
                "";
                "package test.v1;";
                "";
                "message Payment {";
                "  // Paid with";
                "  oneof paid_with {";
                "    CreditCard paid_with_credit_card = 1;";
                "    BankAccount paid_with_bank_account = 2;";
                "  }";
                "  // Amount in cents";
                "  int64 amount_cents = 3;";
                "}";
                "";
              ]
          in
          Alcotest.(check string) "or shape proto output" expected proto)

let test_emit_order () =
  match Chasity_lib.Ntriples.from_file (Path "fixtures/order.ttl") with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples -> (
      let store = Chasity_lib.Triple_store.of_triples triples in
      let shapes = Chasity_lib.Shacl.extract_node_shapes store in
      match Chasity_lib.Proto_emit.emit_proto ~package:"test.v1" shapes with
      | Error _ -> Alcotest.fail "emit_proto returned errors"
      | Ok proto ->
          let expected =
            String.concat "\n"
              [
                "syntax = \"proto3\";";
                "";
                "package test.v1;";
                "";
                "message Address {";
                "  string street = 1;";
                "  string city = 2;";
                "}";
                "";
                "message Order {";
                "  string customer_uri = 1;";
                "  Address shipping_address = 2;";
                "}";
                "";
              ]
          in
          Alcotest.(check string) "order proto output" expected proto)

let test_emit_bad_datatype () =
  match
    Chasity_lib.Ntriples.from_file (Path "fixtures/bad_shapes/bad_shape.ttl")
  with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples -> (
      let store = Chasity_lib.Triple_store.of_triples triples in
      let shapes = Chasity_lib.Shacl.extract_node_shapes store in
      let shape = List.hd shapes in
      match Chasity_lib.Proto_emit.emit_proto ~package:"test.v1" [ shape ] with
      | Ok _ -> Alcotest.fail "expected error but got Ok"
      | Error errs -> Alcotest.(check int) "error count" 1 (List.length errs))

let test_emit_invalid_turtle () =
  match
    Chasity_lib.Ntriples.from_file
      (Path "fixtures/bad_shapes/invalid_turtle.ttl")
  with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = _; exit_code }) ->
      Alcotest.(check int) "exit code" 1 exit_code
  | Ok _ -> Alcotest.fail "expected riot to fail"

let suite =
  ( "proto_emit",
    [
      Alcotest.test_case "datatype mappings" `Quick test_datatype_mappings;
      Alcotest.test_case "unsupported datatype" `Quick test_unsupported_datatype;
      Alcotest.test_case "cardinality" `Quick test_cardinality;
      Alcotest.test_case "sort_by_order" `Quick test_sort_by_order;
      Alcotest.test_case "sort_shapes topo" `Quick test_sort_shapes;
      Alcotest.test_case "to_local_name" `Quick test_to_local_name;
      Alcotest.test_case "enum mapping" `Quick test_enum_mapping;
      Alcotest.test_case "snake_case" `Quick test_snake_case;
      Alcotest.test_case "emit PersonShape" `Quick test_emit_proto;
      Alcotest.test_case "emit sh:or oneof" `Quick test_emit_or_shape;
      Alcotest.test_case "emit order ref+embed" `Quick test_emit_order;
      Alcotest.test_case "reject bad datatype" `Quick test_emit_bad_datatype;
      Alcotest.test_case "reject invalid turtle" `Quick test_emit_invalid_turtle;
    ] )
