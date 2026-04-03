let make_prop ?(path = "http://example.org/name") ?class_ ?node ?(or_ = []) () :
    Chasity_lib.Shacl.property_shape =
  {
    path = Iri path;
    datatype = Some (Iri "http://www.w3.org/2001/XMLSchema#string");
    min_count = None;
    max_count = None;
    pattern = None;
    class_;
    node;
    in_ = [];
    or_;
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

let make_shape ~iri ~target_class properties : Chasity_lib.Shacl.node_shape =
  { iri = Iri iri; target_class = Iri target_class; properties }

(* No references — shape with only datatype properties *)
let test_no_references () =
  let shape =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1" [ ("person.ttl", [ shape ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ group ] ->
      Alcotest.(check (list string)) "no imports" [] group.imports;
      Alcotest.(check int) "no warnings" 0 (List.length group.warnings)
  | Ok _ -> Alcotest.fail "expected one group"

(* Unresolved sh:node — references a shape that isn't loaded *)
let test_unresolved_node () =
  let shape =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [ make_prop ~node:(Iri "http://ex.org/OrgShape") () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1" [ ("person.ttl", [ shape ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ group ] ->
      Alcotest.(check (list string)) "no imports" [] group.imports;
      Alcotest.(check int) "one warning" 1 (List.length group.warnings)
  | Ok _ -> Alcotest.fail "expected one group"

(* Unresolved sh:or — two class IRIs, neither loaded *)
let test_unresolved_or () =
  let shape =
    make_shape ~iri:"http://ex.org/PaymentShape"
      ~target_class:"http://ex.org/Payment"
      [
        make_prop
          ~or_:
            [ Iri "http://ex.org/CreditCard"; Iri "http://ex.org/BankAccount" ]
          ();
      ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("payment.ttl", [ shape ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ group ] ->
      Alcotest.(check (list string)) "no imports" [] group.imports;
      Alcotest.(check int) "two warnings" 2 (List.length group.warnings)
  | Ok _ -> Alcotest.fail "expected one group"

(* Cross-file via sh:node — person references org in another file *)
let test_cross_file_node () =
  let person =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [ make_prop ~node:(Iri "http://ex.org/OrgShape") () ]
  in
  let org =
    make_shape ~iri:"http://ex.org/OrgShape"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("person.ttl", [ person ]); ("organization.ttl", [ org ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ person_group; _ ] ->
      Alcotest.(check (list string))
        "import org"
        [ "test/v1/organization.proto" ]
        person_group.imports;
      Alcotest.(check int) "no warnings" 0 (List.length person_group.warnings)
  | Ok _ -> Alcotest.fail "expected two groups"

(* Cross-file embed — sh:class + sh:node needs import *)
let test_cross_file_embed () =
  let person =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [
        make_prop ~class_:(Iri "http://ex.org/Organization")
          ~node:(Iri "http://ex.org/OrgShape") ();
      ]
  in
  let org =
    make_shape ~iri:"http://ex.org/OrgShape"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("person.ttl", [ person ]); ("organization.ttl", [ org ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ person_group; _ ] ->
      Alcotest.(check (list string))
        "import org"
        [ "test/v1/organization.proto" ]
        person_group.imports;
      Alcotest.(check int) "no warnings" 0 (List.length person_group.warnings)
  | Ok _ -> Alcotest.fail "expected two groups"

(* Cross-file reference — sh:class only is string, no import needed *)
let test_cross_file_reference () =
  let person =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [ make_prop ~class_:(Iri "http://ex.org/Organization") () ]
  in
  let org =
    make_shape ~iri:"http://ex.org/OrgShape"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("person.ttl", [ person ]); ("organization.ttl", [ org ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ person_group; _ ] ->
      Alcotest.(check (list string))
        "no imports for class-only reference" [] person_group.imports;
      Alcotest.(check int) "no warnings" 0 (List.length person_group.warnings)
  | Ok _ -> Alcotest.fail "expected two groups"

(* sh:node and sh:class mismatch — they resolve to different files,
   both imports emitted plus a warning *)
let test_node_class_mismatch () =
  let person =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [
        make_prop ~node:(Iri "http://ex.org/OrgShape")
          ~class_:(Iri "http://ex.org/Company") ();
      ]
  in
  let org =
    make_shape ~iri:"http://ex.org/OrgShape"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  let company =
    make_shape ~iri:"http://ex.org/CompanyShape"
      ~target_class:"http://ex.org/Company"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [
        ("person.ttl", [ person ]);
        ("org.ttl", [ org ]);
        ("company.ttl", [ company ]);
      ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ person_group; _; _ ] ->
      Alcotest.(check (list string))
        "both imports"
        [ "test/v1/company.proto"; "test/v1/org.proto" ]
        person_group.imports;
      Alcotest.(check int)
        "mismatch warning" 1
        (List.length person_group.warnings)
  | Ok _ -> Alcotest.fail "expected three groups"

(* Local reference — two shapes in one file, no import needed *)
let test_local_reference () =
  let person =
    make_shape ~iri:"http://ex.org/PersonShape"
      ~target_class:"http://ex.org/Person"
      [ make_prop ~class_:(Iri "http://ex.org/Organization") () ]
  in
  let org =
    make_shape ~iri:"http://ex.org/OrgShape"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("schema.ttl", [ person; org ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok [ group ] ->
      Alcotest.(check (list string)) "no imports" [] group.imports;
      Alcotest.(check int) "no warnings" 0 (List.length group.warnings)
  | Ok _ -> Alcotest.fail "expected one group"

(* Duplicate IRI across files — same target class in two files *)
let test_duplicate_across_files () =
  let org_a =
    make_shape ~iri:"http://ex.org/OrgShapeA"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  let org_b =
    make_shape ~iri:"http://ex.org/OrgShapeB"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("org_a.ttl", [ org_a ]); ("org_b.ttl", [ org_b ]) ]
  with
  | Error (Duplicate_iri _) -> ()
  | Ok _ -> Alcotest.fail "expected error"

(* Duplicate IRI within same file — two shapes targeting same class *)
let test_duplicate_within_file () =
  let org_a =
    make_shape ~iri:"http://ex.org/OrgShapeA"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  let org_b =
    make_shape ~iri:"http://ex.org/OrgShapeB"
      ~target_class:"http://ex.org/Organization"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1"
      [ ("org.ttl", [ org_a; org_b ]) ]
  with
  | Error (Duplicate_iri _) -> ()
  | Ok _ -> Alcotest.fail "expected error"

(* Same shape IRI equals target class — no error *)
let test_same_iri () =
  let shape =
    make_shape ~iri:"http://ex.org/Person" ~target_class:"http://ex.org/Person"
      [ make_prop () ]
  in
  match
    Chasity_lib.Resolve.resolve ~package:"test.v1" [ ("person.ttl", [ shape ]) ]
  with
  | Error _ -> Alcotest.fail "unexpected error"
  | Ok _ -> ()

let suite =
  ( "resolve",
    [
      Alcotest.test_case "no references" `Quick test_no_references;
      Alcotest.test_case "unresolved sh:node" `Quick test_unresolved_node;
      Alcotest.test_case "unresolved sh:or" `Quick test_unresolved_or;
      Alcotest.test_case "cross-file via sh:node" `Quick test_cross_file_node;
      Alcotest.test_case "cross-file embed" `Quick test_cross_file_embed;
      Alcotest.test_case "cross-file reference" `Quick test_cross_file_reference;
      Alcotest.test_case "sh:node/sh:class mismatch" `Quick
        test_node_class_mismatch;
      Alcotest.test_case "local reference" `Quick test_local_reference;
      Alcotest.test_case "duplicate across files" `Quick
        test_duplicate_across_files;
      Alcotest.test_case "duplicate within file" `Quick
        test_duplicate_within_file;
      Alcotest.test_case "same IRI" `Quick test_same_iri;
    ] )
