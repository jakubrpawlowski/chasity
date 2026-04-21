let make_prop ?(path = "test") ?datatype ?class_ ?node ?min_count ?max_count
    ?(in_ = []) ?(or_ = []) () : Chasity_lib.Shacl.property_shape =
  {
    path = Iri path;
    datatype = Option.map (fun d -> Chasity_lib.Iri.Iri d) datatype;
    min_count;
    max_count;
    pattern = None;
    class_;
    node;
    in_;
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

let kind_testable =
  let open Chasity_lib.Descriptor_emit in
  Alcotest.testable (fun fmt k -> Fmt.string fmt (field_kind_to_string k)) ( = )

let test_classify_field () =
  let open Chasity_lib.Descriptor_emit in
  let iri s = Chasity_lib.Iri.Iri s in
  Alcotest.(check kind_testable)
    "datatype → literal" Literal
    (classify_field
       (make_prop ~datatype:"http://www.w3.org/2001/XMLSchema#string" ()));
  Alcotest.(check kind_testable)
    "node + maxCount 1 → value_object" Value_object
    (classify_field
       (make_prop ~class_:(iri "c:Org") ~node:(iri "s:OrgShape") ~max_count:1 ()));
  Alcotest.(check kind_testable)
    "node + repeated → sub_entity" Sub_entity
    (classify_field
       (make_prop ~class_:(iri "c:Addr") ~node:(iri "s:AddrShape") ()));
  Alcotest.(check kind_testable)
    "class without node + maxCount 1 → uri_ref" Uri_ref
    (classify_field (make_prop ~class_:(iri "c:Person") ~max_count:1 ()));
  Alcotest.(check kind_testable)
    "class without node + repeated → repeated_uri" Repeated_uri
    (classify_field (make_prop ~class_:(iri "c:Tag") ()));
  Alcotest.(check kind_testable)
    "in_ non-empty → enum" Enum
    (classify_field (make_prop ~in_:[ "a"; "b" ] ()));
  Alcotest.(check kind_testable)
    "or_ non-empty → oneof" Oneof
    (classify_field
       (make_prop
          ~or_:[ Chasity_lib.Iri.Iri "c:A"; Chasity_lib.Iri.Iri "c:B" ]
          ()))

let shapes_of path =
  match Chasity_lib.Ntriples.from_file (Path path) with
  | Error (Chasity_lib.Ntriples.Riot_failed { path = Path p; exit_code }) ->
      Alcotest.failf "riot failed on %s (exit %d)" p exit_code
  | Ok triples ->
      Chasity_lib.Triple_store.of_triples triples
      |> Chasity_lib.Shacl.extract_node_shapes

let find_shape name shapes =
  List.find
    (fun (s : Chasity_lib.Shacl.node_shape) ->
      Chasity_lib.Iri.to_local_name s.target_class = name)
    shapes

let test_person_descriptor () =
  let shape = shapes_of "fixtures/person.ttl" |> List.hd in
  let json = Chasity_lib.Descriptor_emit.emit_descriptor shape in
  let expected =
    {|{
  "entity_name": "Person",
  "rdf_type": "http://schema.org/Person",
  "shape_iri": "http://schema.org/PersonShape",
  "fields": [
    {
      "name": "name",
      "predicate": "http://schema.org/name",
      "kind": "literal",
      "datatype": "http://www.w3.org/2001/XMLSchema#string",
      "required": true,
      "repeated": true,
      "validation": {
        "min_length": 1,
        "max_length": 200
      }
    },
    {
      "name": "gender",
      "predicate": "http://schema.org/gender",
      "kind": "enum",
      "values": [
        "male",
        "female"
      ],
      "required": false,
      "repeated": false
    },
    {
      "name": "birth_date_time",
      "predicate": "http://schema.org/birthDateTime",
      "kind": "literal",
      "datatype": "http://www.w3.org/2001/XMLSchema#dateTime",
      "required": false,
      "repeated": false
    },
    {
      "name": "height_cm",
      "predicate": "http://schema.org/heightCm",
      "kind": "literal",
      "datatype": "http://www.w3.org/2001/XMLSchema#integer",
      "required": false,
      "repeated": false,
      "validation": {
        "min_inclusive": 140,
        "max_inclusive": 210
      }
    },
    {
      "name": "weight_lbs",
      "predicate": "http://schema.org/weightLbs",
      "kind": "literal",
      "datatype": "http://www.w3.org/2001/XMLSchema#integer",
      "required": false,
      "repeated": false,
      "validation": {
        "min_exclusive": 80,
        "max_exclusive": 500
      }
    },
    {
      "name": "email",
      "predicate": "http://schema.org/email",
      "kind": "literal",
      "datatype": "http://www.w3.org/2001/XMLSchema#string",
      "required": true,
      "repeated": true,
      "validation": {
        "pattern": "^.+@.+\\\\..+$"
      }
    },
    {
      "name": "employer",
      "predicate": "http://schema.org/employer",
      "kind": "value_object",
      "reference": "Organization",
      "required": false,
      "repeated": false
    }
  ]
}
|}
  in
  Alcotest.(check string) "person descriptor" expected json

let test_order_descriptor () =
  let shapes = shapes_of "fixtures/order.ttl" in
  let order_shape = find_shape "Order" shapes in
  let json = Chasity_lib.Descriptor_emit.emit_descriptor order_shape in
  let expected =
    {|{
  "entity_name": "Order",
  "rdf_type": "http://example.org/Order",
  "shape_iri": "http://example.org/OrderShape",
  "fields": [
    {
      "name": "customer",
      "predicate": "http://example.org/customer",
      "kind": "uri_ref",
      "reference": "Person",
      "required": true,
      "repeated": false
    },
    {
      "name": "shipping_address",
      "predicate": "http://example.org/shippingAddress",
      "kind": "value_object",
      "reference": "Address",
      "required": true,
      "repeated": false
    }
  ]
}
|}
  in
  Alcotest.(check string) "order descriptor" expected json

let test_or_descriptor () =
  let shapes = shapes_of "fixtures/or_shape.ttl" in
  let payment = find_shape "Payment" shapes in
  let json = Chasity_lib.Descriptor_emit.emit_descriptor payment in
  let expected =
    {|{
  "entity_name": "Payment",
  "rdf_type": "http://example.org/Payment",
  "shape_iri": "http://example.org/PaymentShape",
  "fields": [
    {
      "name": "paid_with",
      "predicate": "http://example.org/paidWith",
      "kind": "oneof",
      "alternatives": [
        "CreditCard",
        "BankAccount"
      ],
      "required": true,
      "repeated": true
    },
    {
      "name": "amount_cents",
      "predicate": "http://example.org/amountCents",
      "kind": "literal",
      "datatype": "http://www.w3.org/2001/XMLSchema#integer",
      "required": true,
      "repeated": false
    }
  ]
}
|}
  in
  Alcotest.(check string) "or shape descriptor" expected json

let suite =
  ( "descriptor_emit",
    [
      Alcotest.test_case "classify field kinds" `Quick test_classify_field;
      Alcotest.test_case "person descriptor" `Quick test_person_descriptor;
      Alcotest.test_case "order descriptor" `Quick test_order_descriptor;
      Alcotest.test_case "or shape descriptor" `Quick test_or_descriptor;
    ] )
