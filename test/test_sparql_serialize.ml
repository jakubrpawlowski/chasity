open Chasity_lib

let i s = Iri.Iri s
let schema s = i ("http://schema.org/" ^ s)
let rdf_type = i "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

let test_insert_data_simple () =
  let open Sparql in
  let query =
    Insert_data
      {
        graph = None;
        triples =
          [
            {
              subject = schema "alice";
              predicate = rdf_type;
              object_ = Data_iri (schema "Person");
            };
            {
              subject = schema "alice";
              predicate = schema "name";
              object_ = Data_literal (String "Alice");
            };
          ];
      }
  in
  let result = Sparql_serialize.to_string query in
  let expected =
    {|INSERT DATA {
  <http://schema.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
  <http://schema.org/alice> <http://schema.org/name> "Alice" .
}|}
  in
  Alcotest.(check string) "insert data" expected result

let test_insert_data_with_graph () =
  let open Sparql in
  let query =
    Insert_data
      {
        graph = Some (i "urn:graph:tenant-1");
        triples =
          [
            {
              subject = schema "alice";
              predicate = schema "name";
              object_ = Data_literal (String "Alice");
            };
          ];
      }
  in
  let result = Sparql_serialize.to_string query in
  let expected =
    {|INSERT DATA {
  GRAPH <urn:graph:tenant-1> {
    <http://schema.org/alice> <http://schema.org/name> "Alice" .
  }
}|}
  in
  Alcotest.(check string) "insert data with graph" expected result

let test_delete_data () =
  let open Sparql in
  let query =
    Delete_data
      {
        graph = Some (i "urn:graph:tenant-1");
        triples =
          [
            {
              subject = schema "alice";
              predicate = schema "name";
              object_ = Data_literal (String "Alice");
            };
          ];
      }
  in
  let result = Sparql_serialize.to_string query in
  let expected =
    {|DELETE DATA {
  GRAPH <urn:graph:tenant-1> {
    <http://schema.org/alice> <http://schema.org/name> "Alice" .
  }
}|}
  in
  Alcotest.(check string) "delete data with graph" expected result

let test_literal_types () =
  let open Sparql in
  let check label expected_obj lit =
    let query =
      Insert_data
        {
          graph = None;
          triples =
            [
              {
                subject = i "urn:s";
                predicate = i "urn:p";
                object_ = Data_literal lit;
              };
            ];
        }
    in
    let result = Sparql_serialize.to_string query in
    let expected =
      Printf.sprintf "INSERT DATA {\n  <urn:s> <urn:p> %s .\n}" expected_obj
    in
    Alcotest.(check string) label expected result
  in
  check "int" "42" (Int 42);
  check "float" "3.14" (Float 3.14);
  check "bool true" "true" (Bool true);
  check "bool false" "false" (Bool false);
  check "typed" {|"2024-01-01"^^<http://www.w3.org/2001/XMLSchema#date>|}
    (Typed
       {
         value = "2024-01-01";
         datatype = i "http://www.w3.org/2001/XMLSchema#date";
       })

let test_string_escaping () =
  let open Sparql in
  let check label input expected_str =
    let query =
      Insert_data
        {
          graph = None;
          triples =
            [
              {
                subject = i "urn:s";
                predicate = i "urn:p";
                object_ = Data_literal (String input);
              };
            ];
        }
    in
    let result = Sparql_serialize.to_string query in
    let expected =
      Printf.sprintf "INSERT DATA {\n  <urn:s> <urn:p> \"%s\" .\n}" expected_str
    in
    Alcotest.(check string) label expected result
  in
  check "quotes" "say \"hello\"" {|say \"hello\"|};
  check "backslash" "a\\b" {|a\\b|};
  check "newline" "line1\nline2" {|line1\nline2|};
  check "tab" "a\tb" {|a\tb|}

let test_select () =
  let open Sparql in
  let query =
    Select
      {
        distinct = true;
        variables = [ Var "uri" ];
        where =
          Group
            [
              Triples
                [
                  {
                    subject = Sub_var (Var "uri");
                    predicate = Pred_iri rdf_type;
                    object_ = Obj_iri (schema "Person");
                  };
                ];
              Filter (Gt (Filt_var (Var "uri"), Filt_iri (i "urn:cursor:last")));
            ];
        order_by = [ (Var "uri", Asc) ];
        limit = Some 500;
      }
  in
  let result = Sparql_serialize.to_string query in
  let expected =
    {|SELECT DISTINCT ?uri
WHERE {
  ?uri <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
  FILTER(?uri > <urn:cursor:last>)
}
ORDER BY ASC(?uri)
LIMIT 500|}
  in
  Alcotest.(check string) "select with cursor" expected result

let test_construct () =
  let open Sparql in
  let query =
    Construct
      {
        template =
          [
            {
              subject = Sub_var (Var "uri");
              predicate = Pred_iri (schema "name");
              object_ = Obj_var (Var "name");
            };
            {
              subject = Sub_var (Var "uri");
              predicate = Pred_iri (schema "email");
              object_ = Obj_var (Var "email");
            };
          ];
        where =
          Group
            [
              Values
                {
                  variable = Var "uri";
                  values = [ i "urn:person:alice"; i "urn:person:bob" ];
                };
              Triples
                [
                  {
                    subject = Sub_var (Var "uri");
                    predicate = Pred_iri (schema "name");
                    object_ = Obj_var (Var "name");
                  };
                ];
              Optional
                (Triples
                   [
                     {
                       subject = Sub_var (Var "uri");
                       predicate = Pred_iri (schema "email");
                       object_ = Obj_var (Var "email");
                     };
                   ]);
            ];
      }
  in
  let result = Sparql_serialize.to_string query in
  let expected =
    {|CONSTRUCT {
  ?uri <http://schema.org/name> ?name .
  ?uri <http://schema.org/email> ?email .
}
WHERE {
  VALUES ?uri { <urn:person:alice> <urn:person:bob> }
  ?uri <http://schema.org/name> ?name .
  OPTIONAL {
    ?uri <http://schema.org/email> ?email .
  }
}|}
  in
  Alcotest.(check string) "construct with values and optional" expected result

let test_delete_insert () =
  let open Sparql in
  let g = i "urn:graph:tenant-1" in
  let query =
    Delete_insert
      {
        graph = Some g;
        delete =
          [
            {
              subject = Sub_iri (i "urn:person:alice");
              predicate = Pred_iri (schema "name");
              object_ = Obj_var (Var "old");
            };
          ];
        insert =
          [
            {
              subject = Sub_iri (i "urn:person:alice");
              predicate = Pred_iri (schema "name");
              object_ = Obj_literal (String "Alice Smith");
            };
          ];
        where =
          Graph
            {
              graph = g;
              pattern =
                Triples
                  [
                    {
                      subject = Sub_iri (i "urn:person:alice");
                      predicate = Pred_iri (schema "name");
                      object_ = Obj_var (Var "old");
                    };
                  ];
            };
      }
  in
  let result = Sparql_serialize.to_string query in
  let expected =
    {|DELETE {
  GRAPH <urn:graph:tenant-1> {
    <urn:person:alice> <http://schema.org/name> ?old .
  }
}
INSERT {
  GRAPH <urn:graph:tenant-1> {
    <urn:person:alice> <http://schema.org/name> "Alice Smith" .
  }
}
WHERE {
  GRAPH <urn:graph:tenant-1> {
    <urn:person:alice> <http://schema.org/name> ?old .
  }
}|}
  in
  Alcotest.(check string) "delete insert with graph" expected result

let suite =
  ( "sparql_serialize",
    [
      Alcotest.test_case "insert data" `Quick test_insert_data_simple;
      Alcotest.test_case "insert data with graph" `Quick
        test_insert_data_with_graph;
      Alcotest.test_case "delete data" `Quick test_delete_data;
      Alcotest.test_case "literal types" `Quick test_literal_types;
      Alcotest.test_case "string escaping" `Quick test_string_escaping;
      Alcotest.test_case "select" `Quick test_select;
      Alcotest.test_case "construct" `Quick test_construct;
      Alcotest.test_case "delete insert" `Quick test_delete_insert;
    ] )
