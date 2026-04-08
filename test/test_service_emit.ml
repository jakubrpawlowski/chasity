let test_emit_service () =
  let proto =
    Chasity_lib.Service_emit.emit_service_proto ~package:"test.v1"
      ~entity_import:"test/v1/person.proto" [ "Person" ]
  in
  let expected =
    String.concat "\n"
      [
        "syntax = \"proto3\";";
        "";
        "package test.v1;";
        "";
        "import \"test/v1/person.proto\";";
        "";
        "service PersonService {";
        "  rpc ListPersonUris(ListPersonUrisRequest) returns \
         (ListPersonUrisResponse);";
        "  rpc BatchGetPersons(BatchGetPersonsRequest) returns \
         (BatchGetPersonsResponse);";
        "  rpc CreatePerson(CreatePersonRequest) returns \
         (CreatePersonResponse);";
        "  rpc GetPerson(GetPersonRequest) returns (GetPersonResponse);";
        "  rpc UpdatePerson(UpdatePersonRequest) returns \
         (UpdatePersonResponse);";
        "  rpc DeletePerson(DeletePersonRequest) returns \
         (DeletePersonResponse);";
        "}";
        "";
        "message ListPersonUrisRequest {";
        "  optional int32 page_size = 1;";
        "  optional string after_uri = 2;";
        "}";
        "";
        "message ListPersonUrisResponse {";
        "  repeated string uris = 1;";
        "  optional string next_cursor = 2;";
        "}";
        "";
        "message BatchGetPersonsRequest {";
        "  repeated string uris = 1;";
        "}";
        "";
        "message BatchGetPersonsResponse {";
        "  repeated GetPersonResponse persons = 1;";
        "}";
        "";
        "message CreatePersonRequest {";
        "  Person person = 1;";
        "}";
        "";
        "message CreatePersonResponse {";
        "  string uri = 1;";
        "}";
        "";
        "message GetPersonRequest {";
        "  string uri = 1;";
        "}";
        "";
        "message GetPersonResponse {";
        "  string uri = 1;";
        "  Person person = 2;";
        "}";
        "";
        "message UpdatePersonRequest {";
        "  string uri = 1;";
        "  Person person = 2;";
        "}";
        "";
        "message UpdatePersonResponse {";
        "  string uri = 1;";
        "}";
        "";
        "message DeletePersonRequest {";
        "  string uri = 1;";
        "}";
        "";
        "message DeletePersonResponse {}";
        "";
      ]
  in
  Alcotest.(check string) "person service proto" expected proto

let suite =
  ( "service_emit",
    [ Alcotest.test_case "emit PersonService" `Quick test_emit_service ] )
