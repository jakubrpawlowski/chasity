(* Service generation: emits .proto service files with CRUD RPCs *)

let message name fields =
  match fields with
  | [] -> Printf.sprintf "message %s {}\n" name
  | _ ->
      Printf.sprintf "message %s {\n%s}\n" name
        (fields |> List.map (Printf.sprintf "  %s;\n") |> String.concat "")

let emit_entity_blocks name =
  let snake = String_ext.to_snake_case name in
  let plural = String_ext.pluralize name in
  let snake_plural = String_ext.to_snake_case plural in
  let service =
    Printf.sprintf "service %sService {\n%s}\n" name
      (String.concat ""
         [
           Printf.sprintf
             "  rpc List%sUris(List%sUrisRequest) returns (List%sUrisResponse);\n"
             name name name;
           Printf.sprintf
             "  rpc BatchGet%s(BatchGet%sRequest) returns (BatchGet%sResponse);\n"
             plural plural plural;
           Printf.sprintf
             "  rpc Create%s(Create%sRequest) returns (Create%sResponse);\n"
             name name name;
           Printf.sprintf "  rpc Get%s(Get%sRequest) returns (Get%sResponse);\n"
             name name name;
           Printf.sprintf
             "  rpc Update%s(Update%sRequest) returns (Update%sResponse);\n"
             name name name;
           Printf.sprintf
             "  rpc Delete%s(Delete%sRequest) returns (Delete%sResponse);\n"
             name name name;
         ])
  in
  [
    service;
    message
      (Printf.sprintf "List%sUrisRequest" name)
      [ "optional int32 page_size = 1"; "optional string after_uri = 2" ];
    message
      (Printf.sprintf "List%sUrisResponse" name)
      [ "repeated string uris = 1"; "optional string next_cursor = 2" ];
    message
      (Printf.sprintf "BatchGet%sRequest" plural)
      [ "repeated string uris = 1" ];
    message
      (Printf.sprintf "BatchGet%sResponse" plural)
      [ Printf.sprintf "repeated Get%sResponse %s = 1" name snake_plural ];
    message
      (Printf.sprintf "Create%sRequest" name)
      [ Printf.sprintf "%s %s = 1" name snake ];
    message (Printf.sprintf "Create%sResponse" name) [ "string uri = 1" ];
    message (Printf.sprintf "Get%sRequest" name) [ "string uri = 1" ];
    message
      (Printf.sprintf "Get%sResponse" name)
      [ "string uri = 1"; Printf.sprintf "%s %s = 2" name snake ];
    message
      (Printf.sprintf "Update%sRequest" name)
      [ "string uri = 1"; Printf.sprintf "%s %s = 2" name snake ];
    message (Printf.sprintf "Update%sResponse" name) [ "string uri = 1" ];
    message (Printf.sprintf "Delete%sRequest" name) [ "string uri = 1" ];
    message (Printf.sprintf "Delete%sResponse" name) [];
  ]

let emit_service_proto ~package ~entity_import names =
  let header =
    Printf.sprintf "syntax = \"proto3\";\n\npackage %s;\n\n" package
  in
  let import = Printf.sprintf "import \"%s\";\n\n" entity_import in
  let blocks = List.concat_map emit_entity_blocks names in
  header ^ import ^ String.concat "\n" blocks
