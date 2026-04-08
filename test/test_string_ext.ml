let test_snake_case () =
  let open Chasity_lib.String_ext in
  Alcotest.(check string)
    "camelCase" "birth_date_time"
    (to_snake_case "birthDateTime");
  Alcotest.(check string) "already snake" "name" (to_snake_case "name");
  Alcotest.(check string) "leading upper" "person" (to_snake_case "Person")

let test_pluralize () =
  let open Chasity_lib.String_ext in
  (* regular: append s *)
  Alcotest.(check string) "Person" "Persons" (pluralize "Person");
  Alcotest.(check string) "Order" "Orders" (pluralize "Order");
  Alcotest.(check string)
    "Organization" "Organizations" (pluralize "Organization");
  (* ends in s/sh/ch/x/z: append es *)
  Alcotest.(check string) "Address" "Addresses" (pluralize "Address");
  Alcotest.(check string) "Wish" "Wishes" (pluralize "Wish");
  Alcotest.(check string) "Match" "Matches" (pluralize "Match");
  Alcotest.(check string) "Box" "Boxes" (pluralize "Box");
  Alcotest.(check string) "Topaz" "Topazes" (pluralize "Topaz");
  (* consonant + y: drop y, append ies *)
  Alcotest.(check string) "Category" "Categories" (pluralize "Category");
  Alcotest.(check string) "Policy" "Policies" (pluralize "Policy");
  Alcotest.(check string) "Entry" "Entries" (pluralize "Entry");
  Alcotest.(check string) "History" "Histories" (pluralize "History");
  (* vowel + y: just append s *)
  Alcotest.(check string) "Key" "Keys" (pluralize "Key");
  Alcotest.(check string) "Day" "Days" (pluralize "Day");
  Alcotest.(check string) "Survey" "Surveys" (pluralize "Survey")

let suite =
  ( "string_ext",
    [
      Alcotest.test_case "snake_case" `Quick test_snake_case;
      Alcotest.test_case "pluralize" `Quick test_pluralize;
    ] )
