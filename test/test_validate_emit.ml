let make_prop () : Chasity_lib.Shacl.property_shape =
  {
    path = Iri "test";
    datatype = None;
    min_count = None;
    max_count = None;
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
    order = None;
  }

let test_validate_emit () =
  let open Chasity_lib in
  let base = make_prop () in
  (* no constraints -> empty *)
  Alcotest.(check string)
    "no constraints" ""
    (Validate_emit.emit_field_options "string" base);
  (* string pattern *)
  Alcotest.(check string)
    "pattern" " [(buf.validate.field).string = {pattern: \"^[a-z]+$\"}]"
    (Validate_emit.emit_field_options "string"
       { base with pattern = Some "^[a-z]+$" });
  (* string length *)
  Alcotest.(check string)
    "string length"
    " [(buf.validate.field).string = {min_len: 1, max_len: 100}]"
    (Validate_emit.emit_field_options "string"
       { base with min_length = Some 1; max_length = Some 100 });
  (* numeric inclusive *)
  Alcotest.(check string)
    "int64 inclusive" " [(buf.validate.field).int64 = {gte: 0, lte: 99}]"
    (Validate_emit.emit_field_options "int64"
       { base with min_inclusive = Some 0.; max_inclusive = Some 99. });
  (* numeric exclusive *)
  Alcotest.(check string)
    "double exclusive" " [(buf.validate.field).double = {gt: 0, lt: 100}]"
    (Validate_emit.emit_field_options "double"
       { base with min_exclusive = Some 0.; max_exclusive = Some 100. });
  (* float with fractional constraints *)
  Alcotest.(check string)
    "double fractional" " [(buf.validate.field).double = {gte: 0.5, lt: 99.9}]"
    (Validate_emit.emit_field_options "double"
       { base with min_inclusive = Some 0.5; max_exclusive = Some 99.9 });
  (* repeated min_items *)
  Alcotest.(check string)
    "repeated min_items" " [(buf.validate.field).repeated.min_items = 2]"
    (Validate_emit.emit_field_options "string" { base with min_count = Some 2 });
  (* string constraints + repeated min_items combined *)
  Alcotest.(check string)
    "combined string + repeated"
    " [(buf.validate.field).string = {min_len: 1}, \
     (buf.validate.field).repeated.min_items = 1]"
    (Validate_emit.emit_field_options "string"
       { base with min_length = Some 1; min_count = Some 1 });
  (* constraints for wrong type silently ignored *)
  Alcotest.(check string)
    "string constraints ignored on bool" ""
    (Validate_emit.emit_field_options "bool"
       { base with min_length = Some 1; max_count = Some 1 });
  (* has_constraints *)
  Alcotest.(check bool)
    "has_constraints true" true
    (Validate_emit.has_constraints "string" { base with pattern = Some ".*" });
  Alcotest.(check bool)
    "has_constraints false" false
    (Validate_emit.has_constraints "string" base);
  (* fractional constraints on integer types *)
  Alcotest.(check bool)
    "fractional on int64" true
    (Validate_emit.has_fractional_int_constraints "int64"
       { base with min_inclusive = Some 3.7 });
  Alcotest.(check bool)
    "whole on int64" false
    (Validate_emit.has_fractional_int_constraints "int64"
       { base with min_inclusive = Some 3. });
  Alcotest.(check bool)
    "fractional on double" false
    (Validate_emit.has_fractional_int_constraints "double"
       { base with min_inclusive = Some 3.7 })

let suite =
  ( "validate_emit",
    [ Alcotest.test_case "validate_emit" `Quick test_validate_emit ] )
