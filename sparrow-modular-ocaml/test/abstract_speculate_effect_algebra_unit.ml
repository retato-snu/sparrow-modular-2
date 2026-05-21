module Algebra = Sparrow_modular_ocaml.Abstract_speculate_effect_algebra
module Projection = Sparrow_modular_ocaml.Abstract_speculate_effect_projection
module Schema = Sparrow_modular_ocaml.Abstract_speculate_effect_schema
module Scalar = Sparrow_modular_ocaml.Abstract_speculate_residual_scalar_call

let expect cond msg = if not cond then failwith msg

let expect_defined = function
  | Algebra.Defined x -> x
  | Algebra.Undefined reason ->
      failwith ("unexpected undefined: " ^ Algebra.undefined_reason_to_string reason)

let expect_undefined expected = function
  | Algebra.Defined _ -> failwith "expected undefined operation"
  | Algebra.Undefined reason -> expect (reason = expected) "unexpected undefined reason"

let member = Yojson.Safe.Util.member

let string_field name json =
  match member name json with `String s -> s | _ -> ""

let provenance () =
  expect_defined
    (Algebra.make_provenance ~provider_module:"provider.c"
       ~provider_source_hash:"hash-1" ~provider_artifact_path:"/tmp/provider.json"
       ~provider_phase_index:1 ~export_name:"provide")

let () =
  let provenance = provenance () in
  let return_eff =
    expect_defined
      (Algebra.make_return ~provenance ~return_location:"(provide,__return__)"
         ~abstract_value:"7" ~evidence_path:[ "provider_row.return" ])
  in
  let global_eff =
    expect_defined
      (Algebra.make_memory_transition ~provenance ~kind:Algebra.Memory_read_write
         ~location:"shared_g" ~value:"([7,7], unit)" ~alias_evidence:None
         ~evidence_path:[ "provider_row.memory"; "shared_g" ])
  in
  let taint_eff =
    expect_defined
      (Algebra.make_taint ~provenance ~source:"provider_row.return"
         ~sink:"(provide_taint,__return__)" ~taint_state:"tainted"
         ~evidence_path:[ "provider_row.taint_components" ])
  in
  let product_eff =
    expect_defined
      (Algebra.make_product_pair ~provenance ~left_effect:return_eff
         ~right_effect:taint_eff ~evidence_path:[ "provider_row.product_pair_evidence" ])
  in
  expect_undefined Algebra.Taint_product_mismatch
    (Algebra.make_product_pair ~provenance ~left_effect:return_eff
       ~right_effect:global_eff ~evidence_path:[ "provider_row.bad_product" ]);
  let joined = expect_defined (Algebra.join global_eff global_eff) in
  expect (Algebra.effect_domain joined = Algebra.Memory Algebra.Memory_read_write)
    "join changed domain";
  let restricted = expect_defined (Algebra.restrict ~path:[] global_eff) in
  expect (Algebra.effect_path restricted = []) "restrict did not narrow to root path";
  expect (Algebra.compose_identity_holds return_eff) "identity law failed";
  expect (Algebra.join_idempotent_holds global_eff) "join idempotence law failed";
  expect (Algebra.join_commutative_holds global_eff global_eff) "join commutativity law failed";
  expect (Algebra.restrict_idempotent_holds ~path:[] global_eff) "restrict idempotence failed";
  let return_projection = expect_defined (Projection.observe_return return_eff) in
  let global_projection = expect_defined (Projection.observe_global global_eff) in
  let taint_projection = expect_defined (Projection.observe_taint taint_eff) in
  let product_projection = expect_defined (Projection.observe_product_pair product_eff) in
  expect
    (Algebra.projection_source_effect_id return_projection = Algebra.effect_id return_eff)
    "return projection lost source effect";
  expect_undefined Algebra.Missing_alias_evidence (Projection.observe_pointer return_eff);
  let effect_json = Schema.effect_to_json return_eff in
  let projection_json = Schema.projection_to_json return_projection in
  expect (string_field "schema_version" effect_json = Schema.schema_id)
    "effect JSON missing schema";
  expect (string_field "schema_version" projection_json = Schema.schema_id)
    "projection JSON missing schema";
  expect (Schema.validate_defined_artifact_json effect_json = Ok ())
    "defined effect rejected by schema";
  expect
    (match Schema.defined_effect_to_json (Algebra.Undefined Algebra.Incompatible_domain) with
    | Ok _ -> false
    | Error Algebra.Incompatible_domain -> true
    | Error _ -> false)
    "undefined effect serialized as valid artifact";
  let summary =
    Schema.summary_to_json
      ~effects:[ return_eff; global_eff; taint_eff; product_eff ]
      ~projections:[ return_projection; global_projection; taint_projection; product_projection ]
      ~legacy_projection:(`Assoc [ "legacy_v3_authority_status", `String "projection-only" ])
  in
  expect (string_field "schema_version" summary = Schema.schema_id)
    "summary JSON missing typed schema";
  let legacy_effect_id =
    Scalar.provider_return_effect_id ~provider_module:"provider.c" ~export_name:"provide"
      ~return_location:"(provide,__return__)"
  in
  let scalar_derivation =
    Scalar.add_fields
      [
        "effect_reason", `String "linked-provider-return";
        "derivation_source", `String "provider-stage2-output";
        "external_summary_schema", `String Schema.schema_id;
        "summary_api_status", `String Schema.summary_api_status;
        "external_summary_effect_id", `String legacy_effect_id;
        "return_effect",
        Scalar.add_fields
          [
            "domain", `String "return";
            "derivation_source", `String "provider-stage2-output";
            "source_evidence_path", `String "provider_row.return";
            "witness_scope", `String "selected-sparrow-itv";
            "effect_id", `String legacy_effect_id;
            "location", `String "(provide,__return__)";
            "provider_module", `String "provider.c";
            "provider_source_hash", `String "hash-1";
            "symbol", `String "provide";
            "value", `Int 7;
            "singleton_int", `Int 7;
            "scalar_value_kind", `String "singleton";
            "scalar_protocol_schema", `String Scalar.schema_id;
            "scalar_call_protocol_id", `String "protocol";
            "typed_scalar_metadata_valid", `Bool true;
            ( "typed_scalar_metadata",
              `Assoc
                [
                  "provider_module", `String "provider.c";
                  "provider_source_hash", `String "hash-1";
                  "provider_artifact_path", `String "/tmp/provider.json";
                  "export_name", `String "provide";
                  "return_node", `String "provide-EXIT";
                  "return_location", `String "(provide,__return__)";
                  "provider_phase_index", `Int 1;
                  "return_value", `Int 7;
                  "scalar_value_kind", `String "singleton";
                  "scalar_protocol_schema", `String Scalar.schema_id;
                  "scalar_call_protocol_id", `String "protocol";
                  "effect_id", `String legacy_effect_id;
                  ( "canonical_value",
                    `Assoc [ "kind", `String "singleton"; "value", `Int 7 ] );
                  ( "value_model",
                    `String
                      Sparrow_modular_ocaml.Abstract_speculate_itv_residual_cell.value_model_id );
                ] );
          ]
          (`Assoc []);
        ( "external_summary",
          `Assoc [ "typed_projections", `List [ projection_json ]; "return_effects", `List [] ] );
        "typed_return_projection", projection_json;
        "linked_return_value", `Int 7;
        "provider_module", `String "provider.c";
        "export_name", `String "provide";
        "return_location", `String "(provide,__return__)";
        "provider_source_hash", `String "hash-1";
        "scalar_protocol_schema", `String Scalar.schema_id;
        "scalar_call_protocol_id", `String "protocol";
        "typed_scalar_metadata_valid", `Bool true;
      ]
      (`Assoc [])
  in
  let scalar_validation = Scalar.validate_linked_derivation_json scalar_derivation in
  expect
    (Scalar.validation_ok scalar_validation)
    ("typed projection did not replace legacy return_effect structural equality: "
     ^ String.concat "," (Scalar.validation_reasons scalar_validation));
  print_endline "abstract_speculate_effect_algebra_unit: PASS"
