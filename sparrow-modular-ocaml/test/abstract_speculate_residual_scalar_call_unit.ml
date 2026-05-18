module Scalar = Sparrow_modular_ocaml.Abstract_speculate_residual_scalar_call
module Cell = Sparrow_modular_ocaml.Abstract_speculate_itv_residual_cell

let expect cond msg = if not cond then failwith msg

let member = Yojson.Safe.Util.member

let string_field name json =
  match member name json with
  | `String s -> s
  | _ -> ""

let bool_field name json =
  match member name json with
  | `Bool b -> b
  | _ -> false

let int_field name json =
  match member name json with
  | `Int n -> n
  | _ -> min_int

let provider_row value =
  `Assoc [
    "node", `String "provide-EXIT";
    "memory", `List [`Assoc [
      "location", `String "(provide,__return__)";
      "value", `String value;
      "normalized_value", `String value;
    ]];
  ]

let make_provider ?(hash="hash-1") ?(effect_id="provider.c:provide:return:(provide,__return__)")
    ?(abstract_value="([7,7], unit)") () =
  match
    Scalar.make_provider_return
      ~provider_module:"provider.c"
      ~provider_source_hash:hash
      ~provider_artifact_path:"/tmp/provider.json"
      ~export_name:"provide"
      ~return_node:"provide-EXIT"
      ~return_location:"(provide,__return__)"
      ~effect_id
      ~provider_phase_index:1
      ~return_value:7
      ~abstract_return_value:abstract_value
      ~provider_row:(provider_row abstract_value)
  with
  | Ok p -> p
  | Error reason -> failwith ("provider construction failed: " ^ reason)

let expect_error = function
  | Ok _ -> failwith "expected scalar protocol construction failure"
  | Error _ -> ()

let () =
  let p = make_provider () in
  let eff = Scalar.return_effect_json p in
  let v1 = Scalar.v1_extern_scalar_value_json p in
  let summary = Scalar.function_return_summary_json p in
  expect (string_field "scalar_protocol_schema" eff = Scalar.schema_id)
    "return effect missing scalar schema";
  expect (string_field "scalar_protocol_schema" v1 = Scalar.schema_id)
    "v1 scalar missing scalar schema";
  expect (string_field "scalar_protocol_schema" summary = Scalar.schema_id)
    "function return summary missing scalar schema";
  expect (bool_field "typed_scalar_metadata_valid" eff)
    "return effect metadata validity missing";
  expect (string_field "scalar_value_kind" eff = "singleton")
    "singleton ITV kind not observed";
  expect (string_field "value_model" (member "typed_scalar_metadata" eff) = Cell.value_model_id)
    "scalar protocol did not reuse ITV residual cell model";
  expect (int_field "return_value" (member "typed_scalar_metadata" eff) = 7)
    "metadata return value mismatch";
  expect (Scalar.validation_ok (Scalar.validate_return_effect_json eff))
    "valid return effect rejected";
  expect (Scalar.validation_ok (Scalar.validate_v1_extern_scalar_value_json v1))
    "valid v1 scalar rejected";
  let linked =
    match
      Scalar.make_linked_derivation
        ~importer_module:"importer.c"
        ~importer_extern_root:"extern:provide"
        ~import_name:"provide"
        ~linked_return_value:7
        ~provider_return:p
    with
    | Ok linked -> linked
    | Error reason -> failwith ("linked derivation construction failed: " ^ reason)
  in
  let derivation =
    Scalar.add_fields
      [
        "importer_module", `String "importer.c";
        "importer_extern_root", `String "extern:provide";
        "import_name", `String "provide";
        "provider_module", `String "provider.c";
        "provider_source_hash", `String "hash-1";
        "provider_artifact_path", `String "/tmp/provider.json";
        "export_name", `String "provide";
        "return_location", `String "(provide,__return__)";
        "return_node", `String "provide-EXIT";
        "linked_return_value", `Int 7;
        "effect_reason", `String "linked-provider-return";
        "derivation_source", `String "provider-stage2-output";
        "external_summary_schema", `String "abstract-speculate-external-summary/v2";
        "summary_api_status", `String "prototype-internal";
        "external_summary_effect_id", member "effect_id" eff;
        "return_effect", eff;
        "external_summary", `Assoc ["return_effects", `List [eff]];
        "scalar_protocol_schema", `String Scalar.schema_id;
        "scalar_call_protocol_id", member "scalar_call_protocol_id" eff;
        "typed_scalar_metadata_valid", `Bool true;
        "typed_scalar_linked_derivation", Scalar.linked_derivation_metadata_json linked;
      ]
      (`Assoc [])
  in
  expect (Scalar.validation_ok (Scalar.validate_linked_derivation_json derivation))
    "valid linked derivation rejected";
  expect_error
    (Scalar.make_provider_return
       ~provider_module:"provider.c"
       ~provider_source_hash:"hash-1"
       ~provider_artifact_path:"/tmp/provider.json"
       ~export_name:"provide"
       ~return_node:"provide-EXIT"
       ~return_location:"(provide,__return__)"
       ~effect_id:"wrong-effect-id"
       ~provider_phase_index:1
       ~return_value:7
       ~abstract_return_value:"([7,7], unit)"
       ~provider_row:(provider_row "([7,7], unit)"));
  expect_error
    (Scalar.make_provider_return
       ~provider_module:"provider.c"
       ~provider_source_hash:"hash-1"
       ~provider_artifact_path:"/tmp/provider.json"
       ~export_name:"provide"
       ~return_node:"provide-EXIT"
       ~return_location:"(provide,__return__)"
       ~effect_id:"provider.c:provide:return:(provide,__return__)"
       ~provider_phase_index:1
       ~return_value:7
       ~abstract_return_value:"([8,8], unit)"
       ~provider_row:(provider_row "([8,8], unit)"));
  expect_error
    (Scalar.make_linked_derivation
       ~importer_module:"importer.c"
       ~importer_extern_root:"extern:provide"
       ~import_name:"provide"
       ~linked_return_value:8
       ~provider_return:p);
  let mutated = Scalar.add_fields ["provider_source_hash", `String "wrong-hash"] eff in
  expect (not (Scalar.validation_ok (Scalar.validate_return_effect_json mutated)))
    "wrong provider hash metadata was accepted";
  expect (not (Scalar.validation_ok (Scalar.validate_linked_derivation_json
    (Scalar.add_fields ["external_summary_effect_id", `String "wrong-effect-id"] derivation))))
    "wrong derivation effect id was accepted";
  print_endline "abstract_speculate_residual_scalar_call_unit: PASS"
