module Delta = Sparrow_modular_ocaml.Abstract_speculate_residual_memory_delta

let expect cond msg = if not cond then failwith msg
let expect_ok = function Ok () -> () | Error reasons -> failwith (String.concat "," reasons)
let expect_error_contains needle = function
  | Ok () -> failwith ("expected error containing " ^ needle)
  | Error reasons -> expect (List.exists ((=) needle) reasons) ("missing reason " ^ needle ^ " in " ^ String.concat "," reasons)

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let set_path path value json =
  let rec set path json =
    match path, json with
    | [], _ -> value
    | key :: rest, `Assoc fields ->
        let old = match List.assoc_opt key fields with Some x -> x | None -> `Null in
        `Assoc ((key, set rest old) :: List.remove_assoc key fields)
    | key :: rest, _ -> `Assoc [key, set rest `Null]
  in
  set path json

let summary delta =
  `Assoc [
    "schema_version", `String Delta.external_summary_schema_id;
    "summary_api_status", `String Delta.summary_api_status;
    "summary_scope", `String Delta.summary_scope;
    "summary_language_schema", `String Delta.summary_language_schema_id;
    "summary_language", Delta.summary_language_json ();
    "memory_delta_schema", `String Delta.memory_delta_schema_id;
    "memory_deltas", `List [Delta.delta_to_json delta];
    "delta_chains", `List [`Assoc ["chain_id", `String (Delta.chain_id delta); "entries", `List (Delta.delta_chain_json delta)]];
    "provenance", `Assoc ["derivation_source", `String "provider-stage2-output"];
  ]

let make_global () =
  match
    Delta.make_provider_delta
      ~provider_module:"provider.c"
      ~provider_source_hash:"hash-1"
      ~provider_artifact_path:"/tmp/provider.json"
      ~provider_phase_index:1
      ~export_name:"provide"
      ~domain:Delta.Global_write_read
      ~raw_location:"shared_g"
      ~summary_location:"shared_g"
      ~symbol:"shared_g"
      ~value:"([7,7], unit)"
      ~source_evidence_path:"provider_row.memory:shared_g"
  with
  | Ok delta -> delta
  | Error reason -> failwith reason

let make_pointer () =
  match
    Delta.make_provider_delta
      ~provider_module:"provider.c"
      ~provider_source_hash:"hash-1"
      ~provider_artifact_path:"/tmp/provider.json"
      ~provider_phase_index:1
      ~export_name:"write_ptr"
      ~domain:Delta.Pointer_memory_effect
      ~raw_location:"(main,x)"
      ~summary_location:"(write_ptr,p)"
      ~symbol:"write_ptr"
      ~value:"main,x"
      ~source_evidence_path:"provider_row.memory:(main,x)"
  with
  | Ok delta -> delta
  | Error reason -> failwith reason

let make_taint () =
  match
    Delta.make_taint_product_evidence
      ~taint_witness_id:"taint_product_pair"
      ~taint_source:"provider:taint_source"
      ~taint_sink:"importer:taint_sink"
      ~taint_state:(Delta.Tainted "user-input")
      ~taint_semantic_relation:"source-taints-sink"
      ~related_residual_location:"shared_taint"
      ~itv_observable_value:"([42,42], unit)"
      ~evidence_paths:[
        "fixtures/abstract_speculate_residual_linking_oracle_suite/taint_product_pair/provider.c";
        "fixtures/abstract_speculate_residual_linking_oracle_suite/taint_product_pair/importer.c";
      ]
  with
  | Ok evidence -> evidence
  | Error reason -> failwith reason

let () =
  let global = make_global () in
  let pointer = make_pointer () in
  let taint = Delta.taint_product_evidence_to_json (make_taint ()) in
  expect_ok (Delta.validate_delta_json (Delta.delta_to_json global));
  expect_ok (Delta.validate_delta_json (Delta.delta_to_json pointer));
  expect_ok (Delta.validate_summary_json (summary global));
  expect_ok (Delta.validate_summary_json (summary pointer));
  expect_ok (Delta.validate_taint_product_evidence_json taint);
  expect_error_contains "memory_delta_role_mismatch"
    (Delta.validate_delta_json (set_path ["writer_role"] (`String "reader") (Delta.delta_to_json global)));
  expect_error_contains "memory_delta_location_mismatch"
    (Delta.validate_delta_json (set_path ["location"] (`String "wrong_g") (Delta.delta_to_json global)));
  expect_error_contains "memory_delta_value_transition_mismatch"
    (Delta.validate_delta_json (set_path ["write_value"] (`String "") (Delta.delta_to_json global)));
  expect_error_contains "memory_delta_provenance_mismatch"
    (Delta.validate_delta_json (set_path ["provider_source_hash"] (`String "") (Delta.delta_to_json global)));
  expect_error_contains "memory_delta_chain_missing"
    (Delta.validate_delta_json (set_path ["delta_chain"] (`List []) (Delta.delta_to_json global)));
  expect_error_contains "memory_delta_chain_missing"
    (Delta.validate_summary_json (set_path ["delta_chains"] (`List []) (summary global)));
  expect_error_contains "summary_scope_mismatch"
    (Delta.validate_summary_json
       (set_path ["summary_scope"] (`String "sparrow-itv-selected-witness")
          (summary global)));
  expect_error_contains "summary_language_schema_mismatch"
    (Delta.validate_summary_json
       (set_path ["summary_language"; "claim_boundary"] (`String "arbitrary-C")
          (summary global)));
  expect_error_contains "summary_language_schema_mismatch"
    (Delta.validate_delta_json
       (set_path ["memory_effect_summary"; "effect_operations"] (`List [])
          (Delta.delta_to_json pointer)));
  expect_error_contains "taint_product_component_mismatch"
    (Delta.validate_taint_product_evidence_json (set_path ["product_components"] (`List [`String "Taint"]) taint));
  expect_error_contains "taint_product_relation_mismatch"
    (Delta.validate_taint_product_evidence_json (set_path ["taint_state"] (`String "tainted") taint));
  expect_error_contains "taint_product_relation_mismatch"
    (Delta.validate_taint_product_evidence_json (set_path ["metadata_only"] (`Bool true) taint));
  expect_error_contains "taint_product_chain_missing"
    (Delta.validate_taint_product_evidence_json (set_path ["taint_chain"] (`List []) taint));
  begin match Delta.domain_of_string "Oct" with
  | Ok _ -> failwith "Oct domain accepted"
  | Error _ -> ()
  end;
  begin match Delta.domain_of_string "Taint" with
  | Ok _ -> failwith "Taint memory domain accepted"
  | Error reason -> expect (String.contains reason 'T') "Taint domain rejection must explain product-boundary"
  end;
  print_endline "abstract_speculate_residual_memory_delta_unit: PASS"
