let repo_root = ref "."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "abstract_speculate_residual_linking_pe_check --repo-root <repo> \
   --artifact-dir <dir> --report <json>"

let member = Yojson.Safe.Util.member

module Relation = Sparrow_modular_ocaml.Abstract_speculate_residual_relation

module ScalarCall =
  Sparrow_modular_ocaml.Abstract_speculate_residual_scalar_call

module MemoryDelta =
  Sparrow_modular_ocaml.Abstract_speculate_residual_memory_delta

module EffectSchema =
  Sparrow_modular_ocaml.Abstract_speculate_effect_schema

let to_string = Yojson.Safe.Util.to_string
let expect cond msg = if not cond then failwith msg

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match assoc_field name json with Some (`String s) -> s | _ -> ""

let bool_field name json =
  match assoc_field name json with Some (`Bool b) -> b | _ -> false

let int_field name json =
  match assoc_field name json with Some (`Int n) -> n | _ -> 0

let list_field name json =
  match assoc_field name json with Some (`List xs) -> xs | _ -> []

let bool_true_field name json =
  match assoc_field name json with Some (`Bool true) -> true | _ -> false

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

let project_path rel =
  let direct = Filename.concat !repo_root rel in
  if Sys.file_exists direct then direct
  else
    let prefix = "sparrow-modular-ocaml/" in
    let prefix_len = String.length prefix in
    if String.length rel > prefix_len && String.sub rel 0 prefix_len = prefix
    then
      Filename.concat !repo_root
        (String.sub rel prefix_len (String.length rel - prefix_len))
    else direct

let forbidden_global_entry = "Real_sparrow_frontend." ^ "global_for_" ^ "files"
let forbidden_merge_entry = "Mergecil." ^ "merge"
let old_staged_linking_entry = "real_sparrow_" ^ "staged_linking_pe"
let premerge_observer_entry = "real_sparrow_" ^ "premerge_linked_observer"

let require_source_absent path =
  let source = read_file path in
  [
    forbidden_global_entry;
    forbidden_merge_entry;
    old_staged_linking_entry;
    premerge_observer_entry;
  ]
  |> List.iter (fun needle ->
         expect
           (not (contains source needle))
           (path ^ " contains forbidden shortcut " ^ needle))

let linked_artifact_path manifest = string_field "linked_artifact" manifest

let artifact_paths manifest =
  list_field "artifacts" manifest |> List.map to_string

let linked_output linked = member "linked_output" linked
let unique_string_set xs = List.sort_uniq String.compare xs
let has_duplicates xs = List.length xs <> List.length (unique_string_set xs)
let same_set left right = unique_string_set left = unique_string_set right
let semantic_linkage_ok linked = Relation.primary_linkage_ok linked

let key_of_module_json json =
  string_field "module_id" json ^ ":" ^ string_field "source_hash" json

let key_of_log_json json =
  string_field "module_id" json ^ ":" ^ string_field "source_hash" json

let input_modules linked = list_field "input_modules" linked
let execution_log linked = member "execution_log" (linked_output linked)

let log_evidence linked =
  member "linked_residual_analyzer_evidence" (execution_log linked)

let top_evidence linked = member "linked_residual_analyzer_evidence" linked

let output_evidence linked =
  member "linked_residual_analyzer_evidence" (linked_output linked)

let linked_stage2_keys linked =
  match assoc_field "keys" (member "linked_stage2_input" linked) with
  | Some (`List xs) -> List.map to_string xs
  | _ -> []

let effect_matches_summary_provenance summary eff =
  string_field "provider_module" eff
  = string_field "provider_module" (member "provenance" summary)
  && string_field "provider_source_hash" eff
     = string_field "provider_source_hash" (member "provenance" summary)
  && string_field "provider_artifact_path" eff
     = string_field "provider_artifact_path" (member "provenance" summary)
  && int_field "provider_phase_index" eff
     = int_field "provider_phase_index" (member "provenance" summary)

let expected_effect_id domain eff =
  string_field "provider_module" eff
  ^ ":" ^ string_field "symbol" eff ^ ":" ^ domain ^ ":"
  ^ string_field "location" eff

let scalar_protocol_summary_ok summary eff =
  let extern_scalar =
    summary
    |> member "external_summary_v1_compat"
    |> member "extern_scalar_value"
  in
  ScalarCall.validation_ok (ScalarCall.validate_return_effect_json eff)
  && ScalarCall.validation_ok
       (ScalarCall.validate_v1_extern_scalar_value_json extern_scalar)
  && string_field "scalar_protocol_schema" eff = ScalarCall.schema_id
  && string_field "scalar_protocol_schema" extern_scalar = ScalarCall.schema_id
  && string_field "scalar_call_protocol_id" eff <> ""
  && string_field "scalar_call_protocol_id" eff
     = string_field "scalar_call_protocol_id" extern_scalar
  && bool_true_field "typed_scalar_metadata_valid" eff
  && bool_true_field "typed_scalar_metadata_valid" extern_scalar

let typed_effect_ok expected_domain eff =
  string_field "domain" eff = expected_domain
  && string_field "effect_id" eff <> ""
  && string_field "location" eff <> ""
  && string_field "provider_module" eff <> ""
  && string_field "provider_source_hash" eff <> ""
  && string_field "derivation_source" eff = "provider-stage2-output"
  && string_field "witness_scope" eff = "selected-sparrow-itv"

let scalar_return_metadata_ok summary eff =
  let extern_scalar =
    summary
    |> member "external_summary_v1_compat"
    |> member "extern_scalar_value"
  in
  string_field "scalar_protocol_schema" eff = ScalarCall.schema_id
  && string_field "scalar_protocol_schema" extern_scalar = ScalarCall.schema_id
  && string_field "scalar_call_protocol_id" eff <> ""
  && string_field "scalar_call_protocol_id" eff
     = string_field "scalar_call_protocol_id" extern_scalar
  && string_field "scalar_value_kind" eff <> ""
  && member "scalar_value" eff <> `Null
  && bool_true_field "typed_scalar_metadata_valid" eff
  && bool_true_field "typed_scalar_metadata_valid" extern_scalar

let return_effect_ok summary eff =
  let function_return =
    summary
    |> member "external_summary_v1_compat"
    |> member "function_return_summary"
  in
  typed_effect_ok "return" eff
  && ScalarCall.validation_ok (ScalarCall.validate_return_effect_json eff)
  && ScalarCall.validation_ok
       (ScalarCall.validate_v1_extern_scalar_value_json
          (summary
          |> member "external_summary_v1_compat"
          |> member "extern_scalar_value"))
  && effect_matches_summary_provenance summary eff
  && scalar_protocol_summary_ok summary eff
  && string_field "source_evidence_path" eff = "provider_row.return"
  && string_field "location" eff
     = string_field "return_location" function_return
  && member "value" eff = member "return_value" function_return
  && string_field "effect_id" eff = expected_effect_id "return" eff
  && scalar_return_metadata_ok summary eff

let first_return_effect summary =
  match list_field "return_effects" summary with eff :: _ -> eff | [] -> `Null

let typed_effect_artifacts_ok summary =
  list_field "typed_effects" summary <> []
  && List.for_all
       (fun eff -> EffectSchema.validation_ok (EffectSchema.validate_defined_artifact_json eff))
       (list_field "typed_effects" summary)
  && list_field "typed_projections" summary <> []
  && List.for_all
       (fun projection -> EffectSchema.validation_ok (EffectSchema.validate_projection_json projection))
       (list_field "typed_projections" summary)

let external_summary_ok summary =
  let has_memory_projection =
    list_field "global_effects" summary <> [] || list_field "pointer_effects" summary <> []
  in
  let memory_delta_authority_ok =
    if not has_memory_projection then
      list_field "memory_deltas" summary = [] && list_field "delta_chains" summary = []
    else
      list_field "memory_deltas" summary <> [] &&
      list_field "delta_chains" summary <> [] &&
      string_field "status" (member "typed_memory_delta_validation" summary) = "pass" &&
      List.for_all (fun delta ->
        string_field "memory_delta_schema" delta = MemoryDelta.memory_delta_schema_id &&
        MemoryDelta.validation_ok (MemoryDelta.validate_delta_json delta))
        (list_field "memory_deltas" summary) &&
      List.for_all (fun chain ->
        string_field "memory_delta_schema" chain = MemoryDelta.memory_delta_schema_id &&
        list_field "entries" chain <> [])
        (list_field "delta_chains" summary)
  in
  string_field "schema_version" summary = EffectSchema.schema_id &&
  string_field "summary_api_status" summary = EffectSchema.summary_api_status &&
  string_field "summary_scope" summary = "sparrow-itv-selected-witness" &&
  list_field "effect_domains" summary <> [] &&
  list_field "return_effects" summary <> [] &&
  List.for_all (return_effect_ok summary) (list_field "return_effects" summary) &&
  memory_delta_authority_ok &&
  List.for_all (typed_effect_ok "global-write-read") (list_field "global_effects" summary) &&
  List.for_all (typed_effect_ok "pointer-memory-effect") (list_field "pointer_effects" summary) &&
  member "external_summary_v1_compat" summary <> `Null &&
  string_field "schema_version" (member "external_summary_v1_compat" summary) =
    "abstract-speculate-external-summary/v1" &&
  string_field "derivation_source" (member "provenance" summary) = "provider-stage2-output"

let derivation_uses_v3_return_effect derivation =
  let summary = member "external_summary" derivation in
  let eff = first_return_effect summary in
  external_summary_ok summary
  && string_field "external_summary_schema" derivation
     = EffectSchema.schema_id
  && string_field "summary_api_status" derivation = EffectSchema.summary_api_status
  && string_field "external_summary_effect_id" derivation
     = string_field "effect_id" eff
  && string_field "effect_reason" derivation = "linked-provider-return"
  && string_field "derivation_source" derivation = "provider-stage2-output"
  && member "value" eff = member "linked_return_value" derivation
  && string_field "provider_module" eff
     = string_field "provider_module" derivation
  && string_field "scalar_call_protocol_id" derivation
     = string_field "scalar_call_protocol_id" eff

let api_coverage_rows linked =
  list_field "module_logs" (execution_log linked)
  |> List.concat_map (fun module_log ->
         list_field "api_residual_model_coverage"
           (member "execution_log" module_log))

let api_supported_functions linked =
  api_coverage_rows linked
  |> List.map (string_field "function_name")
  |> unique_string_set

let require_api_residual_model_coverage linked =
  let rows = api_coverage_rows linked in
  let supported = api_supported_functions linked in
  [ "memcpy"; "strcpy"; "strlen" ]
  |> List.iter (fun name ->
         expect (List.mem name supported)
           ("missing API residual coverage for " ^ name));
  [ "memmove"; "strncpy"; "strcat"; "strdup"; "xstrdup"; "fgets"; "scanf" ]
  |> List.iter (fun name ->
         expect
           (not (List.mem name supported))
           ("unsupported API was claimed covered: " ^ name));
  rows
  |> List.iter (fun row ->
         let fn = string_field "function_name" row in
         let target = string_field "residual_cell_target" row in
         let deps =
           list_field "semantically_upstream_dependencies" row
           |> List.map to_string
         in
         expect
           (List.mem fn [ "memcpy"; "strcpy"; "strlen" ])
           ("unexpected API residual coverage row: " ^ fn);
         expect
           (contains (string_field "baseline_source" row) "itvSem.ml")
           ("missing ItvSem baseline for " ^ fn);
         expect
           (contains (string_field "baseline_source" row) "apiSem.ml")
           ("missing ApiSem baseline for " ^ fn);
         expect
           (string_field "residual_equation_id" row <> "")
           ("missing residual equation id for " ^ fn);
         expect (target <> "") ("missing residual cell target for " ^ fn);
         expect (deps <> []) ("empty API residual dependencies for " ^ fn);
         expect
           (not (List.mem target deps))
           ("API residual dependency is target/self-only for " ^ fn);
         expect
           (int_field "solver_state_read_count" row > 0)
           ("API residual equation did not read solver state for " ^ fn);
         expect
           (bool_field "stage2_seed_input_read" row)
           ("API residual equation did not read stage2 seed input for " ^ fn);
         expect
           (string_field "final_cell_provenance" row
           = "residual-api-model-coverage/slice-1")
           ("bad API residual provenance for " ^ fn);
         expect
           (bool_field "unsupported_api_coverage" row = false)
           ("unsupported API coverage flag set for " ^ fn));
  list_field "module_logs" (execution_log linked)
  |> List.iter (fun module_log ->
         expect
           (list_field "unsupported_api_residual_coverage"
              (member "execution_log" module_log)
           = [])
           "unsupported API residual coverage list must stay empty")

let global_residual_report linked = member "global_residual_fixpoint" linked

let table_cell_count rows =
  rows
  |> List.fold_left
       (fun acc row -> acc + List.length (list_field "memory" row))
       0

let require_global_residual_fixpoint linked =
  let report = global_residual_report linked in
  let output = linked_output linked in
  let log = execution_log linked in
  let seed_cells = list_field "global_residual_seed_cells" report in
  let derived_cells = list_field "global_residual_derived_cells" report in
  let dependency_edges = list_field "global_residual_dependency_edges" report in
  let cross_edges =
    list_field "global_residual_cross_module_dependency_edges" report
  in
  let schedule = list_field "global_residual_worklist_schedule" report in
  let equations = list_field "global_residual_equations" report in
  let final_cell_count =
    table_cell_count (list_field "global_residual_final_input_table" report)
    + table_cell_count (list_field "global_residual_final_output_table" report)
  in
  expect
    (bool_field "global_residual_fixpoint_run" linked)
    "linked artifact missing global residual fixpoint run flag";
  expect
    (bool_field "global_residual_fixpoint_run" log)
    "linked execution log missing global residual fixpoint run flag";
  expect
    (bool_field "global_residual_fixpoint_run" report)
    "global residual report did not run";
  expect
    (string_field "global_residual_fixpoint_scope" report
    = "post-link-whole-program-residual-cells")
    "global residual scope mismatch";
  expect
    (string_field "global_sparse_fixpoint_component" report
    = "residual-global-worklist")
    "global residual sparse component mismatch";
  expect
    (bool_field "global_sparse_fixpoint_source_level_rerun" report)
    "global residual report did not record source-level sparse rerun";
  expect
    (bool_field "global_source_rerun_ready_for_relation_gate" report)
    "global residual report missing validated source rerun gate";
  expect
    (bool_field "global_source_rerun_linked_context_consumed" report)
    "global residual source rerun did not consume linked context";
  expect
    (list_field "global_source_rerun_validated_evidence" report <> [])
    "global residual report has no validated source rerun evidence";
  expect (seed_cells <> []) "global residual report has no seed cells";
  expect (derived_cells <> []) "global residual report has no derived cells";
  expect
    (List.length derived_cells = final_cell_count)
    "global residual derived cells do not cover final tables";
  expect (equations <> []) "global residual report has no equations";
  expect (dependency_edges <> []) "global residual report has no dependencies";
  expect (cross_edges <> [])
    "global residual report has no cross-module dependencies";
  expect
    (List.for_all (fun edge -> bool_field "cross_module" edge) cross_edges)
    "global residual cross-module dependency list contains local edge";
  expect
    (int_field "global_residual_state_read_count" report > 0)
    "global residual report has no state reads";
  expect
    (int_field "global_residual_seed_read_count" report > 0)
    "global residual report has no seed reads";
  expect (schedule <> []) "global residual report has no worklist schedule";
  expect
    (bool_field "global_residual_worklist_drained" report)
    "global residual worklist did not drain";
  expect
    (bool_field "global_residual_overlay_only" report = false)
    "global residual path reported overlay-only";
  expect
    (bool_field "global_residual_metadata_only" report = false)
    "metadata-only global residual report accepted";
  expect
    (bool_field "global_residual_derived_cells_recomputed" report)
    "global residual report did not recompute derived cells";
  expect
    (bool_field "global_residual_authoritative_residual_side" report)
    "global residual report was not marked authoritative residual side";
  expect
    (list_field "global_residual_final_input_table" report
    = list_field "final_input_table" output)
    "global residual final input handoff differs from linked output";
  expect
    (list_field "global_residual_final_output_table" report
    = list_field "final_output_table" output)
    "global residual final output handoff differs from linked output";
  if int_field "linked_cycle_scc_count" log > 0 then (
    expect
      (int_field "global_residual_iteration_count" report > 1)
      "cyclic global residual fixpoint did not repeat iterations";
    expect
      (schedule
      |> List.exists (fun event ->
             string_field "event" event = "enqueue"
             && string_field "reason" event = "changed-cell-dependent"))
      "cyclic global residual fixpoint did not reschedule changed dependents")

let require_external_summaries linked =
  let summaries = list_field "external_summaries" linked in
  expect (summaries <> []) "missing ExternalSummary v3 entries";
  expect
    (List.for_all external_summary_ok summaries)
    "malformed ExternalSummary v3 entry";
  list_field "semantic_exports" linked
  |> List.iter (fun export ->
         expect
           (external_summary_ok (member "external_summary" export))
           "semantic export missing ExternalSummary v3");
  list_field "linked_environment" linked
  |> List.iter (fun entry ->
         expect
           (external_summary_ok (member "external_summary" entry))
           "linked environment missing ExternalSummary v3");
  let derivations = list_field "linked_stage2_input_derivation" linked in
  expect (derivations <> []) "missing v3 linked input derivations";
  expect
    (List.for_all derivation_uses_v3_return_effect derivations)
    "linked stage2 derivation does not use ExternalSummary v3 return effect"

type recomputed_evidence = {
  linked_execute_returned : bool;
  module_count : int;
  module_analyzers_executed : int;
  module_identity_set_matches : bool;
  all_modules_executed : bool;
  final_input_row_count : int;
  final_output_row_count : int;
  linked_residual_row_count : int;
  residual_rows_observed : bool;
  matched_obligation_count : int;
  unresolved_obligation_count : int;
  obligations_closed : bool;
  no_shortcut_path : bool;
  derived_from_five_predicates : bool;
  linked_residual_analyzer_ran : bool;
}

let valid_module_artifact path =
  try
    let artifact = Yojson.Safe.from_file path in
    let boundary = member "module_boundary" artifact in
    let forbidden =
      list_field "forbidden_prelink_entrypoints" boundary |> List.map to_string
    in
    string_field "scope" artifact = "module-only-pre-link"
    && bool_field "linked_entrypoints_used" boundary = false
    && List.mem forbidden_global_entry forbidden
    && List.mem forbidden_merge_entry forbidden
  with _ -> false

let recompute_evidence ~source_guard_passed ~manifest linked =
  let inputs = input_modules linked in
  let logs = list_field "module_logs" (execution_log linked) in
  let input_keys = List.map key_of_module_json inputs in
  let declared_input_keys = List.map (string_field "stage2_input_key") inputs in
  let stage2_keys = linked_stage2_keys linked in
  let log_keys = List.map key_of_log_json logs in
  let module_count = int_field "module_count" linked in
  let module_analyzers_executed = List.length logs in
  let input_key_fields_match = same_set input_keys declared_input_keys in
  let module_identity_set_matches =
    module_count = List.length inputs
    && input_key_fields_match
    && same_set input_keys stage2_keys
    && same_set input_keys log_keys
    && (not (has_duplicates input_keys))
    && (not (has_duplicates stage2_keys))
    && not (has_duplicates log_keys)
  in
  let module_logs_executed =
    logs <> []
    && List.for_all
         (fun log ->
           bool_field "module_analyzer_executed" log
           && List.mem
                (string_field "stage2_input_dispatch" log)
                [ "per-module"; "linked-environment" ])
         logs
    && List.exists
         (fun log ->
           string_field "stage2_input_dispatch" log = "linked-environment")
         logs
  in
  let all_modules_executed =
    module_count >= 2
    && module_analyzers_executed = module_count
    && module_logs_executed && module_identity_set_matches
  in
  let output = linked_output linked in
  let final_input_row_count =
    List.length (list_field "final_input_table" output)
  in
  let final_output_row_count =
    List.length (list_field "final_output_table" output)
  in
  let linked_residual_row_count =
    final_input_row_count + final_output_row_count
  in
  let residual_rows_observed = linked_residual_row_count > 0 in
  let matched_obligation_count =
    List.length (list_field "matched_obligations" linked)
  in
  let unresolved_obligation_count =
    List.length (list_field "unresolved_obligations" linked)
  in
  let obligations_closed =
    matched_obligation_count > 0 && unresolved_obligation_count = 0
  in
  let manifest_artifacts = artifact_paths manifest in
  let input_artifacts = List.map (string_field "artifact_path") inputs in
  let artifact_sets_match =
    input_artifacts <> []
    && same_set manifest_artifacts input_artifacts
    && not (has_duplicates input_artifacts)
  in
  let module_artifacts_valid =
    List.for_all valid_module_artifact input_artifacts
  in
  let no_shortcut_path =
    source_guard_passed && artifact_sets_match && module_artifacts_valid
    && string_field "dispatch" (member "linked_stage2_input" linked)
       = "provider-derived-linked-environment"
    && bool_field "linked_environment_generated"
         (member "linked_stage2_input" linked)
    && same_set input_keys stage2_keys
    && bool_field "linked_entrypoints_used_before_pe" linked = false
    && bool_field "linked_facts_prelink" linked = false
  in
  let linked_execute_returned =
    bool_true_field "linked_execute_returned" (log_evidence linked)
  in
  let derived_from_five_predicates =
    bool_true_field "derived_from_five_predicates" (log_evidence linked)
  in
  let linked_residual_analyzer_ran =
    linked_execute_returned && all_modules_executed && residual_rows_observed
    && obligations_closed && no_shortcut_path && derived_from_five_predicates
    && semantic_linkage_ok linked
  in
  {
    linked_execute_returned;
    module_count;
    module_analyzers_executed;
    module_identity_set_matches;
    all_modules_executed;
    final_input_row_count;
    final_output_row_count;
    linked_residual_row_count;
    residual_rows_observed;
    matched_obligation_count;
    unresolved_obligation_count;
    obligations_closed;
    no_shortcut_path;
    derived_from_five_predicates;
    linked_residual_analyzer_ran;
  }

let expect_evidence_matches label evidence recomputed =
  expect
    (bool_field "linked_execute_returned" evidence
    = recomputed.linked_execute_returned)
    (label ^ ": linked_execute_returned mismatch");
  expect
    (int_field "module_count" evidence = recomputed.module_count)
    (label ^ ": module_count mismatch");
  expect
    (int_field "module_analyzers_executed" evidence
    = recomputed.module_analyzers_executed)
    (label ^ ": module_analyzers_executed mismatch");
  expect
    (bool_field "module_identity_set_matches" evidence
    = recomputed.module_identity_set_matches)
    (label ^ ": module_identity_set_matches mismatch");
  expect
    (bool_field "all_modules_executed" evidence
    = recomputed.all_modules_executed)
    (label ^ ": all_modules_executed mismatch");
  expect
    (int_field "final_input_row_count" evidence
    = recomputed.final_input_row_count)
    (label ^ ": final_input_row_count mismatch");
  expect
    (int_field "final_output_row_count" evidence
    = recomputed.final_output_row_count)
    (label ^ ": final_output_row_count mismatch");
  expect
    (int_field "linked_residual_row_count" evidence
    = recomputed.linked_residual_row_count)
    (label ^ ": linked_residual_row_count mismatch");
  expect
    (bool_field "residual_rows_observed" evidence
    = recomputed.residual_rows_observed)
    (label ^ ": residual_rows_observed mismatch");
  expect
    (int_field "matched_obligation_count" evidence
    = recomputed.matched_obligation_count)
    (label ^ ": matched_obligation_count mismatch");
  expect
    (int_field "unresolved_obligation_count" evidence
    = recomputed.unresolved_obligation_count)
    (label ^ ": unresolved_obligation_count mismatch");
  expect
    (bool_field "obligations_closed" evidence = recomputed.obligations_closed)
    (label ^ ": obligations_closed mismatch");
  expect
    (bool_field "no_shortcut_path" evidence = recomputed.no_shortcut_path)
    (label ^ ": no_shortcut_path mismatch");
  expect
    (bool_field "derived_from_five_predicates" evidence
    = recomputed.derived_from_five_predicates)
    (label ^ ": derived_from_five_predicates mismatch");
  expect
    (bool_field "linked_residual_analyzer_ran" evidence
    = recomputed.linked_residual_analyzer_ran)
    (label ^ ": linked_residual_analyzer_ran mismatch")

let rec set_path path value json =
  match (path, json) with
  | [], _ -> value
  | key :: rest, `Assoc fields ->
      let old =
        match List.assoc_opt key fields with Some v -> v | None -> `Assoc []
      in
      `Assoc ((key, set_path rest value old) :: List.remove_assoc key fields)
  | _ :: _, other -> other

let false_case label manifest linked mutate =
  let mutated = mutate linked in
  let evidence =
    recompute_evidence ~source_guard_passed:true ~manifest mutated
  in
  expect
    (not evidence.linked_residual_analyzer_ran)
    ("false case stayed true: " ^ label)

let run_false_case_checks manifest linked =
  let v3_gate_passes json =
    let summaries = list_field "external_summaries" json in
    summaries <> []
    && List.for_all external_summary_ok summaries
    && List.for_all
         (fun export -> external_summary_ok (member "external_summary" export))
         (list_field "semantic_exports" json)
    && List.for_all
         (fun entry -> external_summary_ok (member "external_summary" entry))
         (list_field "linked_environment" json)
    && List.for_all derivation_uses_v3_return_effect
         (list_field "linked_stage2_input_derivation" json)
  in
  let v3_false_case label mutate =
    expect
      (not (v3_gate_passes (mutate linked)))
      ("ExternalSummary v3 false case stayed true: " ^ label)
  in
  let mutate_first_summary path value json =
    match list_field "external_summaries" json with
    | first :: rest ->
        set_path [ "external_summaries" ]
          (`List (set_path path value first :: rest))
          json
    | [] -> json
  in
  let mutate_first_return_effect path value json =
    match list_field "external_summaries" json with
    | first :: rest -> (
        match list_field "return_effects" first with
        | eff :: eff_rest ->
            let mutated =
              set_path [ "return_effects" ]
                (`List (set_path path value eff :: eff_rest))
                first
            in
            set_path [ "external_summaries" ] (`List (mutated :: rest)) json
        | [] -> json)
    | [] -> json
  in
  let mutate_first_memory_delta path value json =
    match list_field "external_summaries" json with
    | first :: rest ->
        begin match list_field "memory_deltas" first with
        | delta :: delta_rest ->
            let mutated = set_path ["memory_deltas"] (`List (set_path path value delta :: delta_rest)) first in
            set_path ["external_summaries"] (`List (mutated :: rest)) json
        | [] -> json
        end
    | [] -> json
  in
  v3_false_case "missing v3 summary" (fun json ->
      set_path [ "external_summaries" ] (`List []) json);
  v3_false_case "v1 compatibility only" (fun json ->
      match list_field "external_summaries" json with
      | first :: _ ->
          set_path [ "external_summaries" ]
            (`List [ member "external_summary_v1_compat" first ])
            json
      | [] -> json);
  v3_false_case "schema downgraded" (fun json ->
      match list_field "external_summaries" json with
      | first :: rest ->
          set_path [ "external_summaries" ]
            (`List
               (set_path [ "schema_version" ]
                  (`String "abstract-speculate-external-summary/v1") first
               :: rest))
            json
      | [] -> json);
  v3_false_case "summary status corrupted" (fun json ->
      mutate_first_summary [ "summary_api_status" ] (`String "stale-or-public")
        json);
  v3_false_case "missing return effect" (fun json ->
    match list_field "external_summaries" json with
    | first :: rest ->
        set_path ["external_summaries"] (`List (set_path ["return_effects"] (`List []) first :: rest)) json
    | [] -> json);
  v3_false_case "missing memory delta authority" (fun json ->
    match list_field "external_summaries" json with
    | first :: rest ->
        let first =
          first
          |> set_path ["memory_deltas"] (`List [])
          |> set_path ["delta_chains"] (`List [])
        in
        set_path ["external_summaries"] (`List (first :: rest)) json
    | [] -> json);
  v3_false_case "memory delta role corrupted" (fun json ->
    mutate_first_memory_delta ["writer_role"] (`String "reader") json);
  v3_false_case "memory delta location corrupted" (fun json ->
    mutate_first_memory_delta ["normalized_location"] (`String "wrong-location") json);
  v3_false_case "memory delta value corrupted" (fun json ->
    mutate_first_memory_delta ["write_value"] (`String "") json);
  v3_false_case "memory delta provenance corrupted" (fun json ->
    mutate_first_memory_delta ["provider_source_hash"] (`String "wrong-hash") json);
  v3_false_case "memory delta chain missing" (fun json ->
    mutate_first_memory_delta ["delta_chain"] (`List []) json);
  v3_false_case "memory delta chain corrupted" (fun json ->
    mutate_first_memory_delta ["chain_hash"] (`String "wrong-chain-hash") json);
  v3_false_case "return effect value corrupted" (fun json ->
      mutate_first_return_effect [ "value" ] (`Int 999) json);
  v3_false_case "return effect location corrupted" (fun json ->
      mutate_first_return_effect [ "location" ] (`String "(wrong,__return__)")
        json);
  v3_false_case "return effect provenance corrupted" (fun json ->
      mutate_first_return_effect [ "provider_source_hash" ]
        (`String "wrong-hash") json);
  v3_false_case "return effect scalar metadata corrupted" (fun json ->
      mutate_first_return_effect
        [ "typed_scalar_metadata_valid" ]
        (`Bool false) json);
  v3_false_case "return effect scalar value corrupted" (fun json ->
      mutate_first_return_effect [ "scalar_value" ]
        (`Assoc
           [ ("kind", `String "singleton"); ("lo", `Int 999); ("hi", `Int 999) ])
        json);
  v3_false_case "derivation effect id mismatch" (fun json ->
      match list_field "linked_stage2_input_derivation" json with
      | first :: rest ->
          set_path
            [ "linked_stage2_input_derivation" ]
            (`List
               (set_path
                  [ "external_summary_effect_id" ]
                  (`String "wrong-effect-id") first
               :: rest))
            json
      | [] -> json);
  v3_false_case "return effect scalar metadata corrupted" (fun json ->
      mutate_first_return_effect
        [ "typed_scalar_metadata"; "provider_source_hash" ]
        (`String "wrong-hash") json);
  v3_false_case "linked scalar protocol id corrupted" (fun json ->
      match list_field "linked_stage2_input_derivation" json with
      | first :: rest ->
          set_path
            [ "linked_stage2_input_derivation" ]
            (`List
               (set_path
                  [ "scalar_call_protocol_id" ]
                  (`String "wrong-protocol-id") first
               :: rest))
            json
      | [] -> json);
  false_case "linked_execute_returned" manifest linked (fun json ->
      set_path
        [
          "linked_output";
          "execution_log";
          "linked_residual_analyzer_evidence";
          "linked_execute_returned";
        ]
        (`Bool false) json);
  false_case "module identity mismatch" manifest linked (fun json ->
      match list_field "module_logs" (execution_log json) with
      | first :: rest ->
          let wrong =
            set_path [ "source_hash" ] (`String "wrong-source-hash") first
          in
          set_path
            [ "linked_output"; "execution_log"; "module_logs" ]
            (`List (wrong :: rest))
            json
      | [] -> json);
  false_case "duplicate module log" manifest linked (fun json ->
      match list_field "module_logs" (execution_log json) with
      | first :: _ ->
          let module_count = int_field "module_count" json in
          set_path
            [ "linked_output"; "execution_log"; "module_logs" ]
            (`List [ first; first ])
            (set_path [ "module_count" ] (`Int module_count) json)
      | [] -> json);
  false_case "zero residual rows" manifest linked (fun json ->
      json
      |> set_path [ "linked_output"; "final_input_table" ] (`List [])
      |> set_path [ "linked_output"; "final_output_table" ] (`List []));
  false_case "no matched obligations" manifest linked (fun json ->
      set_path [ "matched_obligations" ] (`List []) json);
  false_case "unresolved obligations" manifest linked (fun json ->
      set_path
        [ "unresolved_obligations" ]
        (`List [ `Assoc [ ("name", `String "missing") ] ])
        json);
  let shortcut_false =
    recompute_evidence ~source_guard_passed:false ~manifest linked
  in
  expect
    (not shortcut_false.linked_residual_analyzer_ran)
    "false case stayed true: no shortcut source guard";
  false_case "dispatch mismatch" manifest linked (fun json ->
      set_path [ "linked_stage2_input"; "dispatch" ] (`String "premerge") json);
  false_case "missing semantic exports" manifest linked (fun json ->
      set_path [ "semantic_exports" ] (`List []) json);
  false_case "wrong semantic export source" manifest linked (fun json ->
      match list_field "semantic_exports" json with
      | first :: rest ->
          let wrong =
            set_path [ "derivation_source" ] (`String "declaration-only") first
          in
          set_path [ "semantic_exports" ] (`List (wrong :: rest)) json
      | [] -> json);
  false_case "mutated provider return summary" manifest linked (fun json ->
      match list_field "semantic_exports" json with
      | first :: rest ->
          let wrong = set_path [ "return_value" ] (`Int 999) first in
          set_path [ "semantic_exports" ] (`List (wrong :: rest)) json
      | [] -> json);
  false_case "missing linked environment" manifest linked (fun json ->
      set_path [ "linked_environment" ] (`List []) json);
  false_case "manual extern value masquerade" manifest linked (fun json ->
      match list_field "linked_stage2_input_derivation" json with
      | first :: rest ->
          let wrong =
            first
            |> set_path [ "effect_reason" ] (`String "unknown-extern-call")
            |> set_path [ "derivation_source" ] (`String "manual-extern-effect")
          in
          set_path
            [ "linked_stage2_input_derivation" ]
            (`List (wrong :: rest))
            json
      | [] -> json);
  false_case "mismatched importer/provider value" manifest linked (fun json ->
      match list_field "linked_environment" json with
      | first :: rest ->
          let wrong = set_path [ "linked_return_value" ] (`Int 999) first in
          set_path [ "linked_environment" ] (`List (wrong :: rest)) json
      | [] -> json);
  false_case "provider after importer order" manifest linked (fun json ->
      match list_field "phase_log" json with
      | events ->
          let mutated =
            events
            |> List.map (fun event ->
                   if string_field "event" event = "provider-stage2-executed"
                   then set_path [ "phase_index" ] (`Int 9) event
                   else event)
          in
          set_path [ "phase_log" ] (`List mutated) json);
  false_case "wrong obligation mapping" manifest linked (fun json ->
      match list_field "linked_environment" json with
      | first :: rest ->
          let wrong =
            set_path [ "provider_module" ] (`String "wrong-provider.c") first
          in
          set_path [ "linked_environment" ] (`List (wrong :: rest)) json
      | [] -> json)

let evidence_to_json evidence =
  `Assoc
    [
      ("linked_execute_returned", `Bool evidence.linked_execute_returned);
      ("module_count", `Int evidence.module_count);
      ("module_analyzers_executed", `Int evidence.module_analyzers_executed);
      ("module_identity_set_matches", `Bool evidence.module_identity_set_matches);
      ("all_modules_executed", `Bool evidence.all_modules_executed);
      ("final_input_row_count", `Int evidence.final_input_row_count);
      ("final_output_row_count", `Int evidence.final_output_row_count);
      ("linked_residual_row_count", `Int evidence.linked_residual_row_count);
      ("residual_rows_observed", `Bool evidence.residual_rows_observed);
      ("matched_obligation_count", `Int evidence.matched_obligation_count);
      ("unresolved_obligation_count", `Int evidence.unresolved_obligation_count);
      ("obligations_closed", `Bool evidence.obligations_closed);
      ("no_shortcut_path", `Bool evidence.no_shortcut_path);
      ( "derived_from_five_predicates",
        `Bool evidence.derived_from_five_predicates );
      ( "linked_residual_analyzer_ran",
        `Bool evidence.linked_residual_analyzer_ran );
    ]

let () =
  Arg.parse
    [
      ("--repo-root", Arg.Set_string repo_root, "repository root");
      ( "--artifact-dir",
        Arg.Set_string artifact_dir,
        "active artifact directory" );
      ("--report", Arg.Set_string report, "report path");
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !artifact_dir = "" || !report = "" then failwith usage;
  let manifest_path = Filename.concat !artifact_dir "manifest.json" in
  expect (Sys.file_exists manifest_path) ("missing manifest: " ^ manifest_path);
  let manifest = Yojson.Safe.from_file manifest_path in
  let artifacts = artifact_paths manifest in
  expect
    (List.length artifacts >= 2)
    "expected at least two independent module artifacts";
  artifacts
  |> List.iter (fun path ->
         let artifact = Yojson.Safe.from_file path in
         expect
           (string_field "scope" artifact = "module-only-pre-link")
           (path ^ ": not module-only-pre-link");
         expect
           (bool_field "linked_entrypoints_used"
              (member "module_boundary" artifact)
           = false)
           (path ^ ": linked entrypoint used before PE"));
  let linked_path = linked_artifact_path manifest in
  expect (linked_path <> "") "manifest missing linked_artifact";
  expect
    (Sys.file_exists linked_path)
    ("missing linked artifact: " ^ linked_path);
  let linked = Yojson.Safe.from_file linked_path in
  expect
    (string_field "artifact_kind" linked
    = "abstract-speculate-linked-residual-analyzer")
    "linked artifact kind mismatch";
  expect
    (int_field "module_count" linked >= 2)
    "linked artifact module_count < 2";
  expect
    (bool_field "residual_linking_performed" linked)
    "residual linking was not performed";
  expect
    (bool_field "per_module_stage2_inputs_used" linked)
    "per-module stage2 input dispatch missing";
  expect
    (bool_field "provider_derived_importer_inputs_used" linked)
    "provider-derived importer inputs missing";
  require_external_summaries linked;
  expect
    (bool_field "linked_facts_prelink" linked = false)
    "linked facts were used before PE";
  expect
    (bool_field "metadata_only_proof" linked = false)
    "metadata-only proof accepted";
  expect
    (list_field "declared_imports" linked <> [])
    "no parsed-CIL imports recorded";
  expect
    (list_field "declared_exports" linked <> [])
    "no parsed-CIL exports recorded";
  expect
    (list_field "matched_obligations" linked <> [])
    "no matched structural obligations recorded";
  let output = linked_output linked in
  let log = execution_log linked in
  expect
    (bool_field "per_module_stage2_inputs_used" log)
    "linked output log missing per-module dispatch";
  expect
    (bool_field "provider_derived_importer_inputs_used" log)
    "linked output log missing provider-derived importer input dispatch";
  expect
    (bool_field "linked_residual_solver_run" linked)
    "linked artifact did not report solver-backed linked residual run";
  expect
    (bool_field "linked_residual_solver_run" log)
    "linked execution log did not report solver-backed residual run";
  expect
    (bool_field "linked_solver_backed_residual_fixpoint" log)
    "linked execution log did not report solver-backed residual fixpoint";
  expect
    (bool_field "linked_worklist_drained" log)
    "linked residual solver worklist did not drain";
  expect
    (bool_field "linked_overlay_only" log = false)
    "linked residual path reported overlay-only";
  expect
    (int_field "linked_residual_equation_count" log > 0)
    "linked residual path did not expose residual equations";
  expect
    (int_field "linked_solver_iteration_count" log > 1)
    "linked residual path did not iterate residual equations";
  expect
    (int_field "linked_state_read_count" log > 0)
    "linked residual path did not read solver state";
  expect
    (int_field "linked_seed_input_read_count" log > 0)
    "linked residual path did not report dynamic seed reads";
  expect
    (bool_field "linked_equation_apply_reads_solver_state" log)
    "linked residual path did not report state-reading equation applications";
  expect
    (list_field "linked_exact_cell_dependencies" log <> [])
    "linked residual path did not expose exact cell dependencies";
  require_global_residual_fixpoint linked;
  require_api_residual_model_coverage linked;
  require_source_absent
    (project_path
       "sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml");
  require_source_absent
    (project_path
       "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_dump.ml");
  let recomputed =
    recompute_evidence ~source_guard_passed:true ~manifest linked
  in
  let primary_linkage_check = Relation.primary_linkage_check_json linked in
  expect
    (string_field "status" primary_linkage_check = "pass")
    "primary linkage selected-observation invariant check failed";
  expect_evidence_matches "top evidence" (top_evidence linked) recomputed;
  expect_evidence_matches "linked output evidence" (output_evidence linked)
    recomputed;
  expect_evidence_matches "execution log evidence" (log_evidence linked)
    recomputed;
  expect
    (bool_field "linked_residual_analyzer_ran" linked
    = recomputed.linked_residual_analyzer_ran)
    "top-level linked_residual_analyzer_ran does not match recomputed evidence";
  expect
    (bool_field "linked_residual_analyzer_ran" log
    = recomputed.linked_residual_analyzer_ran)
    "execution-log linked_residual_analyzer_ran does not match recomputed \
     evidence";
  expect recomputed.linked_residual_analyzer_ran
    "linked residual analyzer evidence predicate is false";
  expect
    (recomputed.final_input_row_count
    = List.length (list_field "final_input_table" output))
    "final input row count is not recomputed from linked output";
  expect
    (recomputed.final_output_row_count
    = List.length (list_field "final_output_table" output))
    "final output row count is not recomputed from linked output";
  run_false_case_checks manifest linked;
  let memory_delta_validations =
    list_field "external_summaries" linked
    |> List.map (fun summary -> member "memory_delta_validation" summary)
  in
  expect
    (memory_delta_validations <> []
    && List.for_all
         (fun validation ->
           string_field "compatibility_projection_status" validation
           = "typed-effect-projection-non-authoritative"
           && int_field "checked_delta_count" validation > 0
           && int_field "checked_chain_count" validation > 0
           && string_field "status" (member "validation" validation) = "pass")
         memory_delta_validations)
    "ExternalSummary typed memory_delta_validation summary missing or failing";
  let report_json =
    `Assoc
      [
        ( "schema_version",
          `String
            Sparrow_modular_ocaml.Abstract_speculate_residual_linker
            .schema_version );
        ("status", `String "pass");
        ("module_artifact_count", `Int (List.length artifacts));
        ("linked_artifact", `String linked_path);
        ("linked_residual_analyzer_evidence", evidence_to_json recomputed);
        ( "primary_linkage_observation_summary",
          member "primary_linkage_observation_summary" primary_linkage_check );
        ("primary_linkage_observation_check", primary_linkage_check);
        ( "primary_linkage_matched_observations",
          member "matched_observations" primary_linkage_check );
        ("primary_linkage_failures", member "failures" primary_linkage_check);
        ("matched_obligation_count", `Int recomputed.matched_obligation_count);
        ( "unresolved_obligation_count",
          `Int recomputed.unresolved_obligation_count );
        ("per_module_stage2_inputs_used", `Bool true);
        ("external_summary_v3_checked", `Bool true);
        ("external_summary_v1_compat_non_authoritative", `Bool true);
        ("memory_delta_validation", `List memory_delta_validations);
        ( "linked_residual_analyzer_ran",
          `Bool recomputed.linked_residual_analyzer_ran );
        ("global_residual_fixpoint", global_residual_report linked);
        ( "global_residual_fixpoint_run",
          member "global_residual_fixpoint_run" linked );
        ("shortcut_guard", `String "pass");
        ( "false_case_checks",
          `List
            [
              `String "linked_execute_returned";
              `String "module_identity_set";
              `String "duplicate_module_log";
              `String "residual_rows_observed";
              `String "obligations_closed";
              `String "no_shortcut_path";
              `String "semantic_exports";
              `String "semantic_export_source";
              `String "provider_return_summary";
              `String "linked_environment";
              `String "external_summary_v3_missing";
              `String "external_summary_v1_compat_only";
              `String "external_summary_schema_downgrade";
              `String "external_summary_status_corruption";
              `String "external_summary_return_effect_missing";
              `String "external_summary_memory_delta_missing";
              `String "external_summary_memory_delta_role_corruption";
              `String "external_summary_memory_delta_location_corruption";
              `String "external_summary_memory_delta_value_corruption";
              `String "external_summary_memory_delta_provenance_corruption";
              `String "external_summary_memory_delta_chain_missing";
              `String "external_summary_memory_delta_chain_corruption";
              `String "external_summary_return_effect_value_corruption";
              `String "external_summary_return_effect_location_corruption";
              `String "external_summary_return_effect_provenance_corruption";
              `String "linked_derivation_effect_id_mismatch";
              `String "typed_scalar_metadata_corruption";
              `String "linked_scalar_protocol_id_corruption";
              `String "manual_extern_value";
              `String "provider_importer_value_match";
              `String "provider_before_importer_order";
              `String "obligation_mapping";
            ] );
      ]
  in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report report_json;
  print_endline "abstract_speculate_residual_linking_pe_check: PASS"
