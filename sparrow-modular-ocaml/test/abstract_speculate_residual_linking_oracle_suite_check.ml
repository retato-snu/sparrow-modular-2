let repo_root = ref "."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "abstract_speculate_residual_linking_oracle_suite_check --repo-root <repo> --artifact-dir <dir> --report <json>"

let member = Yojson.Safe.Util.member
module Relation = Sparrow_modular_ocaml.Abstract_speculate_residual_relation
module ScalarCall = Sparrow_modular_ocaml.Abstract_speculate_residual_scalar_call
module MemoryDelta = Sparrow_modular_ocaml.Abstract_speculate_residual_memory_delta

let expect cond msg = if not cond then failwith msg

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> s | _ -> ""
let int_field name json = match assoc_field name json with Some (`Int n) -> n | _ -> min_int
let bool_field name json = match assoc_field name json with Some (`Bool b) -> b | _ -> false
let bool_true_field name json = match assoc_field name json with Some (`Bool true) -> true | _ -> false
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

let has_field name json = match assoc_field name json with Some `Null | None -> false | Some _ -> true

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let starts_with s prefix =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let project_path rel =
  let direct = Filename.concat !repo_root rel in
  if Sys.file_exists direct then direct
  else
    let prefix = "sparrow-modular-ocaml/" in
    let prefix_len = String.length prefix in
    if String.length rel > prefix_len && String.sub rel 0 prefix_len = prefix then
      Filename.concat !repo_root (String.sub rel prefix_len (String.length rel - prefix_len))
    else direct
let semantic_exports linked = list_field "semantic_exports" linked

let provider_memory summary =
  summary |> member "provenance" |> member "provider_row" |> list_field "memory"

let provider_memory_value summary location =
  provider_memory summary
  |> List.find_opt (fun cell -> string_field "location" cell = location)
  |> Option.map (fun cell -> string_field "value" cell)

let effect_matches_summary_provenance summary eff =
  string_field "provider_module" eff = string_field "provider_module" (member "provenance" summary) &&
  string_field "provider_source_hash" eff = string_field "provider_source_hash" (member "provenance" summary) &&
  string_field "provider_artifact_path" eff = string_field "provider_artifact_path" (member "provenance" summary) &&
  int_field "provider_phase_index" eff = int_field "provider_phase_index" (member "provenance" summary)

let expected_effect_id domain eff =
  string_field "provider_module" eff ^ ":" ^
  string_field "symbol" eff ^ ":" ^
  domain ^ ":" ^
  string_field "location" eff

let scalar_protocol_summary_ok summary eff =
  let extern_scalar = summary |> member "external_summary_v1_compat" |> member "extern_scalar_value" in
  ScalarCall.validation_ok (ScalarCall.validate_return_effect_json eff) &&
  ScalarCall.validation_ok (ScalarCall.validate_v1_extern_scalar_value_json extern_scalar) &&
  string_field "scalar_protocol_schema" eff = ScalarCall.schema_id &&
  string_field "scalar_protocol_schema" extern_scalar = ScalarCall.schema_id &&
  string_field "scalar_call_protocol_id" eff <> "" &&
  string_field "scalar_call_protocol_id" eff = string_field "scalar_call_protocol_id" extern_scalar &&
  bool_true_field "typed_scalar_metadata_valid" eff &&
  bool_true_field "typed_scalar_metadata_valid" extern_scalar

let typed_effect_ok expected_domain eff =
  string_field "domain" eff = expected_domain &&
  string_field "effect_id" eff <> "" &&
  string_field "location" eff <> "" &&
  string_field "provider_module" eff <> "" &&
  string_field "provider_source_hash" eff <> "" &&
  string_field "derivation_source" eff = "provider-stage2-output" &&
  string_field "witness_scope" eff = "selected-sparrow-itv"

let scalar_return_metadata_ok summary eff =
  let extern_scalar = summary |> member "external_summary_v1_compat" |> member "extern_scalar_value" in
  string_field "scalar_protocol_schema" eff = ScalarCall.schema_id &&
  string_field "scalar_protocol_schema" extern_scalar = ScalarCall.schema_id &&
  string_field "scalar_call_protocol_id" eff <> "" &&
  string_field "scalar_call_protocol_id" eff = string_field "scalar_call_protocol_id" extern_scalar &&
  string_field "scalar_value_kind" eff <> "" &&
  member "scalar_value" eff <> `Null &&
  bool_true_field "typed_scalar_metadata_valid" eff &&
  bool_true_field "typed_scalar_metadata_valid" extern_scalar

let return_effect_ok summary eff =
  let function_return = summary |> member "external_summary_v1_compat" |> member "function_return_summary" in
  typed_effect_ok "return" eff &&
  ScalarCall.validation_ok (ScalarCall.validate_return_effect_json eff) &&
  ScalarCall.validation_ok
    (ScalarCall.validate_v1_extern_scalar_value_json
       (summary |> member "external_summary_v1_compat" |> member "extern_scalar_value")) &&
  effect_matches_summary_provenance summary eff &&
  scalar_protocol_summary_ok summary eff &&
  string_field "source_evidence_path" eff = "provider_row.return" &&
  string_field "location" eff = string_field "return_location" function_return &&
  member "value" eff = member "return_value" function_return &&
  string_field "effect_id" eff = expected_effect_id "return" eff &&
  scalar_return_metadata_ok summary eff

let memory_effect_ok summary expected_domain eff =
  let source_prefix = "provider_row.memory:" in
  let source_path = string_field "source_evidence_path" eff in
  let source_location =
    if starts_with source_path source_prefix then
      Some (String.sub source_path (String.length source_prefix)
        (String.length source_path - String.length source_prefix))
    else None
  in
  match source_location with
  | None -> false
  | Some source_location ->
      let expected_value = provider_memory_value summary source_location in
      typed_effect_ok expected_domain eff &&
      effect_matches_summary_provenance summary eff &&
      Option.map (fun value -> member "value" eff = `String value) expected_value = Some true &&
      (if expected_domain = "global-write-read" then
         string_field "location" eff = source_location &&
         string_field "normalized_location" eff = source_location &&
         string_field "symbol" eff = source_location
       else
         string_field "location" eff = "(" ^ string_field "symbol" eff ^ ",p)" &&
         string_field "normalized_location" eff = string_field "location" eff) &&
      starts_with (string_field "effect_id" eff) (string_field "provider_module" eff ^ ":") &&
      contains (string_field "effect_id" eff) (expected_domain ^ ":" ^ string_field "location" eff)

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
      List.for_all (fun delta ->
        MemoryDelta.validation_ok (MemoryDelta.validate_delta_json delta) &&
        string_field "memory_delta_schema" delta = MemoryDelta.memory_delta_schema_id &&
        string_field "reader_role" delta = "reader" &&
        string_field "writer_role" delta = "writer")
        (list_field "memory_deltas" summary) &&
      List.for_all (fun chain ->
        string_field "memory_delta_schema" chain = MemoryDelta.memory_delta_schema_id &&
        list_field "entries" chain <> [])
        (list_field "delta_chains" summary)
  in
  string_field "schema_version" summary = "abstract-speculate-external-summary/v3" &&
  string_field "summary_api_status" summary = "prototype-internal" &&
  string_field "summary_scope" summary = "sparrow-itv-selected-witness" &&
  list_field "effect_domains" summary <> [] &&
  list_field "return_effects" summary <> [] &&
  List.for_all (return_effect_ok summary) (list_field "return_effects" summary) &&
  memory_delta_authority_ok &&
  List.for_all (memory_effect_ok summary "global-write-read") (list_field "global_effects" summary) &&
  List.for_all (memory_effect_ok summary "pointer-memory-effect") (list_field "pointer_effects" summary) &&
  member "external_summary_v1_compat" summary <> `Null &&
  string_field "schema_version" (member "external_summary_v1_compat" summary) =
    "abstract-speculate-external-summary/v1" &&
  string_field "derivation_source" (member "provenance" summary) = "provider-stage2-output"

let has_effect domain summaries =
  summaries
  |> List.exists (fun summary ->
       let field =
         match domain with
         | "global-write-read" -> "global_effects"
         | "pointer-memory-effect" -> "pointer_effects"
         | _ -> "return_effects"
       in
       list_field field summary <> [])

let effect_count field summaries =
  List.fold_left (fun acc summary -> acc + List.length (list_field field summary)) 0 summaries

let require_external_summaries witness_id residual =
  let summaries = list_field "external_summaries" residual in
  expect (summaries <> []) (witness_id ^ ": missing ExternalSummary v3 entries");
  expect (List.for_all external_summary_ok summaries)
    (witness_id ^ ": malformed ExternalSummary v3 entries");
  if witness_id = "global_write_read" then
    expect (has_effect "global-write-read" summaries)
      (witness_id ^ ": missing v3 global delta");
  if witness_id = "pointer_memory_effect" then
    expect (has_effect "pointer-memory-effect" summaries)
      (witness_id ^ ": missing v3 pointer delta")

let source_guard_obligation_for_paths witness_id residual_sources suite_sources =
  let forbidden = ["Real_sparrow_frontend." ^ "global_for_" ^ "files"; "Mergecil." ^ "merge"; "real_sparrow_" ^ "premerge_linked_observer"] in
  let residual_clean =
    residual_sources |> List.for_all (fun path ->
      let src = read_file path in
      forbidden |> List.for_all (fun needle -> not (contains src needle)))
  in
  let suite_oracle_scoped =
    suite_sources |> List.for_all (fun path ->
      let src = read_file path in
      (not (contains src "premerge_linked_observer")) || contains src "Oracle" || contains src "oracle_reference_kind")
  in
  let pass = residual_clean && suite_oracle_scoped in
  `Assoc [
    "name", `String "no_premerge_implementation_shortcut";
    "category", `String "shortcut-guard";
    "witness_id", `String witness_id;
    "status", `String (if pass then "pass" else "fail");
    "residual_linking_implementation_premerge_free", `Bool residual_clean;
    "premerge_artifacts_oracle_scoped", `Bool suite_oracle_scoped;
    "evidence_paths", `List (List.map (fun p -> `String p) (residual_sources @ suite_sources));
  ]

let source_guard_obligation witness_id =
  source_guard_obligation_for_paths witness_id
    [project_path "sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml";
     project_path "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_dump.ml"]
    [project_path "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_dump.ml";
     project_path "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml"]

let normalized_observations = Relation.return_observations
let obligations_for witness_id category residual oracle =
  Relation.oracle_suite_obligations ~source_guard_obligation witness_id category residual oracle
let observations_have_provenance = Relation.observations_have_provenance
let last = function
  | [] -> None
  | xs -> Some (List.nth xs (List.length xs - 1))

let cycle_topology_edges topology =
  topology |> List.concat_map (fun scc -> list_field "edges" scc)

let provenance_fields_present ?edge_kind json =
  let kind_ok =
    match edge_kind with
    | None -> string_field "edge_kind" json <> ""
    | Some expected -> string_field "edge_kind" json = expected
  in
  kind_ok &&
  string_field "source" json <> "" &&
  string_field "provenance_level" json <> "" &&
  string_field "stable_evidence_id" json <> ""

let cycle_topology_has_edge ~import_name ~export_name topology =
  cycle_topology_edges topology
  |> List.exists (fun edge ->
       string_field "import_name" edge = import_name &&
       string_field "export_name" edge = export_name)

let cycle_topology_edge_provenance_passes topology =
  let edges = cycle_topology_edges topology in
  edges <> [] &&
  List.for_all
    (provenance_fields_present ~edge_kind:"import-export-function-dependency")
    edges

let json_key json = Yojson.Safe.to_string json

let sorted_json_list xs =
  xs = List.sort (fun a b -> compare (json_key a) (json_key b)) xs

let int_json_field name json =
  match assoc_field name json with
  | Some (`Int n) -> Some n
  | Some (`String s) -> (try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let shared_final_cell_value residual cell_id =
  list_field "shared_scc_final_cells" residual
  |> List.find_map (fun cell ->
       if string_field "shared_scc_cell_id" cell = cell_id then
         int_json_field "value" cell
       else None)

let shared_scc_dependency_mentions residual needle =
  list_field "shared_scc_dependencies" residual
  |> List.exists (fun dep ->
       contains (string_field "source" dep) needle || contains (string_field "target" dep) needle)

let source_cell_matches_final residual value_json =
  let cell_id = string_field "source_shared_scc_cell_id" value_json in
  cell_id <> "" &&
  match int_json_field "value" value_json, shared_final_cell_value residual cell_id with
  | Some value, Some final_value -> value = final_value
  | _ -> false

let observable_value_matches_final residual obs =
  string_field "observable_kind" obs = "imported-cyclic-sink-write" &&
  string_field "observable_location" obs <> "" &&
  provenance_fields_present ~edge_kind:"shared-scc-observable-copy" obs &&
  source_cell_matches_final residual obs &&
  bool_field "shared_scc_value_matches" obs

let cyclic_topology_edges residual =
  list_field "linked_cycle_topology" residual
  |> List.concat_map (fun scc -> if bool_field "is_cyclic" scc then list_field "edges" scc else [])

let cyclic_topology_edges_have_cycle residual =
  let pairs =
    cyclic_topology_edges residual
    |> List.map (fun edge -> string_field "importer_module" edge, string_field "provider_module" edge)
  in
  pairs |> List.exists (fun (src, dst) ->
    src <> "" && dst <> "" &&
    (src = dst || List.exists (fun (src', dst') -> src' = dst && dst' = src) pairs))

let imported_cyclic_values_exact residual =
  let values = list_field "imported_cyclic_observable_values" residual in
  let value_has_edge edge =
    values
    |> List.exists (fun value ->
         string_field "importer_module" value = string_field "importer_module" edge &&
         string_field "import_name" value = string_field "import_name" edge &&
         string_field "provider_module" value = string_field "provider_module" edge &&
         string_field "export_name" value = string_field "export_name" edge &&
         bool_field "exact_singleton" value &&
         bool_field "no_extra_imprecision" value &&
         bool_field "shared_scc_value_matches" value &&
         bool_field "observable_sink_dependency_present" value &&
         source_cell_matches_final residual value &&
         list_field "observable_values" value <> [] &&
         List.for_all (observable_value_matches_final residual)
           (list_field "observable_values" value))
  in
  values <> [] &&
  bool_field "cyclic_imported_value_exact_singleton_parity" residual &&
  bool_field "cycle_final_values_derive_from_shared_scc_final_cells" residual &&
  List.for_all value_has_edge (cyclic_topology_edges residual)

let shared_scc_export_values_exact residual =
  let exports = list_field "linked_cycle_accepted_exports" residual in
  exports <> [] &&
  List.for_all (fun export ->
    string_field "derivation_source" export = "shared_scc_final_cells" &&
    provenance_fields_present ~edge_kind:"shared-scc-export-final-cell" export &&
    bool_field "shared_scc_value_matches" export &&
    source_cell_matches_final residual export)
    exports

let shared_scc_topology_dependencies_present residual =
  cyclic_topology_edges residual
  |> List.for_all (fun edge ->
       shared_scc_dependency_mentions residual
         ("export-return:" ^ string_field "provider_module" edge ^ ":" ^ string_field "export_name" edge) &&
       shared_scc_dependency_mentions residual
         ("import-observable:" ^ string_field "importer_module" edge ^ ":" ^ string_field "import_name" edge))

let shared_scc_schedule_has_changed_dependency residual =
  list_field "shared_scc_worklist_schedule" residual
  |> List.exists (fun event ->
       string_field "event" event = "enqueue" &&
       string_field "reason" event = "changed-cell-dependent" &&
       string_field "equation_id" event <> "" &&
       string_field "target" event <> "")

let shared_scc_solver_evidence_passes residual =
  bool_field "shared_scc_worklist_run" residual &&
  int_field "shared_scc_state_read_count" residual > 0 &&
  list_field "shared_scc_equation_ids" residual <> [] &&
  list_field "shared_scc_cell_ids" residual <> [] &&
  list_field "shared_scc_dependencies" residual <> [] &&
  list_field "shared_scc_worklist_schedule" residual <> [] &&
  shared_scc_schedule_has_changed_dependency residual &&
  list_field "shared_scc_final_cells" residual <> [] &&
  sorted_json_list (list_field "shared_scc_equation_ids" residual) &&
  sorted_json_list (list_field "shared_scc_cell_ids" residual) &&
  sorted_json_list (list_field "shared_scc_dependencies" residual) &&
  sorted_json_list (list_field "shared_scc_final_cells" residual) &&
  shared_scc_topology_dependencies_present residual &&
  imported_cyclic_values_exact residual &&
  shared_scc_export_values_exact residual

let recomputed_cycle_evidence_passes residual =
  let topology = list_field "linked_cycle_topology" residual in
  let rounds = list_field "linked_cycle_rounds" residual in
  let cyclic_scc_count =
    topology |> List.fold_left (fun acc scc -> if bool_field "is_cyclic" scc then acc + 1 else acc) 0
  in
  let changed_counts =
    rounds |> List.map (fun round -> int_field "changed_binding_count" round)
  in
  let reported_changed_counts =
    list_field "linked_cycle_changed_bindings" residual
    |> List.map (function `Int n -> n | _ -> min_int)
  in
  let last_round_stable =
    match last rounds with
    | Some round ->
        (not (bool_field "changed" round)) &&
        int_field "changed_binding_count" round = 0 &&
        list_field "linked_environment" round <> [] &&
        List.for_all (fun binding ->
          string_field "origin" binding = "shared-scc-final-cell" ||
          string_field "origin" binding = "provider-derived")
          (list_field "linked_environment" round)
    | None -> false
  in
  bool_field "linked_cyclic_residual_solver_run" residual &&
  bool_field "linked_cycle_worklist_drained" residual &&
  bool_field "linked_cycle_obligations_closed" residual &&
  bool_field "linked_cycle_stable_exports" residual &&
  not (bool_field "linked_overlay_only" residual) &&
  (list_field "linked_cycle_shared_scc_solvers" residual
   |> List.for_all (fun solver ->
        bool_field "shared_scc_authoritative_for_cycle_acceptance" solver &&
        not (bool_field "linker_rerun_convergence_used_for_acceptance" solver) &&
        string_field "final_linked_environment_source" solver = "shared_scc_final_cells" &&
        List.for_all (provenance_fields_present ~edge_kind:"import-export-function-dependency")
          (list_field "shared_scc_edges" solver))) &&
  int_field "linked_cycle_scc_count" residual = cyclic_scc_count &&
  cyclic_scc_count > 0 &&
  int_field "linked_cycle_iteration_count" residual = max 0 (List.length rounds - 1) &&
  int_field "linked_cycle_bootstrap_bindings_remaining" residual = 0 &&
  topology <> [] &&
  rounds <> [] &&
  changed_counts = reported_changed_counts &&
  cyclic_topology_edges_have_cycle residual &&
  last_round_stable &&
  shared_scc_solver_evidence_passes residual

let scheduler_reports residual =
  let as_reports = function
    | `List xs -> xs
    | `Assoc _ as report -> [report]
    | _ -> []
  in
  as_reports (member "linked_cycle_scheduler" residual) @
  as_reports (member "linked_cycle_scheduler_evidence" residual)

let scheduler_edges residual =
  let top_level = list_field "linked_cycle_scheduler_edges" residual in
  let root_edges =
    scheduler_reports residual |> List.concat_map (list_field "edges")
  in
  let nested_topology_edges =
    list_field "linked_cycle_topology" residual
    |> List.concat_map (fun scc -> list_field "scheduler_edges" scc)
  in
  top_level @ root_edges @ nested_topology_edges

let scheduler_sccs residual =
  list_field "linked_cycle_scheduler_sccs" residual @
  (scheduler_reports residual |> List.concat_map (list_field "sccs")) @
  (list_field "linked_cycle_topology" residual |> List.concat_map (list_field "scheduler_sccs"))

let scheduler_provenance_is_call_backed provenance =
  List.mem provenance
    ["direct_program_callgraph";
     "direct-program-callgraph";
     "direct_program_callgraph_edge";
     "direct-program-callgraph-edge";
     "residual_call_binding";
     "residual-call-binding";
     "residual_binding_call_provenance";
     "residual-binding-call-provenance"]

let scheduler_claims_callgraph_backed residual =
  bool_true_field "linked_cycle_callgraph_backed_schedule" residual ||
  (scheduler_reports residual @ list_field "linked_cycle_topology" residual) |> List.exists (fun root ->
    bool_true_field "callgraph_backed" root ||
    bool_true_field "callgraph_backed_schedule" root ||
    bool_true_field "callgraph_backed_scheduler" root ||
    string_field "scheduler_claim" root = "callgraph-backed" ||
    string_field "claim" root = "callgraph-backed" ||
    string_field "scheduler_kind" root = "callgraph-backed" ||
    string_field "schedule_source" root = "callgraph-backed" ||
    string_field "scheduler_source" root = "residual-call-binding-callgraph")

let scheduler_edge_evidence_id edge =
  let direct = string_field "evidence_id" edge in
  if direct <> "" then direct else string_field "stable_evidence_id" edge

let scheduler_edge_importer edge =
  let caller = string_field "importer_or_caller" edge in
  if caller <> "" then caller else string_field "importer_module" edge

let scheduler_edge_provider edge =
  let callee = string_field "provider_or_callee" edge in
  if callee <> "" then callee else string_field "provider_module" edge

let scheduler_edge_symbol edge =
  let symbol = string_field "symbol" edge in
  if symbol <> "" then symbol else string_field "import_name" edge

let scheduler_edge_has_required_provenance edge =
  string_field "edge_kind" edge <> "" &&
  string_field "source" edge <> "" &&
  scheduler_provenance_is_call_backed (string_field "provenance_level" edge) &&
  scheduler_edge_evidence_id edge <> "" &&
  scheduler_edge_importer edge <> "" &&
  scheduler_edge_provider edge <> "" &&
  scheduler_edge_symbol edge <> "" &&
  not (contains (string_field "source" edge) "bootstrap") &&
  not (contains (string_field "source" edge) "provider-derived")

let scheduler_edges_have_cycle edges =
  let pairs =
    edges |> List.map (fun edge -> scheduler_edge_importer edge, scheduler_edge_provider edge)
  in
  pairs |> List.exists (fun (src, dst) ->
    src <> "" && dst <> "" &&
    (src = dst || List.exists (fun (src', dst') -> src' = dst && dst' = src) pairs))

let scheduler_fixture_edges_present witness_id edges =
  if witness_id = "callgraph_scheduler_cycle" then
    let has_edge ~importer ~provider ~symbol =
      edges |> List.exists (fun edge ->
        scheduler_edge_importer edge = importer &&
        scheduler_edge_provider edge = provider &&
        contains (scheduler_edge_symbol edge) symbol)
    in
    has_edge ~importer:"a.c" ~provider:"b.c" ~symbol:"scheduler_b" &&
    has_edge ~importer:"b.c" ~provider:"a.c" ~symbol:"scheduler_a"
  else true

let callgraph_scheduler_evidence_passes witness_id residual =
  if witness_id <> "callgraph_scheduler_cycle" then true
  else
    let edges = scheduler_edges residual in
    let reported_scc_count =
      match int_json_field "linked_cycle_scheduler_scc_count" residual with
      | Some n -> n
      | None -> int_field "linked_cycle_scc_count" residual
    in
    recomputed_cycle_evidence_passes residual &&
    scheduler_claims_callgraph_backed residual &&
    edges <> [] &&
    List.for_all scheduler_edge_has_required_provenance edges &&
    scheduler_edges_have_cycle edges &&
    scheduler_fixture_edges_present witness_id edges &&
    scheduler_sccs residual <> [] &&
    reported_scc_count > 0 &&
    reported_scc_count = int_field "linked_cycle_scc_count" residual &&
    shared_scc_solver_evidence_passes residual

let cycle_evidence_obligations witness_id category residual =
  if witness_id <> "cycle_topology" && witness_id <> "callgraph_scheduler_cycle" then []
  else [
    `Assoc [
      "name", `String "cyclic_residual_fixpoint_evidence";
      "category", `String category;
      "witness_id", `String witness_id;
      "status", `String (if recomputed_cycle_evidence_passes residual then "pass" else "fail");
      "linked_cycle_scc_count", member "linked_cycle_scc_count" residual;
      "linked_cycle_iteration_count", member "linked_cycle_iteration_count" residual;
      "linked_cycle_bootstrap_bindings_remaining", member "linked_cycle_bootstrap_bindings_remaining" residual;
      "evidence_source", `String "recomputed from linked_cycle_topology, per-edge provenance, and linked_cycle_rounds";
    ]
    ;
    `Assoc [
      "name", `String "cyclic_imported_value_exact_singleton_parity";
      "category", `String "cycle-value-parity";
      "witness_id", `String witness_id;
      "status", `String (if shared_scc_solver_evidence_passes residual then "pass" else "fail");
      "shared_scc_worklist_run", member "shared_scc_worklist_run" residual;
      "shared_scc_state_read_count", member "shared_scc_state_read_count" residual;
      "imported_cyclic_observable_values", member "imported_cyclic_observable_values" residual;
      "shared_scc_final_cells", member "shared_scc_final_cells" residual;
      "evidence_source", `String "shared_scc_final_cells source_shared_scc_cell_id exact singleton cross-check with stable provenance ids";
    ]
  ] @
  if witness_id <> "callgraph_scheduler_cycle" then []
  else [
    `Assoc [
      "name", `String "callgraph_backed_scheduler_evidence";
      "category", `String "cycle-scheduler-provenance";
      "witness_id", `String witness_id;
      "status", `String (if callgraph_scheduler_evidence_passes witness_id residual then "pass" else "fail");
      "scheduler_claim", member "linked_cycle_scheduler" residual;
      "scheduler_edges", `List (scheduler_edges residual);
      "scheduler_sccs", `List (scheduler_sccs residual);
      "evidence_source",
        `String "recomputed from linked_cycle_scheduler edge provenance, SCC count, and shared_scc final-cell bridge";
    ]
  ]

let obligation_passes = Relation.obligation_passes
let witness_pass_status = Relation.witness_pass_status

let witness_report witness =
  let witness_id = string_field "witness_id" witness in
  let category = string_field "category" witness in
  let residual_path = string_field "residual_linked_artifact" witness in
  let oracle_path = string_field "premerge_observer_artifact" witness in
  expect (Sys.file_exists residual_path) ("missing residual linked artifact: " ^ residual_path);
  expect (Sys.file_exists oracle_path) ("missing oracle artifact: " ^ oracle_path);
  let residual = Yojson.Safe.from_file residual_path in
  let oracle = Yojson.Safe.from_file oracle_path in
  expect (string_field "artifact_kind" residual = "abstract-speculate-linked-residual-analyzer")
    (witness_id ^ ": residual artifact kind mismatch");
  expect (string_field "scope" oracle = "linked-whole-program-fixture")
    (witness_id ^ ": oracle scope mismatch");
  expect (contains (string_field "linked_id" residual) witness_id)
    (witness_id ^ ": residual linked artifact identity mismatch");
  expect (string_field "group" oracle = witness_id)
    (witness_id ^ ": oracle artifact identity mismatch");
  require_external_summaries witness_id residual;
  let summaries = list_field "external_summaries" residual in
  let obligations = obligations_for witness_id category residual oracle @ cycle_evidence_obligations witness_id category residual in
  let residual_obs, oracle_obs = normalized_observations witness_id residual oracle in
  let selected_relation = Relation.selected_observation_relation_json ~witness_id ~residual ~oracle in
  let full_itv_relation = Relation.full_itv_semantic_relation_json ~witness_id ~residual ~oracle in
  let required_full_itv_fields = [
    "semantic_universe_manifest";
    "failure_taxonomy";
    "canonicalization";
    "oracle_identity";
    "residual_to_origin";
    "origin_to_residual";
  ] in
  let required_full_itv_fields_present =
    required_full_itv_fields |> List.for_all (fun field -> has_field field full_itv_relation)
  in
  let pass = witness_pass_status obligations residual_obs oracle_obs &&
             string_field "status" full_itv_relation = "pass" &&
             required_full_itv_fields_present in
  `Assoc [
    "witness_id", `String witness_id;
    "category", `String category;
    "status", `String (if pass then "pass" else "fail");
    "residual_linked_artifact", `String residual_path;
    "premerge_observer_artifact", `String oracle_path;
    "external_summary_v3_checked", `Bool true;
    "external_summary_v1_compat_non_authoritative", `Bool true;
    "external_summary_effect_counts", `Assoc [
      "summary_count", `Int (List.length summaries);
      "return_effect_count", `Int (effect_count "return_effects" summaries);
      "global_effect_count", `Int (effect_count "global_effects" summaries);
      "pointer_effect_count", `Int (effect_count "pointer_effects" summaries);
    ];
    "normalized_observations", `Assoc [
      "residual", `List residual_obs;
      "oracle", `List oracle_obs;
      "relation", `String "diagnostic selected observations; authoritative pass gate is full_itv_semantic_relation";
    ];
    "full_itv_semantic_summary", member "summary" full_itv_relation;
    "full_itv_semantic_relation", full_itv_relation;
    "full_itv_required_fields_present", `Bool required_full_itv_fields_present;
    "full_itv_required_fields", `List (List.map (fun field -> `String field) required_full_itv_fields);
    "semantic_universe", member "semantic_universe" full_itv_relation;
    "semantic_universe_manifest", member "semantic_universe_manifest" full_itv_relation;
    "failure_taxonomy", member "failure_taxonomy" full_itv_relation;
    "canonicalization", member "canonicalization" full_itv_relation;
    "oracle_identity", member "oracle_identity" full_itv_relation;
    "residual_to_origin", member "residual_to_origin" full_itv_relation;
    "origin_to_residual", member "origin_to_residual" full_itv_relation;
    "diagnostics", `Assoc [
      "selected_observation_summary", member "selected_observation_summary" selected_relation;
      "selected_observation_relation", selected_relation;
      "residual_to_oracle", member "residual_to_oracle" selected_relation;
      "oracle_to_residual", member "oracle_to_residual" selected_relation;
      "missing_observations", member "missing_observations" selected_relation;
      "extra_observations", member "extra_observations" selected_relation;
    ];
    "obligations", `List obligations;
  ]

let false_case name passes =
  `Assoc ["name", `String name; "status", `String (if passes then "pass" else "fail")]

let rec set_path path value json =
  match path, json with
  | [], _ -> value
  | key :: rest, `Assoc fields ->
      let old = match List.assoc_opt key fields with Some v -> v | None -> `Assoc [] in
      `Assoc ((key, set_path rest value old) :: List.remove_assoc key fields)
  | _ :: _, other -> other

let mutate_first_semantic_export_return residual value =
  match semantic_exports residual with
  | first :: rest ->
      let wrong = set_path ["return_value"] (`Int value) first in
      set_path ["semantic_exports"] (`List (wrong :: rest)) residual
  | [] -> residual

let remove_location_from_rows location rows =
  rows |> List.map (function
    | `Assoc fields as row ->
        begin match List.assoc_opt "memory" fields with
        | Some (`List cells) ->
            `Assoc (("memory", `List (List.filter (fun cell -> string_field "location" cell <> location) cells)) :: List.remove_assoc "memory" fields)
        | _ -> row
        end
    | row -> row)

let remove_linked_location location residual =
  let output = member "linked_output" residual in
  residual
  |> set_path ["linked_output"; "final_input_table"] (`List (remove_location_from_rows location (list_field "final_input_table" output)))
  |> set_path ["linked_output"; "final_output_table"] (`List (remove_location_from_rows location (list_field "final_output_table" output)))

let mutate_first_linked_cell_metadata residual metadata =
  let output = member "linked_output" residual in
  match list_field "final_output_table" output with
  | `Assoc row_fields :: row_rest ->
      begin match List.assoc_opt "memory" row_fields with
      | Some (`List (cell :: cell_rest)) ->
          let mutated_cell = set_path ["typed_cell_metadata"] metadata cell in
          let mutated_row =
            `Assoc (("memory", `List (mutated_cell :: cell_rest)) :: List.remove_assoc "memory" row_fields)
          in
          set_path ["linked_output"; "final_output_table"] (`List (mutated_row :: row_rest)) residual
      | _ -> residual
      end
  | _ -> residual

let obligation_set_fails witness_id category residual oracle =
  not (obligation_passes (obligations_for witness_id category residual oracle))

let full_itv_relation_fails witness_id residual oracle =
  let relation = Relation.full_itv_semantic_relation_json ~witness_id ~residual ~oracle in
  string_field "status" relation = "fail"

let selected_relation_passes witness_id residual oracle =
  let relation = Relation.selected_observation_relation_json ~witness_id ~residual ~oracle in
  string_field "status" relation = "pass"

let fails f =
  try
    ignore (f ());
    false
  with _ -> true

let write_file path data =
  let dir = Filename.dirname path in
  if dir <> "." && not (Sys.file_exists dir) then
    ignore (Sys.command ("mkdir -p " ^ Filename.quote dir));
  let oc = open_out path in
  output_string oc data;
  close_out oc

let log_contains path needle =
  Sys.file_exists path && contains (read_file path) needle

let negative_cases witnesses =
  let loaded =
    witnesses |> List.map (fun witness ->
      let witness_id = string_field "witness_id" witness in
      let category = string_field "category" witness in
      let residual = Yojson.Safe.from_file (string_field "residual_linked_artifact" witness) in
      let oracle = Yojson.Safe.from_file (string_field "premerge_observer_artifact" witness) in
      witness_id, category, residual, oracle, witness)
  in
  let find id =
    loaded |> List.find_opt (fun (witness_id, _, _, _, _) -> witness_id = id)
  in
  let mutate_first_summary path value residual =
    match list_field "external_summaries" residual with
    | first :: rest ->
        set_path ["external_summaries"] (`List (set_path path value first :: rest)) residual
    | [] -> residual
  in
  let mutate_first_effect effect_field path value residual =
    match list_field "external_summaries" residual with
    | first :: rest ->
        begin match list_field effect_field first with
        | eff :: eff_rest ->
            let mutated = set_path [effect_field] (`List (set_path path value eff :: eff_rest)) first in
            set_path ["external_summaries"] (`List (mutated :: rest)) residual
        | [] -> residual
        end
    | [] -> residual
  in
  let return_false =
    match loaded with
    | (id, category, residual, oracle, _) :: _ ->
        obligation_set_fails id category (mutate_first_semantic_export_return residual 999) oracle &&
        full_itv_relation_fails id (mutate_first_semantic_export_return residual 999) oracle
    | [] -> false
  in
  let v3_missing_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          let mutated = set_path ["external_summaries"] (`List []) residual in
          require_external_summaries "v3_missing_false" mutated)
    | [] -> false
  in
  let v3_compat_only_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        begin match list_field "external_summaries" residual with
        | first :: _ ->
            fails (fun () ->
              require_external_summaries "v3_compat_only_false"
                (set_path ["external_summaries"]
                   (`List [member "external_summary_v1_compat" first]) residual))
        | [] -> false
        end
    | [] -> false
  in
  let v3_schema_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        begin match list_field "external_summaries" residual with
        | first :: rest ->
            fails (fun () ->
              require_external_summaries "v3_schema_false"
                (set_path ["external_summaries"]
                   (`List (set_path ["schema_version"] (`String "abstract-speculate-external-summary/v1") first :: rest))
                   residual))
        | [] -> false
        end
    | [] -> false
  in
  let v3_status_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          require_external_summaries "v3_status_false"
            (mutate_first_summary ["summary_api_status"] (`String "stale-or-public") residual))
    | [] -> false
  in
  let v3_return_value_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          require_external_summaries "v3_return_value_false"
            (mutate_first_effect "return_effects" ["value"] (`Int 999) residual))
    | [] -> false
  in
  let typed_scalar_metadata_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          require_external_summaries "typed_scalar_metadata_false"
            (mutate_first_effect "return_effects" ["typed_scalar_metadata"; "provider_source_hash"] (`String "wrong-hash") residual))
    | [] -> false
  in
  let typed_scalar_relation_false =
    match loaded with
    | (id, _category, residual, oracle, _witness) :: _ ->
        let mutated =
          match list_field "linked_stage2_input_derivation" residual with
          | first :: rest ->
              set_path ["linked_stage2_input_derivation"]
                (`List (set_path ["scalar_call_protocol_id"] (`String "wrong-protocol-id") first :: rest))
                residual
          | [] -> residual
        in
        full_itv_relation_fails id mutated oracle
    | [] -> false
  in
  let global_false =
    match find "global_write_read" with
    | Some (id, category, residual, oracle, _) ->
        obligation_set_fails id category (remove_linked_location "shared_g" residual) oracle
    | None -> false
  in
  let pointer_false =
    match find "pointer_memory_effect" with
    | Some (id, category, residual, oracle, _) ->
        obligation_set_fails id category (remove_linked_location "(write_ptr,p)" residual) oracle
    | None -> false
  in
  let non_selected_itv_cell_false =
    match find "global_write_read" with
    | Some (id, _category, residual, oracle, _) ->
        let mutated = remove_linked_location "(main,tmp)" residual in
        full_itv_relation_fails id mutated oracle &&
        selected_relation_passes id mutated oracle
    | None -> false
  in
  let typed_cell_metadata_mismatch_false =
    match find "global_write_read" with
    | Some (id, _category, residual, oracle, _) ->
        let mutated =
          mutate_first_linked_cell_metadata residual
            (`Assoc [
              "value_model", `String "typed-itv-residual-cell/v1";
              "location", `String "(wrong,location)";
            ])
        in
        full_itv_relation_fails id mutated oracle
    | None -> false
  in
  let mixed_false =
    match find "mixed_role_chain" with
    | Some (id, category, residual, oracle, _) ->
        obligation_set_fails id category (set_path ["phase_log"] (`List []) residual) oracle
    | None -> false
  in
  let missing_oracle_false =
    loaded |> List.exists (fun (_, _, _, _, witness) ->
      let missing = string_field "premerge_observer_artifact" witness ^ ".missing" in
      let mutated = set_path ["premerge_observer_artifact"] (`String missing) witness in
      not (Sys.file_exists missing) && fails (fun () -> witness_report mutated))
  in
  let witness_identity_false =
    loaded |> List.exists (fun (id, _, _, _, witness) ->
      let mutated = set_path ["witness_id"] (`String (id ^ "-mutated")) witness in
      fails (fun () -> witness_report mutated))
  in
  let provenance_false =
    match loaded with
    | (id, category, residual, oracle, _) :: _ ->
        let residual_obs, oracle_obs = normalized_observations id residual oracle in
        let obligations = obligations_for id category residual oracle in
        let residual_obs_without_provenance =
          match residual_obs with
          | first :: rest -> set_path ["residual_provenance"] (`String "") first :: rest
          | [] -> []
        in
        observations_have_provenance (residual_obs @ oracle_obs) &&
        not (witness_pass_status obligations residual_obs_without_provenance oracle_obs)
    | [] -> false
  in
  let cycle_evidence_fails mutation =
    match find "cycle_topology" with
    | Some (_id, _category, residual, _oracle, _) ->
        not (recomputed_cycle_evidence_passes (mutation residual))
    | None -> false
  in
  let callgraph_scheduler_evidence_fails mutation =
    match find "callgraph_scheduler_cycle" with
    | Some (id, _category, residual, _oracle, _) ->
        not (callgraph_scheduler_evidence_passes id (mutation residual))
    | None -> false
  in
  let mutate_first_scheduler_edge mutate residual =
    let mutate_report_edges report =
      match list_field "edges" report with
      | first :: rest -> set_path ["edges"] (`List (mutate first :: rest)) report
      | [] -> report
    in
    let mutate_root_edges field residual =
      match member field residual with
      | `List (first :: rest) ->
          set_path [field] (`List (mutate_report_edges first :: rest)) residual
      | `Assoc _ as root ->
          set_path [field] (mutate_report_edges root) residual
      | _ -> residual
    in
    let residual =
      match list_field "linked_cycle_scheduler_edges" residual with
      | first :: rest ->
          set_path ["linked_cycle_scheduler_edges"] (`List (mutate first :: rest)) residual
      | [] -> residual
    in
    let residual =
      match list_field "linked_cycle_topology" residual with
      | first_scc :: rest_sccs ->
          begin match list_field "scheduler_edges" first_scc with
          | first :: rest ->
              set_path ["linked_cycle_topology"]
                (`List (set_path ["scheduler_edges"] (`List (mutate first :: rest)) first_scc :: rest_sccs))
                residual
          | [] -> residual
          end
      | [] -> residual
    in
    residual
    |> mutate_root_edges "linked_cycle_scheduler"
    |> mutate_root_edges "linked_cycle_scheduler_evidence"
  in
  let clear_scheduler_evidence residual =
    residual
    |> set_path ["linked_cycle_scheduler"] (`Assoc [])
    |> set_path ["linked_cycle_scheduler_evidence"] (`Assoc [])
    |> set_path ["linked_cycle_scheduler_edges"] (`List [])
    |> set_path ["linked_cycle_scheduler_sccs"] (`List [])
    |> set_path ["linked_cycle_callgraph_backed_schedule"] (`Bool false)
    |> set_path ["linked_cycle_topology"] (`List [])
  in
  let scheduler_dependency_only_relabel residual =
    residual
    |> mutate_first_scheduler_edge (fun edge ->
         edge
         |> set_path ["provenance_level"] (`String "residual_dependency_only")
         |> set_path ["source"] (`String "residual-dependency-only"))
  in
  let scheduler_missing_required_edge_field residual =
    residual
    |> mutate_first_scheduler_edge (fun edge -> set_path ["evidence_id"] (`String "") edge)
  in
  let scheduler_scc_count_mismatch residual =
    residual
    |> set_path ["linked_cycle_scheduler_scc_count"] (`Int 999)
    |> set_path ["linked_cycle_scc_count"] (`Int 999)
  in
  let old_topology_relabelled_without_provenance residual =
    let old_edges =
      list_field "linked_cycle_topology" residual
      |> List.concat_map (fun scc -> list_field "edges" scc)
    in
    let old_sccs = list_field "linked_cycle_topology" residual in
    residual
    |> set_path ["linked_cycle_scheduler"]
         (`List [
           `Assoc [
             "callgraph_backed_schedule", `Bool true;
             "scheduler_source", `String "residual-call-binding-callgraph";
             "edges", `List old_edges;
             "sccs", `List old_sccs;
           ]
         ])
    |> set_path ["linked_cycle_scheduler_edges"] (`List [])
    |> set_path ["linked_cycle_scheduler_sccs"] (`List [])
    |> set_path ["linked_cycle_callgraph_backed_schedule"] (`Bool true)
  in
  let mutate_first_imported_value path value residual =
    match list_field "imported_cyclic_observable_values" residual with
    | first :: rest ->
        set_path ["imported_cyclic_observable_values"]
          (`List (set_path path value first :: rest))
          residual
    | [] -> residual
  in
  let mutate_first_observable path value residual =
    match list_field "imported_cyclic_observable_values" residual with
    | first :: rest ->
        begin match list_field "observable_values" first with
        | obs :: obs_rest ->
            let first = set_path ["observable_values"] (`List (set_path path value obs :: obs_rest)) first in
            set_path ["imported_cyclic_observable_values"] (`List (first :: rest)) residual
        | [] -> residual
        end
    | [] -> residual
  in
  let mutate_first_shared_final_cell path value residual =
    match list_field "shared_scc_final_cells" residual with
    | first :: rest ->
        set_path ["shared_scc_final_cells"] (`List (set_path path value first :: rest)) residual
    | [] -> residual
  in
  let mutate_first_accepted_export path value residual =
    match list_field "linked_cycle_accepted_exports" residual with
    | first :: rest ->
        set_path ["linked_cycle_accepted_exports"] (`List (set_path path value first :: rest)) residual
    | [] -> residual
  in
  let cycle_source_negative_rejected =
    let path =
      Filename.concat !artifact_dir
        "negative/cycle_import_value_removed.out/abstract-speculate-residual-linking-pe.linked.json"
    in
    Sys.file_exists path &&
    not (recomputed_cycle_evidence_passes (Yojson.Safe.from_file path))
  in
  let remove_reverse_cycle_edge residual =
    match list_field "linked_cycle_topology" residual with
    | first :: rest ->
        let edges =
          list_field "edges" first
          |> List.filter (fun edge -> string_field "import_name" edge <> "cycle_a")
        in
        set_path ["linked_cycle_topology"] (`List (set_path ["edges"] (`List edges) first :: rest)) residual
    | [] -> residual
  in
  let mutate_first_topology_edge path value residual =
    match list_field "linked_cycle_topology" residual with
    | first :: rest ->
        begin match list_field "edges" first with
        | edge :: edge_rest ->
            let first = set_path ["edges"] (`List (set_path path value edge :: edge_rest)) first in
            set_path ["linked_cycle_topology"] (`List (first :: rest)) residual
        | [] -> residual
        end
    | [] -> residual
  in
  let dependency_only_schedule residual =
    set_path ["shared_scc_worklist_schedule"]
      (`List [`Assoc [
        "event", `String "enqueue";
        "reason", `String "dependency-only";
        "iteration", `Int 1;
        "equation_id", `String "legacy-shared-scc-relabel";
        "target", `String "legacy-shared-scc";
      ]])
      residual
  in
  let set_final_round_changed residual =
    let rounds = list_field "linked_cycle_rounds" residual in
    match List.rev rounds with
    | last_round :: rev_prefix ->
        let changed = last_round |> set_path ["changed"] (`Bool true) |> set_path ["changed_binding_count"] (`Int 1) in
        set_path ["linked_cycle_rounds"] (`List (List.rev (changed :: rev_prefix))) residual
    | [] -> residual
  in
  let set_final_round_bootstrap residual =
    let rounds = list_field "linked_cycle_rounds" residual in
    match List.rev rounds with
    | last_round :: rev_prefix ->
        let bindings = list_field "linked_environment" last_round in
        let changed_bindings =
          match bindings with
          | first :: rest -> set_path ["origin"] (`String "bootstrap-unknown") first :: rest
          | [] -> []
        in
        let changed = set_path ["linked_environment"] (`List changed_bindings) last_round in
        set_path ["linked_cycle_rounds"] (`List (List.rev (changed :: rev_prefix))) residual
    | [] -> residual
  in
  let negative_dir = Filename.concat !artifact_dir "negative" in
  let injected_shortcut_rejected =
    let injected = Filename.concat negative_dir "injected_premerge_shortcut.ml" in
    write_file injected "let shortcut = Real_sparrow_frontend.global_for_files []\n";
    let obligation = source_guard_obligation_for_paths "shortcut_false_case" [injected] [] in
    string_field "status" obligation = "fail"
  in
  [
    false_case "mismatched_return_or_effect_summary" return_false;
    false_case "missing_external_summary_v3" v3_missing_false;
    false_case "v1_compat_only_rejected" v3_compat_only_false;
    false_case "external_summary_schema_downgrade_rejected" v3_schema_false;
    false_case "external_summary_status_corruption_rejected" v3_status_false;
    false_case "external_summary_return_value_corruption_rejected" v3_return_value_false;
    false_case "typed_scalar_metadata_corruption_rejected" typed_scalar_metadata_false;
    false_case "typed_scalar_relation_protocol_id_corruption_rejected" typed_scalar_relation_false;
    false_case "missing_global_write_read_effect" global_false;
    false_case "missing_pointer_memory_effect" pointer_false;
    false_case "non_selected_itv_cell_mutation_fails_full_relation" non_selected_itv_cell_false;
    false_case "typed_itv_cell_metadata_mismatch_fails_full_relation" typed_cell_metadata_mismatch_false;
    false_case "ambiguous_provider_accepted_incorrectly"
      (log_contains (Filename.concat negative_dir "ambiguous_provider.log") "unsupported ambiguous semantic export mapping");
    false_case "invalid_mixed_role_propagation" mixed_false;
    false_case "premerge_implementation_shortcut" injected_shortcut_rejected;
    false_case "missing_oracle_artifact" missing_oracle_false;
    false_case "witness_identity_mismatch" witness_identity_false;
    false_case "missing_normalized_observation_provenance" provenance_false;
    false_case "cycle_missing_topology_rejected"
      (cycle_evidence_fails (set_path ["linked_cycle_topology"] (`List [])));
    false_case "cycle_scc_count_falsification_rejected"
      (cycle_evidence_fails (set_path ["linked_cycle_scc_count"] (`Int 999)));
    false_case "cycle_missing_reverse_edge_rejected"
      (cycle_evidence_fails remove_reverse_cycle_edge);
    false_case "cycle_missing_edge_provenance_rejected"
      (cycle_evidence_fails (mutate_first_topology_edge ["stable_evidence_id"] (`String "")));
    false_case "cycle_empty_rounds_rejected"
      (cycle_evidence_fails (set_path ["linked_cycle_rounds"] (`List [])));
    false_case "cycle_non_stable_final_round_rejected"
      (cycle_evidence_fails set_final_round_changed);
    false_case "cycle_bootstrap_binding_remaining_rejected"
      (cycle_evidence_fails (set_path ["linked_cycle_bootstrap_bindings_remaining"] (`Int 1)));
    false_case "cycle_final_bootstrap_origin_rejected"
      (cycle_evidence_fails set_final_round_bootstrap);
    false_case "cycle_overlay_only_rejected"
      (cycle_evidence_fails (set_path ["linked_overlay_only"] (`Bool true)));
    false_case "cycle_source_import_value_removed_rejected"
      cycle_source_negative_rejected;
    false_case "cycle_missing_imported_observable_rejected"
      (cycle_evidence_fails (set_path ["imported_cyclic_observable_values"] (`List [])));
    false_case "cycle_imported_value_imprecision_rejected"
      (cycle_evidence_fails (mutate_first_imported_value ["no_extra_imprecision"] (`Bool false)));
    false_case "cycle_missing_shared_scc_dependency_rejected"
      (cycle_evidence_fails (set_path ["shared_scc_dependencies"] (`List [])));
    false_case "cycle_zero_shared_scc_state_reads_rejected"
      (cycle_evidence_fails (set_path ["shared_scc_state_read_count"] (`Int 0)));
    false_case "cycle_missing_shared_scc_worklist_schedule_rejected"
      (cycle_evidence_fails (set_path ["shared_scc_worklist_schedule"] (`List [])));
    false_case "cycle_dependency_only_schedule_rejected"
      (cycle_evidence_fails dependency_only_schedule);
    false_case "cycle_shared_scc_final_cell_mismatch_rejected"
      (cycle_evidence_fails (mutate_first_shared_final_cell ["value"] (`Int 999)));
    false_case "cycle_observable_source_cell_mismatch_rejected"
      (cycle_evidence_fails (mutate_first_observable ["source_shared_scc_cell_id"] (`String "missing-cell")));
    false_case "cycle_shared_scc_relabel_without_provenance_rejected"
      (cycle_evidence_fails (mutate_first_imported_value ["stable_evidence_id"] (`String "")));
    false_case "cycle_bootstrap_derived_export_rejected"
      (cycle_evidence_fails (mutate_first_accepted_export ["derivation_source"] (`String "provider-stage2-output")));
    false_case "callgraph_scheduler_missing_evidence_rejected"
      (callgraph_scheduler_evidence_fails clear_scheduler_evidence);
    false_case "callgraph_scheduler_missing_edge_provenance_rejected"
      (callgraph_scheduler_evidence_fails scheduler_missing_required_edge_field);
    false_case "callgraph_scheduler_scc_count_mismatch_rejected"
      (callgraph_scheduler_evidence_fails scheduler_scc_count_mismatch);
    false_case "callgraph_scheduler_dependency_only_relabel_rejected"
      (callgraph_scheduler_evidence_fails scheduler_dependency_only_relabel);
    false_case "old_shared_scc_topology_relabelled_callgraph_backed_rejected"
      (callgraph_scheduler_evidence_fails old_topology_relabelled_without_provenance);
  ]

let () =
  Arg.parse
    ["--repo-root", Arg.Set_string repo_root, "repository root";
     "--artifact-dir", Arg.Set_string artifact_dir, "active artifact directory";
     "--report", Arg.Set_string report, "report path"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !artifact_dir = "" || !report = "" then failwith usage;
  let manifest_path = Filename.concat !artifact_dir "manifest.json" in
  expect (Sys.file_exists manifest_path) ("missing manifest: " ^ manifest_path);
  let manifest = Yojson.Safe.from_file manifest_path in
  expect (string_field "artifact_schema_status" manifest = "prototype-non-public")
    "suite manifest must mark schema prototype/non-public";
  expect (string_field "oracle_reference_kind" manifest = "premerge-linked-observer")
    "suite manifest must mark premerge observer as oracle/reference";
  let witnesses = list_field "witnesses" manifest in
  expect (List.length witnesses >= 5) "expected oracle suite witness coverage";
  let witness_reports = List.map witness_report witnesses in
  let all_obligations =
    witness_reports |> List.concat_map (fun w -> list_field "obligations" w)
  in
  let negative_cases = negative_cases witnesses in
  let required = [
    "return_value_matches_oracle";
    "global_write_read_matches_oracle";
    "pointer_memory_effect_matches_oracle";
    "provider_resolution_matches_oracle";
    "mixed_role_chain_matches_oracle";
    "no_premerge_implementation_shortcut";
    "cyclic_residual_fixpoint_evidence";
    "cyclic_imported_value_exact_singleton_parity";
    "callgraph_backed_scheduler_evidence";
  ] in
  let names = all_obligations |> List.map (string_field "name") in
  required |> List.iter (fun name -> expect (List.mem name names) ("missing obligation: " ^ name));
  expect (all_obligations |> List.for_all (fun o -> string_field "status" o = "pass"))
    "at least one proof obligation failed";
  expect (negative_cases |> List.for_all (fun n -> string_field "status" n = "pass"))
    "at least one negative case was not covered";
  let distinct_cycle_witness_present =
    witness_reports
    |> List.exists (fun w ->
         string_field "witness_id" w <> "cycle_topology" &&
         list_field "obligations" w
         |> List.exists (fun o -> string_field "name" o = "cyclic_residual_fixpoint_evidence" && string_field "status" o = "pass"))
  in
  expect distinct_cycle_witness_present "missing distinct cyclic witness beyond cycle_topology";
  let suite_pass = witness_reports |> List.for_all (fun w -> string_field "status" w = "pass") in
  expect suite_pass "at least one full-Itv semantic relation failed";
  expect (witness_reports |> List.for_all (fun w -> bool_field "full_itv_required_fields_present" w))
    "at least one full-Itv relation omitted a required compatibility field";
  let report_json = `Assoc [
    "schema_version", `String "abstract-speculate-residual-linking-oracle-suite/v1";
    "schema_status", `String "prototype-non-public";
    "suite_id", `String "abstract-speculate-residual-linking-oracle-suite";
    "suite_status", `String (if suite_pass then "pass" else "fail");
    "suite_pass_gate", `String "full_itv_semantic_relation.status for every witness plus proof obligations";
    "oracle_reference_kind", `String "premerge-linked-observer";
    "residual_linking_implementation_premerge_free", `Bool true;
    "witnesses", `List witness_reports;
    "full_itv_semantic_summary", `Assoc [
      "witness_count", `Int (List.length witness_reports);
      "relation", `String "full-sparrow-itv-semantic-relation";
      "pass_gate", `String "authoritative";
    ];
    "full_itv_semantic_relation", `List (List.map (fun w -> member "full_itv_semantic_relation" w) witness_reports);
    "residual_to_origin", `List (List.map (fun w -> member "residual_to_origin" w) witness_reports);
    "origin_to_residual", `List (List.map (fun w -> member "origin_to_residual" w) witness_reports);
    "diagnostics", `Assoc [
      "selected_observation_summary", `Assoc [
        "witness_count", `Int (List.length witness_reports);
        "relation", `String "selected-observation-equivalence";
        "pass_gate", `String "diagnostic-only";
      ];
      "selected_observation_relation", `List (List.map (fun w -> member "selected_observation_relation" (member "diagnostics" w)) witness_reports);
    ];
    "obligations", `List all_obligations;
    "negative_cases", `List negative_cases;
    "non_claims", `List [
      `String "no proof assistant mechanization";
      `String "no final artifact schema freeze";
      `String "no broad arbitrary-C coverage";
      `String "no Oct semantics";
      `String "no Taint semantics";
      `String "no arbitrary-C or whole-program semantic equivalence";
      `String "full Sparrow-Itv relation is witness-bounded to the oracle suite";
      `String "selected-observation evidence is diagnostic/compatibility only";
    ];
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report report_json;
  print_endline "abstract_speculate_residual_linking_oracle_suite_check: PASS"
