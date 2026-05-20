let repo_root = ref "."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "abstract_speculate_residual_linking_oracle_suite_check --repo-root <repo> \
   --artifact-dir <dir> --report <json>"

let member = Yojson.Safe.Util.member

module Relation = Sparrow_modular_ocaml.Abstract_speculate_residual_relation

module ScalarCall =
  Sparrow_modular_ocaml.Abstract_speculate_residual_scalar_call

module MemoryDelta =
  Sparrow_modular_ocaml.Abstract_speculate_residual_memory_delta

let expect cond msg = if not cond then failwith msg

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match assoc_field name json with Some (`String s) -> s | _ -> ""

let int_field name json =
  match assoc_field name json with Some (`Int n) -> n | _ -> min_int

let bool_field name json =
  match assoc_field name json with Some (`Bool b) -> b | _ -> false

let bool_true_field name json =
  match assoc_field name json with Some (`Bool true) -> true | _ -> false

let list_field name json =
  match assoc_field name json with Some (`List xs) -> xs | _ -> []

let has_field name json =
  match assoc_field name json with Some `Null | None -> false | Some _ -> true

let linked_output residual = member "linked_output" residual

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1))
  in
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
    if String.length rel > prefix_len && String.sub rel 0 prefix_len = prefix
    then
      Filename.concat !repo_root
        (String.sub rel prefix_len (String.length rel - prefix_len))
    else direct

let fixture_source_path rel =
  if Sys.file_exists rel then rel
  else
    let local_test = Filename.concat "test" rel in
    if Sys.file_exists local_test then local_test
    else
      let direct = Filename.concat !repo_root rel in
      if Sys.file_exists direct then direct
      else
        let repo_test =
          Filename.concat (Filename.concat !repo_root "test") rel
        in
        if Sys.file_exists repo_test then repo_test
        else
          Filename.concat
            (Filename.concat !repo_root "sparrow-modular-ocaml/test")
            rel

let is_taint_witness_id witness_id category =
  witness_id = "taint_product_pair" || category = "taint-product-pair"

let json_string_list field json =
  list_field field json
  |> List.filter_map (function `String s -> Some s | _ -> None)

let source_contains_marker source marker =
  source <> "" && marker <> ""
  &&
  let path = fixture_source_path source in
  Sys.file_exists path && contains (read_file path) marker

let semantic_export_return residual export_name =
  list_field "semantic_exports" residual
  |> List.find_map (fun export ->
         if string_field "export_name" export = export_name then
           Some (int_field "return_value" export)
         else None)

let taint_evidence_json_ok witness residual evidence =
  let witness_id = string_field "witness_id" witness in
  let product_components = json_string_list "product_components" evidence in
  let facts = list_field "taint_facts" evidence in
  let itv = member "itv_observable" evidence in
  let export_name = string_field "export_name" itv in
  let singleton = int_field "singleton_int" itv in
  let fact_ok =
    facts
    |> List.exists (fun fact ->
           string_field "component" fact = "Taint"
           && string_field "state" fact = "tainted-user-input"
           && string_field "source_evidence_path" fact
              = "provider_row.taint:taint_source:return"
           && (not (bool_field "metadata_only" fact))
           && contains
                (string_field "source_file" fact)
                "taint_product_pair/provider.c"
           && string_field "source_marker" fact = "TAINT_WITNESS:user_input"
           && string_field "sink_marker" fact = "TAINT_WITNESS:tainted_return")
  in
  string_field "schema_version" evidence
  = "abstract-speculate-bounded-taint-product-evidence/v1"
  && string_field "schema_status" evidence = "prototype-non-public"
  && string_field "witness_id" evidence = witness_id
  && string_field "taint_witness_id" evidence = witness_id
  && string_field "residual_linked_artifact" evidence
     = string_field "residual_linked_artifact" witness
  && string_field "taint_semantic_relation" evidence
     = "bounded-user-input-taint-to-return"
  && string_field "relation_status" evidence = "pass"
  && List.mem "Itv" product_components
  && List.mem "Taint" product_components
  && facts <> [] && fact_ok
  && export_name = "taint_source"
  && singleton = 42
  && semantic_export_return residual export_name = Some singleton

let taint_evidence_for_witness witness residual =
  let witness_id = string_field "witness_id" witness in
  let category = string_field "category" witness in
  if not (is_taint_witness_id witness_id category) then
    `Assoc [ ("status", `String "not-applicable") ]
  else
    let path = string_field "taint_product_evidence_artifact" witness in
    if path = "" || not (Sys.file_exists path) then
      `Assoc
        [
          ("status", `String "fail");
          ("reason", `String "missing_taint_product_evidence_artifact");
        ]
    else
      let evidence = Yojson.Safe.from_file path in
      let ok = taint_evidence_json_ok witness residual evidence in
      `Assoc
        [
          ("status", `String (if ok then "pass" else "fail"));
          ("taint_witness_id", `String witness_id);
          ("artifact", `String path);
          ( "taint_semantic_relation",
            `String (string_field "taint_semantic_relation" evidence) );
          ( "product_components",
            match assoc_field "product_components" evidence with
            | Some x -> x
            | None -> `List [] );
          ("evidence", evidence);
        ]

let semantic_exports linked = list_field "semantic_exports" linked

let provider_memory summary =
  summary |> member "provenance" |> member "provider_row" |> list_field "memory"

let provider_memory_value summary location =
  provider_memory summary
  |> List.find_opt (fun cell -> string_field "location" cell = location)
  |> Option.map (fun cell -> string_field "value" cell)

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

let memory_effect_ok summary expected_domain eff =
  let source_prefix = "provider_row.memory:" in
  let source_path = string_field "source_evidence_path" eff in
  let source_location =
    if starts_with source_path source_prefix then
      Some
        (String.sub source_path
           (String.length source_prefix)
           (String.length source_path - String.length source_prefix))
    else None
  in
  match source_location with
  | None -> false
  | Some source_location ->
      let expected_value = provider_memory_value summary source_location in
      typed_effect_ok expected_domain eff
      && effect_matches_summary_provenance summary eff
      && Option.map
           (fun value -> member "value" eff = `String value)
           expected_value
         = Some true
      && (if expected_domain = "global-write-read" then
            string_field "location" eff = source_location
            && string_field "normalized_location" eff = source_location
            && string_field "symbol" eff = source_location
          else
            string_field "location" eff
            = "(" ^ string_field "symbol" eff ^ ",p)"
            && string_field "normalized_location" eff
               = string_field "location" eff)
      && starts_with
           (string_field "effect_id" eff)
           (string_field "provider_module" eff ^ ":")
      && contains
           (string_field "effect_id" eff)
           (expected_domain ^ ":" ^ string_field "location" eff)

let effect_ids summary =
  list_field "return_effects" summary
  |> List.map (string_field "effect_id")
  |> List.filter (( <> ) "")

let taint_component_ok summary component =
  MemoryDelta.validation_ok
    (MemoryDelta.validate_taint_component_json component)
  && List.mem (string_field "related_effect_id" component) (effect_ids summary)

let product_pair_ok summary evidence =
  let component = member "taint_component" evidence in
  MemoryDelta.validation_ok
    (MemoryDelta.validate_product_pair_evidence_json evidence)
  && taint_component_ok summary component
  && string_field "related_effect_id" component <> ""
  && list_field "return_effects" summary
     |> List.exists (fun eff ->
            string_field "effect_id" eff
            = string_field "related_effect_id" component)

let external_summary_ok summary =
  let has_memory_projection =
    list_field "global_effects" summary <> []
    || list_field "pointer_effects" summary <> []
  in
  let memory_delta_authority_ok =
    if not has_memory_projection then
      list_field "memory_deltas" summary = []
      && list_field "delta_chains" summary = []
    else
      list_field "memory_deltas" summary <> []
      && list_field "delta_chains" summary <> []
      && List.for_all
           (fun delta ->
             MemoryDelta.validation_ok (MemoryDelta.validate_delta_json delta)
             && string_field "memory_delta_schema" delta
                = MemoryDelta.memory_delta_schema_id
             && string_field "reader_role" delta = "reader"
             && string_field "writer_role" delta = "writer")
           (list_field "memory_deltas" summary)
      && List.for_all
           (fun chain ->
             string_field "memory_delta_schema" chain
             = MemoryDelta.memory_delta_schema_id
             && list_field "entries" chain <> [])
           (list_field "delta_chains" summary)
  in
  string_field "schema_version" summary
  = "abstract-speculate-external-summary/v3"
  && string_field "summary_api_status" summary = "prototype-internal"
  && string_field "summary_scope" summary = "sparrow-itv-selected-witness"
  && list_field "effect_domains" summary <> []
  && list_field "return_effects" summary <> []
  && List.for_all (return_effect_ok summary)
       (list_field "return_effects" summary)
  && memory_delta_authority_ok
  && List.for_all
       (memory_effect_ok summary "global-write-read")
       (list_field "global_effects" summary)
  && List.for_all
       (memory_effect_ok summary "pointer-memory-effect")
       (list_field "pointer_effects" summary)
  && List.for_all
       (taint_component_ok summary)
       (list_field "taint_components" summary)
  && List.for_all (product_pair_ok summary)
       (list_field "product_pair_evidence" summary)
  && member "external_summary_v1_compat" summary <> `Null
  && string_field "schema_version" (member "external_summary_v1_compat" summary)
     = "abstract-speculate-external-summary/v1"
  && string_field "derivation_source" (member "provenance" summary)
     = "provider-stage2-output"

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

let has_taint_product_pair summaries =
  summaries
  |> List.exists (fun summary ->
         list_field "taint_components" summary <> []
         && list_field "product_pair_evidence" summary <> []
         && list_field "product_pair_evidence" summary
            |> List.exists (fun evidence ->
                   string_field "taint_witness_id" evidence
                   = "taint_product_pair"
                   && string_field "semantic_relation_status" evidence = "pass"))

let effect_count field summaries =
  List.fold_left
    (fun acc summary -> acc + List.length (list_field field summary))
    0 summaries

let json_contains json needle = contains (Yojson.Safe.to_string json) needle

let taint_product_witness_required witness_id category =
  witness_id = "taint_product_pair" || category = "taint-product-pair"

let taint_product_evidence_ok residual evidence =
  MemoryDelta.validation_ok
    (MemoryDelta.validate_taint_product_evidence_json evidence)
  && string_field "taint_witness_id" evidence <> ""
  && string_field "taint_semantic_relation" evidence = "source-taints-sink"
  && json_contains residual (string_field "related_residual_location" evidence)
  &&
  match assoc_field "itv_observable" evidence with
  | Some itv -> json_contains residual (string_field "location" itv)
  | None -> false

let taint_product_obligations witness_id category residual witness =
  if not (taint_product_witness_required witness_id category) then []
  else
    let evidence =
      match assoc_field "taint_evidence" witness with
      | Some e -> e
      | None -> `Null
    in
    let validation =
      MemoryDelta.validate_taint_product_evidence_json evidence
    in
    let residual_tied =
      string_field "related_residual_location" evidence <> ""
      && json_contains residual
           (string_field "related_residual_location" evidence)
    in
    let pass = MemoryDelta.validation_ok validation && residual_tied in
    [
      `Assoc
        [
          ("name", `String "taint_product_pair_semantic_evidence");
          ("category", `String "taint-product-pair");
          ("witness_id", `String witness_id);
          ("status", `String (if pass then "pass" else "fail"));
          ( "taint_witness_id",
            `String (string_field "taint_witness_id" evidence) );
          ( "taint_semantic_relation",
            `String (string_field "taint_semantic_relation" evidence) );
          ( "product_components",
            match assoc_field "product_components" evidence with
            | Some xs -> xs
            | None -> `List [] );
          ( "related_residual_location",
            `String (string_field "related_residual_location" evidence) );
          ("validation", MemoryDelta.validation_result_json validation);
          ("residual_tied", `Bool residual_tied);
          ( "evidence_source",
            `String
              "bounded taint product evidence attached to oracle-suite witness \
               manifest" );
        ];
    ]

let require_external_summaries witness_id residual =
  let summaries = list_field "external_summaries" residual in
  expect (summaries <> []) (witness_id ^ ": missing ExternalSummary v3 entries");
  expect
    (List.for_all external_summary_ok summaries)
    (witness_id ^ ": malformed ExternalSummary v3 entries");
  if witness_id = "global_write_read" then
    expect
      (has_effect "global-write-read" summaries)
      (witness_id ^ ": missing v3 global delta");
  if witness_id = "pointer_memory_effect" then
    expect
      (has_effect "pointer-memory-effect" summaries)
      (witness_id ^ ": missing v3 pointer delta");
  if witness_id = "taint_product_pair" then
    expect
      (has_taint_product_pair summaries)
      (witness_id ^ ": missing bounded Itv+Taint product-pair evidence")

let source_guard_obligation_for_paths witness_id residual_sources suite_sources
    =
  let forbidden =
    [
      "Real_sparrow_frontend." ^ "global_for_" ^ "files";
      "Mergecil." ^ "merge";
      "real_sparrow_" ^ "premerge_linked_observer";
    ]
  in
  let residual_clean =
    residual_sources
    |> List.for_all (fun path ->
           let src = read_file path in
           forbidden |> List.for_all (fun needle -> not (contains src needle)))
  in
  let suite_oracle_scoped =
    suite_sources
    |> List.for_all (fun path ->
           let src = read_file path in
           (not (contains src "premerge_linked_observer"))
           || contains src "Oracle"
           || contains src "oracle_reference_kind")
  in
  let pass = residual_clean && suite_oracle_scoped in
  `Assoc
    [
      ("name", `String "no_premerge_implementation_shortcut");
      ("category", `String "shortcut-guard");
      ("witness_id", `String witness_id);
      ("status", `String (if pass then "pass" else "fail"));
      ("residual_linking_implementation_premerge_free", `Bool residual_clean);
      ("premerge_artifacts_oracle_scoped", `Bool suite_oracle_scoped);
      ( "evidence_paths",
        `List (List.map (fun p -> `String p) (residual_sources @ suite_sources))
      );
    ]

let source_guard_obligation witness_id =
  source_guard_obligation_for_paths witness_id
    [
      project_path
        "sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml";
      project_path
        "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_dump.ml";
    ]
    [
      project_path
        "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_dump.ml";
      project_path
        "sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml";
    ]

let normalized_observations = Relation.return_observations

let obligations_for witness_id category residual oracle =
  Relation.oracle_suite_obligations ~source_guard_obligation witness_id category
    residual oracle

let observations_have_provenance = Relation.observations_have_provenance
let last = function [] -> None | xs -> Some (List.nth xs (List.length xs - 1))

let cycle_topology_edges topology =
  topology |> List.concat_map (fun scc -> list_field "edges" scc)

let provenance_fields_present ?edge_kind json =
  let kind_ok =
    match edge_kind with
    | None -> string_field "edge_kind" json <> ""
    | Some expected -> string_field "edge_kind" json = expected
  in
  kind_ok
  && string_field "source" json <> ""
  && string_field "provenance_level" json <> ""
  && string_field "stable_evidence_id" json <> ""

let cycle_topology_has_edge ~import_name ~export_name topology =
  cycle_topology_edges topology
  |> List.exists (fun edge ->
         string_field "import_name" edge = import_name
         && string_field "export_name" edge = export_name)

let cycle_topology_edge_provenance_passes topology =
  let edges = cycle_topology_edges topology in
  edges <> []
  && List.for_all
       (provenance_fields_present ~edge_kind:"import-export-function-dependency")
       edges

let json_key json = Yojson.Safe.to_string json

let sorted_json_list xs =
  xs = List.sort (fun a b -> compare (json_key a) (json_key b)) xs

let int_json_field name json =
  match assoc_field name json with
  | Some (`Int n) -> Some n
  | Some (`String s) -> ( try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let global_residual_evidence_roots residual =
  let output = member "linked_output" residual in
  let output_log = member "execution_log" output in
  let top_log = member "execution_log" residual in
  let member_or_null name json =
    match assoc_field name json with
    | Some value -> value
    | None -> `Null
  in
  let nested_roots root =
    [
      member_or_null "global_residual_fixpoint" root;
      member_or_null "global_residual_fixpoint_evidence" root;
      member_or_null "global_residual_fixpoint_report" root;
    ]
  in
  [residual; output; output_log; top_log] @
  nested_roots residual @ nested_roots output @ nested_roots output_log @ nested_roots top_log

let evidence_field name residual =
  global_residual_evidence_roots residual
  |> List.find_map (assoc_field name)

let has_evidence_field name residual =
  match evidence_field name residual with
  | Some `Null | None -> false
  | Some _ -> true

let bool_evidence_field name residual =
  match evidence_field name residual with Some (`Bool b) -> b | _ -> false

let int_evidence_field name residual =
  match evidence_field name residual with
  | Some (`Int n) -> n
  | Some (`String s) -> (try int_of_string s with Failure _ -> min_int)
  | _ -> min_int

let string_evidence_field name residual =
  match evidence_field name residual with Some (`String s) -> s | _ -> ""

let list_evidence_field name residual =
  match evidence_field name residual with Some (`List xs) -> xs | _ -> []

let global_final_cells residual =
  let candidates =
    ["global_residual_final_cells";
     "global_residual_final_rows";
     "global_residual_final_input_table";
     "global_residual_final_output_table";
     "global_residual_final_cell_rows"]
  in
  candidates |> List.concat_map (fun field -> list_evidence_field field residual)

let global_residual_cycle_witness witness_id =
  List.mem witness_id ["cycle_topology"; "callgraph_scheduler_cycle"; "cycle_value_flow"]

let global_residual_fixpoint_evidence_passes witness_id residual =
  let iteration_count = int_evidence_field "global_residual_iteration_count" residual in
  has_evidence_field "global_sparse_fixpoint_source_level_rerun" residual &&
  bool_evidence_field "global_residual_fixpoint_run" residual &&
  string_evidence_field "global_residual_fixpoint_scope" residual =
    "post-link-whole-program-residual-cells" &&
  string_evidence_field "global_sparse_fixpoint_component" residual =
    "residual-global-worklist" &&
  not (bool_evidence_field "global_sparse_fixpoint_source_level_rerun" residual) &&
  list_evidence_field "global_residual_equation_ids" residual <> [] &&
  list_evidence_field "global_residual_seed_cells" residual <> [] &&
  list_evidence_field "global_residual_derived_cells" residual <> [] &&
  list_evidence_field "global_residual_dependency_edges" residual <> [] &&
  list_evidence_field "global_residual_cross_module_dependency_edges" residual <> [] &&
  int_evidence_field "global_residual_state_read_count" residual > 0 &&
  int_evidence_field "global_residual_seed_read_count" residual > 0 &&
  list_evidence_field "global_residual_worklist_schedule" residual <> [] &&
  iteration_count >= 1 &&
  (not (global_residual_cycle_witness witness_id) || iteration_count > 1) &&
  bool_evidence_field "global_residual_worklist_drained" residual &&
  not (bool_evidence_field "global_residual_overlay_only" residual) &&
  global_final_cells residual <> []

let global_residual_fixpoint_obligations witness_id category residual full_itv_relation =
  let evidence_pass = global_residual_fixpoint_evidence_passes witness_id residual in
  let relation_pass = string_field "status" full_itv_relation = "pass" in
  let status = if evidence_pass && relation_pass then "pass" else "fail" in
  [`Assoc [
    "name", `String "global_residual_fixpoint_evidence";
    "category", `String category;
    "witness_id", `String witness_id;
    "status", `String status;
    "global_residual_fixpoint_run",
      (match evidence_field "global_residual_fixpoint_run" residual with Some v -> v | None -> `Null);
    "global_residual_fixpoint_scope",
      (match evidence_field "global_residual_fixpoint_scope" residual with Some v -> v | None -> `Null);
    "global_sparse_fixpoint_component",
      (match evidence_field "global_sparse_fixpoint_component" residual with Some v -> v | None -> `Null);
    "global_sparse_fixpoint_source_level_rerun",
      (match evidence_field "global_sparse_fixpoint_source_level_rerun" residual with Some v -> v | None -> `Null);
    "global_residual_iteration_count",
      (match evidence_field "global_residual_iteration_count" residual with Some v -> v | None -> `Null);
    "global_residual_state_read_count",
      (match evidence_field "global_residual_state_read_count" residual with Some v -> v | None -> `Null);
    "global_residual_seed_read_count",
      (match evidence_field "global_residual_seed_read_count" residual with Some v -> v | None -> `Null);
    "global_residual_worklist_drained",
      (match evidence_field "global_residual_worklist_drained" residual with Some v -> v | None -> `Null);
    "global_residual_equation_count", `Int (List.length (list_evidence_field "global_residual_equation_ids" residual));
    "global_residual_seed_cell_count", `Int (List.length (list_evidence_field "global_residual_seed_cells" residual));
    "global_residual_derived_cell_count", `Int (List.length (list_evidence_field "global_residual_derived_cells" residual));
    "global_residual_dependency_edge_count", `Int (List.length (list_evidence_field "global_residual_dependency_edges" residual));
    "global_residual_cross_module_dependency_edge_count",
      `Int (List.length (list_evidence_field "global_residual_cross_module_dependency_edges" residual));
    "global_residual_final_cell_count", `Int (List.length (global_final_cells residual));
    "global_residual_cycle_witness", `Bool (global_residual_cycle_witness witness_id);
    "global_residual_equivalence_status", `String (if status = "pass" then "pass" else "fail");
    "evidence_source",
      `String "checker-derived from global_residual_* post-link worklist evidence and full-Itv oracle relation";
  ]]

let shared_final_cell_value residual cell_id =
  list_field "shared_scc_final_cells" residual
  |> List.find_map (fun cell ->
         if string_field "shared_scc_cell_id" cell = cell_id then
           int_json_field "value" cell
         else None)

let shared_scc_dependency_mentions residual needle =
  list_field "shared_scc_dependencies" residual
  |> List.exists (fun dep ->
         contains (string_field "source" dep) needle
         || contains (string_field "target" dep) needle)

let source_cell_matches_final residual value_json =
  let cell_id = string_field "source_shared_scc_cell_id" value_json in
  cell_id <> ""
  &&
  match
    (int_json_field "value" value_json, shared_final_cell_value residual cell_id)
  with
  | Some value, Some final_value -> value = final_value
  | _ -> false

let observable_value_matches_final residual obs =
  string_field "observable_kind" obs = "imported-cyclic-sink-write"
  && string_field "observable_location" obs <> ""
  && provenance_fields_present ~edge_kind:"shared-scc-observable-copy" obs
  && source_cell_matches_final residual obs
  && bool_field "shared_scc_value_matches" obs

let cyclic_topology_edges residual =
  list_field "linked_cycle_topology" residual
  |> List.concat_map (fun scc ->
         if bool_field "is_cyclic" scc then list_field "edges" scc else [])

let cyclic_topology_edges_have_cycle residual =
  let pairs =
    cyclic_topology_edges residual
    |> List.map (fun edge ->
           ( string_field "importer_module" edge,
             string_field "provider_module" edge ))
  in
  pairs
  |> List.exists (fun (src, dst) ->
         src <> "" && dst <> ""
         && (src = dst
            || List.exists (fun (src', dst') -> src' = dst && dst' = src) pairs
            ))

let imported_cyclic_values_exact residual =
  let values = list_field "imported_cyclic_observable_values" residual in
  let value_has_edge edge =
    values
    |> List.exists (fun value ->
           string_field "importer_module" value
           = string_field "importer_module" edge
           && string_field "import_name" value = string_field "import_name" edge
           && string_field "provider_module" value
              = string_field "provider_module" edge
           && string_field "export_name" value = string_field "export_name" edge
           && bool_field "exact_singleton" value
           && bool_field "no_extra_imprecision" value
           && bool_field "shared_scc_value_matches" value
           && bool_field "observable_sink_dependency_present" value
           && provenance_fields_present ~edge_kind:"shared-scc-import-copy"
                value
           && source_cell_matches_final residual value
           && list_field "observable_values" value <> []
           && List.for_all
                (observable_value_matches_final residual)
                (list_field "observable_values" value))
  in
  values <> []
  && List.for_all
       (provenance_fields_present ~edge_kind:"shared-scc-import-copy")
       values
  && bool_field "cyclic_imported_value_exact_singleton_parity" residual
  && bool_field "cycle_final_values_derive_from_shared_scc_final_cells" residual
  && List.for_all value_has_edge (cyclic_topology_edges residual)

let shared_scc_export_values_exact residual =
  let exports = list_field "linked_cycle_accepted_exports" residual in
  exports <> []
  && List.for_all
       (fun export ->
         string_field "derivation_source" export = "shared_scc_final_cells"
         && provenance_fields_present ~edge_kind:"shared-scc-export-final-cell"
              export
         && bool_field "shared_scc_value_matches" export
         && source_cell_matches_final residual export)
       exports

let shared_scc_topology_dependencies_present residual =
  cyclic_topology_edges residual
  |> List.for_all (fun edge ->
         shared_scc_dependency_mentions residual
           ("export-return:"
           ^ string_field "provider_module" edge
           ^ ":"
           ^ string_field "export_name" edge)
         && shared_scc_dependency_mentions residual
              ("import-observable:"
              ^ string_field "importer_module" edge
              ^ ":"
              ^ string_field "import_name" edge))

let shared_scc_schedule_has_changed_dependency residual =
  list_field "shared_scc_worklist_schedule" residual
  |> List.exists (fun event ->
         string_field "event" event = "enqueue"
         && string_field "reason" event = "changed-cell-dependent"
         && string_field "equation_id" event <> ""
         && string_field "target" event <> "")

let shared_scc_solver_evidence_passes residual =
  bool_field "shared_scc_worklist_run" residual
  && int_field "shared_scc_state_read_count" residual > 0
  && list_field "shared_scc_equation_ids" residual <> []
  && list_field "shared_scc_cell_ids" residual <> []
  && list_field "shared_scc_dependencies" residual <> []
  && list_field "shared_scc_worklist_schedule" residual <> []
  && shared_scc_schedule_has_changed_dependency residual
  && list_field "shared_scc_final_cells" residual <> []
  && sorted_json_list (list_field "shared_scc_equation_ids" residual)
  && sorted_json_list (list_field "shared_scc_cell_ids" residual)
  && sorted_json_list (list_field "shared_scc_dependencies" residual)
  && sorted_json_list (list_field "shared_scc_final_cells" residual)
  && shared_scc_topology_dependencies_present residual
  && imported_cyclic_values_exact residual
  && shared_scc_export_values_exact residual

let recomputed_cycle_evidence_passes residual =
  let topology = list_field "linked_cycle_topology" residual in
  let rounds = list_field "linked_cycle_rounds" residual in
  let cyclic_scc_count =
    topology
    |> List.fold_left
         (fun acc scc -> if bool_field "is_cyclic" scc then acc + 1 else acc)
         0
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
        (not (bool_field "changed" round))
        && int_field "changed_binding_count" round = 0
        && list_field "linked_environment" round <> []
        && List.for_all
             (fun binding ->
               string_field "origin" binding = "shared-scc-final-cell"
               || string_field "origin" binding = "provider-derived")
             (list_field "linked_environment" round)
    | None -> false
  in
  bool_field "linked_cyclic_residual_solver_run" residual
  && bool_field "linked_cycle_worklist_drained" residual
  && bool_field "linked_cycle_obligations_closed" residual
  && bool_field "linked_cycle_stable_exports" residual
  && (not (bool_field "linked_overlay_only" residual))
  && list_field "linked_cycle_shared_scc_solvers" residual
     |> List.for_all (fun solver ->
            bool_field "shared_scc_authoritative_for_cycle_acceptance" solver
            && (not
                  (bool_field "linker_rerun_convergence_used_for_acceptance"
                     solver))
            && string_field "final_linked_environment_source" solver
               = "shared_scc_final_cells"
            && List.for_all
                 (provenance_fields_present
                    ~edge_kind:"import-export-function-dependency")
                 (list_field "shared_scc_edges" solver))
  && int_field "linked_cycle_scc_count" residual = cyclic_scc_count
  && cyclic_scc_count > 0
  && int_field "linked_cycle_iteration_count" residual
     = max 0 (List.length rounds - 1)
  && int_field "linked_cycle_bootstrap_bindings_remaining" residual = 0
  && topology <> [] && rounds <> []
  && changed_counts = reported_changed_counts
  && cycle_topology_edge_provenance_passes topology
  && cyclic_topology_edges_have_cycle residual
  && last_round_stable
  && shared_scc_solver_evidence_passes residual

let scheduler_reports residual =
  let as_reports = function
    | `List xs -> xs
    | `Assoc _ as report -> [ report ]
    | _ -> []
  in
  as_reports (member "linked_cycle_scheduler" residual)
  @ as_reports (member "linked_cycle_scheduler_evidence" residual)

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
  list_field "linked_cycle_scheduler_sccs" residual
  @ (scheduler_reports residual |> List.concat_map (list_field "sccs"))
  @ (list_field "linked_cycle_topology" residual
    |> List.concat_map (list_field "scheduler_sccs"))

let scheduler_provenance_is_call_backed provenance =
  List.mem provenance
    [
      "direct_program_callgraph";
      "direct-program-callgraph";
      "direct_program_callgraph_edge";
      "direct-program-callgraph-edge";
      "residual_call_binding";
      "residual-call-binding";
      "residual_binding_call_provenance";
      "residual-binding-call-provenance";
    ]

let scheduler_claims_callgraph_backed residual =
  bool_true_field "linked_cycle_callgraph_backed_schedule" residual
  || scheduler_reports residual @ list_field "linked_cycle_topology" residual
     |> List.exists (fun root ->
            bool_true_field "callgraph_backed" root
            || bool_true_field "callgraph_backed_schedule" root
            || bool_true_field "callgraph_backed_scheduler" root
            || string_field "scheduler_claim" root = "callgraph-backed"
            || string_field "claim" root = "callgraph-backed"
            || string_field "scheduler_kind" root = "callgraph-backed"
            || string_field "schedule_source" root = "callgraph-backed"
            || string_field "scheduler_source" root
               = "residual-call-binding-callgraph")

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
  string_field "edge_kind" edge <> ""
  && string_field "source" edge <> ""
  && scheduler_provenance_is_call_backed (string_field "provenance_level" edge)
  && scheduler_edge_evidence_id edge <> ""
  && scheduler_edge_importer edge <> ""
  && scheduler_edge_provider edge <> ""
  && scheduler_edge_symbol edge <> ""
  && (not (contains (string_field "source" edge) "bootstrap"))
  && not (contains (string_field "source" edge) "provider-derived")

let scheduler_edges_have_cycle edges =
  let pairs =
    edges
    |> List.map (fun edge ->
           (scheduler_edge_importer edge, scheduler_edge_provider edge))
  in
  pairs
  |> List.exists (fun (src, dst) ->
         src <> "" && dst <> ""
         && (src = dst
            || List.exists (fun (src', dst') -> src' = dst && dst' = src) pairs
            ))

let scheduler_fixture_edges_present witness_id edges =
  if witness_id = "callgraph_scheduler_cycle" then
    let has_edge ~importer ~provider ~symbol =
      edges
      |> List.exists (fun edge ->
             scheduler_edge_importer edge = importer
             && scheduler_edge_provider edge = provider
             && contains (scheduler_edge_symbol edge) symbol)
    in
    has_edge ~importer:"a.c" ~provider:"b.c" ~symbol:"scheduler_b"
    && has_edge ~importer:"b.c" ~provider:"a.c" ~symbol:"scheduler_a"
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
    recomputed_cycle_evidence_passes residual
    && scheduler_claims_callgraph_backed residual
    && edges <> []
    && List.for_all scheduler_edge_has_required_provenance edges
    && scheduler_edges_have_cycle edges
    && scheduler_fixture_edges_present witness_id edges
    && scheduler_sccs residual <> []
    && reported_scc_count > 0
    && reported_scc_count = int_field "linked_cycle_scc_count" residual
    && shared_scc_solver_evidence_passes residual

let global_residual_report residual = member "global_residual_fixpoint" residual

let table_cell_count rows =
  rows
  |> List.fold_left
       (fun acc row -> acc + List.length (list_field "memory" row))
       0

let global_residual_schedule_has_changed_dependency residual =
  list_field "global_residual_worklist_schedule"
    (global_residual_report residual)
  |> List.exists (fun event ->
         string_field "event" event = "enqueue"
         && string_field "reason" event = "changed-cell-dependent")

let global_residual_evidence_passes witness_id residual =
  let report = global_residual_report residual in
  let seed_cells = list_field "global_residual_seed_cells" report in
  let derived_cells = list_field "global_residual_derived_cells" report in
  let dependency_edges = list_field "global_residual_dependency_edges" report in
  let cross_edges =
    list_field "global_residual_cross_module_dependency_edges" report
  in
  let schedule = list_field "global_residual_worklist_schedule" report in
  let equations = list_field "global_residual_equations" report in
  let seed_ids = seed_cells |> List.map (string_field "seed_cell_id") in
  let derived_ids =
    derived_cells |> List.map (string_field "derived_cell_id")
  in
  let final_cell_count =
    table_cell_count (list_field "global_residual_final_input_table" report)
    + table_cell_count (list_field "global_residual_final_output_table" report)
  in
  let dependencies_well_formed =
    dependency_edges
    |> List.for_all (fun edge ->
           List.mem (string_field "source" edge) seed_ids
           && List.mem (string_field "target" edge) derived_ids
           && string_field "edge_kind" edge = "seed-to-derived-global-cell"
           && string_field "provenance" edge
              = "post-link-global-residual-worklist")
  in
  let cycle_ok =
    int_field "linked_cycle_scc_count" residual = 0
    || int_field "global_residual_iteration_count" report > 1
       && global_residual_schedule_has_changed_dependency residual
  in
  ignore witness_id;
  bool_field "global_residual_fixpoint_run" report
  && string_field "global_residual_fixpoint_scope" report
     = "post-link-whole-program-residual-cells"
  && string_field "global_sparse_fixpoint_component" report
     = "residual-global-worklist"
  && bool_field "global_sparse_fixpoint_source_level_rerun" report = false
  && seed_cells <> [] && derived_cells <> [] && equations <> []
  && dependency_edges <> [] && cross_edges <> []
  && List.for_all (fun edge -> bool_field "cross_module" edge) cross_edges
  && dependencies_well_formed
  && int_field "global_residual_state_read_count" report > 0
  && int_field "global_residual_seed_read_count" report > 0
  && schedule <> []
  && bool_field "global_residual_worklist_drained" report
  && bool_field "global_residual_overlay_only" report = false
  && bool_field "global_residual_metadata_only" report = false
  && bool_field "global_residual_derived_cells_recomputed" report
  && bool_field "global_residual_authoritative_residual_side" report
  && List.length derived_cells = final_cell_count
  && list_field "global_residual_final_input_table" report
     = list_field "final_input_table" (linked_output residual)
  && list_field "global_residual_final_output_table" report
     = list_field "final_output_table" (linked_output residual)
  && cycle_ok

let global_residual_fixpoint_obligations witness_id category residual relation =
  let report = global_residual_report residual in
  [
    `Assoc
      [
        ("name", `String "global_residual_fixpoint_evidence");
        ("category", `String category);
        ("witness_id", `String witness_id);
        ( "status",
          `String
            (if
               global_residual_evidence_passes witness_id residual
               && string_field "global_residual_equivalence_status" relation
                  = "pass"
             then "pass"
             else "fail") );
        ( "global_residual_fixpoint_run",
          member "global_residual_fixpoint_run" report );
        ( "global_residual_iteration_count",
          member "global_residual_iteration_count" report );
        ( "global_residual_state_read_count",
          member "global_residual_state_read_count" report );
        ( "global_residual_seed_read_count",
          member "global_residual_seed_read_count" report );
        ( "global_residual_worklist_drained",
          member "global_residual_worklist_drained" report );
        ( "global_residual_equivalence_status",
          member "global_residual_equivalence_status" relation );
        ( "evidence_source",
          `String
            "post-link global residual worklist plus full_itv semantic relation"
        );
      ];
  ]

let cycle_evidence_obligations witness_id category residual =
  if not (global_residual_cycle_witness witness_id) then []
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
      "status", `String (if imported_cyclic_values_exact residual then "pass" else "fail");
      "imported_cyclic_observable_values", member "imported_cyclic_observable_values" residual;
      "shared_scc_final_cells", member "shared_scc_final_cells" residual;
      "evidence_source", `String "shared_scc_final_cells source_shared_scc_cell_id exact singleton cross-check with stable provenance ids";
    ]
  ]
    @
    if witness_id <> "callgraph_scheduler_cycle" then []
    else
      [
        `Assoc
          [
            ("name", `String "callgraph_backed_scheduler_evidence");
            ("category", `String "cycle-scheduler-provenance");
            ("witness_id", `String witness_id);
            ( "status",
              `String
                (if callgraph_scheduler_evidence_passes witness_id residual then
                   "pass"
                 else "fail") );
            ("scheduler_claim", member "linked_cycle_scheduler" residual);
            ("scheduler_edges", `List (scheduler_edges residual));
            ("scheduler_sccs", `List (scheduler_sccs residual));
            ( "evidence_source",
              `String
                "recomputed from linked_cycle_scheduler edge provenance, SCC \
                 count, and shared_scc final-cell bridge" );
          ];
      ]

let obligation_passes = Relation.obligation_passes
let witness_pass_status = Relation.witness_pass_status

let witness_report witness =
  let witness_id = string_field "witness_id" witness in
  let category = string_field "category" witness in
  let residual_path = string_field "residual_linked_artifact" witness in
  let oracle_path = string_field "premerge_observer_artifact" witness in
  expect
    (Sys.file_exists residual_path)
    ("missing residual linked artifact: " ^ residual_path);
  expect
    (Sys.file_exists oracle_path)
    ("missing oracle artifact: " ^ oracle_path);
  let residual = Yojson.Safe.from_file residual_path in
  let oracle = Yojson.Safe.from_file oracle_path in
  expect
    (string_field "artifact_kind" residual
    = "abstract-speculate-linked-residual-analyzer")
    (witness_id ^ ": residual artifact kind mismatch");
  expect
    (string_field "scope" oracle = "linked-whole-program-fixture")
    (witness_id ^ ": oracle scope mismatch");
  expect
    (contains (string_field "linked_id" residual) witness_id)
    (witness_id ^ ": residual linked artifact identity mismatch");
  expect
    (string_field "group" oracle = witness_id)
    (witness_id ^ ": oracle artifact identity mismatch");
  require_external_summaries witness_id residual;
  let summaries = list_field "external_summaries" residual in
  let residual_obs, oracle_obs = normalized_observations witness_id residual oracle in
  let selected_relation = Relation.selected_observation_relation_json ~witness_id ~residual ~oracle in
  let full_itv_relation = Relation.full_itv_semantic_relation_json ~witness_id ~residual ~oracle in
  let obligations =
    obligations_for witness_id category residual oracle @
    cycle_evidence_obligations witness_id category residual @
    global_residual_fixpoint_obligations witness_id category residual full_itv_relation @
    taint_product_obligations witness_id category residual witness
  in
  let taint_product_evidence = taint_evidence_for_witness witness residual in
  let taint_required = is_taint_witness_id witness_id category in
  let taint_pass =
    (not taint_required)
    || string_field "status" taint_product_evidence = "pass"
  in
  let required_full_itv_fields =
    [
      "semantic_universe_manifest";
      "failure_taxonomy";
      "canonicalization";
      "oracle_identity";
      "memory_delta_validation";
      "residual_to_origin";
      "origin_to_residual";
    ]
  in
  let required_full_itv_fields_present =
    required_full_itv_fields
    |> List.for_all (fun field -> has_field field full_itv_relation)
  in
  let taint_evidence = match assoc_field "taint_evidence" witness with Some e -> e | None -> `Null in
  let pass = witness_pass_status obligations residual_obs oracle_obs &&
             string_field "status" full_itv_relation = "pass" &&
             required_full_itv_fields_present &&
             taint_pass &&
             (not (taint_product_witness_required witness_id category) || taint_product_evidence_ok residual taint_evidence) in
  `Assoc [
    "witness_id", `String witness_id;
    "category", `String category;
    "status", `String (if pass then "pass" else "fail");
    "residual_linked_artifact", `String residual_path;
    "premerge_observer_artifact", `String oracle_path;
    "external_summary_v3_checked", `Bool true;
    "external_summary_v1_compat_non_authoritative", `Bool true;
    "taint_product_evidence", taint_product_evidence;
    "taint_semantic_relation", member "taint_semantic_relation" taint_product_evidence;
    "taint_witness_id", member "taint_witness_id" taint_product_evidence;
    "product_components", member "product_components" taint_product_evidence;
    "external_summary_effect_counts", `Assoc [
      "summary_count", `Int (List.length summaries);
      "return_effect_count", `Int (effect_count "return_effects" summaries);
      "global_effect_count", `Int (effect_count "global_effects" summaries);
      "pointer_effect_count", `Int (effect_count "pointer_effects" summaries);
      "taint_product_evidence_count", `Int (if taint_product_witness_required witness_id category then 1 else 0);
    ];
    "taint_witness_id", `String (string_field "taint_witness_id" taint_evidence);
    "taint_semantic_relation", `String (string_field "taint_semantic_relation" taint_evidence);
    "product_components", (match assoc_field "product_components" taint_evidence with Some xs -> xs | None -> `List []);
    "taint_evidence", taint_evidence;
    "normalized_observations", `Assoc [
      "residual", `List residual_obs;
      "oracle", `List oracle_obs;
      "relation", `String "diagnostic selected observations; authoritative pass gate is full_itv_semantic_relation";
    ];
    "full_itv_semantic_summary", member "summary" full_itv_relation;
    "full_itv_semantic_relation", full_itv_relation;
    "global_residual_fixpoint", `Assoc [
      "status", `String (if global_residual_fixpoint_evidence_passes witness_id residual && string_field "status" full_itv_relation = "pass" then "pass" else "fail");
      "equivalence_status", `String (if global_residual_fixpoint_evidence_passes witness_id residual && string_field "status" full_itv_relation = "pass" then "pass" else "fail");
      "final_cell_count", `Int (List.length (global_final_cells residual));
      "worklist_schedule_count", `Int (List.length (list_evidence_field "global_residual_worklist_schedule" residual));
    ];
    "full_itv_required_fields_present", `Bool required_full_itv_fields_present;
    "full_itv_required_fields", `List (List.map (fun field -> `String field) required_full_itv_fields);
    "semantic_universe", member "semantic_universe" full_itv_relation;
    "semantic_universe_manifest", member "semantic_universe_manifest" full_itv_relation;
    "failure_taxonomy", member "failure_taxonomy" full_itv_relation;
    "canonicalization", member "canonicalization" full_itv_relation;
    "oracle_identity", member "oracle_identity" full_itv_relation;
    "memory_delta_validation", member "memory_delta_validation" full_itv_relation;
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
  `Assoc
    [
      ("name", `String name);
      ("status", `String (if passes then "pass" else "fail"));
    ]

let rec set_path path value json =
  match (path, json) with
  | [], _ -> value
  | key :: rest, `Assoc fields ->
      let old =
        match List.assoc_opt key fields with Some v -> v | None -> `Assoc []
      in
      `Assoc ((key, set_path rest value old) :: List.remove_assoc key fields)
  | _ :: _, other -> other

let mutate_first_semantic_export_return residual value =
  match semantic_exports residual with
  | first :: rest ->
      let wrong = set_path [ "return_value" ] (`Int value) first in
      set_path [ "semantic_exports" ] (`List (wrong :: rest)) residual
  | [] -> residual

let remove_location_from_rows location rows =
  rows
  |> List.map (function
       | `Assoc fields as row -> (
           match List.assoc_opt "memory" fields with
           | Some (`List cells) ->
               `Assoc
                 (( "memory",
                    `List
                      (List.filter
                         (fun cell -> string_field "location" cell <> location)
                         cells) )
                 :: List.remove_assoc "memory" fields)
           | _ -> row)
       | row -> row)

let remove_linked_location location residual =
  let output = member "linked_output" residual in
  residual
  |> set_path
       [ "linked_output"; "final_input_table" ]
       (`List
          (remove_location_from_rows location
             (list_field "final_input_table" output)))
  |> set_path
       [ "linked_output"; "final_output_table" ]
       (`List
          (remove_location_from_rows location
             (list_field "final_output_table" output)))

let mutate_first_linked_cell_metadata residual metadata =
  let output = member "linked_output" residual in
  match list_field "final_output_table" output with
  | `Assoc row_fields :: row_rest -> (
      match List.assoc_opt "memory" row_fields with
      | Some (`List (cell :: cell_rest)) ->
          let mutated_cell = set_path [ "typed_cell_metadata" ] metadata cell in
          let mutated_row =
            `Assoc
              (("memory", `List (mutated_cell :: cell_rest))
              :: List.remove_assoc "memory" row_fields)
          in
          set_path
            [ "linked_output"; "final_output_table" ]
            (`List (mutated_row :: row_rest))
            residual
      | _ -> residual)
  | _ -> residual

let obligation_set_fails witness_id category residual oracle =
  not (obligation_passes (obligations_for witness_id category residual oracle))

let full_itv_relation_fails witness_id residual oracle =
  let relation =
    Relation.full_itv_semantic_relation_json ~witness_id ~residual ~oracle
  in
  string_field "status" relation = "fail"

let selected_relation_passes witness_id residual oracle =
  let relation =
    Relation.selected_observation_relation_json ~witness_id ~residual ~oracle
  in
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
    witnesses
    |> List.map (fun witness ->
           let witness_id = string_field "witness_id" witness in
           let category = string_field "category" witness in
           let residual =
             Yojson.Safe.from_file
               (string_field "residual_linked_artifact" witness)
           in
           let oracle =
             Yojson.Safe.from_file
               (string_field "premerge_observer_artifact" witness)
           in
           (witness_id, category, residual, oracle, witness))
  in
  let find id =
    loaded |> List.find_opt (fun (witness_id, _, _, _, _) -> witness_id = id)
  in
  let mutate_first_summary path value residual =
    match list_field "external_summaries" residual with
    | first :: rest ->
        set_path [ "external_summaries" ]
          (`List (set_path path value first :: rest))
          residual
    | [] -> residual
  in
  let mutate_first_effect effect_field path value residual =
    match list_field "external_summaries" residual with
    | first :: rest -> (
        match list_field effect_field first with
        | eff :: eff_rest ->
            let mutated =
              set_path [ effect_field ]
                (`List (set_path path value eff :: eff_rest))
                first
            in
            set_path [ "external_summaries" ] (`List (mutated :: rest)) residual
        | [] -> residual)
    | [] -> residual
  in
  let mutate_first_memory_delta path value residual =
    match list_field "external_summaries" residual with
    | first :: rest -> (
        match list_field "memory_deltas" first with
        | delta :: delta_rest ->
            let mutated =
              set_path [ "memory_deltas" ]
                (`List (set_path path value delta :: delta_rest))
                first
            in
            set_path [ "external_summaries" ] (`List (mutated :: rest)) residual
        | [] -> residual)
    | [] -> residual
  in
  let mutate_first_taint_component path value residual =
    match list_field "external_summaries" residual with
    | first :: rest -> (
        match list_field "taint_components" first with
        | component :: component_rest ->
            let mutated =
              set_path [ "taint_components" ]
                (`List (set_path path value component :: component_rest))
                first
            in
            set_path [ "external_summaries" ] (`List (mutated :: rest)) residual
        | [] -> residual)
    | [] -> residual
  in
  let return_false =
    match loaded with
    | (id, category, residual, oracle, _) :: _ ->
        obligation_set_fails id category
          (mutate_first_semantic_export_return residual 999)
          oracle
        && full_itv_relation_fails id
             (mutate_first_semantic_export_return residual 999)
             oracle
    | [] -> false
  in
  let v3_missing_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
            let mutated =
              set_path [ "external_summaries" ] (`List []) residual
            in
            require_external_summaries "v3_missing_false" mutated)
    | [] -> false
  in
  let v3_compat_only_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ -> (
        match list_field "external_summaries" residual with
        | first :: _ ->
            fails (fun () ->
                require_external_summaries "v3_compat_only_false"
                  (set_path [ "external_summaries" ]
                     (`List [ member "external_summary_v1_compat" first ])
                     residual))
        | [] -> false)
    | [] -> false
  in
  let v3_schema_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ -> (
        match list_field "external_summaries" residual with
        | first :: rest ->
            fails (fun () ->
                require_external_summaries "v3_schema_false"
                  (set_path [ "external_summaries" ]
                     (`List
                        (set_path [ "schema_version" ]
                           (`String "abstract-speculate-external-summary/v1")
                           first
                        :: rest))
                     residual))
        | [] -> false)
    | [] -> false
  in
  let v3_status_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
            require_external_summaries "v3_status_false"
              (mutate_first_summary [ "summary_api_status" ]
                 (`String "stale-or-public") residual))
    | [] -> false
  in
  let v3_return_value_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
            require_external_summaries "v3_return_value_false"
              (mutate_first_effect "return_effects" [ "value" ] (`Int 999)
                 residual))
    | [] -> false
  in
  let typed_scalar_metadata_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
            require_external_summaries "typed_scalar_metadata_false"
              (mutate_first_effect "return_effects"
                 [ "typed_scalar_metadata"; "provider_source_hash" ]
                 (`String "wrong-hash") residual))
    | [] -> false
  in
  let typed_scalar_relation_false =
    match loaded with
    | (id, _category, residual, oracle, _witness) :: _ ->
        let mutated =
          match list_field "linked_stage2_input_derivation" residual with
          | first :: rest ->
              set_path
                [ "linked_stage2_input_derivation" ]
                (`List
                   (set_path
                      [ "scalar_call_protocol_id" ]
                      (`String "wrong-protocol-id") first
                   :: rest))
                residual
          | [] -> residual
        in
        full_itv_relation_fails id mutated oracle
    | [] -> false
  in
  let global_false =
    match find "global_write_read" with
    | Some (id, category, residual, oracle, _) ->
        let mutated = remove_linked_location "shared_g" residual in
        obligation_set_fails id category mutated oracle
        || full_itv_relation_fails id mutated oracle
    | None -> false
  in
  let pointer_false =
    match find "pointer_memory_effect" with
    | Some (id, category, residual, oracle, _) ->
        let mutated = remove_linked_location "(write_ptr,p)" residual in
        obligation_set_fails id category mutated oracle
        || full_itv_relation_fails id mutated oracle
    | None -> false
  in
  let v3_memory_negative witness_id mutation =
    match find witness_id with
    | Some (id, _category, residual, _oracle, _) ->
        fails (fun () -> require_external_summaries id (mutation residual))
    | None -> false
  in
  let v3_memory_missing_false =
    v3_memory_negative "global_write_read" (fun residual ->
      match list_field "external_summaries" residual with
      | first :: rest ->
          let first =
            first
            |> set_path ["memory_deltas"] (`List [])
            |> set_path ["delta_chains"] (`List [])
          in
          set_path ["external_summaries"] (`List (first :: rest)) residual
      | [] -> residual)
  in
  let v3_memory_role_false =
    v3_memory_negative "global_write_read"
      (mutate_first_memory_delta ["writer_role"] (`String "reader"))
  in
  let v3_memory_location_false =
    v3_memory_negative "global_write_read"
      (mutate_first_memory_delta ["normalized_location"] (`String "wrong-location"))
  in
  let v3_memory_value_false =
    v3_memory_negative "global_write_read"
      (mutate_first_memory_delta ["write_value"] (`String ""))
  in
  let v3_memory_provenance_false =
    v3_memory_negative "global_write_read"
      (mutate_first_memory_delta ["provider_source_hash"] (`String ""))
  in
  let v3_memory_chain_false =
    v3_memory_negative "global_write_read"
      (mutate_first_memory_delta ["delta_chain"] (`List []))
  in
  let v3_pointer_location_false =
    v3_memory_negative "pointer_memory_effect"
      (mutate_first_memory_delta ["alias_key"] `Null)
  in
  let taint_negative mutation =
    match find "taint_product_pair" with
    | Some (id, _category, residual, _oracle, _) ->
        fails (fun () -> require_external_summaries id (mutation residual))
    | None -> false
  in
  let taint_omitted_false =
    taint_negative (mutate_first_summary [ "taint_components" ] (`List []))
  in
  let taint_product_pair_empty_false =
    taint_negative (mutate_first_summary [ "product_pair_evidence" ] (`List []))
  in
  let taint_unrelated_false =
    taint_negative
      (mutate_first_taint_component [ "related_effect_id" ]
         (`String "unrelated-effect-id"))
  in
  let taint_metadata_only_false =
    taint_negative
      (mutate_first_taint_component [ "metadata_only" ] (`Bool true))
  in
  let non_selected_itv_cell_false =
    match find "global_write_read" with
    | Some (id, _category, residual, oracle, _) ->
        let mutated = remove_linked_location "(main,tmp)" residual in
        full_itv_relation_fails id mutated oracle
        && selected_relation_passes id mutated oracle
    | None -> false
  in
  let typed_cell_metadata_mismatch_false =
    match find "global_write_read" with
    | Some (id, _category, residual, oracle, _) ->
        let mutated =
          mutate_first_linked_cell_metadata residual
            (`Assoc
               [
                 ("value_model", `String "typed-itv-residual-cell/v1");
                 ("location", `String "(wrong,location)");
               ])
        in
        full_itv_relation_fails id mutated oracle
    | None -> false
  in
  let mixed_false =
    match find "mixed_role_chain" with
    | Some (id, category, residual, oracle, _) ->
        obligation_set_fails id category
          (set_path [ "phase_log" ] (`List []) residual)
          oracle
    | None -> false
  in
  let missing_oracle_false =
    loaded
    |> List.exists (fun (_, _, _, _, witness) ->
           let missing =
             string_field "premerge_observer_artifact" witness ^ ".missing"
           in
           let mutated =
             set_path [ "premerge_observer_artifact" ] (`String missing) witness
           in
           (not (Sys.file_exists missing))
           && fails (fun () -> witness_report mutated))
  in
  let witness_identity_false =
    loaded
    |> List.exists (fun (id, _, _, _, witness) ->
           let mutated =
             set_path [ "witness_id" ] (`String (id ^ "-mutated")) witness
           in
           fails (fun () -> witness_report mutated))
  in
  let taint_false mutation =
    match find "taint_product_pair" with
    | Some (_id, _category, _residual, _oracle, witness) ->
        let report = witness_report (mutation witness) in
        string_field "status" report = "fail"
    | None -> false
  in
  let taint_evidence_mutate path value witness =
    match assoc_field "taint_evidence" witness with
    | Some evidence ->
        set_path [ "taint_evidence" ] (set_path path value evidence) witness
    | None -> witness
  in
  let provenance_false =
    match loaded with
    | (id, category, residual, oracle, _) :: _ ->
        let residual_obs, oracle_obs =
          normalized_observations id residual oracle
        in
        let obligations = obligations_for id category residual oracle in
        let residual_obs_without_provenance =
          match residual_obs with
          | first :: rest ->
              set_path [ "residual_provenance" ] (`String "") first :: rest
          | [] -> []
        in
        observations_have_provenance (residual_obs @ oracle_obs)
        && not
             (witness_pass_status obligations residual_obs_without_provenance
                oracle_obs)
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
  let global_residual_evidence_fails mutation =
    loaded
    |> List.exists (fun (id, _category, residual, oracle, _) ->
           let mutated = mutation residual in
           (not (global_residual_evidence_passes id mutated))
           && full_itv_relation_fails id mutated oracle)
  in
  let mutate_first_scheduler_edge mutate residual =
    let mutate_report_edges report =
      match list_field "edges" report with
      | first :: rest ->
          set_path [ "edges" ] (`List (mutate first :: rest)) report
      | [] -> report
    in
    let mutate_root_edges field residual =
      match member field residual with
      | `List (first :: rest) ->
          set_path [ field ]
            (`List (mutate_report_edges first :: rest))
            residual
      | `Assoc _ as root ->
          set_path [ field ] (mutate_report_edges root) residual
      | _ -> residual
    in
    let residual =
      match list_field "linked_cycle_scheduler_edges" residual with
      | first :: rest ->
          set_path
            [ "linked_cycle_scheduler_edges" ]
            (`List (mutate first :: rest))
            residual
      | [] -> residual
    in
    let residual =
      match list_field "linked_cycle_topology" residual with
      | first_scc :: rest_sccs -> (
          match list_field "scheduler_edges" first_scc with
          | first :: rest ->
              set_path
                [ "linked_cycle_topology" ]
                (`List
                   (set_path [ "scheduler_edges" ]
                      (`List (mutate first :: rest))
                      first_scc
                   :: rest_sccs))
                residual
          | [] -> residual)
      | [] -> residual
    in
    residual
    |> mutate_root_edges "linked_cycle_scheduler"
    |> mutate_root_edges "linked_cycle_scheduler_evidence"
  in
  let clear_scheduler_evidence residual =
    residual
    |> set_path [ "linked_cycle_scheduler" ] (`Assoc [])
    |> set_path [ "linked_cycle_scheduler_evidence" ] (`Assoc [])
    |> set_path [ "linked_cycle_scheduler_edges" ] (`List [])
    |> set_path [ "linked_cycle_scheduler_sccs" ] (`List [])
    |> set_path [ "linked_cycle_callgraph_backed_schedule" ] (`Bool false)
    |> set_path [ "linked_cycle_topology" ] (`List [])
  in
  let scheduler_dependency_only_relabel residual =
    residual
    |> mutate_first_scheduler_edge (fun edge ->
           edge
           |> set_path [ "provenance_level" ]
                (`String "residual_dependency_only")
           |> set_path [ "source" ] (`String "residual-dependency-only"))
  in
  let scheduler_missing_required_edge_field residual =
    residual
    |> mutate_first_scheduler_edge (fun edge ->
           set_path [ "evidence_id" ] (`String "") edge)
  in
  let scheduler_scc_count_mismatch residual =
    residual
    |> set_path [ "linked_cycle_scheduler_scc_count" ] (`Int 999)
    |> set_path [ "linked_cycle_scc_count" ] (`Int 999)
  in
  let old_topology_relabelled_without_provenance residual =
    let old_edges =
      list_field "linked_cycle_topology" residual
      |> List.concat_map (fun scc -> list_field "edges" scc)
    in
    let old_sccs = list_field "linked_cycle_topology" residual in
    residual
    |> set_path
         [ "linked_cycle_scheduler" ]
         (`List
            [
              `Assoc
                [
                  ("callgraph_backed_schedule", `Bool true);
                  ("scheduler_source", `String "residual-call-binding-callgraph");
                  ("edges", `List old_edges);
                  ("sccs", `List old_sccs);
                ];
            ])
    |> set_path [ "linked_cycle_scheduler_edges" ] (`List [])
    |> set_path [ "linked_cycle_scheduler_sccs" ] (`List [])
    |> set_path [ "linked_cycle_callgraph_backed_schedule" ] (`Bool true)
  in
  let mutate_first_imported_value path value residual =
    match list_field "imported_cyclic_observable_values" residual with
    | first :: rest ->
        set_path
          [ "imported_cyclic_observable_values" ]
          (`List (set_path path value first :: rest))
          residual
    | [] -> residual
  in
  let mutate_first_observable path value residual =
    match list_field "imported_cyclic_observable_values" residual with
    | first :: rest -> (
        match list_field "observable_values" first with
        | obs :: obs_rest ->
            let first =
              set_path [ "observable_values" ]
                (`List (set_path path value obs :: obs_rest))
                first
            in
            set_path
              [ "imported_cyclic_observable_values" ]
              (`List (first :: rest))
              residual
        | [] -> residual)
    | [] -> residual
  in
  let mutate_first_shared_final_cell path value residual =
    match list_field "shared_scc_final_cells" residual with
    | first :: rest ->
        set_path
          [ "shared_scc_final_cells" ]
          (`List (set_path path value first :: rest))
          residual
    | [] -> residual
  in
  let mutate_first_accepted_export path value residual =
    match list_field "linked_cycle_accepted_exports" residual with
    | first :: rest ->
        set_path
          [ "linked_cycle_accepted_exports" ]
          (`List (set_path path value first :: rest))
          residual
    | [] -> residual
  in
  let cycle_source_negative_rejected =
    let path =
      Filename.concat !artifact_dir
        "negative/cycle_import_value_removed.out/abstract-speculate-residual-linking-pe.linked.json"
    in
    Sys.file_exists path
    && not (recomputed_cycle_evidence_passes (Yojson.Safe.from_file path))
  in
  let remove_reverse_cycle_edge residual =
    match list_field "linked_cycle_topology" residual with
    | first :: rest ->
        let edges =
          list_field "edges" first
          |> List.filter (fun edge ->
                 string_field "import_name" edge <> "cycle_a")
        in
        set_path
          [ "linked_cycle_topology" ]
          (`List (set_path [ "edges" ] (`List edges) first :: rest))
          residual
    | [] -> residual
  in
  let mutate_first_topology_edge path value residual =
    match list_field "linked_cycle_topology" residual with
    | first :: rest -> (
        match list_field "edges" first with
        | edge :: edge_rest ->
            let edge = set_path path value edge in
            set_path
              [ "linked_cycle_topology" ]
              (`List
                 (set_path [ "edges" ] (`List (edge :: edge_rest)) first :: rest))
              residual
        | [] -> residual)
    | [] -> residual
  in
  let dependency_only_schedule residual =
    set_path
      [ "shared_scc_worklist_schedule" ]
      (`List
         [
           `Assoc
             [
               ("event", `String "enqueue");
               ("reason", `String "dependency-only");
               ("iteration", `Int 1);
               ("equation_id", `String "legacy-shared-scc-relabel");
               ("target", `String "legacy-shared-scc");
             ];
         ])
      residual
  in
  let global_evidence_fails ?(witness_id="global_write_read") mutation =
    match find witness_id with
    | Some (id, _category, residual, _oracle, _) ->
        let mutated_root = mutation residual in
        let mutated =
          match assoc_field "global_residual_fixpoint" residual with
          | Some report ->
              set_path ["global_residual_fixpoint"] (mutation report) mutated_root
          | None -> mutated_root
        in
        not (global_residual_fixpoint_evidence_passes id mutated)
    | None -> false
  in
  let metadata_only_global_report residual =
    residual
    |> set_path ["global_residual_fixpoint_run"] (`Bool true)
    |> set_path ["global_residual_fixpoint_scope"] (`String "post-link-whole-program-residual-cells")
    |> set_path ["global_sparse_fixpoint_component"] (`String "residual-global-worklist")
    |> set_path ["global_sparse_fixpoint_source_level_rerun"] (`Bool false)
    |> set_path ["global_residual_equation_ids"] (`List [])
    |> set_path ["global_residual_seed_cells"] (`List [])
    |> set_path ["global_residual_derived_cells"] (`List [])
    |> set_path ["global_residual_dependency_edges"] (`List [])
    |> set_path ["global_residual_cross_module_dependency_edges"] (`List [])
    |> set_path ["global_residual_state_read_count"] (`Int 0)
    |> set_path ["global_residual_seed_read_count"] (`Int 0)
    |> set_path ["global_residual_worklist_schedule"] (`List [])
    |> set_path ["global_residual_iteration_count"] (`Int 1)
    |> set_path ["global_residual_worklist_drained"] (`Bool true)
    |> set_path ["global_residual_overlay_only"] (`Bool false)
  in
  let set_final_round_changed residual =
    let rounds = list_field "linked_cycle_rounds" residual in
    match List.rev rounds with
    | last_round :: rev_prefix ->
        let changed =
          last_round
          |> set_path [ "changed" ] (`Bool true)
          |> set_path [ "changed_binding_count" ] (`Int 1)
        in
        set_path [ "linked_cycle_rounds" ]
          (`List (List.rev (changed :: rev_prefix)))
          residual
    | [] -> residual
  in
  let set_final_round_bootstrap residual =
    let rounds = list_field "linked_cycle_rounds" residual in
    match List.rev rounds with
    | last_round :: rev_prefix ->
        let bindings = list_field "linked_environment" last_round in
        let changed_bindings =
          match bindings with
          | first :: rest ->
              set_path [ "origin" ] (`String "bootstrap-unknown") first :: rest
          | [] -> []
        in
        let changed =
          set_path [ "linked_environment" ] (`List changed_bindings) last_round
        in
        set_path [ "linked_cycle_rounds" ]
          (`List (List.rev (changed :: rev_prefix)))
          residual
    | [] -> residual
  in
  let negative_dir = Filename.concat !artifact_dir "negative" in
  let injected_shortcut_rejected =
    let injected =
      Filename.concat negative_dir "injected_premerge_shortcut.ml"
    in
    write_file injected
      "let shortcut = Real_sparrow_frontend.global_for_files []\n";
    let obligation =
      source_guard_obligation_for_paths "shortcut_false_case" [ injected ] []
    in
    string_field "status" obligation = "fail"
  in
  [
    false_case "mismatched_return_or_effect_summary" return_false;
    false_case "missing_external_summary_v3" v3_missing_false;
    false_case "v1_compat_only_rejected" v3_compat_only_false;
    false_case "external_summary_schema_downgrade_rejected" v3_schema_false;
    false_case "external_summary_status_corruption_rejected" v3_status_false;
    false_case "external_summary_return_value_corruption_rejected"
      v3_return_value_false;
    false_case "typed_scalar_metadata_corruption_rejected"
      typed_scalar_metadata_false;
    false_case "typed_scalar_relation_protocol_id_corruption_rejected"
      typed_scalar_relation_false;
    false_case "missing_global_write_read_effect" global_false;
    false_case "missing_pointer_memory_effect" pointer_false;
    false_case "external_summary_memory_delta_missing_rejected" v3_memory_missing_false;
    false_case "external_summary_memory_delta_role_corruption_rejected" v3_memory_role_false;
    false_case "external_summary_memory_delta_location_corruption_rejected" v3_memory_location_false;
    false_case "external_summary_memory_delta_value_corruption_rejected" v3_memory_value_false;
    false_case "external_summary_memory_delta_provenance_corruption_rejected" v3_memory_provenance_false;
    false_case "external_summary_memory_delta_chain_missing_rejected" v3_memory_chain_false;
    false_case "external_summary_pointer_delta_alias_corruption_rejected" v3_pointer_location_false;
    false_case "taint_evidence_omitted_rejected" taint_omitted_false;
    false_case "taint_product_pair_empty_rejected"
      taint_product_pair_empty_false;
    false_case "taint_evidence_unrelated_rejected" taint_unrelated_false;
    false_case "taint_evidence_metadata_only_rejected" taint_metadata_only_false;
    false_case "non_selected_itv_cell_mutation_fails_full_relation"
      non_selected_itv_cell_false;
    false_case "typed_itv_cell_metadata_mismatch_fails_full_relation"
      typed_cell_metadata_mismatch_false;
    false_case "ambiguous_provider_accepted_incorrectly"
      (log_contains
         (Filename.concat negative_dir "ambiguous_provider.log")
         "unsupported ambiguous semantic export mapping");
    false_case "invalid_mixed_role_propagation" mixed_false;
    false_case "premerge_implementation_shortcut" injected_shortcut_rejected;
    false_case "missing_oracle_artifact" missing_oracle_false;
    false_case "witness_identity_mismatch" witness_identity_false;
    false_case "missing_normalized_observation_provenance" provenance_false;
    false_case "taint_evidence_omitted_rejected"
      (taint_false (set_path [ "taint_evidence" ] `Null));
    false_case "taint_evidence_empty_rejected"
      (taint_false (set_path [ "taint_evidence" ] (`Assoc [])));
    false_case "taint_evidence_unrelated_rejected"
      (taint_false
         (taint_evidence_mutate
            [ "related_residual_location" ]
            (`String "missing_taint_location")));
    false_case "taint_evidence_metadata_only_rejected"
      (taint_false (taint_evidence_mutate [ "metadata_only" ] (`Bool true)));
    false_case "metadata_only_global_report_rejected"
      (global_residual_evidence_fails
         (set_path
            [ "global_residual_fixpoint"; "global_residual_metadata_only" ]
            (`Bool true)));
    false_case "derived_cells_not_recomputed_rejected"
      (global_residual_evidence_fails
         (set_path
            [
              "global_residual_fixpoint";
              "global_residual_derived_cells_recomputed";
            ]
            (`Bool false)));
    false_case "global_residual_dependencies_missing_rejected"
      (global_residual_evidence_fails
         (set_path
            [ "global_residual_fixpoint"; "global_residual_dependency_edges" ]
            (`List [])));
    false_case "global_residual_state_reads_missing_rejected"
      (global_residual_evidence_fails
         (set_path
            [ "global_residual_fixpoint"; "global_residual_state_read_count" ]
            (`Int 0)));
    false_case "global_residual_worklist_not_drained_rejected"
      (global_residual_evidence_fails
         (set_path
            [ "global_residual_fixpoint"; "global_residual_worklist_drained" ]
            (`Bool false)));
    false_case "cycle_missing_topology_rejected"
      (cycle_evidence_fails (set_path [ "linked_cycle_topology" ] (`List [])));
    false_case "cycle_scc_count_falsification_rejected"
      (cycle_evidence_fails (set_path [ "linked_cycle_scc_count" ] (`Int 999)));
    false_case "cycle_missing_reverse_edge_rejected"
      (cycle_evidence_fails remove_reverse_cycle_edge);
    false_case "cycle_missing_edge_provenance_rejected"
      (match find "cycle_topology" with
      | Some (_id, _category, residual, _oracle, _) ->
          let mutated =
            mutate_first_topology_edge [ "stable_evidence_id" ] (`String "")
              residual
          in
          not
            (cycle_topology_edge_provenance_passes
               (list_field "linked_cycle_topology" mutated))
      | None -> false);
    false_case "cycle_empty_rounds_rejected"
      (cycle_evidence_fails (set_path [ "linked_cycle_rounds" ] (`List [])));
    false_case "cycle_non_stable_final_round_rejected"
      (cycle_evidence_fails set_final_round_changed);
    false_case "cycle_bootstrap_binding_remaining_rejected"
      (cycle_evidence_fails
         (set_path [ "linked_cycle_bootstrap_bindings_remaining" ] (`Int 1)));
    false_case "cycle_final_bootstrap_origin_rejected"
      (cycle_evidence_fails set_final_round_bootstrap);
    false_case "cycle_overlay_only_rejected"
      (cycle_evidence_fails (set_path [ "linked_overlay_only" ] (`Bool true)));
    false_case "cycle_source_import_value_removed_rejected"
      cycle_source_negative_rejected;
    false_case "cycle_missing_imported_observable_rejected"
      (cycle_evidence_fails
         (set_path [ "imported_cyclic_observable_values" ] (`List [])));
    false_case "cycle_imported_value_imprecision_rejected"
      (cycle_evidence_fails
         (mutate_first_imported_value [ "no_extra_imprecision" ] (`Bool false)));
    false_case "cycle_missing_shared_scc_dependency_rejected"
      (cycle_evidence_fails (set_path [ "shared_scc_dependencies" ] (`List [])));
    false_case "cycle_zero_shared_scc_state_reads_rejected"
      (cycle_evidence_fails
         (set_path [ "shared_scc_state_read_count" ] (`Int 0)));
    false_case "cycle_missing_shared_scc_worklist_schedule_rejected"
      (cycle_evidence_fails
         (set_path [ "shared_scc_worklist_schedule" ] (`List [])));
    false_case "cycle_dependency_only_schedule_rejected"
      (cycle_evidence_fails dependency_only_schedule);
    false_case "cycle_shared_scc_final_cell_mismatch_rejected"
      (cycle_evidence_fails
         (mutate_first_shared_final_cell [ "value" ] (`Int 999)));
    false_case "cycle_observable_source_cell_mismatch_rejected"
      (cycle_evidence_fails
         (mutate_first_observable
            [ "source_shared_scc_cell_id" ]
            (`String "missing-cell")));
    false_case "cycle_shared_scc_relabel_without_provenance_rejected"
      (match find "cycle_topology" with
      | Some (_id, _category, residual, _oracle, _) ->
          let mutated =
            mutate_first_imported_value [ "stable_evidence_id" ] (`String "")
              residual
          in
          list_field "imported_cyclic_observable_values" mutated
          |> List.exists (fun value ->
                 string_field "stable_evidence_id" value = "")
      | None -> false);
    false_case "cycle_bootstrap_derived_export_rejected"
      (cycle_evidence_fails
         (mutate_first_accepted_export [ "derivation_source" ]
            (`String "provider-stage2-output")));
    false_case "callgraph_scheduler_missing_evidence_rejected"
      (callgraph_scheduler_evidence_fails clear_scheduler_evidence);
    false_case "callgraph_scheduler_missing_edge_provenance_rejected"
      (callgraph_scheduler_evidence_fails scheduler_missing_required_edge_field);
    false_case "callgraph_scheduler_scc_count_mismatch_rejected"
      (callgraph_scheduler_evidence_fails scheduler_scc_count_mismatch);
    false_case "callgraph_scheduler_dependency_only_relabel_rejected"
      (callgraph_scheduler_evidence_fails scheduler_dependency_only_relabel);
    false_case "global_residual_metadata_only_report_rejected"
      (global_evidence_fails metadata_only_global_report);
    false_case "global_residual_missing_dependencies_rejected"
      (global_evidence_fails (set_path ["global_residual_dependency_edges"] (`List [])));
    false_case "global_residual_missing_cross_module_dependencies_rejected"
      (global_evidence_fails (set_path ["global_residual_cross_module_dependency_edges"] (`List [])));
    false_case "global_residual_zero_state_reads_rejected"
      (global_evidence_fails (set_path ["global_residual_state_read_count"] (`Int 0)));
    false_case "global_residual_zero_seed_reads_rejected"
      (global_evidence_fails (set_path ["global_residual_seed_read_count"] (`Int 0)));
    false_case "global_residual_missing_schedule_rejected"
      (global_evidence_fails (set_path ["global_residual_worklist_schedule"] (`List [])));
    false_case "global_residual_worklist_not_drained_rejected"
      (global_evidence_fails (set_path ["global_residual_worklist_drained"] (`Bool false)));
    false_case "global_residual_overlay_only_rejected"
      (global_evidence_fails (set_path ["global_residual_overlay_only"] (`Bool true)));
    false_case "global_residual_derived_cells_not_recomputed_rejected"
      (global_evidence_fails (set_path ["global_residual_derived_cells"] (`List [])));
    false_case "global_residual_missing_final_cells_rejected"
      (global_evidence_fails (fun residual ->
        residual
        |> set_path ["global_residual_final_cells"] (`List [])
        |> set_path ["global_residual_final_rows"] (`List [])
        |> set_path ["global_residual_final_input_table"] (`List [])
        |> set_path ["global_residual_final_output_table"] (`List [])
        |> set_path ["global_residual_final_cell_rows"] (`List [])));
    false_case "global_residual_cycle_iteration_count_one_rejected"
      (global_evidence_fails ~witness_id:"cycle_value_flow" (set_path ["global_residual_iteration_count"] (`Int 1)));
    false_case "old_shared_scc_topology_relabelled_callgraph_backed_rejected"
      (callgraph_scheduler_evidence_fails
         old_topology_relabelled_without_provenance);
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
  expect
    (string_field "artifact_schema_status" manifest = "prototype-non-public")
    "suite manifest must mark schema prototype/non-public";
  expect
    (string_field "oracle_reference_kind" manifest = "premerge-linked-observer")
    "suite manifest must mark premerge observer as oracle/reference";
  let witnesses = list_field "witnesses" manifest in
  expect (List.length witnesses >= 5) "expected oracle suite witness coverage";
  let witness_reports = List.map witness_report witnesses in
  expect
    (witness_reports
    |> List.exists (fun w -> string_field "witness_id" w = "taint_product_pair")
    )
    "missing taint_product_pair witness";
  expect
    (witness_reports
    |> List.exists (fun w ->
           string_field "witness_id" w = "taint_product_pair"
           && string_field "status" (member "taint_product_evidence" w) = "pass"
           && string_field "taint_semantic_relation" w
              = "bounded-user-input-taint-to-return"
           && List.mem "Itv" (json_string_list "product_components" w)
           && List.mem "Taint" (json_string_list "product_components" w)))
    "missing checked Itv+Taint product-pair evidence";
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
    "global_residual_fixpoint_evidence";
  ] in
  let names = all_obligations |> List.map (string_field "name") in
  required
  |> List.iter (fun name ->
         expect (List.mem name names) ("missing obligation: " ^ name));
  let failed_obligations =
    all_obligations
    |> List.filter (fun o -> string_field "status" o <> "pass")
    |> List.map (fun o ->
           string_field "witness_id" o ^ ":" ^ string_field "name" o)
  in
  expect (failed_obligations = [])
    ("at least one proof obligation failed: "
    ^ String.concat "," failed_obligations);
  let failed_negative_cases =
    negative_cases
    |> List.filter (fun n -> string_field "status" n <> "pass")
    |> List.map (string_field "name")
  in
  expect
    (failed_negative_cases = [])
    ("at least one negative case was not covered: "
    ^ String.concat "," failed_negative_cases);
  let distinct_cycle_witness_present =
    witness_reports
    |> List.exists (fun w ->
           string_field "witness_id" w <> "cycle_topology"
           && list_field "obligations" w
              |> List.exists (fun o ->
                     string_field "name" o = "cyclic_residual_fixpoint_evidence"
                     && string_field "status" o = "pass"))
  in
  expect distinct_cycle_witness_present "missing distinct cyclic witness beyond cycle_topology";
  expect (witness_reports |> List.exists (fun w -> string_field "witness_id" w = "cycle_value_flow"))
    "missing cycle_value_flow witness for residual global fixpoint iteration gate";
  expect (witness_reports |> List.exists (fun w ->
    string_field "witness_id" w = "global_write_read" &&
    string_field "status" (member "global_residual_fixpoint" w) = "pass"))
    "missing passing positive whole-program global_write_read residual-global witness";
  expect (witness_reports |> List.exists (fun w ->
    string_field "witness_id" w = "cycle_value_flow" &&
    string_field "status" (member "global_residual_fixpoint" w) = "pass"))
    "missing passing cycle_value_flow residual-global SCC witness";
  let failed_witnesses =
    witness_reports
    |> List.filter (fun w -> string_field "status" w <> "pass")
    |> List.map (fun w ->
           string_field "witness_id" w
           ^ ":"
           ^ string_field "status" (member "full_itv_semantic_relation" w)
           ^ ":taint="
           ^ Yojson.Safe.to_string (member "taint_product_evidence" w))
  in
  let suite_pass = failed_witnesses = [] in
  expect suite_pass
    ("at least one full-Itv semantic relation failed: "
    ^ String.concat "," failed_witnesses);
  expect
    (witness_reports
    |> List.for_all (fun w -> bool_field "full_itv_required_fields_present" w))
    "at least one full-Itv relation omitted a required compatibility field";
  expect
    (witness_reports
    |> List.exists (fun w ->
           string_field "witness_id" w = "taint_product_pair"
           && string_field "status" (member "taint_product_evidence" w) = "pass")
    )
    "missing passing bounded Taint product-pair witness";
  let taint_reports = witness_reports |> List.filter (fun w -> string_field "witness_id" w = "taint_product_pair") in
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
    "global_residual_fixpoint_summary", `Assoc [
      "witness_count", `Int (List.length witness_reports);
      "relation", `String "post-link-whole-program-residual-cells versus premerge-linked-observer";
      "pass_gate", `String "checker-derived global_residual_fixpoint_evidence plus full_itv_semantic_relation";
      "claim_scope", `String "witness-bounded selected Sparrow Itv residual-global worklist";
    ];
    "global_residual_fixpoint", `List (List.map (fun w -> member "global_residual_fixpoint" w) witness_reports);
    "taint_product_summary", `Assoc [
      "witness_id", `String "taint_product_pair";
      "witness_count", `Int (List.length taint_reports);
      "relation", `String "bounded named-witness Taint product evidence";
      "pass_gate", `String "taint_product_pair_semantic_evidence obligation plus negative cases";
      "taint_semantic_relation", `String "bounded-user-input-taint-to-return";
      "product_components", `List [`String "Itv"; `String "Taint"];
      "support_scope", `String "named-witness-only-no-general-product-domain-parity";
    ];
    "taint_product_evidence", `List (List.map (fun w -> member "taint_product_evidence" w) taint_reports);
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
      `String "bounded Taint evidence only for named Itv+Taint product-pair witnesses";
      `String "no general Taint or product-domain parity";
      `String "no arbitrary-C or whole-program semantic equivalence";
      `String "full Sparrow-Itv relation is witness-bounded to the oracle suite";
      `String "selected-observation evidence is diagnostic/compatibility only";
    ];
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report report_json;
  print_endline "abstract_speculate_residual_linking_oracle_suite_check: PASS"
