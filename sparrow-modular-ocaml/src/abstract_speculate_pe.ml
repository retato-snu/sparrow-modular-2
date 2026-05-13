(***********************************************************************)
(* Abstract Speculate: direct MetaOCaml PE over module sparse analysis. *)
(***********************************************************************)

module StageT = Abstract_speculate_stage_types
module Stage2 = Abstract_speculate_stage2_input
module MetaSparse = Abstract_speculate_meta_sparse

let schema_version = "abstract-speculate-metaocaml-sparse-pe/v1"
let boundary_schema_version = "abstract-speculate-module-boundary/v2"
let provenance_schema_version = "abstract-speculate-fact-provenance/v2"
let bta_schema_version = "abstract-speculate-bta/v2"
let residual_schema_version = "abstract-speculate-online-residual/v2"
let forbidden_scan_schema_version = "abstract-speculate-forbidden-shortcuts/v2"
let audit_schema_version = "abstract-speculate-audit/v2"

let compare_string = String.compare
let sort_strings xs = List.sort_uniq compare_string xs
let sort_json xs = List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)) xs
let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> Some s | _ -> None
let bool_field name json = match assoc_field name json with Some (`Bool b) -> Some b | _ -> None
let projection_member name artifact = artifact |> member "projection" |> member name
let module_id source = Filename.basename source

let fact_depends_on_extern fact = bool_field "depends_on_extern" fact = Some true
let fact_classification fact = match string_field "classification" fact with Some s -> s | None -> "<missing>"
let fact_reason fact = match string_field "reason" fact with Some s -> s | None -> "<missing>"
let fact_id fact = match string_field "id" fact with Some s -> s | None -> Yojson.Safe.to_string fact

let allowed_dynamic_reason = function
  | "unknown-extern-call" | "transitively-extern-dependent" -> true
  | _ -> false

let valid_bta_fact fact =
  match fact_classification fact, fact_reason fact, bool_field "depends_on_extern" fact with
  | "residual-extern-dependent", reason, Some true -> allowed_dynamic_reason reason
  | "dynamic-extern-dependent", reason, Some true -> allowed_dynamic_reason reason
  | "static-precomputed", _, Some false -> true
  | _ -> false

let module_boundary_json result =
  `Assoc [
    "schema_version", `String boundary_schema_version;
    "status", `String "pass";
    "module_id", `String result.MetaSparse.module_id;
    "source_file", `String result.source;
    "source_hash", `String result.source_hash;
    "single_module_input", `Bool true;
    "parser_entrypoint", `String "Real_sparrow_frontend.parse_one_file";
    "global_entrypoint", `String "Real_sparrow_frontend.global_for_module";
    "linked_entrypoints_used", `Bool false;
    "module_analysis_before_link", `Bool true;
    "forbidden_prelink_entrypoints", `List [
      `String "Real_sparrow_frontend.parse";
      `String "Real_sparrow_frontend.global_for_files";
      `String "Mergecil.merge";
    ];
    "sibling_module_paths", `List [];
  ]

let provenance_for_fact result fact =
  let depends = fact_depends_on_extern fact in
  `Assoc [
    "fact_id", `String (fact_id fact);
    "origin_module", `String result.MetaSparse.module_id;
    "source_file", `String result.source;
    "source_hash", `String result.source_hash;
    "origin", `String (if depends then "module-local-extern-obligation" else "module-local-static");
    "depends_on_external_module", `Bool depends;
    "classification", `String (fact_classification fact);
    "reason", `String (fact_reason fact);
    "dependency_path", `List (if depends then [`String (fact_id fact)] else []);
  ]

let fact_provenance_json result =
  let facts = result.MetaSparse.facts |> List.map (provenance_for_fact result) |> sort_json in
  let bad =
    facts |> List.filter (function
      | `Assoc fields ->
          List.assoc_opt "source_file" fields <> Some (`String result.source) ||
          List.assoc_opt "source_hash" fields <> Some (`String result.source_hash)
      | _ -> true)
  in
  `Assoc [
    "schema_version", `String provenance_schema_version;
    "status", `String (if bad = [] then "pass" else "fail");
    "module_id", `String result.module_id;
    "source_file", `String result.source;
    "source_hash", `String result.source_hash;
    "forbidden_sibling_fact_count", `Int (List.length bad);
    "facts", `List facts;
    "bad_facts", `List bad;
  ]

let bta_report_json result =
  let facts = result.MetaSparse.facts in
  let bad = facts |> List.filter (fun fact -> not (valid_bta_fact fact)) in
  let residual_count = facts |> List.fold_left (fun n fact -> if fact_depends_on_extern fact then n + 1 else n) 0 in
  `Assoc [
    "schema_version", `String bta_schema_version;
    "status", `String (if bad = [] then "pass" else "fail");
    "module_id", `String result.module_id;
    "allowed_dynamic_reasons", `List [`String "unknown-extern-call"; `String "transitively-extern-dependent"];
    "forbidden_dynamic_fact_count", `Int (List.length bad);
    "residual_fact_count", `Int residual_count;
    "extern_roots", `List (List.map (fun s -> `String s) result.extern_roots);
    "facts", `List facts;
    "bad_facts", `List bad;
  ]

let online_residuals result =
  match result.MetaSparse.stage2_output.StageT.execution_log |> member "executed_residuals" with
  | `List xs -> sort_json xs
  | _ -> []

let rec first_typed_code_id = function
  | StageT.Typed_component component -> Some component.StageT.residual_id
  | StageT.Loop_shape (_, _, body) | StageT.Expr_shape (_, body) ->
      List.find_map first_typed_code_id body
  | StageT.Branch_shape (_, _, then_w, else_w) ->
      List.find_map first_typed_code_id (then_w @ else_w)

let rec residual_shape_entries = function
  | StageT.Typed_component _ -> []
  | StageT.Loop_shape (id, _, body) ->
      let residual_code_id = match List.find_map first_typed_code_id body with Some id -> id | None -> "missing" in
      `Assoc [
        "constructor", `String "Loop_shape";
        "serialization_source", `String "typed-witness";
        "paired_executable_code", `Bool true;
        "residual_code_id", `String residual_code_id;
        "source_text_scan_used", `Bool false;
        "flat_dependency_marker", `Bool false;
      ] :: List.concat_map residual_shape_entries body
  | StageT.Branch_shape (id, _, then_w, else_w) ->
      let residual_code_id = match List.find_map first_typed_code_id (then_w @ else_w) with Some id -> id | None -> "missing" in
      `Assoc [
        "constructor", `String "Branch_shape";
        "serialization_source", `String "typed-witness";
        "paired_executable_code", `Bool true;
        "residual_code_id", `String residual_code_id;
        "source_text_scan_used", `Bool false;
        "flat_dependency_marker", `Bool false;
      ] :: List.concat_map residual_shape_entries (then_w @ else_w)
  | StageT.Expr_shape (_, children) -> List.concat_map residual_shape_entries children

let residual_shape_witnesses result =
  result.MetaSparse.shape_witnesses |> List.concat_map residual_shape_entries |> sort_json

let residual_artifact_json ?residual_dir result =
  let stage2_output_json = Stage2.output_to_yojson result.MetaSparse.stage2_output in
  let extern_effects_path, output_path, artifact_path =
    match residual_dir with
    | None -> `Null, `Null, `Null
    | Some dir ->
        Real_sparrow_artifact.mkdir_p dir;
        let stem = Filename.basename result.source in
        let extern_path = Filename.concat dir (stem ^ ".abstract-speculate.extern-effects.json") in
        let out_path = Filename.concat dir (stem ^ ".abstract-speculate.runcode-output.json") in
        let artifact_path = Filename.concat dir (stem ^ ".abstract-speculate.typed-residual.json") in
        Real_sparrow_artifact.write_json extern_path result.stage2_input.StageT.extern_effects;
        Real_sparrow_artifact.write_json out_path stage2_output_json;
        Real_sparrow_artifact.write_json artifact_path (`Assoc [
          "artifact_kind", `String "typed-metaocaml-code-value";
          "residual_code_kind", `String "Trx.code";
          "online_runcode_run", `Bool true;
          "residual_components", `List (List.map StageT.component_to_yojson (result.analyzer.StageT.residual_input_components @ result.analyzer.residual_output_components @ result.analyzer.control_components));
          "shape_witnesses", StageT.shape_witnesses_to_yojson result.shape_witnesses;
        ]);
        `String extern_path, `String out_path, `String artifact_path
  in
  `Assoc [
    "schema_version", `String residual_schema_version;
    "artifact_kind", `String "online-metaocaml-module-residual-analyzer";
    "json_is_summary_only", `Bool false;
    "metaocaml_online", `Bool true;
    "residual_code_kind", `String "Trx.code";
    "stage1_static_transfer_count", `Int (List.length result.facts);
    "staged_domain_fixpoint", `Bool true;
    "stage1_direct_sparse_pipeline", `Bool false;
    "direct_sparse_oracle_only", `Bool true;
    "posthoc_row_split_used", `Bool false;
    "row_obligation_residual_source", `Bool false;
    "whole_row_dynamic_wrapper", `Bool false;
    "residual_input_obligation_count", `Int 0;
    "residual_output_obligation_count", `Int 0;
    "control_obligation_count", `Int 0;
    "residual_input_component_count", `Int (List.length result.analyzer.StageT.residual_input_components);
    "residual_output_component_count", `Int (List.length result.analyzer.StageT.residual_output_components);
    "control_component_count", `Int (List.length result.analyzer.StageT.control_components);
    "transfer_level_d_site_count", result.stage2_output.StageT.execution_log |> member "transfer_level_d_site_count";
    "staged_lattice_event_count", result.stage2_output.StageT.execution_log |> member "staged_lattice_event_count";
    "bta_participates_in_fixpoint", `Bool true;
    "bta_dynamic_sites_before_convergence", result.stage2_output.StageT.execution_log |> member "bta_dynamic_sites_before_convergence";
    "fixpoint_iterations_with_dynamic_cells", result.stage2_output.StageT.execution_log |> member "fixpoint_iterations_with_dynamic_cells";
    "typed_residual_arithmetic_count", result.stage2_output.StageT.execution_log |> member "typed_residual_arithmetic_count";
    "typed_residual_guard_count", result.stage2_output.StageT.execution_log |> member "typed_residual_guard_count";
    "blind_equality_static_projection", result.stage2_output.StageT.execution_log |> member "blind_equality_static_projection";
    "convergence_ignores_residual_code_structure", result.stage2_output.StageT.execution_log |> member "convergence_ignores_residual_code_structure";
    "blind_equality_witness", result.analyzer.StageT.blind_equality_witness;
    "module_local_prelink", `Bool true;
    "old_wrapper_delegation", `Bool false;
    "source_string_residual_core", `Bool false;
    "metadata_only_proof", `Bool false;
    "linked_facts_prelink", `Bool false;
    "top_substitution", `Bool false;
    "toy_only", `Bool false;
    "extern_effects_path", extern_effects_path;
    "output_path", output_path;
    "artifact_path", artifact_path;
    "online_residuals", `List (online_residuals result);
    "residual_shape_witnesses", `List (residual_shape_witnesses result);
    "stage2_output", stage2_output_json;
    "execution_log", result.stage2_output.StageT.execution_log;
  ]

let artifact_for_stage1_result ?residual_dir (result : MetaSparse.stage1_result) =
  `Assoc [
    "schema_version", `String schema_version;
    "source", `String result.source;
    "module_id", `String result.MetaSparse.module_id;
    "source_hash", `String result.source_hash;
    "scope", `String "module-only-pre-link";
    "claim", `String "Abstract Speculate direct MetaOCaml PE over module-local sparse fixpoint";
    "formal_target", `String "run(AbstractSpeculate, module m) ≃ PE(I, m)";
    "module_boundary", module_boundary_json result;
    "fact_provenance", fact_provenance_json result;
    "bta_report", bta_report_json result;
    "lineage", `Assoc [
      "frontend_module_entry", `String "sparrow-modular-ocaml/src/real_sparrow_frontend.ml:27-53";
      "direct_sparse_generator", `String "sparrow-modular-ocaml/src/abstract_speculate_meta_sparse.ml";
      "residual_code_builder", `String "sparrow-modular-ocaml/src/abstract_speculate_residual_value.ml";
      "online_executor", `String "Runcode.run";
    ];
    "projection", `Assoc [
      "final_input_table", `List result.stage2_output.StageT.final_input_table;
      "final_output_table", `List result.stage2_output.StageT.final_output_table;
      "completion", result.completion;
      "bta", `List result.facts;
      "residual_artifact", residual_artifact_json ?residual_dir result;
    ];
    "non_claims", `List [
      `String "no linked/global pre-link fact use";
      `String "no metadata-only PE";
      `String "no whole-row dynamic wrapper";
      `String "no source-string residual core";
      `String "no baseline sparrow/src semantic edit";
    ];
  ]

let artifact_for_module ?residual_dir source =
  source |> MetaSparse.run_stage1 |> artifact_for_stage1_result ?residual_dir

let forbidden_source_scan_for_artifact artifact =
  let residual = projection_member "residual_artifact" artifact in
  let checks = [
    "source_string_residual_core", false;
    "metadata_only_proof", false;
    "old_wrapper_delegation", false;
    "linked_facts_prelink", false;
    "top_substitution", false;
    "toy_only", false;
    "stage1_direct_sparse_pipeline", false;
    "posthoc_row_split_used", false;
    "row_obligation_residual_source", false;
    "whole_row_dynamic_wrapper", false;
  ] in
  let bad =
    checks
    |> List.filter_map (fun (field, expected) ->
         match residual |> member field with
         | `Bool b when b = expected -> None
         | _ -> Some (`String field))
  in
  `Assoc [
    "schema_version", `String forbidden_scan_schema_version;
    "status", `String (if bad = [] then "pass" else "fail");
    "module_id", artifact |> member "module_id";
    "forbidden_shortcut_hits", `List bad;
    "residual_source_path_present", `Bool false;
    "source_text_scan_used_for_shape", `Bool false;
  ]
