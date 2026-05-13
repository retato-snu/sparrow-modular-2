let repo_root = ref ".."
let report = ref ""
let module_boundary_report = ref ""
let fact_provenance_report = ref ""
let bta_report = ref ""
let residual_report = ref ""
let forbidden_source_report = ref ""
let source_lineage_report = ref ""

let usage =
  "abstract_speculate_pe_audit --repo-root <path> --report <json> --module-boundary-report <json> --fact-provenance-report <json> --bta-report <json> --residual-report <json> --forbidden-source-report <json> --source-lineage-report <json>"

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let run cmd = Sys.command cmd = 0
let member = Yojson.Safe.Util.member
let status_is_pass json = match member "status" json with `String "pass" -> true | _ -> false
let project_path root rel =
  let nested = Filename.concat (Filename.concat root "sparrow-modular-ocaml") rel in
  if Sys.file_exists nested then nested else Filename.concat root rel

let process_status_string = function
  | Unix.WEXITED n -> Printf.sprintf "exited %d" n
  | Unix.WSIGNALED n -> Printf.sprintf "signaled %d" n
  | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n

let collect_files root =
  let dirs = [project_path root "src"; project_path root "test"; project_path root "doc"] |> List.filter Sys.file_exists in
  let command = Printf.sprintf "find %s -type f \\( -name '*.ml' -o -name '*.mli' -o -name dune -o -name '*.md' \\)" (String.concat " " (List.map Filename.quote dirs)) in
  let ic = Unix.open_process_in command in
  let rec read_lines acc =
    match input_line ic with
    | line -> read_lines (line :: acc)
    | exception End_of_file ->
        begin match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> List.rev acc
        | status -> failwith ("source-claim scan file collection failed: " ^ process_status_string status)
        end
  in
  read_lines []

let is_fixture path = contains path "/test/fixtures/"
let lowercase_ascii = String.lowercase_ascii
let is_nonclaim_line line =
  let l = lowercase_ascii line in
  contains l "no " || contains l "not " || contains l "non-claim" || contains l "out of scope" ||
  contains l "rejected" || contains l "forbidden" || contains l "deferred" || contains l "future" ||
  contains l "must not" || contains l "does not" || contains l "without" || contains l "audit rejects" ||
  contains l "audit must" || contains l "cannot count" || contains l "json-only residuals fail"
let lines data = String.split_on_char '\n' data

let forbidden_claim_needles = [
  "metadata-only PE is accepted";
  "JSON-only PE evidence is accepted";
  "linked facts are allowed pre-link";
  "baseline sparrow/src semantic edits are required";
  "hand-wrapper substitution is accepted";
  "Alarm/Report PE is implemented";
  "PFS staging/ranking parity is implemented";
  "octDom/domain-generic PE is implemented";
  "non-extern residualization is allowed";
  "dynamic-by-default";
]

let hits root needles =
  collect_files root
  |> List.filter (fun path -> not (is_fixture path || contains (Filename.basename path) "_audit.ml"))
  |> List.concat_map (fun path ->
       let data = read_file path in
       needles
       |> List.filter (fun needle -> lines data |> List.exists (fun line -> contains line needle && not (is_nonclaim_line line)))
       |> List.map (fun needle -> `Assoc ["path", `String path; "needle", `String needle]))

let report_pass path = Sys.file_exists path && status_is_pass (Yojson.Safe.from_file path)
let report_status path =
  if not (Sys.file_exists path) then `Assoc ["path", `String path; "exists", `Bool false; "status", `String "missing"]
  else `Assoc ["path", `String path; "exists", `Bool true; "status", Yojson.Safe.from_file path |> member "status"]

let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let bool_field name json = field name json = Some (`Bool true)
let false_field name json = field name json = Some (`Bool false)
let int_field name expected json = field name json = Some (`Int expected)
let int_value = function Some (`Int n) -> n | _ -> 0
let positive_int_field name json = match field name json with Some (`Int n) -> n > 0 | _ -> false
let string_field name expected json = field name json = Some (`String expected)
let list_field name json = match field name json with Some (`List xs) -> xs | _ -> []
let set_field name value = function
  | `Assoc fields -> `Assoc ((name, value) :: List.remove_assoc name fields)
  | json -> json

let remove_field name = function
  | `Assoc fields -> `Assoc (List.remove_assoc name fields)
  | json -> json

let fixtures path =
  if not (Sys.file_exists path) then [] else
  match Yojson.Safe.from_file path |> member "fixtures" with `List xs -> xs | _ -> []

let all_fixtures path pred =
  let xs = fixtures path in
  xs <> [] && List.for_all pred xs

let all_fixture_field path field_name expected =
  all_fixtures path (function
    | `Assoc fields -> List.assoc_opt field_name fields = Some expected
    | _ -> false)

let residual_code_ok json =
  string_field "code_kind" "Trx.code" json &&
  bool_field "typed_code_present" json &&
  bool_field "runcode_executed" json &&
  string_field "execution_relation" "structural-equiv" json

let residual_log_ok log =
  let needs_dynamic_transfer = int_value (field "extern_root_count" log) > 0 in
  bool_field "typed_metaocaml_code_value" log &&
  bool_field "online_runcode_run" log &&
  bool_field "runcode_run_performed" log &&
  false_field "source_string_residual_core" log &&
  false_field "json_metadata_only_proof" log &&
  false_field "wrapper_reuse" log &&
  false_field "linked_facts_prelink" log &&
  false_field "top_substitution" log &&
  false_field "toy_only" log &&
  bool_field "extern_effect_input_valid" log &&
  bool_field "blind_equality_static_projection" log &&
  bool_field "convergence_ignores_residual_code_structure" log &&
  positive_int_field "executed_residual_count" log &&
  list_field "executed_residuals" log <> [] &&
  string_field "residual_runtime_scope" "staged-cell-component-residuals" log &&
  bool_field "staged_domain_fixpoint" log &&
  bool_field "bta_participates_in_fixpoint" log &&
  false_field "stage1_direct_sparse_pipeline" log &&
  false_field "posthoc_row_split_used" log &&
  false_field "row_obligation_residual_source" log &&
  false_field "whole_row_dynamic_wrapper" log &&
  false_field "metadata_only_proof" log &&
  ((not needs_dynamic_transfer) || positive_int_field "transfer_level_d_site_count" log) &&
  ((not needs_dynamic_transfer) || positive_int_field "staged_lattice_event_count" log) &&
  ((not needs_dynamic_transfer) || positive_int_field "typed_residual_arithmetic_count" log) &&
  ((not needs_dynamic_transfer) || positive_int_field "typed_residual_guard_count" log) &&
  false_field "stage2_sparse_recompute" log

let residual_executable_ok entry =
  (string_field "status" "pass" entry || field "status" entry = None) &&
  string_field "artifact_kind" "online-metaocaml-module-residual-analyzer" entry &&
  bool_field "executable_built" entry &&
  bool_field "executable_ran" entry &&
  bool_field "runcode_run_performed" entry &&
  false_field "print_only_shape" entry &&
  bool_field "typed_metaocaml_code_value" entry &&
  (bool_field "blind_equality_static_projection" entry || field "blind_equality_static_projection" entry = None) &&
  (bool_field "convergence_ignores_residual_code_structure" entry || field "convergence_ignores_residual_code_structure" entry = None) &&
  begin match list_field "online_residuals" entry with
  | [] -> false
  | residuals -> List.for_all residual_code_ok residuals
  end &&
  begin match field "execution_log" entry with
  | Some log -> residual_log_ok log
  | None -> false
  end

let residual_report_ok path = all_fixtures path residual_executable_ok

let first_good_residual_log path =
  fixtures path
  |> List.find_map (fun entry ->
       match field "execution_log" entry with
       | Some log when residual_log_ok log -> Some log
       | _ -> None)

let residual_audit_mutation_results path =
  match first_good_residual_log path with
  | None ->
      [`Assoc [
        "name", `String "known_good_residual_log_present";
        "status", `String "fail";
        "detail", `String "no passing residual execution_log found to mutation-test";
      ]]
  | Some log ->
      [
        "online_runcode_run=false", set_field "online_runcode_run" (`Bool false) log;
        "runcode_run_performed=false", set_field "runcode_run_performed" (`Bool false) log;
        "extern_effect_input_valid=false", set_field "extern_effect_input_valid" (`Bool false) log;
        "executed_residual_count=0", set_field "executed_residual_count" (`Int 0) log;
        "stage2_sparse_recompute=true", set_field "stage2_sparse_recompute" (`Bool true) log;
        "executed_residuals missing", remove_field "executed_residuals" log;
        "executed_residuals empty", set_field "executed_residuals" (`List []) log;
        "json_metadata_only_proof=true", set_field "json_metadata_only_proof" (`Bool true) log;
        "metadata_only_proof=true", set_field "metadata_only_proof" (`Bool true) log;
        "top_substitution=true", set_field "top_substitution" (`Bool true) log;
        "whole_row_dynamic_wrapper=true", set_field "whole_row_dynamic_wrapper" (`Bool true) log;
        "linked_facts_prelink=true", set_field "linked_facts_prelink" (`Bool true) log;
      ]
      |> List.map (fun (name, mutated_log) ->
           let rejected = not (residual_log_ok mutated_log) in
           `Assoc [
             "name", `String name;
             "status", `String (if rejected then "pass" else "fail");
             "detail", `String (if rejected then "tampered residual log rejected" else "tampered residual log accepted");
           ])

let bta_no_forbidden path =
  all_fixtures path (function
    | `Assoc fields -> List.assoc_opt "forbidden_dynamic_fact_count" fields = Some (`Int 0)
    | _ -> false)

let boundary_no_linked path =
  all_fixtures path (fun entry ->
    string_field "status" "pass" entry &&
    bool_field "single_module_input" entry &&
    false_field "linked_entrypoints_used" entry &&
    bool_field "module_analysis_before_link" entry &&
    string_field "parser_entrypoint" "Real_sparrow_frontend.parse_one_file" entry &&
    string_field "global_entrypoint" "Real_sparrow_frontend.global_for_module" entry)

let fact_provenance_ok path =
  all_fixtures path (fun entry ->
    string_field "status" "pass" entry &&
    int_field "forbidden_sibling_fact_count" 0 entry)

let forbidden_scan_ok path =
  all_fixtures path (fun entry ->
    string_field "status" "pass" entry &&
    false_field "residual_source_path_present" entry &&
    false_field "source_text_scan_used_for_shape" entry &&
    list_field "forbidden_shortcut_hits" entry = [])

let source_lineage_ok path =
  all_fixtures path (fun entry ->
    let rel_ok =
      string_field "relation" "structural-equiv" entry ||
      (string_field "relation" "⊒" entry && field "comparison_limitation" entry <> Some `Null)
    in
    rel_ok &&
    string_field "module_boundary_status" "pass" entry &&
    string_field "fact_provenance_status" "pass" entry &&
    string_field "bta_status" "pass" entry &&
    match field "residual_executable" entry with
    | Some residual -> residual_executable_ok residual
    | None -> false)

let evidence_item ~name ok detail =
  `Assoc ["name", `String name; "status", `String (if ok then "pass" else "fail"); "detail", `String detail]

let implementation_evidence root =
  let pe_path = project_path root "src/abstract_speculate_pe.ml" in
  let meta_path = project_path root "src/abstract_speculate_meta_sparse.ml" in
  let residual_path = project_path root "src/abstract_speculate_residual_value.ml" in
  let stage_types_path = project_path root "src/abstract_speculate_stage_types.ml" in
  let stage2_path = project_path root "src/abstract_speculate_stage2_input.ml" in
  let pe = if Sys.file_exists pe_path then read_file pe_path else "" in
  let meta = if Sys.file_exists meta_path then read_file meta_path else "" in
  let residual = if Sys.file_exists residual_path then read_file residual_path else "" in
  let stage_types = if Sys.file_exists stage_types_path then read_file stage_types_path else "" in
  let stage2 = if Sys.file_exists stage2_path then read_file stage2_path else "" in
  let checks = [
    evidence_item ~name:"direct_module_frontend"
      (contains meta "Real_sparrow_frontend.global_for_module source" && not (contains meta "global_for_files"))
      "MetaSparse parses one module through global_for_module and contains no pre-link global_for_files call";
    evidence_item ~name:"staged_domain_fixpoint"
      (contains meta "staged_sparse_pipeline ~module_id ~source_file:source ~source_hash spec pre access dug" &&
       contains meta "extern_nodes = extern_dependency_nodes global dug" &&
       contains meta "transfer_level_d_site_count" &&
       contains meta "staged_lattice_event_count" &&
       not (contains meta "direct_sparse_pipeline spec pre access dug") &&
       not (contains meta "SparseItv.perform"))
      "Stage 1 routes production through a staged-domain fixpoint with transfer/lattice D evidence";
    evidence_item ~name:"typed_metaocaml_residual_value"
      (contains residual "Runcode.run analyzer.T.code" &&
       contains stage_types "code : (stage2_input -> stage2_output) Trx.code" &&
       contains stage_types "component_code : (stage2_input -> residual_component_result) Trx.code" &&
       not (contains stage_types "row_code :"))
      "Residual analyzer and each staged component are typed Trx.code values executed by Runcode.run";
    evidence_item ~name:"code_bearing_d_components"
      (contains stage_types "type 'a ps =" &&
       contains stage_types "| D of 'a Trx.code" &&
       contains residual "T.D component.T.component_code" &&
       contains residual "run_components" &&
       contains residual "(.~head .~input_code)")
      "Main residual path wires S/D component code into generated analyzer code instead of reporting metadata only";
    evidence_item ~name:"typed_shape_witness_pairs_code"
      (contains stage_types "Typed_component of staged_residual_component" &&
       contains meta "StageT.Loop_shape" &&
       contains meta "StageT.Branch_shape" &&
       contains pe "paired_executable_code\", `Bool true")
      "Loop/branch witnesses carry typed residual obligations and serialize as paired executable-code witnesses";
    evidence_item ~name:"blind_equality_generation_gate"
      (contains residual "Blind.convergence_witness left right" &&
       contains residual "if not (Blind.equal_static_projection left right) then" &&
       contains stage2 "blind_equality_witness")
      "Residual analyzer generation gates residual-code growth with static-projection blind equality";
    evidence_item ~name:"online_stage2_contract"
      (contains stage2 "extern_effect_input_valid" &&
       contains stage2 "execute_staged_component" &&
       contains stage2 "executed_residuals" &&
       contains stage2 "source_string_residual_core" &&
       contains stage2 "json_metadata_only_proof")
      "Stage 2 validates external obligations, executes spliced component/control code, and records negative shortcut flags";
    evidence_item ~name:"no_sparse_wrapper_delegation"
      (not (contains pe "Real_sparrow_sparse_fixpoint_pe.artifact_for_module"))
      "Abstract Speculate entrypoint does not hand off to the old sparse-fixpoint wrapper";
  ] in
  let ok = List.for_all status_is_pass checks in
  checks, ok

let () =
  Arg.parse
    ["--repo-root", Arg.Set_string repo_root, "repository root";
     "--report", Arg.Set_string report, "aggregate audit report path";
     "--module-boundary-report", Arg.Set_string module_boundary_report, "module boundary report";
     "--fact-provenance-report", Arg.Set_string fact_provenance_report, "fact provenance report";
     "--bta-report", Arg.Set_string bta_report, "BTA report";
     "--residual-report", Arg.Set_string residual_report, "residual report";
     "--forbidden-source-report", Arg.Set_string forbidden_source_report, "forbidden source scan report";
     "--source-lineage-report", Arg.Set_string source_lineage_report, "source lineage report"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !report = "" || !module_boundary_report = "" || !fact_provenance_report = "" || !bta_report = "" || !residual_report = "" || !forbidden_source_report = "" || !source_lineage_report = "" then failwith usage;
  let root = !repo_root in
  let sparrow_root = Filename.concat root "../sparrow" in
  let semantic_clean =
    run (Printf.sprintf "git -C %s diff --exit-code -- src >/dev/null" (Filename.quote sparrow_root)) &&
    run (Printf.sprintf "test -z \"$(git -C %s status --porcelain -- src)\"" (Filename.quote sparrow_root))
  in
  let required_docs = [
    Filename.concat root "../.omx/plans/plan-abstract-speculate-metaocaml-retry.md";
    Filename.concat root "../.omx/plans/prd-abstract-speculate-metaocaml-retry.md";
    Filename.concat root "../.omx/plans/test-spec-abstract-speculate-metaocaml-retry.md";
    Filename.concat root "../.omx/research/abstract-speculate-metaocaml-pe-archive.md";
    Filename.concat root "../Doc/FOUNDATIONS.md";
    Filename.concat root "../Doc/METAOCAML_CONSTRAINTS.md";
    Filename.concat root "../Doc/METAOCAML_REFERENCE.md";
    project_path root "doc/adr/ADR-0006-abstract-speculate-metaocaml-sparse-proof-gate.md";
  ] in
  let docs_present = List.for_all Sys.file_exists required_docs in
  let missing_docs = required_docs |> List.filter (fun path -> not (Sys.file_exists path)) |> List.map (fun path -> `String path) in
  let report_paths = [!module_boundary_report; !fact_provenance_report; !bta_report; !residual_report; !forbidden_source_report; !source_lineage_report] in
  let reports_pass = List.for_all report_pass report_paths in
  let boundary_ok = boundary_no_linked !module_boundary_report in
  let fact_ok = fact_provenance_ok !fact_provenance_report in
  let bta_ok = bta_no_forbidden !bta_report in
  let residual_ok = residual_report_ok !residual_report in
  let residual_audit_mutation_results = residual_audit_mutation_results !residual_report in
  let residual_audit_mutation_ok = List.for_all status_is_pass residual_audit_mutation_results in
  let forbidden_ok = forbidden_scan_ok !forbidden_source_report in
  let lineage_ok = source_lineage_ok !source_lineage_report in
  let forbidden_hits = hits root forbidden_claim_needles in
  let implementation_evidence, implementation_ok = implementation_evidence root in
  let ok = semantic_clean && docs_present && reports_pass && boundary_ok && fact_ok && bta_ok && residual_ok && residual_audit_mutation_ok && forbidden_ok && lineage_ok && implementation_ok && forbidden_hits = [] in
  let json = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Abstract_speculate_pe.audit_schema_version;
    "status", `String (if ok then "pass" else "fail");
    "baseline_semantic_clean", `Bool semantic_clean;
    "docs_present", `Bool docs_present;
    "missing_docs", `List missing_docs;
    "reports", `List (List.map report_status report_paths);
    "module_boundary_prelink_only", `Bool boundary_ok;
    "fact_provenance_module_local", `Bool fact_ok;
    "bta_forbidden_dynamic_fact_count_zero", `Bool bta_ok;
    "residual_metaocaml_runcode_verified", `Bool residual_ok;
    "residual_audit_mutation_gates", `Bool residual_audit_mutation_ok;
    "residual_audit_mutation_cases", `List residual_audit_mutation_results;
    "forbidden_source_scan_clean", `Bool forbidden_ok;
    "source_lineage_accepted_relation", `Bool lineage_ok;
    "implementation_evidence_ok", `Bool implementation_ok;
    "implementation_evidence", `List implementation_evidence;
    "forbidden_claim_hits", `List forbidden_hits;
    "claim", `String "Abstract Speculate module-only PE proof gate with typed online MetaOCaml residual analyzer evidence";
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if ok then print_endline ("PASS " ^ !report) else failwith ("Abstract Speculate PE audit failed; see " ^ !report)
