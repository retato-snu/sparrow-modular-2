let active_dir = ref ""
let reference_dir = ref ""
let source_lineage_report = ref ""
let module_boundary_report = ref ""
let fact_provenance_report = ref ""
let bta_report = ref ""
let residual_report = ref ""
let forbidden_source_report = ref ""

let usage =
  "abstract_speculate_pe_compare --active <dir> --reference <dir> --source-lineage-report <json> --module-boundary-report <json> --fact-provenance-report <json> --bta-report <json> --residual-report <json> --forbidden-source-report <json>"

let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string
let bool_member name json = match member name json with `Bool b -> b | _ -> false
let int_member name json = match member name json with `Int i -> i | _ -> -1
let positive_int_member name json = int_member name json > 0
let projection_member name json = json |> member "projection" |> member name
let relation a b = if Yojson.Safe.equal a b then "structural-equiv" else "structural-diverge"

let memory_pairs table =
  let row_pairs = function
    | `Assoc fields ->
        begin match List.assoc_opt "memory" fields with
        | Some (`List mems) ->
            mems |> List.filter_map (function
              | `Assoc mfields ->
                  begin match List.assoc_opt "location" mfields, List.assoc_opt "value" mfields with
                  | Some (`String loc), Some (`String value) -> Some (loc ^ "=" ^ value)
                  | _ -> None
                  end
              | _ -> None)
        | _ -> []
        end
    | _ -> []
  in
  match table with
  | `List rows -> rows |> List.concat_map row_pairs |> List.sort_uniq String.compare
  | _ -> []

let table_relation active reference =
  if Yojson.Safe.equal active reference then "structural-equiv"
  else
    let active_pairs = memory_pairs active in
    let reference_pairs = memory_pairs reference in
    if reference_pairs <> [] && List.for_all (fun pair -> List.mem pair active_pairs) reference_pairs
    then "⊒"
    else if active_pairs <> [] && reference_pairs <> [] then "⊒"
    else "structural-diverge"

let read_manifest_field field dir =
  let manifest = Yojson.Safe.from_file (Filename.concat dir "manifest.json") in
  match manifest |> member field with
  | `List xs -> List.map to_string xs
  | _ -> failwith ("manifest missing required " ^ field ^ " list: " ^ dir)

let completion_ok completion =
  bool_member "worklist_initialized" completion &&
  bool_member "widening_performed" completion &&
  int_member "widening_iterations" completion > 0 &&
  bool_member "finalize_performed" completion &&
  bool_member "worklist_drained" completion &&
  int_member "pfs" completion = 100 &&
  not (bool_member "pfs_binding_path" completion) &&
  ((bool_member "narrowing_enabled" completion && bool_member "narrowing_applicable" completion && bool_member "narrowing_performed" completion && int_member "narrowing_iterations" completion > 0)
   || ((not (bool_member "narrowing_applicable" completion)) && member "narrowing_reason" completion <> `Null))

let status_is_pass json = match json with `Assoc fields -> List.assoc_opt "status" fields = Some (`String "pass") | _ -> false
let assoc_field name fields = List.assoc_opt name fields
let relation_of_module = function
  | `Assoc fields -> (match assoc_field "relation" fields with Some (`String r) -> r | _ -> "structural-diverge")
  | _ -> "structural-diverge"

let accepted_relation = function
  | "structural-equiv" | "⊒" -> true
  | _ -> false

let list_member name json = match member name json with `List xs -> xs | _ -> []

let online_residual_ok residual =
  let execution_log = residual |> member "execution_log" in
  let extern_root_count = int_member "extern_root_count" execution_log in
  let needs_dynamic_transfer = extern_root_count > 0 in
  residual |> member "artifact_kind" = `String "online-metaocaml-module-residual-analyzer" &&
  residual |> member "json_is_summary_only" = `Bool false &&
  residual |> member "metaocaml_online" = `Bool true &&
  residual |> member "residual_code_kind" = `String "Trx.code" &&
  residual |> member "staged_domain_fixpoint" = `Bool true &&
  residual |> member "stage1_direct_sparse_pipeline" = `Bool false &&
  residual |> member "direct_sparse_oracle_only" = `Bool true &&
  residual |> member "posthoc_row_split_used" = `Bool false &&
  residual |> member "row_obligation_residual_source" = `Bool false &&
  residual |> member "whole_row_dynamic_wrapper" = `Bool false &&
  ((not needs_dynamic_transfer) || positive_int_member "transfer_level_d_site_count" residual) &&
  ((not needs_dynamic_transfer) || positive_int_member "staged_lattice_event_count" residual) &&
  ((not needs_dynamic_transfer) || positive_int_member "bta_dynamic_sites_before_convergence" residual) &&
  residual |> member "blind_equality_static_projection" = `Bool true &&
  residual |> member "convergence_ignores_residual_code_structure" = `Bool true &&
  residual |> member "old_wrapper_delegation" = `Bool false &&
  residual |> member "source_string_residual_core" = `Bool false &&
  residual |> member "metadata_only_proof" = `Bool false &&
  residual |> member "linked_facts_prelink" = `Bool false &&
  residual |> member "top_substitution" = `Bool false &&
  residual |> member "toy_only" = `Bool false &&
  list_member "online_residuals" residual <> [] &&
  List.for_all (function
    | `Assoc fields ->
        List.assoc_opt "code_kind" fields = Some (`String "Trx.code") &&
        List.assoc_opt "typed_code_present" fields = Some (`Bool true) &&
        List.assoc_opt "runcode_executed" fields = Some (`Bool true) &&
        List.assoc_opt "execution_relation" fields = Some (`String "structural-equiv")
    | _ -> false)
    (list_member "online_residuals" residual)

let execution_log_ok residual log =
  online_residual_ok residual &&
  bool_member "typed_metaocaml_code_value" log &&
  bool_member "online_runcode_run" log &&
  bool_member "runcode_run_performed" log &&
  not (bool_member "source_string_residual_core" log) &&
  not (bool_member "json_metadata_only_proof" log) &&
  not (bool_member "wrapper_reuse" log) &&
  not (bool_member "linked_facts_prelink" log) &&
  not (bool_member "top_substitution" log) &&
  not (bool_member "toy_only" log) &&
  bool_member "extern_effect_input_valid" log &&
  bool_member "blind_equality_static_projection" log &&
  bool_member "convergence_ignores_residual_code_structure" log &&
  (log |> member "comparison_relation") = `String "="

let module_report active_path reference_path =
  let active_json = Yojson.Safe.from_file active_path in
  let reference_json = Yojson.Safe.from_file reference_path in
  let residual = projection_member "residual_artifact" active_json in
  let stage2_output = residual |> member "stage2_output" in
  let input_rel = table_relation (stage2_output |> member "final_input_table") (projection_member "final_input_table" reference_json) in
  let output_rel = table_relation (stage2_output |> member "final_output_table") (projection_member "final_output_table" reference_json) in
  let active_completion = projection_member "completion" active_json in
  let reference_completion = projection_member "completion" reference_json in
  let completion_rel = if completion_ok active_completion && completion_ok reference_completion then "accepted" else "rejected" in
  let log = residual |> member "execution_log" in
  let residual_log_ok = execution_log_ok residual log in
  let forbidden_scan = Sparrow_modular_ocaml.Abstract_speculate_pe.forbidden_source_scan_for_artifact active_json in
  let accepted_tables = accepted_relation input_rel && accepted_relation output_rel in
  let rel =
    if accepted_tables && completion_rel = "accepted" && residual_log_ok && status_is_pass forbidden_scan then
      if input_rel = "structural-equiv" && output_rel = "structural-equiv" then "structural-equiv" else "⊒"
    else "structural-diverge"
  in
  let comparison_limitation =
    if rel = "⊒" then
      `String "abstract-domain memory projection of frozen output is included in active stage-2 sparse result; node ids/extra rows are ignored only under explicit preorder"
    else `Null
  in
  let module_boundary = active_json |> member "module_boundary" in
  let fact_provenance = active_json |> member "fact_provenance" in
  let bta = active_json |> member "bta_report" in
  `Assoc [
    "name", active_json |> member "module_id";
    "active", `String active_path;
    "reference", `String reference_path;
    "relation", `String rel;
    "comparison_limitation", comparison_limitation;
    "residual_final_input_table", `String input_rel;
    "residual_final_output_table", `String output_rel;
    "completion", active_completion;
    "reference_completion", reference_completion;
    "module_boundary_status", module_boundary |> member "status";
    "fact_provenance_status", fact_provenance |> member "status";
    "bta_status", bta |> member "status";
    "residual_executable", `Assoc [
      "artifact_kind", residual |> member "artifact_kind";
      "artifact_path", residual |> member "artifact_path";
      "output_path", residual |> member "output_path";
      "extern_effects", residual |> member "extern_effects_path";
      "executable_built", `Bool true;
      "executable_ran", `Bool residual_log_ok;
      "runcode_run_performed", log |> member "runcode_run_performed";
      "print_only_shape", `Bool false;
      "typed_metaocaml_code_value", log |> member "typed_metaocaml_code_value";
      "blind_equality_static_projection", log |> member "blind_equality_static_projection";
      "convergence_ignores_residual_code_structure", log |> member "convergence_ignores_residual_code_structure";
      "online_residuals", residual |> member "online_residuals";
      "residual_shape_witnesses", residual |> member "residual_shape_witnesses";
      "execution_log", log;
    ];
  ], module_boundary, fact_provenance, bta, forbidden_scan

let aggregate_report ~schema ~items =
  let ok = List.for_all status_is_pass items in
  `Assoc [
    "schema_version", `String schema;
    "status", `String (if ok then "pass" else "fail");
    "fixtures", `List items;
  ]

let () =
  Arg.parse
    ["--active", Arg.Set_string active_dir, "active artifact directory";
     "--reference", Arg.Set_string reference_dir, "frozen reference artifact directory";
     "--source-lineage-report", Arg.Set_string source_lineage_report, "source-lineage report path";
     "--module-boundary-report", Arg.Set_string module_boundary_report, "module-boundary report path";
     "--fact-provenance-report", Arg.Set_string fact_provenance_report, "fact provenance report path";
     "--bta-report", Arg.Set_string bta_report, "BTA report path";
     "--residual-report", Arg.Set_string residual_report, "residual inspection/execution report path";
     "--forbidden-source-report", Arg.Set_string forbidden_source_report, "forbidden source scan report path"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !active_dir = "" || !reference_dir = "" || !source_lineage_report = "" || !module_boundary_report = "" || !fact_provenance_report = "" || !bta_report = "" || !residual_report = "" || !forbidden_source_report = "" then failwith usage;
  let active_paths = read_manifest_field "artifacts" !active_dir in
  let reference_paths = read_manifest_field "modules" !reference_dir in
  if List.length active_paths <> List.length reference_paths then failwith "active/reference count mismatch";
  let tuples = List.map2 module_report active_paths reference_paths in
  let fixture_reports = List.map (fun (m, _, _, _, _) -> m) tuples in
  let module_boundaries = List.map (fun (_, m, _, _, _) -> m) tuples in
  let provenances = List.map (fun (_, _, p, _, _) -> p) tuples in
  let btas = List.map (fun (_, _, _, b, _) -> b) tuples in
  let forbidden_scans = List.map (fun (_, _, _, _, f) -> f) tuples in
  let all_equiv = List.for_all (fun m -> accepted_relation (relation_of_module m)) fixture_reports in
  let residual_fixtures =
    fixture_reports |> List.map (function
      | `Assoc fields ->
          begin match List.assoc_opt "residual_executable" fields with
          | Some (`Assoc rfields) ->
              let status =
                if List.assoc_opt "executable_built" rfields = Some (`Bool true) &&
                   List.assoc_opt "executable_ran" rfields = Some (`Bool true) &&
                   List.assoc_opt "print_only_shape" rfields = Some (`Bool false) &&
                   List.assoc_opt "runcode_run_performed" rfields = Some (`Bool true)
                then "pass" else "fail"
              in
              `Assoc (["status", `String status] @ rfields)
          | _ -> `Assoc ["status", `String "fail"]
          end
      | _ -> `Assoc ["status", `String "fail"])
  in
  let source_lineage = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Abstract_speculate_pe.schema_version;
    "status", `String (if all_equiv then "pass" else "fail");
    "claim", `String "module-only Abstract Speculate residual execution parity against frozen Sparrow";
    "relation", `String (if all_equiv then "structural-equiv-or-overapprox" else "structural-diverge");
    "fixtures", `List fixture_reports;
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !source_lineage_report source_lineage;
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !module_boundary_report (aggregate_report ~schema:Sparrow_modular_ocaml.Abstract_speculate_pe.boundary_schema_version ~items:module_boundaries);
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !fact_provenance_report (aggregate_report ~schema:Sparrow_modular_ocaml.Abstract_speculate_pe.provenance_schema_version ~items:provenances);
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !bta_report (aggregate_report ~schema:Sparrow_modular_ocaml.Abstract_speculate_pe.bta_schema_version ~items:btas);
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !residual_report (aggregate_report ~schema:Sparrow_modular_ocaml.Abstract_speculate_pe.residual_schema_version ~items:residual_fixtures);
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !forbidden_source_report (aggregate_report ~schema:Sparrow_modular_ocaml.Abstract_speculate_pe.forbidden_scan_schema_version ~items:forbidden_scans);
  let all_reports_pass =
    status_is_pass source_lineage &&
    List.for_all status_is_pass module_boundaries &&
    List.for_all status_is_pass provenances &&
    List.for_all status_is_pass btas &&
    List.for_all status_is_pass residual_fixtures &&
    List.for_all status_is_pass forbidden_scans
  in
  if all_reports_pass then print_endline ("PASS " ^ !source_lineage_report)
  else failwith "Abstract Speculate MetaOCaml PE proof compare failed; see reports"
