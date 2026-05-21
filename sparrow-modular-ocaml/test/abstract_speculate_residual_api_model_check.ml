let repo_root = ref "."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "abstract_speculate_residual_api_model_check --repo-root <repo> --artifact-dir <dir> --report <json>"

let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string

let expect cond msg = if not cond then failwith msg

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> s | _ -> ""
let int_field name json = match assoc_field name json with Some (`Int n) -> n | _ -> 0
let bool_field name json = match assoc_field name json with Some (`Bool b) -> b | _ -> false
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let sort_uniq xs = List.sort_uniq String.compare xs
let required_functions = ["memcpy"; "strcpy"; "strlen"]

let rec set_path path value json =
  match path, json with
  | [], _ -> value
  | key :: rest, `Assoc fields ->
      let old = match List.assoc_opt key fields with Some v -> v | None -> `Assoc [] in
      `Assoc ((key, set_path rest value old) :: List.remove_assoc key fields)
  | _ :: _, other -> other

let artifact_paths manifest = list_field "artifacts" manifest |> List.map to_string
let residual_artifact artifact = artifact |> member "projection" |> member "residual_artifact"
let execution_logs artifacts = List.map (fun artifact -> residual_artifact artifact |> member "execution_log") artifacts
let api_rows artifacts =
  execution_logs artifacts
  |> List.concat_map (fun log -> list_field "api_residual_model_coverage" log)

let json_string_list json = list_field "semantically_upstream_dependencies" json |> List.map to_string

let baseline_ok function_name baseline =
  function_name <> "" &&
  contains baseline "sparrow-modular-ocaml/src/itvSem.ml" &&
  contains baseline "sparrow-modular-ocaml/src/apiSem.ml"

let effect_ok function_name abstract_effect =
  abstract_effect <> "" &&
  not (contains abstract_effect "unsupported") &&
  match function_name with
  | "memcpy" -> contains abstract_effect "copied" || contains abstract_effect "copy"
  | "strcpy" -> contains abstract_effect "null-position" || contains abstract_effect "copied"
  | "strlen" -> contains abstract_effect "length" || contains abstract_effect "null-position"
  | _ -> false

let row_ok row =
  let function_name = string_field "function_name" row in
  let target = string_field "residual_cell_target" row in
  let deps = json_string_list row in
  List.mem function_name required_functions &&
  baseline_ok function_name (string_field "baseline_source" row) &&
  effect_ok function_name (string_field "abstract_effect" row) &&
  string_field "residual_equation_id" row <> "" &&
  target <> "" &&
  deps <> [] &&
  List.exists (fun dep -> dep <> target) deps &&
  bool_field "stage2_seed_input_read" row &&
  int_field "solver_state_read_count" row > 0 &&
  string_field "final_cell_provenance" row = "residual-api-model-coverage/slice-1" &&
  string_field "positive_fixture" row <> "" &&
  string_field "negative_mutation" row <> "" &&
  (bool_field "unsupported_api_coverage" row = false)

let coverage_ok rows =
  let functions = rows |> List.map (fun row -> string_field "function_name" row) |> sort_uniq in
  functions = required_functions &&
  List.for_all (fun f -> List.exists (fun row -> string_field "function_name" row = f && row_ok row) rows) required_functions &&
  List.for_all row_ok rows

let rows_for function_name rows =
  List.filter (fun row -> string_field "function_name" row = function_name) rows

let negative_status name negatives =
  List.exists
    (fun json -> string_field "name" json = name && string_field "status" json = "pass")
    negatives

let api_selected_observation_negative_passes negatives =
  List.for_all
    (fun name -> negative_status name negatives)
    [
      "empty_exact_dependencies_rejected";
      "target_self_only_dependencies_rejected";
      "metadata_only_api_sem_coverage_rejected";
      "corrupted_api_provenance_rejected";
    ]

let api_model_function_summary rows negatives function_name =
  let function_rows = rows_for function_name rows in
  let row_count = List.length function_rows in
  let valid_rows = function_rows <> [] && List.for_all row_ok function_rows in
  let final_cell_coverage =
    valid_rows &&
    List.for_all
      (fun row ->
        string_field "residual_cell_target" row <> "" &&
        string_field "final_cell_provenance" row = "residual-api-model-coverage/slice-1")
      function_rows
  in
  let typed_metadata_consistency =
    valid_rows &&
    List.for_all
      (fun row ->
        baseline_ok function_name (string_field "baseline_source" row) &&
        effect_ok function_name (string_field "abstract_effect" row) &&
        string_field "residual_equation_id" row <> "" &&
        string_field "positive_fixture" row <> "" &&
        string_field "negative_mutation" row <> "")
      function_rows
  in
  let source_rerun_evidence =
    valid_rows &&
    List.for_all
      (fun row -> bool_field "stage2_seed_input_read" row && int_field "solver_state_read_count" row > 0)
      function_rows
  in
  let selected_observation_relation =
    valid_rows &&
    List.for_all
      (fun row ->
        let target = string_field "residual_cell_target" row in
        let deps = json_string_list row in
        deps <> [] && List.exists (fun dep -> dep <> target) deps)
      function_rows
  in
  let selected_observation_masking_negative =
    api_selected_observation_negative_passes negatives
  in
  let unsupported_api_coverage_rejected =
    negative_status "unsupported_optional_api_not_covered_rejected" negatives &&
    List.for_all (fun row -> bool_field "unsupported_api_coverage" row = false) function_rows
  in
  let pass =
    final_cell_coverage &&
    typed_metadata_consistency &&
    source_rerun_evidence &&
    selected_observation_relation &&
    selected_observation_masking_negative &&
    unsupported_api_coverage_rejected
  in
  `Assoc [
    "function_name", `String function_name;
    "semantic_path", `String ("api-model-" ^ function_name);
    "status", `String (if pass then "pass" else "fail");
    "claim_scope", `String "sparrow-itv-supported-api-fixture-evidence-only";
    "row_count", `Int row_count;
    "final_cell_coverage", `Bool final_cell_coverage;
    "typed_metadata_consistency", `Bool typed_metadata_consistency;
    "source_rerun_evidence", `Bool source_rerun_evidence;
    "selected_observation_relation", `Bool selected_observation_relation;
    "selected_observation_masking_negative", `Bool selected_observation_masking_negative;
    "unsupported_api_coverage_rejected", `Bool unsupported_api_coverage_rejected;
  ]

let summary_status = function
  | `Assoc fields -> (match List.assoc_opt "status" fields with Some (`String s) -> s | _ -> "fail")
  | _ -> "fail"

let api_model_summary rows negatives =
  let coverage = List.map (api_model_function_summary rows negatives) required_functions in
  let pass = List.for_all (fun summary -> summary_status summary = "pass") coverage in
  `Assoc [
    "schema_version", `String "abstract-speculate-residual-api-model-summary/v1";
    "status", `String (if pass then "pass" else "fail");
    "claim_boundary", `String "fixture-evidence-only-no-arbitrary-c-no-oct-no-general-taint-product-parity";
    "required_semantic_paths", `List (List.map (fun f -> `String ("api-model-" ^ f)) required_functions);
    "coverage", `List coverage;
  ]

let api_semantic_universe_manifest rows negatives =
  let summary = api_model_summary rows negatives in
  let coverage = list_field "coverage" summary in
  `Assoc [
    "schema_version", `String "abstract-speculate-api-model-semantic-universe-manifest/v1";
    "status", `String (string_field "status" summary);
    "claim_boundary", `String "fixture-evidence-only-no-arbitrary-c-no-oct-no-general-taint-product-parity";
    "semantic_paths", member "required_semantic_paths" summary;
    "required_evidence", `List [
      `String "final_cell_coverage";
      `String "typed_metadata_consistency";
      `String "source_rerun_evidence";
      `String "selected_observation_masking_negative";
      `String "unsupported_api_coverage_rejected";
    ];
    "coverage", `List coverage;
    "negative_cases", `List negatives;
    "unsupported_semantics", `List [
      `String "memmove";
      `String "strncpy";
      `String "strcat";
      `String "strdup";
      `String "fgets";
      `String "scanf";
      `String "allocation APIs";
    ];
  ]

let false_case label rows mutate =
  let mutated = mutate rows in
  `Assoc [
    "name", `String label;
    "status", `String (if not (coverage_ok mutated) then "pass" else "fail");
  ]

let mutate_function function_name path value rows =
  rows |> List.map (fun row -> if string_field "function_name" row = function_name then set_path path value row else row)

let negative_cases rows =
  [
    false_case "missing_solver_state_read_rejected" rows
      (mutate_function "strlen" ["solver_state_read_count"] (`Int 0));
    false_case "missing_seed_read_rejected" rows
      (mutate_function "strlen" ["stage2_seed_input_read"] (`Bool false));
    false_case "empty_exact_dependencies_rejected" rows
      (mutate_function "strlen" ["semantically_upstream_dependencies"] (`List []));
    false_case "target_self_only_dependencies_rejected" rows
      (fun rows ->
        rows |> List.map (fun row ->
          if string_field "function_name" row = "strlen" then
            set_path ["semantically_upstream_dependencies"]
              (`List [`String (string_field "residual_cell_target" row)]) row
          else row));
    false_case "metadata_only_api_sem_coverage_rejected" rows
      (mutate_function "strlen" ["residual_equation_id"] (`String ""));
    false_case "unsupported_optional_api_not_covered_rejected" rows
      (fun rows ->
        (`Assoc [
          "function_name", `String "memmove";
          "baseline_source", `String "sparrow-modular-ocaml/src/apiSem.ml:unsupported";
          "abstract_effect", `String "unsupported optional API";
          "residual_equation_id", `String "metadata-only";
          "residual_cell_target", `String "memmove-target";
          "semantically_upstream_dependencies", `List [`String "memmove-target"];
          "stage2_seed_input_read", `Bool true;
          "solver_state_read_count", `Int 1;
          "final_cell_provenance", `String "residual-api-model-coverage/slice-1";
          "positive_fixture", `String "unsupported";
          "negative_mutation", `String "unsupported optional API accepted as covered";
          "unsupported_api_coverage", `Bool false;
        ]) :: rows);
    false_case "corrupted_api_provenance_rejected" rows
      (mutate_function "strlen" ["final_cell_provenance"] (`String "metadata-only-api-sem"));
    false_case "baseline_source_corruption_rejected" rows
      (mutate_function "strlen" ["baseline_source"] (`String "sparrow-modular-ocaml/src/apiSem.ml:metadata-only"));
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
  let paths = artifact_paths manifest in
  expect (paths <> []) "manifest missing artifacts";
  List.iter (fun path -> expect (Sys.file_exists path) ("missing artifact: " ^ path)) paths;
  let artifacts = List.map Yojson.Safe.from_file paths in
  let logs = execution_logs artifacts in
  expect (List.exists (bool_field "residual_solver_run") logs) "residual solver did not run";
  expect (List.fold_left (fun acc log -> acc + int_field "state_read_count" log) 0 logs > 0) "state reads missing";
  expect (List.fold_left (fun acc log -> acc + int_field "seed_input_read_count" log) 0 logs > 0) "seed reads missing";
  expect (List.concat_map (list_field "exact_cell_dependencies") logs <> []) "exact dependencies missing";
  let rows = api_rows artifacts in
  expect (coverage_ok rows) "residual API model coverage rows failed semantic-chain checks";
  let negatives = negative_cases rows in
  expect (List.for_all (fun n -> string_field "status" n = "pass") negatives)
    "at least one residual API negative mutation was not rejected";
  let api_model_summary = api_model_summary rows negatives in
  expect (string_field "status" api_model_summary = "pass")
    "residual API model summary failed final-cell/source-rerun/negative gates";
  let semantic_universe_manifest = api_semantic_universe_manifest rows negatives in
  expect (string_field "status" semantic_universe_manifest = "pass")
    "residual API semantic-universe manifest failed";
  let functions = rows |> List.map (fun row -> string_field "function_name" row) |> sort_uniq in
  let report_json = `Assoc [
    "schema_version", `String "abstract-speculate-residual-api-model-coverage/v1";
    "status", `String "pass";
    "scope", `String "exactly memcpy,strcpy,strlen core memory/string copy/length residual semantics";
    "covered_functions", `List (List.map (fun f -> `String f) functions);
    "unsupported_api_residual_coverage", `List (List.concat_map (list_field "unsupported_api_residual_coverage") logs);
    "coverage_rows", `List rows;
    "api_model_summary", api_model_summary;
    "semantic_universe_manifest", semantic_universe_manifest;
    "negative_cases", `List negatives;
    "non_claims", `List [
      `String "no memmove/strncpy/strcat/strdup/fgets/scanf/allocation API coverage";
      `String "no broad alias or arbitrary-C whole-program equivalence claim";
      `String "strcpy C-level return semantics are not claimed by this checker";
    ];
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report report_json;
  print_endline "abstract_speculate_residual_api_model_check: PASS"
