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
  let functions = rows |> List.map (fun row -> string_field "function_name" row) |> sort_uniq in
  let report_json = `Assoc [
    "schema_version", `String "abstract-speculate-residual-api-model-coverage/v1";
    "status", `String "pass";
    "scope", `String "exactly memcpy,strcpy,strlen core memory/string copy/length residual semantics";
    "covered_functions", `List (List.map (fun f -> `String f) functions);
    "unsupported_api_residual_coverage", `List (List.concat_map (list_field "unsupported_api_residual_coverage") logs);
    "coverage_rows", `List rows;
    "negative_cases", `List negatives;
    "non_claims", `List [
      `String "no memmove/strncpy/strcat/strdup/fgets/scanf/allocation API coverage";
      `String "no broad alias or arbitrary-C whole-program equivalence claim";
      `String "strcpy C-level return semantics are not claimed by this checker";
    ];
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report report_json;
  print_endline "abstract_speculate_residual_api_model_check: PASS"
