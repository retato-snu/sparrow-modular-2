let active_dir = ref ""
let reference_dir = ref ""
let report = ref ""
let bta_report = ref ""
let residual_report = ref ""
let usage = "real_sparrow_staged_linking_pe_compare --active <dir> --reference <dir> --report <json> --bta-report <json> --residual-report <json>"

let member = Yojson.Safe.Util.member
let to_list = Yojson.Safe.Util.to_list
let to_string = Yojson.Safe.Util.to_string
let bool_member name json = match member name json with `Bool b -> b | _ -> false
let int_member name json = match member name json with `Int i -> i | _ -> -1
let projection_member name json = json |> member "projection" |> member name
let relation a b = if Yojson.Safe.equal a b then "structural-equiv" else "structural-diverge"
let global_projection summary =
  `Assoc [
    "procedures", summary |> member "procedures";
    "global", summary |> member "global";
  ]

let read_manifest dir =
  Yojson.Safe.from_file (Filename.concat dir "manifest.json")
  |> member "artifacts" |> to_list |> List.map to_string

let run cmd = Sys.command cmd = 0
let quote = Filename.quote

let build_and_run_residual residual =
  let source_path = residual |> member "source_path" |> to_string in
  let executable_path = residual |> member "executable_path" |> to_string in
  let output_path = residual |> member "output_path" |> to_string in
  let extern_effects_path = residual |> member "extern_effects_path" |> to_string in
  let forbidden = ["Frontend.parse"; "Mergecil.merge"; "Global.init"; "SparseItv.perform"; "SparseAnalysis.perform"] in
  let required_runtime_markers =
    ["recompute_residual_closure";
     "overlay_table";
     "staged_input_rows";
     "residual_input_rows";
     "runtime_recomposition_performed";
     "print_only_residual";
     "read_file";
     "extern_effect_input_valid"]
  in
  let source =
    let ic = open_in source_path in
    let len = in_channel_length ic in
    let data = really_input_string ic len in
    close_in ic; data
  in
  let contains s sub =
    let len = String.length s and sub_len = String.length sub in
    let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
    sub_len = 0 || loop 0
  in
  let forbidden_hits = List.filter (contains source) forbidden in
  let missing_runtime_markers = List.filter (fun marker -> not (contains source marker)) required_runtime_markers in
  let print_only_shape = contains source "let () = print_endline " && not (contains source "overlay_table") in
  let extern_input_present = Sys.file_exists extern_effects_path in
  let built =
    forbidden_hits = [] && missing_runtime_markers = [] && not print_only_shape && extern_input_present &&
    run (Printf.sprintf "ocamlc -o %s %s" (quote executable_path) (quote source_path))
  in
  let ran = built && run (Printf.sprintf "%s %s > %s" (quote executable_path) (quote extern_effects_path) (quote output_path)) in
  let output = if ran then Yojson.Safe.from_file output_path else `Null in
  built, ran, forbidden_hits, missing_runtime_markers, print_only_shape, extern_input_present, extern_effects_path, output

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

let assoc_field name fields = List.assoc_opt name fields
let string_field name fields = match assoc_field name fields with Some (`String s) -> Some s | _ -> None
let bool_field name fields = match assoc_field name fields with Some (`Bool b) -> Some b | _ -> None
let list_field name fields = match assoc_field name fields with Some (`List xs) -> xs | _ -> []

let facts_of artifact = match projection_member "bta" artifact with `List xs -> xs | _ -> []
let entries_of artifact = match projection_member "final_table_entries" artifact with `List xs -> xs | _ -> []

let valid_bta_fact = function
  | `Assoc fields ->
      begin match string_field "classification" fields, string_field "reason" fields, bool_field "depends_on_extern" fields with
      | Some "residual-extern-dependent", Some reason, Some true -> List.mem reason ["unknown-extern-call"; "transitively-extern-dependent"] && list_field "dependency_path" fields <> []
      | Some "static-precomputed", _, Some false -> list_field "dependency_path" fields = []
      | _ -> false
      end
  | _ -> false

let valid_entry = function
  | `Assoc fields ->
      begin match string_field "origin" fields, string_field "classification" fields, bool_field "depends_on_extern" fields with
      | Some "staged-static-table", Some "static-precomputed", Some false -> list_field "dependency_path" fields = []
      | Some "residual-extern-closure", Some "residual-extern-dependent", Some true -> list_field "dependency_path" fields <> []
      | _ -> false
      end
  | _ -> false

let fixture_bta_report path artifact =
  let facts = facts_of artifact in
  let entries = entries_of artifact in
  let bad = List.filter (fun fact -> not (valid_bta_fact fact)) facts in
  let bad_entries = List.filter (fun entry -> not (valid_entry entry)) entries in
  let roots =
    facts |> List.filter_map (function
      | `Assoc fields when string_field "reason" fields = Some "unknown-extern-call" -> string_field "node" fields
      | _ -> None)
    |> List.sort_uniq String.compare
  in
  let residual_count = entries |> List.fold_left (fun n -> function `Assoc f when string_field "origin" f = Some "residual-extern-closure" -> n + 1 | _ -> n) 0 in
  let forbidden_dynamic_fact_count = List.length bad + List.length bad_entries in
  `Assoc [
    "name", `String (artifact |> member "group" |> to_string);
    "path", `String path;
    "extern_roots", `List (List.map (fun r -> `String r) roots);
    "residual_fact_count", `Int residual_count;
    "forbidden_dynamic_fact_count", `Int forbidden_dynamic_fact_count;
    "bad_facts", `List bad;
    "bad_entries", `List bad_entries;
    "status", `String (if forbidden_dynamic_fact_count = 0 then "pass" else "fail");
  ]

let module_report active_path reference_path =
  let active_json = Yojson.Safe.from_file active_path in
  let reference_json = Yojson.Safe.from_file reference_path in
  let residual = projection_member "residual_artifact" active_json in
  let built, ran, forbidden_hits, missing_runtime_markers, print_only_shape, extern_input_present, extern_effects_path, residual_output = build_and_run_residual residual in
  let global_rel =
    relation
      (global_projection (projection_member "linked_global_summary" active_json))
      (global_projection (projection_member "linked_global_summary" reference_json))
  in
  let input_rel = relation (residual_output |> member "final_input_table") (projection_member "final_input_table" reference_json) in
  let output_rel = relation (residual_output |> member "final_output_table") (projection_member "final_output_table" reference_json) in
  let active_completion = projection_member "completion" active_json in
  let reference_completion = projection_member "completion" reference_json in
  let completion_rel = if completion_ok active_completion && completion_ok reference_completion then "accepted" else "rejected" in
  let log = residual_output |> member "execution_log" in
  let residual_log_ok =
    built && ran && forbidden_hits = [] &&
    missing_runtime_markers = [] && not print_only_shape &&
    extern_input_present &&
    bool_member "executable_built" log && bool_member "executable_ran" log &&
    bool_member "runtime_recomposition_performed" log &&
    not (bool_member "print_only_residual" log) &&
    bool_member "extern_effect_input_read" log &&
    bool_member "extern_effect_input_valid" log &&
    int_member "extern_effect_input_bytes" log > 0 &&
    int_member "extern_root_match_count" log = int_member "extern_root_count" log &&
    (log |> member "residual_runtime_scope") = `String "extern-dependent-closure-only" &&
    int_member "forbidden_dynamic_fact_count" log = 0 &&
    int_member "residual_closure_entries_recomputed" log >= 0 &&
    (log |> member "recomposition_relation") = `String "structural-equiv"
  in
  let rel =
    if global_rel = "structural-equiv" && input_rel = "structural-equiv" && output_rel = "structural-equiv" && completion_rel = "accepted" && residual_log_ok
    then "structural-equiv" else "structural-diverge"
  in
  `Assoc [
    "name", active_json |> member "group";
    "active", `String active_path;
    "reference", `String reference_path;
    "relation", `String rel;
    "linked_global_summary", `String global_rel;
    "residual_final_input_table", `String input_rel;
    "residual_final_output_table", `String output_rel;
    "completion", active_completion;
    "reference_completion", reference_completion;
    "residual_executable", `Assoc [
      "source", residual |> member "source_path";
      "executable", residual |> member "executable_path";
      "output", residual |> member "output_path";
      "extern_effects", `String extern_effects_path;
      "extern_effect_input_present", `Bool extern_input_present;
      "executable_built", `Bool built;
      "executable_ran", `Bool ran;
      "forbidden_source_hits", `List (List.map (fun hit -> `String hit) forbidden_hits);
      "missing_runtime_markers", `List (List.map (fun hit -> `String hit) missing_runtime_markers);
      "print_only_shape", `Bool print_only_shape;
      "execution_log", log;
    ];
  ], residual_output, fixture_bta_report active_path active_json

let relation_of_module = function
  | `Assoc fields -> (match List.assoc_opt "relation" fields with Some (`String r) -> r | _ -> "structural-diverge")
  | _ -> "structural-diverge"

let () =
  Arg.parse
    ["--active", Arg.Set_string active_dir, "active artifact directory";
     "--reference", Arg.Set_string reference_dir, "frozen artifact directory";
     "--report", Arg.Set_string report, "source-lineage report path";
     "--bta-report", Arg.Set_string bta_report, "BTA report path";
     "--residual-report", Arg.Set_string residual_report, "residual execution report path"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !active_dir = "" || !reference_dir = "" || !report = "" || !bta_report = "" || !residual_report = "" then failwith usage;
  let active_paths = read_manifest !active_dir in
  let reference_paths = read_manifest !reference_dir in
  if List.length active_paths <> List.length reference_paths then failwith "active/reference count mismatch";
  let triples = List.map2 module_report active_paths reference_paths in
  let fixtures = List.map (fun (m, _, _) -> m) triples in
  let residual_outputs = List.map (fun (_, r, _) -> r) triples in
  let bta_fixtures = List.map (fun (_, _, b) -> b) triples in
  let all_equiv = List.for_all (fun m -> relation_of_module m = "structural-equiv") fixtures in
  let bta_ok = bta_fixtures |> List.for_all (function `Assoc fields -> List.assoc_opt "status" fields = Some (`String "pass") | _ -> false) in
  let residual_ok =
    residual_outputs
    |> List.for_all (fun r ->
         let log = r |> member "execution_log" in
         int_member "forbidden_dynamic_fact_count" log = 0 &&
         bool_member "runtime_recomposition_performed" log &&
         not (bool_member "print_only_residual" log) &&
         bool_member "extern_effect_input_read" log &&
         bool_member "extern_effect_input_valid" log &&
         int_member "extern_effect_input_bytes" log > 0 &&
         int_member "extern_root_match_count" log = int_member "extern_root_count" log)
  in
  let json = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_staged_linking_pe.schema_version;
    "claim", `String "linked ItvDom staged linking PE residual final-table parity";
    "relation", `String (if all_equiv then "structural-equiv" else "structural-diverge");
    "fixtures", `List fixtures;
  ] in
  let bta = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_staged_linking_pe.bta_schema_version;
    "status", `String (if bta_ok then "pass" else "fail");
    "allowed_dynamic_reasons", `List [`String "unknown-extern-call"; `String "transitively-extern-dependent"];
    "fixtures", `List bta_fixtures;
  ] in
  let residual = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_staged_linking_pe.residual_schema_version;
    "status", `String (if residual_ok && all_equiv then "pass" else "fail");
    "json_is_summary_only", `Bool false;
    "fixtures", `List residual_outputs;
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !bta_report bta;
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !residual_report residual;
  if all_equiv && bta_ok && residual_ok then print_endline ("PASS " ^ !report)
  else failwith "Staged linking PE parity failed; see reports"
