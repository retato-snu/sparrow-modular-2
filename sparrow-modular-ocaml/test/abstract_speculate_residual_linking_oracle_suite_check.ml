let repo_root = ref "."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "abstract_speculate_residual_linking_oracle_suite_check --repo-root <repo> --artifact-dir <dir> --report <json>"

let member = Yojson.Safe.Util.member
module Relation = Sparrow_modular_ocaml.Abstract_speculate_residual_relation

let expect cond msg = if not cond then failwith msg

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> s | _ -> ""
let int_field name json = match assoc_field name json with Some (`Int n) -> n | _ -> min_int
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

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

let typed_effect_ok expected_domain eff =
  string_field "domain" eff = expected_domain &&
  string_field "effect_id" eff <> "" &&
  string_field "location" eff <> "" &&
  string_field "provider_module" eff <> "" &&
  string_field "provider_source_hash" eff <> "" &&
  string_field "derivation_source" eff = "provider-stage2-output" &&
  string_field "witness_scope" eff = "selected-sparrow-itv"

let return_effect_ok summary eff =
  let function_return = summary |> member "external_summary_v1_compat" |> member "function_return_summary" in
  typed_effect_ok "return" eff &&
  effect_matches_summary_provenance summary eff &&
  string_field "source_evidence_path" eff = "provider_row.return" &&
  string_field "location" eff = string_field "return_location" function_return &&
  member "value" eff = member "return_value" function_return &&
  string_field "effect_id" eff = expected_effect_id "return" eff

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
  string_field "schema_version" summary = "abstract-speculate-external-summary/v2" &&
  string_field "summary_api_status" summary = "prototype-internal" &&
  string_field "summary_scope" summary = "sparrow-itv-selected-witness" &&
  list_field "effect_domains" summary <> [] &&
  list_field "return_effects" summary <> [] &&
  List.for_all (return_effect_ok summary) (list_field "return_effects" summary) &&
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
  expect (summaries <> []) (witness_id ^ ": missing ExternalSummary v2 entries");
  expect (List.for_all external_summary_ok summaries)
    (witness_id ^ ": malformed ExternalSummary v2 entries");
  if witness_id = "global_write_read" then
    expect (has_effect "global-write-read" summaries)
      (witness_id ^ ": missing v2 global effect");
  if witness_id = "pointer_memory_effect" then
    expect (has_effect "pointer-memory-effect" summaries)
      (witness_id ^ ": missing v2 pointer effect")

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
  let obligations = obligations_for witness_id category residual oracle in
  let residual_obs, oracle_obs = normalized_observations witness_id residual oracle in
  let selected_relation = Relation.selected_observation_relation_json ~witness_id ~residual ~oracle in
  let full_itv_relation = Relation.full_itv_semantic_relation_json ~witness_id ~residual ~oracle in
  let pass = witness_pass_status obligations residual_obs oracle_obs &&
             string_field "status" full_itv_relation = "pass" in
  `Assoc [
    "witness_id", `String witness_id;
    "category", `String category;
    "status", `String (if pass then "pass" else "fail");
    "residual_linked_artifact", `String residual_path;
    "premerge_observer_artifact", `String oracle_path;
    "external_summary_v2_checked", `Bool true;
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
  let v2_missing_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          let mutated = set_path ["external_summaries"] (`List []) residual in
          require_external_summaries "v2_missing_false" mutated)
    | [] -> false
  in
  let v2_compat_only_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        begin match list_field "external_summaries" residual with
        | first :: _ ->
            fails (fun () ->
              require_external_summaries "v2_compat_only_false"
                (set_path ["external_summaries"]
                   (`List [member "external_summary_v1_compat" first]) residual))
        | [] -> false
        end
    | [] -> false
  in
  let v2_schema_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        begin match list_field "external_summaries" residual with
        | first :: rest ->
            fails (fun () ->
              require_external_summaries "v2_schema_false"
                (set_path ["external_summaries"]
                   (`List (set_path ["schema_version"] (`String "abstract-speculate-external-summary/v1") first :: rest))
                   residual))
        | [] -> false
        end
    | [] -> false
  in
  let v2_status_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          require_external_summaries "v2_status_false"
            (mutate_first_summary ["summary_api_status"] (`String "stale-or-public") residual))
    | [] -> false
  in
  let v2_return_value_false =
    match loaded with
    | (_id, _category, residual, _oracle, _witness) :: _ ->
        fails (fun () ->
          require_external_summaries "v2_return_value_false"
            (mutate_first_effect "return_effects" ["value"] (`Int 999) residual))
    | [] -> false
  in
  let global_false =
    match find "global_write_read" with
    | Some (id, category, residual, oracle, _) ->
        obligation_set_fails id category (remove_linked_location "shared_g" residual) oracle
    | None -> false
  in
  let v2_global_effect_false =
    match find "global_write_read" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        begin match list_field "external_summaries" residual with
        | first :: rest ->
            fails (fun () ->
              require_external_summaries "global_write_read"
                (set_path ["external_summaries"]
                   (`List (set_path ["global_effects"] (`List []) first :: rest))
                   residual))
        | [] -> false
        end
    | None -> false
  in
  let v2_global_effect_location_false =
    match find "global_write_read" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        fails (fun () ->
          require_external_summaries "global_write_read"
            (mutate_first_effect "global_effects" ["location"] (`String "wrong_g") residual))
    | None -> false
  in
  let v2_global_effect_value_false =
    match find "global_write_read" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        fails (fun () ->
          require_external_summaries "global_write_read"
            (mutate_first_effect "global_effects" ["value"] (`String "wrong-value") residual))
    | None -> false
  in
  let v2_global_effect_provenance_false =
    match find "global_write_read" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        fails (fun () ->
          require_external_summaries "global_write_read"
            (mutate_first_effect "global_effects" ["provider_source_hash"] (`String "wrong-hash") residual))
    | None -> false
  in
  let pointer_false =
    match find "pointer_memory_effect" with
    | Some (id, category, residual, oracle, _) ->
        obligation_set_fails id category (remove_linked_location "(write_ptr,p)" residual) oracle
    | None -> false
  in
  let v2_pointer_effect_false =
    match find "pointer_memory_effect" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        begin match list_field "external_summaries" residual with
        | first :: rest ->
            fails (fun () ->
              require_external_summaries "pointer_memory_effect"
                (set_path ["external_summaries"]
                   (`List (set_path ["pointer_effects"] (`List []) first :: rest))
                   residual))
        | [] -> false
        end
    | None -> false
  in
  let v2_pointer_effect_location_false =
    match find "pointer_memory_effect" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        fails (fun () ->
          require_external_summaries "pointer_memory_effect"
            (mutate_first_effect "pointer_effects" ["location"] (`String "(wrong,p)") residual))
    | None -> false
  in
  let v2_pointer_effect_value_false =
    match find "pointer_memory_effect" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        fails (fun () ->
          require_external_summaries "pointer_memory_effect"
            (mutate_first_effect "pointer_effects" ["value"] (`String "wrong-value") residual))
    | None -> false
  in
  let v2_pointer_effect_provenance_false =
    match find "pointer_memory_effect" with
    | Some (_id, _category, residual, _oracle, _witness) ->
        fails (fun () ->
          require_external_summaries "pointer_memory_effect"
            (mutate_first_effect "pointer_effects" ["provider_source_hash"] (`String "wrong-hash") residual))
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
  let negative_dir = Filename.concat !artifact_dir "negative" in
  let injected_shortcut_rejected =
    let injected = Filename.concat negative_dir "injected_premerge_shortcut.ml" in
    write_file injected "let shortcut = Real_sparrow_frontend.global_for_files []\n";
    let obligation = source_guard_obligation_for_paths "shortcut_false_case" [injected] [] in
    string_field "status" obligation = "fail"
  in
  [
    false_case "mismatched_return_or_effect_summary" return_false;
    false_case "missing_external_summary_v2" v2_missing_false;
    false_case "v1_compat_only_rejected" v2_compat_only_false;
    false_case "external_summary_schema_downgrade_rejected" v2_schema_false;
    false_case "external_summary_status_corruption_rejected" v2_status_false;
    false_case "external_summary_return_value_corruption_rejected" v2_return_value_false;
    false_case "missing_global_write_read_effect" global_false;
    false_case "missing_external_summary_v2_global_effect" v2_global_effect_false;
    false_case "external_summary_v2_global_location_corruption_rejected" v2_global_effect_location_false;
    false_case "external_summary_v2_global_value_corruption_rejected" v2_global_effect_value_false;
    false_case "external_summary_v2_global_provenance_corruption_rejected" v2_global_effect_provenance_false;
    false_case "missing_pointer_memory_effect" pointer_false;
    false_case "missing_external_summary_v2_pointer_effect" v2_pointer_effect_false;
    false_case "external_summary_v2_pointer_location_corruption_rejected" v2_pointer_effect_location_false;
    false_case "external_summary_v2_pointer_value_corruption_rejected" v2_pointer_effect_value_false;
    false_case "external_summary_v2_pointer_provenance_corruption_rejected" v2_pointer_effect_provenance_false;
    false_case "non_selected_itv_cell_mutation_fails_full_relation" non_selected_itv_cell_false;
    false_case "ambiguous_provider_accepted_incorrectly"
      (log_contains (Filename.concat negative_dir "ambiguous_provider.log") "unsupported ambiguous semantic export mapping");
    false_case "invalid_mixed_role_propagation" mixed_false;
    false_case "premerge_implementation_shortcut" injected_shortcut_rejected;
    false_case "missing_oracle_artifact" missing_oracle_false;
    false_case "witness_identity_mismatch" witness_identity_false;
    false_case "missing_normalized_observation_provenance" provenance_false;
    false_case "mixed_role_dependency_cycle"
      (log_contains (Filename.concat negative_dir "cycle_topology.log") "unsupported cyclic mixed importer/provider residual-linking topology");
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
  expect (List.length witnesses >= 4) "expected oracle suite witness coverage";
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
  ] in
  let names = all_obligations |> List.map (string_field "name") in
  required |> List.iter (fun name -> expect (List.mem name names) ("missing obligation: " ^ name));
  expect (all_obligations |> List.for_all (fun o -> string_field "status" o = "pass"))
    "at least one proof obligation failed";
  expect (negative_cases |> List.for_all (fun n -> string_field "status" n = "pass"))
    "at least one negative case was not covered";
  let suite_pass = witness_reports |> List.for_all (fun w -> string_field "status" w = "pass") in
  expect suite_pass "at least one full-Itv semantic relation failed";
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
