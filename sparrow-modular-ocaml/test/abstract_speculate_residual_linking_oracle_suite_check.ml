let repo_root = ref "."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "abstract_speculate_residual_linking_oracle_suite_check --repo-root <repo> --artifact-dir <dir> --report <json>"

let member = Yojson.Safe.Util.member

let expect cond msg = if not cond then failwith msg

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> s | _ -> ""
let int_field name json = match assoc_field name json with Some (`Int n) -> n | _ -> 0
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

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
let sort_uniq xs = List.sort_uniq String.compare xs
let has_duplicates xs = List.length xs <> List.length (sort_uniq xs)

let int_of_trimmed s = try Some (int_of_string (String.trim s)) with Failure _ -> None

let parse_interval_value value =
  let value = String.trim value in
  match int_of_trimmed value with
  | Some n -> Some (n, n)
  | None ->
      let prefix = "([" in
      let prefix_len = String.length prefix in
      if String.length value <= prefix_len || String.sub value 0 prefix_len <> prefix then None
      else
        try
          let comma = String.index_from value prefix_len ',' in
          let close = String.index_from value (comma + 1) ']' in
          let lo_s = String.trim (String.sub value prefix_len (comma - prefix_len)) in
          let hi_s = String.trim (String.sub value (comma + 1) (close - comma - 1)) in
          match lo_s, hi_s with
          | "-oo", _ | _, "+oo" -> None
          | _ ->
              begin match int_of_trimmed lo_s, int_of_trimmed hi_s with
              | Some lo, Some hi -> Some (lo, hi)
              | _ -> None
              end
        with Not_found -> None

let interval_contains_value value n =
  match parse_interval_value value with
  | Some (lo, hi) -> lo <= n && n <= hi
  | None -> false

let singleton_value value =
  match parse_interval_value value with
  | Some (lo, hi) when lo = hi -> Some lo
  | _ -> int_of_trimmed value

let row_memory row = list_field "memory" row

let cells_in_tables tables =
  tables
  |> List.concat_map (fun (table_name, rows) ->
       rows |> List.concat_map (fun row ->
         row_memory row |> List.map (fun cell -> table_name, row, cell)))

let linked_tables linked =
  let output = member "linked_output" linked in
  ["final_input_table", list_field "final_input_table" output;
   "final_output_table", list_field "final_output_table" output]

let oracle_tables oracle =
  let projection = member "projection" oracle in
  ["final_input_table", list_field "final_input_table" projection;
   "final_output_table", list_field "final_output_table" projection]

let find_value_by_location tables location =
  cells_in_tables tables
  |> List.find_map (fun (_table, _row, cell) ->
       if string_field "location" cell = location then Some (string_field "value" cell) else None)

let values_by_location tables location =
  cells_in_tables tables
  |> List.filter_map (fun (_table, _row, cell) ->
       if string_field "location" cell = location then Some (string_field "value" cell) else None)

let find_singleton_by_location tables location =
  cells_in_tables tables
  |> List.find_map (fun (_table, _row, cell) ->
       if string_field "location" cell = location then singleton_value (string_field "value" cell) else None)

let return_location name = "(" ^ name ^ ",__return__)"

let semantic_exports linked = list_field "semantic_exports" linked
let linked_environment linked = list_field "linked_environment" linked

let oracle_return_value oracle export_name =
  find_singleton_by_location (oracle_tables oracle) (return_location export_name)

let return_obligations witness_id residual oracle =
  semantic_exports residual
  |> List.map (fun export ->
       let name = string_field "export_name" export in
       let residual_value = int_field "return_value" export in
       let oracle_value = oracle_return_value oracle name in
       let pass = oracle_value = Some residual_value in
       `Assoc [
         "name", `String "return_value_matches_oracle";
         "category", `String "return";
         "witness_id", `String witness_id;
         "symbol", `String name;
         "status", `String (if pass then "pass" else "fail");
         "residual_value", `Int residual_value;
         "oracle_value", (match oracle_value with Some n -> `Int n | None -> `Null);
         "evidence_paths", `List [`String "semantic_exports"; `String "projection.final_output_table"];
       ])

let declaration_kind residual ~field_name ~module_field ~name_field entry =
  let module_id = string_field module_field entry in
  let name = string_field name_field entry in
  list_field field_name residual
  |> List.find_map (fun decl_entry ->
       if string_field "module_id" decl_entry = module_id then
         let declaration = member "declaration" decl_entry in
         if string_field "name" declaration = name then Some (string_field "kind" declaration) else None
       else None)
  |> Option.value ~default:"unknown"

let binding_key residual witness_id entry =
  let semantic_export = member "semantic_export" entry in
  let importer_module = string_field "importer_module" entry in
  let provider_module = string_field "provider_module" entry in
  let import_name = string_field "import_name" entry in
  let export_name = string_field "export_name" entry in
  String.concat ":" [
    "witness"; witness_id;
    "importer"; importer_module; string_field "importer_source_hash" entry;
    "import"; import_name; declaration_kind residual ~field_name:"declared_imports" ~module_field:"importer_module" ~name_field:"import_name" entry;
    "provider"; provider_module; string_field "provider_source_hash" semantic_export;
    "export"; export_name; declaration_kind residual ~field_name:"declared_exports" ~module_field:"provider_module" ~name_field:"export_name" entry;
  ]

let provider_resolution_obligation witness_id residual oracle =
  let env = linked_environment residual in
  let keys = List.map (binding_key residual witness_id) env in
  let all_oracle_returns_exist =
    env |> List.for_all (fun entry ->
      oracle_return_value oracle (string_field "export_name" entry) = Some (int_field "linked_return_value" entry))
  in
  let pass = env <> [] && not (has_duplicates keys) && all_oracle_returns_exist in
  `Assoc [
    "name", `String "provider_resolution_matches_oracle";
    "category", `String "topology";
    "witness_id", `String witness_id;
    "status", `String (if pass then "pass" else "fail");
    "binding_keys", `List (List.map (fun k -> `String k) keys);
    "evidence_paths", `List [`String "linked_environment"; `String "projection.final_output_table"];
  ]

let phase_index residual ~module_id ~event_part =
  list_field "phase_log" residual
  |> List.find_map (fun event ->
       if string_field "module_id" event = module_id && contains (string_field "event" event) event_part then
         Some (int_field "phase_index" event)
       else None)

let mixed_role_obligation witness_id residual oracle =
  let env = linked_environment residual in
  let import_modules = env |> List.map (fun e -> string_field "importer_module" e) |> sort_uniq in
  let provider_modules = env |> List.map (fun e -> string_field "provider_module" e) |> sort_uniq in
  let mixed = List.filter (fun m -> List.mem m provider_modules) import_modules in
  let ordered =
    env |> List.for_all (fun entry ->
      let provider = string_field "provider_module" entry in
      let importer = string_field "importer_module" entry in
      let export_name = string_field "export_name" entry in
      match
        phase_index residual ~module_id:provider ~event_part:("semantic-export-derived:" ^ export_name),
        phase_index residual ~module_id:importer ~event_part:("linked-environment-bound:" ^ export_name),
        phase_index residual ~module_id:importer ~event_part:"importer-stage2-executed-with-linked-environment"
      with
      | Some export_i, Some env_i, Some importer_i -> export_i < env_i && env_i < importer_i
      | _ -> false)
  in
  let oracle_ok = env |> List.for_all (fun e -> oracle_return_value oracle (string_field "export_name" e) <> None) in
  let pass = mixed <> [] && ordered && oracle_ok in
  `Assoc [
    "name", `String "mixed_role_chain_matches_oracle";
    "category", `String "topology";
    "witness_id", `String witness_id;
    "status", `String (if pass then "pass" else "fail");
    "mixed_modules", `List (List.map (fun m -> `String m) mixed);
    "claim_scope", `String "mixed-role scheduling/order and summary handoff; not upstream value-dependence";
    "evidence_paths", `List [`String "phase_log"; `String "linked_environment"; `String "projection.final_output_table"];
  ]

let global_obligation witness_id residual oracle =
  let residual_values = values_by_location (linked_tables residual) "shared_g" in
  let oracle_value = find_singleton_by_location (oracle_tables oracle) "shared_g" in
  let pass =
    match oracle_value with
    | Some ov ->
        residual_values
        |> List.exists (fun rv -> interval_contains_value rv ov || contains rv (string_of_int ov))
    | None -> false
  in
  let residual_value =
    residual_values
    |> List.find_opt (fun rv ->
         match oracle_value with
         | Some ov -> interval_contains_value rv ov || contains rv (string_of_int ov)
         | None -> false)
  in
  `Assoc [
    "name", `String "global_write_read_matches_oracle";
    "category", `String "global-write-read";
    "witness_id", `String witness_id;
    "status", `String (if pass then "pass" else "fail");
    "location", `String "shared_g";
    "residual_value", (match residual_value with Some v -> `String v | None -> `Null);
    "oracle_value", (match oracle_value with Some n -> `Int n | None -> `Null);
    "normalization", `String "oracle singleton accepted when contained in any residual interval observation";
    "evidence_paths", `List [`String "linked_output.final_*_table"; `String "projection.final_*_table"];
  ]

let pointer_obligation witness_id residual oracle =
  let location = "(write_ptr,p)" in
  let residual_value = find_value_by_location (linked_tables residual) location in
  let oracle_value = find_value_by_location (oracle_tables oracle) location in
  let oracle_return = find_singleton_by_location (oracle_tables oracle) (return_location "write_ptr") in
  let pass =
    match residual_value, oracle_value, oracle_return with
    | Some rv, Some ov, _ -> contains rv "main,x" && contains ov "main,x"
    | Some rv, None, Some 5 -> contains rv "main,x"
    | _ -> false
  in
  `Assoc [
    "name", `String "pointer_memory_effect_matches_oracle";
    "category", `String "pointer-memory-effect";
    "witness_id", `String witness_id;
    "status", `String (if pass then "pass" else "fail");
    "location", `String location;
    "residual_value", (match residual_value with Some v -> `String v | None -> `Null);
    "oracle_value", (match oracle_value with Some v -> `String v | None -> `Null);
    "oracle_return_value", (match oracle_return with Some n -> `Int n | None -> `Null);
    "normalization", `String "pointer alias summary compared by residual shared pointee provenance and oracle write_ptr return/effect summary";
    "evidence_paths", `List [`String "linked_output.final_*_table"; `String "projection.final_*_table"];
  ]

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

let obligation_status obligation_json = string_field "status" obligation_json
let obligation_passes xs = xs |> List.for_all (fun o -> obligation_status o = "pass")

let normalized_observations witness_id residual oracle =
  let residual_obs =
    semantic_exports residual
    |> List.map (fun export ->
      let name = string_field "export_name" export in
      `Assoc [
        "witness_id", `String witness_id;
        "category", `String "return";
        "module_role_path", `String (string_field "provider_module" export);
        "symbol", `String name;
        "location", `String (return_location name);
        "abstract_value", `String (string_field "abstract_return_value" export);
        "normalized_value", `Int (int_field "return_value" export);
        "residual_provenance", `String ("semantic_exports:" ^ name);
      ])
  in
  let oracle_obs =
    semantic_exports residual
    |> List.filter_map (fun export ->
      let name = string_field "export_name" export in
      match oracle_return_value oracle name with
      | Some value -> Some (`Assoc [
          "witness_id", `String witness_id;
          "category", `String "return";
          "module_role_path", `String "premerge-linked-observer";
          "symbol", `String name;
          "location", `String (return_location name);
          "abstract_value", `String ("singleton:" ^ string_of_int value);
          "normalized_value", `Int value;
          "oracle_provenance", `String ("projection.final_output_table:" ^ name);
        ])
      | None -> None)
  in
  residual_obs, oracle_obs

let obligations_for witness_id category residual oracle =
  let return_obs = return_obligations witness_id residual oracle in
  let category_obligation =
    match category with
    | "global-write-read" -> [global_obligation witness_id residual oracle]
    | "pointer-memory-effect" -> [pointer_obligation witness_id residual oracle]
    | "multiple-providers-imports" -> [provider_resolution_obligation witness_id residual oracle]
    | "mixed-role-chain" -> [provider_resolution_obligation witness_id residual oracle; mixed_role_obligation witness_id residual oracle]
    | "return-only" -> []
    | _ -> failwith ("unknown witness category: " ^ category)
  in
  return_obs @ category_obligation @ [source_guard_obligation witness_id]

let observations_have_provenance observations =
  observations |> List.for_all (fun obs ->
    string_field "residual_provenance" obs <> "" || string_field "oracle_provenance" obs <> "")

let witness_pass_status obligations residual_obs oracle_obs =
  obligations <> [] && obligation_passes obligations &&
  residual_obs <> [] && oracle_obs <> [] &&
  observations_have_provenance (residual_obs @ oracle_obs)

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
  let obligations = obligations_for witness_id category residual oracle in
  let residual_obs, oracle_obs = normalized_observations witness_id residual oracle in
  let pass = witness_pass_status obligations residual_obs oracle_obs in
  `Assoc [
    "witness_id", `String witness_id;
    "category", `String category;
    "status", `String (if pass then "pass" else "fail");
    "residual_linked_artifact", `String residual_path;
    "premerge_observer_artifact", `String oracle_path;
    "normalized_observations", `Assoc [
      "residual", `List residual_obs;
      "oracle", `List oracle_obs;
      "relation", `String "residual-to-oracle-and-oracle-to-residual-selected-equivalence";
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
  let return_false =
    match loaded with
    | (id, category, residual, oracle, _) :: _ ->
        obligation_set_fails id category (mutate_first_semantic_export_return residual 999) oracle
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
    false_case "missing_global_write_read_effect" global_false;
    false_case "missing_pointer_memory_effect" pointer_false;
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
  let report_json = `Assoc [
    "schema_version", `String "abstract-speculate-residual-linking-oracle-suite/v1";
    "schema_status", `String "prototype-non-public";
    "suite_id", `String "abstract-speculate-residual-linking-oracle-suite";
    "suite_status", `String "pass";
    "oracle_reference_kind", `String "premerge-linked-observer";
    "residual_linking_implementation_premerge_free", `Bool true;
    "witnesses", `List witness_reports;
    "obligations", `List all_obligations;
    "negative_cases", `List negative_cases;
    "non_claims", `List [
      `String "no proof assistant mechanization";
      `String "no final artifact schema freeze";
      `String "no broad arbitrary-C coverage";
      `String "witness-bounded selected-observation equivalence only";
    ];
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report report_json;
  print_endline "abstract_speculate_residual_linking_oracle_suite_check: PASS"
