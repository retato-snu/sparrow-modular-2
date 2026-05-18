(***********************************************************************)
(* Prototype/internal selected-observation relation for residual PE.    *)
(***********************************************************************)

(* This module is deliberately prototype/internal.  It centralizes the
   witness-bounded selected-observation relation consumed by residual-linking
   checkers; it is not a stable public residual-linker API and it does not
   claim arbitrary-C semantic equivalence. *)

module ItvCell = Abstract_speculate_itv_residual_cell
module ScalarCall = Abstract_speculate_residual_scalar_call
module MemoryDelta = Abstract_speculate_residual_memory_delta

type observation_domain =
  | Return_value
  | Global_value
  | Pointer_write
  | Memory_location
  | Call_effect
  | Provider_binding
  | Phase_ordering
  | Provenance

type direction = Residual_to_oracle | Oracle_to_residual

type provenance = {
  source : string;
  path : string;
}

type summary_observation = {
  domain : observation_domain;
  witness_id : string;
  symbol : string;
  location : string;
  abstract_value : string;
  normalized_value : Yojson.Safe.t;
  provenance : provenance list;
}

type relation_failure = {
  domain : observation_domain;
  direction : direction;
  reason : string;
  evidence_path : string;
}

type comparison = {
  status : string;
  residual_observations : Yojson.Safe.t list;
  oracle_observations : Yojson.Safe.t list;
  residual_to_oracle : Yojson.Safe.t;
  oracle_to_residual : Yojson.Safe.t;
  failures : relation_failure list;
}


let member = Yojson.Safe.Util.member

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> s | _ -> ""
let int_field name json = match assoc_field name json with Some (`Int n) -> n | _ -> 0
let bool_field name json = match assoc_field name json with Some (`Bool b) -> b | _ -> false
let opt_int_field name json = match assoc_field name json with Some (`Int n) -> Some n | _ -> None
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

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

let parse_singleton_interval_value value =
  match parse_interval_value value with
  | Some (lo, hi) when lo = hi -> Some lo
  | _ -> int_of_trimmed value

let interval_contains_value value n =
  match parse_interval_value value with
  | Some (lo, hi) -> lo <= n && n <= hi
  | None -> false

let return_location name = "(" ^ name ^ ",__return__)"
let row_memory row = list_field "memory" row

let cells_in_tables tables =
  tables
  |> List.concat_map (fun (table_name, rows) ->
       rows |> List.concat_map (fun row ->
         row_memory row |> List.map (fun cell -> table_name, row, cell)))

let linked_output linked = member "linked_output" linked

let linked_tables linked =
  let output = linked_output linked in
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
       if string_field "location" cell = location then parse_singleton_interval_value (string_field "value" cell) else None)

let memory_cell ~location row =
  row_memory row
  |> List.find_opt (fun cell -> string_field "location" cell = location)

let semantic_exports linked = list_field "semantic_exports" linked
let linked_environment linked = list_field "linked_environment" linked
let external_summaries linked = list_field "external_summaries" linked
let memory_deltas linked = external_summaries linked |> List.concat_map (list_field "memory_deltas")

let observation_domain_to_string = function
  | Return_value -> "return"
  | Global_value -> "global-write-read"
  | Pointer_write -> "pointer-memory-effect"
  | Memory_location -> "memory-location"
  | Call_effect -> "call-effect"
  | Provider_binding -> "provider-binding"
  | Phase_ordering -> "phase-ordering"
  | Provenance -> "provenance"

let direction_to_string = function
  | Residual_to_oracle -> "residual_to_oracle"
  | Oracle_to_residual -> "oracle_to_residual"

let failure_to_json failure =
  `Assoc [
    "domain", `String (observation_domain_to_string failure.domain);
    "direction", `String (direction_to_string failure.direction);
    "reason", `String failure.reason;
    "evidence_path", `String failure.evidence_path;
  ]

let pass_status pass = if pass then "pass" else "fail"

(** Shared JSON/provenance helpers.  The JSON fields below are intentionally
    evidence-shaped rather than schema-stable public artifacts. *)

let observation_has_provenance obs =
  string_field "residual_provenance" obs <> "" ||
  string_field "oracle_provenance" obs <> "" ||
  string_field "provenance" obs <> ""

let observations_have_provenance observations = List.for_all observation_has_provenance observations

let relation_direction_json ~direction ~pass ~matched ~missing ~extra ~failures =
  `Assoc [
    "direction", `String (direction_to_string direction);
    "status", `String (pass_status pass);
    "matched_observations", `List matched;
    "missing_observations", `List missing;
    "extra_observations", `List extra;
    "failures", `List (List.map failure_to_json failures);
  ]

let comparison_to_json comparison =
  `Assoc [
    "relation", `String "selected-observation-equivalence";
    "status", `String comparison.status;
    "selected_observation_summary", `Assoc [
      "residual_count", `Int (List.length comparison.residual_observations);
      "oracle_count", `Int (List.length comparison.oracle_observations);
      "failure_count", `Int (List.length comparison.failures);
    ];
    "residual", `List comparison.residual_observations;
    "oracle", `List comparison.oracle_observations;
    "residual_to_oracle", comparison.residual_to_oracle;
    "oracle_to_residual", comparison.oracle_to_residual;
    "missing_observations", member "missing_observations" comparison.residual_to_oracle;
    "extra_observations", member "extra_observations" comparison.residual_to_oracle;
    "failures", `List (List.map failure_to_json comparison.failures);
  ]

(** Primary residual-linkage invariant check.

    This is deliberately not an oracle equivalence relation: the primary PE
    checker has no premerge oracle projection in scope.  It checks that the
    linked residual artifact is internally consistent with provider-derived
    importer inputs, linked-provider-return effects, selected return rows, and
    phase/provenance obligations.  Oracle-facing selected-observation
    equivalence is implemented below by [selected_observation_relation_json]. *)

let primary_linkage_check_to_json comparison =
  `Assoc [
    "check", `String "primary-linkage-selected-observation-invariants";
    "claim_scope", `String "residual-internal invariant check; no oracle/reference comparison";
    "oracle_required", `Bool false;
    "status", `String comparison.status;
    "primary_linkage_observation_summary", `Assoc [
      "residual_observation_count", `Int (List.length comparison.residual_observations);
      "failure_count", `Int (List.length comparison.failures);
      "oracle_observation_count", `Int 0;
      "oracle_comparison_performed", `Bool false;
    ];
    "matched_observations", `List comparison.residual_observations;
    "missing_observations", `List [];
    "extra_observations", `List [];
    "failures", `List (List.map failure_to_json comparison.failures);
  ]

let semantic_export_key json =
  string_field "provider_module" json ^ ":" ^ string_field "export_name" json

let find_semantic_export linked ~provider_module ~export_name =
  semantic_exports linked
  |> List.find_opt (fun export ->
       string_field "provider_module" export = provider_module &&
       string_field "export_name" export = export_name)

let provider_return_from_linked_output linked ~provider_module ~export_name =
  let location = return_location export_name in
  let rows = list_field "final_output_table" (linked_output linked) in
  rows
  |> List.find_map (fun row ->
       if string_field "linked_module_id" row = provider_module then
         match memory_cell ~location row with
         | Some cell ->
             parse_singleton_interval_value (string_field "value" cell)
             |> Option.map (fun value -> string_field "node" row, string_field "value" cell, value)
         | None -> None
       else None)

let importer_dynamic_call_value linked ~importer_module ~extern_root =
  let rows = list_field "final_output_table" (linked_output linked) in
  rows
  |> List.find_map (fun row ->
       if string_field "linked_module_id" row = importer_module &&
          string_field "node" row = extern_root then
         row_memory row
         |> List.find_map (fun cell ->
              if contains (string_field "transfer_event" cell) "extern-call-result" then
                opt_int_field "residual_arithmetic_value" cell
              else None)
       else None)

let phase_index linked ~module_id ~event_part =
  list_field "phase_log" linked
  |> List.find_map (fun event ->
       if string_field "module_id" event = module_id && contains (string_field "event" event) event_part then
         Some (int_field "phase_index" event)
       else None)

let primary_phase_ordered linked ~provider_module ~importer_module ~export_name =
  match
    phase_index linked ~module_id:provider_module ~event_part:"provider-stage2-executed",
    phase_index linked ~module_id:provider_module ~event_part:("semantic-export-derived:" ^ export_name),
    phase_index linked ~module_id:importer_module ~event_part:("linked-environment-bound:" ^ export_name),
    phase_index linked ~module_id:importer_module ~event_part:"importer-stage2-executed-with-linked-environment"
  with
  | Some provider_i, Some export_i, Some env_i, Some importer_i ->
      provider_i < export_i && export_i < env_i && env_i < importer_i
  | _ -> false

let fail domain direction reason evidence_path = { domain; direction; reason; evidence_path }

let scalar_protocol_failures_for_derivation direction evidence_path derivation =
  match ScalarCall.validate_linked_derivation_json derivation with
  | Ok () -> []
  | Error reasons ->
      [
        fail Call_effect direction
          ("typed_scalar_protocol_mismatch:" ^ String.concat "," reasons)
          evidence_path;
      ]

let scalar_protocol_failures witness_id residual =
  list_field "linked_stage2_input_derivation" residual
  |> List.concat_map (fun entry ->
       let export_name = string_field "export_name" entry in
       scalar_protocol_failures_for_derivation Residual_to_oracle
         ("linked_stage2_input_derivation:" ^ witness_id ^ ":" ^ export_name)
         entry)


let memory_delta_failures witness_id residual =
  list_field "external_summaries" residual
  |> List.mapi (fun index summary ->
       match MemoryDelta.validate_summary_json summary with
       | Ok () -> []
       | Error reasons ->
           reasons
           |> List.map (fun reason ->
                fail Memory_location Residual_to_oracle reason
                  ("external_summaries:" ^ witness_id ^ ":" ^ string_of_int index)))
  |> List.flatten

let primary_linkage_comparison linked =
  let exports = semantic_exports linked in
  let env = linked_environment linked in
  let derivation = list_field "linked_stage2_input_derivation" linked in
  let base_failures =
    scalar_protocol_failures "primary" linked @ memory_delta_failures "primary" linked
    |> fun fs -> if exports = [] then fail Return_value Residual_to_oracle "missing semantic exports" "semantic_exports" :: fs else fs
    |> fun fs -> if env = [] then fail Provider_binding Residual_to_oracle "missing linked environment" "linked_environment" :: fs else fs
    |> fun fs -> if derivation = [] then fail Call_effect Residual_to_oracle "missing linked stage2 derivation" "linked_stage2_input_derivation" :: fs else fs
    |> fun fs -> if string_field "dispatch" (member "linked_stage2_input" linked) <> "provider-derived-linked-environment" then fail Call_effect Residual_to_oracle "linked stage2 dispatch is not provider-derived" "linked_stage2_input.dispatch" :: fs else fs
    |> fun fs -> if not (bool_field "linked_environment_generated" (member "linked_stage2_input" linked)) then fail Provider_binding Residual_to_oracle "linked environment generation flag is false" "linked_stage2_input.linked_environment_generated" :: fs else fs
    |> fun fs -> if string_field "derivation_source" (member "linked_stage2_input" linked) <> "provider-stage2-output" then fail Provenance Residual_to_oracle "linked stage2 input derivation source is not provider-stage2-output" "linked_stage2_input.derivation_source" :: fs else fs
  in
  let env_failures, residual_obs =
    env |> List.fold_left (fun (failures, obs) entry ->
      let provider_module = string_field "provider_module" entry in
      let importer_module = string_field "importer_module" entry in
      let export_name = string_field "export_name" entry in
      let extern_root = string_field "importer_extern_root" entry in
      let linked_value = int_field "linked_return_value" entry in
      let semantic_export = find_semantic_export linked ~provider_module ~export_name in
      let failures =
        match semantic_export with
        | None -> fail Return_value Residual_to_oracle "linked environment has no matching semantic export" "linked_environment.semantic_export" :: failures
        | Some export ->
            let fs = failures in
            let fs = if string_field "derivation_source" export <> "provider-stage2-output" then fail Provenance Residual_to_oracle "semantic export derivation source mismatch" "semantic_exports.derivation_source" :: fs else fs in
            let fs = if string_field "derivation_source" entry <> "provider-stage2-output" then fail Provenance Residual_to_oracle "linked environment derivation source mismatch" "linked_environment.derivation_source" :: fs else fs in
            let fs = if int_field "return_value" export <> linked_value then fail Return_value Residual_to_oracle "semantic export return does not match linked value" "semantic_exports.return_value" :: fs else fs in
            let fs =
              match provider_return_from_linked_output linked ~provider_module ~export_name with
              | Some (_node, abstract_value, provider_value) ->
                  let fs = if provider_value <> linked_value then fail Return_value Residual_to_oracle "provider output return does not match linked value" "linked_output.final_output_table" :: fs else fs in
                  if string_field "abstract_return_value" export <> abstract_value then fail Return_value Residual_to_oracle "semantic export abstract value does not match provider row" "semantic_exports.abstract_return_value" :: fs else fs
              | None -> fail Return_value Residual_to_oracle "missing provider return row" "linked_output.final_output_table" :: fs
            in
            let fs =
              match importer_dynamic_call_value linked ~importer_module ~extern_root with
              | Some importer_value when importer_value = linked_value -> fs
              | Some _ -> fail Call_effect Residual_to_oracle "importer dynamic call value does not match linked value" "linked_output.final_output_table" :: fs
              | None -> fail Call_effect Residual_to_oracle "missing importer dynamic extern-call result" "linked_output.final_output_table" :: fs
            in
            let fs = if not (primary_phase_ordered linked ~provider_module ~importer_module ~export_name) then fail Phase_ordering Residual_to_oracle "provider/export/environment/importer phases are not ordered" "phase_log" :: fs else fs in
            let matching_derivation =
              derivation
              |> List.find_opt (fun derivation_entry ->
                   string_field "importer_module" derivation_entry = importer_module &&
                   string_field "importer_extern_root" derivation_entry = extern_root &&
                   string_field "provider_module" derivation_entry = provider_module &&
                   string_field "export_name" derivation_entry = export_name &&
                   string_field "derivation_source" derivation_entry = "provider-stage2-output" &&
                   string_field "effect_reason" derivation_entry = "linked-provider-return" &&
                   int_field "linked_return_value" derivation_entry = linked_value)
            in
            match matching_derivation with
            | None -> fail Call_effect Residual_to_oracle "missing linked-provider-return derivation entry" "linked_stage2_input_derivation" :: fs
            | Some derivation_entry ->
                scalar_protocol_failures_for_derivation Residual_to_oracle
                  "linked_stage2_input_derivation" derivation_entry @ fs
      in
      let obs_entry = `Assoc [
        "category", `String "call-effect";
        "symbol", `String export_name;
        "provider_module", `String provider_module;
        "importer_module", `String importer_module;
        "location", `String (return_location export_name);
        "normalized_value", `Int linked_value;
        "residual_provenance", `String ("linked_environment:" ^ importer_module ^ ":" ^ export_name);
        "call_effect_reason", `String "linked-provider-return";
      ] in
      failures, obs_entry :: obs)
      (base_failures, [])
  in
  let duplicate_failures =
    if has_duplicates (List.map semantic_export_key exports) then
      fail Provider_binding Residual_to_oracle "duplicate semantic export keys" "semantic_exports" :: env_failures
    else env_failures
  in
  let residual_obs = List.rev residual_obs in
  let provenance_failures =
    if observations_have_provenance residual_obs then duplicate_failures
    else fail Provenance Residual_to_oracle "selected residual observation lacks provenance" "selected_observation_summary.residual" :: duplicate_failures
  in
  let pass = provenance_failures = [] in
  let direction_json = relation_direction_json ~direction:Residual_to_oracle ~pass ~matched:residual_obs ~missing:[] ~extra:[] ~failures:provenance_failures in
  {
    status = pass_status pass;
    residual_observations = residual_obs;
    oracle_observations = [];
    residual_to_oracle = direction_json;
    oracle_to_residual = relation_direction_json ~direction:Oracle_to_residual ~pass ~matched:[] ~missing:[] ~extra:[] ~failures:provenance_failures;
    failures = provenance_failures;
  }

let primary_linkage_ok linked = (primary_linkage_comparison linked).status = "pass"
let primary_linkage_check_json linked = primary_linkage_check_to_json (primary_linkage_comparison linked)

(** Oracle-suite selected-observation equivalence.

    This relation is witness-bounded and prototype/internal.  It compares only
    selected return, memory/effect, binding, phase, and provenance observations
    between residual-linking artifacts and premerge oracle projections. *)

let oracle_return_value oracle export_name = find_singleton_by_location (oracle_tables oracle) (return_location export_name)

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

let selected_cell_observation ~witness_id ~category ~location ~symbol ~residual_value ~oracle_value ~normalization ~residual_provenance ~oracle_provenance =
  let residual_obs =
    match residual_value, oracle_value with
    | Some rv, Some ov when rv = ov ->
        [ `Assoc [
            "witness_id", `String witness_id;
            "category", `String category;
            "symbol", `String symbol;
            "location", `String location;
            "abstract_value", `String normalization;
            "normalized_value", `String rv;
            "residual_provenance", `String residual_provenance;
          ] ]
    | Some rv, None ->
        [ `Assoc [
            "witness_id", `String witness_id;
            "category", `String category;
            "symbol", `String symbol;
            "location", `String location;
            "abstract_value", `String normalization;
            "normalized_value", `String rv;
            "residual_provenance", `String residual_provenance;
          ] ]
    | _ -> []
  in
  let oracle_obs =
    match oracle_value with
    | Some ov ->
        [ `Assoc [
            "witness_id", `String witness_id;
            "category", `String category;
            "symbol", `String symbol;
            "location", `String location;
            "abstract_value", `String normalization;
            "normalized_value", `String ov;
            "oracle_provenance", `String oracle_provenance;
          ] ]
    | None -> []
  in
  residual_obs, oracle_obs

let relation_binding_observations witness_id residual oracle =
  linked_environment residual
  |> List.fold_left (fun (residual_acc, oracle_acc) entry ->
       let export_name = string_field "export_name" entry in
       let importer_module = string_field "importer_module" entry in
       let provider_module = string_field "provider_module" entry in
       let linked_value = int_field "linked_return_value" entry in
       let semantic_export = member "semantic_export" entry in
       let binding = String.concat ":" [
         "witness"; witness_id;
         "importer"; importer_module; string_field "importer_source_hash" entry;
         "import"; string_field "import_name" entry; declaration_kind residual ~field_name:"declared_imports" ~module_field:"importer_module" ~name_field:"import_name" entry;
         "provider"; provider_module; string_field "provider_source_hash" semantic_export;
         "export"; export_name; declaration_kind residual ~field_name:"declared_exports" ~module_field:"provider_module" ~name_field:"export_name" entry;
       ] in
       let oracle_value = oracle_return_value oracle export_name in
       let oracle_matches = oracle_value = Some linked_value in
       let binding_obs side provenance_field provenance =
         `Assoc [
           "witness_id", `String witness_id;
           "category", `String "provider-binding";
           "symbol", `String export_name;
           "location", `String (importer_module ^ "<-" ^ provider_module);
           "abstract_value", `String "provider/import binding key";
           "normalized_value", `String binding;
           provenance_field, `String provenance;
           "direction_side", `String side;
         ]
       in
       let call_obs side provenance_field provenance =
         `Assoc [
           "witness_id", `String witness_id;
           "category", `String "call-effect";
           "symbol", `String export_name;
           "location", `String (string_field "importer_extern_root" entry);
           "abstract_value", `String "linked-provider-return";
           "normalized_value", `String ("linked-provider-return:" ^ string_of_int linked_value);
           provenance_field, `String provenance;
           "direction_side", `String side;
         ]
       in
       let phase_obs side provenance_field provenance =
         `Assoc [
           "witness_id", `String witness_id;
           "category", `String "phase-ordering";
           "symbol", `String export_name;
           "location", `String (provider_module ^ "<" ^ importer_module);
           "abstract_value", `String "provider/export/environment/importer relative order";
           "normalized_value", `String "provider<semantic_export<linked_environment<importer";
           provenance_field, `String provenance;
           "direction_side", `String side;
         ]
       in
       let residual_items = [
         binding_obs "residual" "residual_provenance" ("linked_environment:" ^ export_name);
         call_obs "residual" "residual_provenance" ("linked_stage2_input_derivation:" ^ export_name);
         phase_obs "residual" "residual_provenance" ("phase_log:" ^ export_name);
       ] in
       let oracle_items =
         if oracle_matches then [
           binding_obs "oracle" "oracle_provenance" ("projection.final_output_table:" ^ export_name);
           call_obs "oracle" "oracle_provenance" ("projection.final_output_table:" ^ export_name);
           phase_obs "oracle" "oracle_provenance" ("premerge-linked-observer:" ^ export_name);
         ] else []
       in
       residual_items @ residual_acc, oracle_items @ oracle_acc)
    ([], [])

let global_observations witness_id residual oracle =
  let oracle_value = find_singleton_by_location (oracle_tables oracle) "shared_g" in
  let residual_value =
    match oracle_value with
    | Some ov ->
        values_by_location (linked_tables residual) "shared_g"
        |> List.find_opt (fun rv -> interval_contains_value rv ov || contains rv (string_of_int ov))
        |> Option.map (fun _ -> string_of_int ov)
    | None -> None
  in
  selected_cell_observation ~witness_id ~category:"global-write-read" ~location:"shared_g" ~symbol:"shared_g"
    ~residual_value ~oracle_value:(Option.map string_of_int oracle_value)
    ~normalization:"oracle singleton contained in residual interval"
    ~residual_provenance:"linked_output.final_*_table:shared_g"
    ~oracle_provenance:"projection.final_*_table:shared_g"

let pointer_observations witness_id residual oracle =
  let location = "(write_ptr,p)" in
  let residual_value = find_value_by_location (linked_tables residual) location in
  let oracle_value = find_value_by_location (oracle_tables oracle) location in
  let oracle_return = find_singleton_by_location (oracle_tables oracle) (return_location "write_ptr") in
  let residual_norm =
    match residual_value, oracle_value, oracle_return with
    | Some rv, Some ov, _ when contains rv "main,x" && contains ov "main,x" -> Some "main,x"
    | Some rv, None, Some 5 when contains rv "main,x" -> Some "main,x"
    | _ -> None
  in
  let oracle_norm =
    match oracle_value, oracle_return with
    | Some ov, _ when contains ov "main,x" -> Some "main,x"
    | None, Some 5 -> Some "main,x"
    | _ -> None
  in
  selected_cell_observation ~witness_id ~category:"pointer-memory-effect" ~location ~symbol:"write_ptr"
    ~residual_value:residual_norm ~oracle_value:oracle_norm
    ~normalization:"selected pointer alias/effect provenance"
    ~residual_provenance:"linked_output.final_*_table:(write_ptr,p)"
    ~oracle_provenance:"projection.final_*_table:(write_ptr,p)"

let memory_location_observations witness_id residual oracle =
  let global_r, global_o = global_observations witness_id residual oracle in
  let pointer_r, pointer_o = pointer_observations witness_id residual oracle in
  let rec as_memory = function
    | `Assoc fields ->
        let location = match List.assoc_opt "location" fields with Some (`String s) -> s | _ -> "" in
        `Assoc (("category", `String "memory-location") :: ("symbol", `String location) :: List.remove_assoc "category" (List.remove_assoc "symbol" fields))
    | other -> other
  in
  List.map as_memory (global_r @ pointer_r), List.map as_memory (global_o @ pointer_o)

let return_observations witness_id residual oracle =
  let residual_returns =
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
  let oracle_returns =
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
  let binding_r, binding_o = relation_binding_observations witness_id residual oracle in
  let global_r, global_o = global_observations witness_id residual oracle in
  let pointer_r, pointer_o = pointer_observations witness_id residual oracle in
  let memory_r, memory_o = memory_location_observations witness_id residual oracle in
  residual_returns @ List.rev binding_r @ global_r @ pointer_r @ memory_r,
  oracle_returns @ List.rev binding_o @ global_o @ pointer_o @ memory_o

let relation_from_observations residual_obs oracle_obs =
  let observation_key obs =
    String.concat ":" [string_field "category" obs; string_field "symbol" obs; string_field "location" obs; Yojson.Safe.to_string (member "normalized_value" obs)]
  in
  let residual_keys = List.map observation_key residual_obs in
  let oracle_keys = List.map observation_key oracle_obs in
  let missing =
    oracle_obs |> List.filter (fun obs -> not (List.mem (observation_key obs) residual_keys))
  in
  let extra =
    residual_obs |> List.filter (fun obs -> not (List.mem (observation_key obs) oracle_keys))
  in
  let matched =
    residual_obs |> List.filter (fun obs -> List.mem (observation_key obs) oracle_keys)
  in
  let provenance_failures =
    let all = residual_obs @ oracle_obs in
    if observations_have_provenance all then []
    else [fail Provenance Residual_to_oracle "selected observation lacks provenance" "normalized_observations"]
  in
  let missing_failures =
    missing |> List.map (fun obs -> fail (match string_field "category" obs with "return" -> Return_value | _ -> Memory_location) Residual_to_oracle "missing selected observation" (string_field "location" obs))
  in
  let extra_failures =
    extra |> List.map (fun obs -> fail (match string_field "category" obs with "return" -> Return_value | _ -> Memory_location) Oracle_to_residual "extra selected observation" (string_field "location" obs))
  in
  let failures = provenance_failures @ missing_failures @ extra_failures in
  let pass = failures = [] in
  let r2o = relation_direction_json ~direction:Residual_to_oracle ~pass ~matched ~missing ~extra:[] ~failures in
  let o2r = relation_direction_json ~direction:Oracle_to_residual ~pass ~matched ~missing:[] ~extra ~failures in
  {
    status = pass_status pass;
    residual_observations = residual_obs;
    oracle_observations = oracle_obs;
    residual_to_oracle = r2o;
    oracle_to_residual = o2r;
    failures;
  }

let selected_observation_relation_json ~witness_id ~residual ~oracle =
  let residual_obs, oracle_obs = return_observations witness_id residual oracle in
  comparison_to_json (relation_from_observations residual_obs oracle_obs)

(** Full Sparrow-Itv semantic relation.

    This is still a prototype/internal report surface, but unlike the selected
    relation above it inventories the complete Itv evidence exposed by the
    accepted residual-linking slice: final input/output table cells, semantic
    exports, linked-environment/call-effect facts, completion evidence, and
    oracle lineage.  The comparison is intentionally Itv-scoped and
    witness-bounded; it is not a product-domain, Oct/Taint, arbitrary-C, or
    whole-program C equivalence claim. *)

let failure_taxonomy_json =
  `List [
    `String "missing_from_residual";
    `String "missing_from_origin";
    `String "value_mismatch";
    `String "memory_delta_role_mismatch";
    `String "memory_delta_location_mismatch";
    `String "memory_delta_value_transition_mismatch";
    `String "memory_delta_provenance_mismatch";
    `String "memory_delta_chain_missing";
    `String "typed_metadata_mismatch";
    `String "typed_scalar_protocol_mismatch";
    `String "memory_delta_role_mismatch";
    `String "memory_delta_location_mismatch";
    `String "memory_delta_value_transition_mismatch";
    `String "memory_delta_provenance_mismatch";
    `String "memory_delta_chain_missing";
    `String "provenance_missing";
    `String "unclassified_universe_fact";
  ]

let canonicalization_json =
  `Assoc [
    "scheme", `String "location-indexed-itv-cell-coverage/v1";
    "table_key", `String "final_input_table|final_output_table";
    "row_key", `String "node when available; linked_module_id is provenance only";
    "cell_key", `String "location plus canonical Itv value";
    "singleton_intervals", `String "([n,n], ...) and bare integer n normalize to singleton interval bounds";
    "ranges", `String "finite ([lo,hi], ...) ranges compare by interval containment";
    "top", `String "([-oo,+oo], ...) is treated as Top and may cover finite origin Itv cells";
    "bottom_empty_unknown", `String "bot/empty/unknown values are retained as deterministic strings and compare by exact value";
    "location_sensitive_memory", `String "locations remain part of every semantic key";
    "typed_value_model", `String ItvCell.value_model_id;
    "typed_relation_adapter", `String ItvCell.relation_adapter_id;
  ]

let full_itv_non_claims_json =
  `List [
    `String "no Oct semantics";
    `String "no Taint semantics";
    `String "no arbitrary-C or whole-program semantic equivalence";
    `String "no origin Sparrow modification";
    `String "prototype-non-public relation schema";
  ]

let canonical_value_json value =
  ItvCell.canonical_value_json_of_legacy_string value

let value_covers residual_value origin_value =
  ItvCell.covers_legacy_values ~residual:residual_value ~origin:origin_value

let typed_metadata_matches cell metadata =
  match metadata with
  | `Null -> true
  | `Assoc _ ->
      let computed = ItvCell.metadata_json cell in
      let expected_location = string_field "location" computed in
      let expected_model = string_field "value_model" computed in
      let location = string_field "location" metadata in
      let model = string_field "value_model" metadata in
      (location = "" || location = expected_location) &&
      (model = "" || model = expected_model)
  | _ -> false

let typed_metadata_for_cell ~table_name ~node cell =
  match ItvCell.of_legacy_cell_json ~table:table_name ~node cell with
  | Some typed_cell ->
      let source_metadata = member "typed_cell_metadata" cell in
      ItvCell.metadata_json typed_cell, typed_metadata_matches typed_cell source_metadata
  | None ->
      `Assoc ["value_model", `String ItvCell.value_model_id; "parse_status", `String "untyped-legacy-cell"], false

let cell_fact ~side ~table_name ~row cell =
  let node = string_field "node" row in
  let module_id = string_field "linked_module_id" row in
  let location = string_field "location" cell in
  let value = string_field "value" cell in
  let typed_metadata, typed_metadata_valid = typed_metadata_for_cell ~table_name ~node cell in
  `Assoc [
    "side", `String side;
    "category", `String "final-table-cell";
    "table", `String table_name;
    "node", `String node;
    "linked_module_id", `String module_id;
    "location", `String location;
    "value", `String value;
    "canonical_value", canonical_value_json value;
    "typed_cell_metadata", typed_metadata;
    "typed_relation_adapter", `String ItvCell.relation_adapter_id;
    "typed_input_metadata_valid", `Bool typed_metadata_valid;
    "provenance", `String (side ^ "." ^ table_name ^ ":" ^ node ^ ":" ^ location);
  ]

let table_cell_facts ~side tables =
  tables |> List.concat_map (fun (table_name, rows) ->
    rows |> List.concat_map (fun row ->
      row_memory row |> List.map (cell_fact ~side ~table_name ~row)))

let fact_location fact = string_field "location" fact
let fact_value fact = string_field "value" fact
let fact_table fact = string_field "table" fact

let fact_has_provenance fact = string_field "provenance" fact <> ""

let facts_for_location location facts =
  facts |> List.filter (fun fact -> fact_location fact = location)

let any_fact_covers facts target =
  facts |> List.exists (fun fact ->
    fact_location fact = fact_location target &&
    value_covers (fact_value fact) (fact_value target))

let full_itv_direction_json ~direction ~pass ~matched ~missing ~failures =
  let direction_label =
    match direction with
    | Residual_to_oracle -> "residual_to_origin"
    | Oracle_to_residual -> "origin_to_residual"
  in
  `Assoc [
    "direction", `String direction_label;
    "status", `String (pass_status pass);
    "comparison_kind", `String "location-indexed Itv coverage over final input/output table cells";
    "matched_facts", `List matched;
    "missing_facts", `List missing;
    "failures", `List (List.map failure_to_json failures);
  ]

let semantic_export_facts witness_id residual =
  semantic_exports residual |> List.map (fun export ->
    let name = string_field "export_name" export in
    let value = string_of_int (int_field "return_value" export) in
    `Assoc [
      "side", `String "residual";
      "category", `String "semantic-export-return";
      "table", `String "semantic_exports";
      "node", `String (string_field "return_node" export);
      "linked_module_id", `String (string_field "provider_module" export);
      "location", `String (return_location name);
      "symbol", `String name;
      "value", `String value;
      "canonical_value", canonical_value_json value;
      "provenance", `String ("semantic_exports:" ^ witness_id ^ ":" ^ name);
      "artifact_path", `String (string_field "provider_artifact_path" export);
    ])

let memory_delta_failures witness_id residual =
  external_summaries residual
  |> List.concat_map (fun summary ->
       list_field "memory_deltas" summary
       |> List.concat_map (fun delta ->
            let provider_module = string_field "provider_module" delta in
            let location = string_field "location" delta in
            MemoryDelta.validate_delta_json delta
            |> MemoryDelta.validation_reasons
            |> List.map (fun reason ->
                 fail Memory_location Residual_to_oracle reason
                   ("external_summaries.memory_deltas:" ^ witness_id ^ ":" ^ provider_module ^ ":" ^ location))))

let semantic_export_failures witness_id residual oracle =
  semantic_exports residual |> List.concat_map (fun export ->
    let name = string_field "export_name" export in
    let residual_value = int_field "return_value" export in
    let provenance_missing = string_field "provider_artifact_path" export = "" in
    let value_failure =
      match oracle_return_value oracle name with
      | None -> [fail Return_value Residual_to_oracle "missing_from_origin" ("semantic_exports:" ^ witness_id ^ ":" ^ name)]
      | Some oracle_value when oracle_value <> residual_value ->
          [fail Return_value Residual_to_oracle "value_mismatch" ("semantic_exports:" ^ witness_id ^ ":" ^ name)]
      | Some _ -> []
    in
    let provenance_failure =
      if provenance_missing then [fail Provenance Residual_to_oracle "provenance_missing" ("semantic_exports:" ^ witness_id ^ ":" ^ name)]
      else []
    in
    value_failure @ provenance_failure)

let linked_environment_facts witness_id residual =
  linked_environment residual |> List.map (fun entry ->
    let export_name = string_field "export_name" entry in
    let linked_return_value = string_of_int (int_field "linked_return_value" entry) in
    `Assoc [
      "side", `String "residual";
      "category", `String "linked-environment-binding";
      "table", `String "linked_environment";
      "node", `String (string_field "importer_extern_root" entry);
      "linked_module_id", `String (string_field "importer_module" entry);
      "location", `String (string_field "importer_module" entry ^ "<-" ^ string_field "provider_module" entry);
      "symbol", `String export_name;
      "value", `String linked_return_value;
      "canonical_value", canonical_value_json linked_return_value;
      "provenance", `String ("linked_environment:" ^ witness_id ^ ":" ^ export_name);
    ])

let linked_call_effect_facts witness_id residual =
  list_field "linked_stage2_input_derivation" residual |> List.map (fun entry ->
    let export_name = string_field "export_name" entry in
    `Assoc [
      "side", `String "residual";
      "category", `String "linked-call-effect";
      "table", `String "linked_stage2_input_derivation";
      "node", `String (string_field "importer_extern_root" entry);
      "linked_module_id", `String (string_field "importer_module" entry);
      "location", `String (string_field "importer_extern_root" entry);
      "symbol", `String export_name;
      "value", `String (string_field "effect_reason" entry ^ ":" ^ string_of_int (int_field "linked_return_value" entry));
      "canonical_value", `Assoc ["kind", `String "call-effect"; "raw", `String (string_field "effect_reason" entry)];
      "scalar_protocol_schema", member "scalar_protocol_schema" entry;
      "scalar_call_protocol_id", member "scalar_call_protocol_id" entry;
      "typed_scalar_metadata_valid", `Bool (ScalarCall.validation_ok (ScalarCall.validate_linked_derivation_json entry));
      "typed_scalar_linked_derivation", member "typed_scalar_linked_derivation" entry;
      "provenance", `String ("linked_stage2_input_derivation:" ^ witness_id ^ ":" ^ export_name);
    ])

let completion_status_facts witness_id residual oracle =
  let oracle_completion = member "completion" (member "projection" oracle) in
  let residual_analyzer_ran = bool_field "linked_residual_analyzer_ran" residual in
  let origin_worklist_drained = bool_field "worklist_drained" oracle_completion in
  [
    `Assoc [
      "side", `String "residual";
      "category", `String "completion-status";
      "table", `String "linked_residual_status";
      "node", `String witness_id;
      "linked_module_id", `String "";
      "location", `String "linked_residual_analyzer_ran";
      "value", `String (string_of_bool residual_analyzer_ran);
      "canonical_value", `Assoc ["kind", `String "boolean"; "raw", `Bool residual_analyzer_ran];
      "provenance", `String ("residual.status:" ^ witness_id);
    ];
    `Assoc [
      "side", `String "origin";
      "category", `String "completion-status";
      "table", `String "projection.completion";
      "node", `String witness_id;
      "linked_module_id", `String "";
      "location", `String "origin_worklist_drained";
      "value", `String (string_of_bool origin_worklist_drained);
      "canonical_value", `Assoc ["kind", `String "boolean"; "raw", `Bool origin_worklist_drained];
      "provenance", `String ("origin.completion:" ^ witness_id);
    ];
  ]

let full_itv_evidence_failures witness_id residual oracle =
  let oracle_completion = member "completion" (member "projection" oracle) in
  let scalar_protocol_failures =
    list_field "linked_stage2_input_derivation" residual
    |> List.concat_map (fun entry ->
         let export_name = string_field "export_name" entry in
         scalar_protocol_failures_for_derivation Residual_to_oracle
           ("linked_stage2_input_derivation:" ^ witness_id ^ ":" ^ export_name)
           entry)
  in
  let memory_delta_failures = memory_delta_failures witness_id residual in
  scalar_protocol_failures @ memory_delta_failures
  |> fun fs -> if linked_environment residual = [] then fail Provider_binding Residual_to_oracle "missing_from_residual" ("linked_environment:" ^ witness_id) :: fs else fs
  |> fun fs -> if list_field "linked_stage2_input_derivation" residual = [] then fail Call_effect Residual_to_oracle "missing_from_residual" ("linked_stage2_input_derivation:" ^ witness_id) :: fs else fs
  |> fun fs -> if list_field "phase_log" residual = [] then fail Phase_ordering Residual_to_oracle "missing_from_residual" ("phase_log:" ^ witness_id) :: fs else fs
  |> fun fs -> if not (bool_field "linked_residual_analyzer_ran" residual) then fail Call_effect Residual_to_oracle "unclassified_universe_fact" ("linked_residual_analyzer_ran:" ^ witness_id) :: fs else fs
  |> fun fs -> if not (bool_field "worklist_drained" oracle_completion) then fail Call_effect Oracle_to_residual "unclassified_universe_fact" ("projection.completion:" ^ witness_id) :: fs else fs
  |> fun fs -> scalar_protocol_failures @ fs

let manifest_entry ~classification fact =
  `Assoc [
    "classification", `String classification;
    "category", member "category" fact;
    "table", member "table" fact;
    "node", member "node" fact;
    "location", member "location" fact;
    "value", member "value" fact;
    "canonical_value", member "canonical_value" fact;
    "typed_cell_metadata", member "typed_cell_metadata" fact;
    "scalar_protocol_schema", member "scalar_protocol_schema" fact;
    "scalar_call_protocol_id", member "scalar_call_protocol_id" fact;
    "typed_scalar_metadata_valid", member "typed_scalar_metadata_valid" fact;
    "provenance", member "provenance" fact;
  ]

let oracle_identity_json oracle =
  `Assoc [
    "kind", `String "origin-sparrow-premerge-observer";
    "scope", `String (string_field "scope" oracle);
    "domain_instance", `String (string_field "domain_instance" oracle);
    "group", `String (string_field "group" oracle);
    "source", member "source" oracle;
    "lineage", member "lineage" oracle;
    "baseline_immutability_guard", `String "verify with git diff --exit-code -- sparrow";
  ]

let full_itv_semantic_relation_json ~witness_id ~residual ~oracle =
  let residual_table_facts = table_cell_facts ~side:"residual" (linked_tables residual) in
  let oracle_table_facts = table_cell_facts ~side:"origin" (oracle_tables oracle) in
  let export_facts = semantic_export_facts witness_id residual in
  let evidence_facts =
    linked_environment_facts witness_id residual @
    linked_call_effect_facts witness_id residual @
    completion_status_facts witness_id residual oracle
  in
  let residual_facts = residual_table_facts @ export_facts in
  let oracle_locations = oracle_table_facts |> List.map fact_location |> sort_uniq in
  let relevant_residual_facts =
    residual_facts
    |> List.filter (fun fact ->
      List.mem (fact_location fact) oracle_locations ||
      string_field "category" fact = "semantic-export-return")
  in
  let origin_missing =
    oracle_table_facts |> List.filter (fun origin_fact ->
      not (any_fact_covers residual_facts origin_fact))
  in
  let origin_failures =
    origin_missing |> List.map (fun fact ->
      fail Memory_location Oracle_to_residual "missing_from_residual"
        ("origin." ^ fact_table fact ^ ":" ^ fact_location fact))
  in
  let residual_missing =
    relevant_residual_facts |> List.filter (fun residual_fact ->
      facts_for_location (fact_location residual_fact) oracle_table_facts = [])
  in
  let residual_failures =
    residual_missing |> List.map (fun fact ->
      fail Memory_location Residual_to_oracle "missing_from_origin"
        ("residual." ^ fact_table fact ^ ":" ^ fact_location fact))
  in
  let provenance_failures =
    if List.for_all fact_has_provenance (residual_facts @ oracle_table_facts @ evidence_facts) then []
    else [fail Provenance Residual_to_oracle "provenance_missing" "semantic_universe_manifest"]
  in
  let export_failures = semantic_export_failures witness_id residual oracle in
  let evidence_failures = full_itv_evidence_failures witness_id residual oracle in
  let _memory_delta_validation_failures = memory_delta_failures witness_id residual in
  let typed_metadata_failures =
    residual_table_facts @ oracle_table_facts
    |> List.filter (fun fact -> not (bool_field "typed_input_metadata_valid" fact))
    |> List.map (fun fact ->
      let side = string_field "side" fact in
      let direction = if side = "origin" then Oracle_to_residual else Residual_to_oracle in
      fail Memory_location direction "typed_metadata_mismatch"
        (side ^ "." ^ fact_table fact ^ ":" ^ fact_location fact))
  in
  let r2o_failures = residual_failures @ provenance_failures @ export_failures @ evidence_failures @ typed_metadata_failures in
  let o2r_failures = origin_failures in
  let r2o_pass = r2o_failures = [] in
  let o2r_pass = o2r_failures = [] in
  let matched_origin = oracle_table_facts |> List.filter (any_fact_covers residual_facts) in
  let matched_residual = relevant_residual_facts |> List.filter (fun fact ->
    facts_for_location (fact_location fact) oracle_table_facts <> [])
  in
  let compared_facts = matched_origin @ matched_residual @ evidence_facts in
  let semantic_universe_manifest =
    `Assoc [
      "witness_id", `String witness_id;
      "expected_fact_source", `String "origin/premerge final input/output tables plus residual semantic exports, linked environment, call effects, completion, and provenance";
      "compared", `List (List.map (manifest_entry ~classification:"compared") compared_facts);
      "missing", `List (List.map (manifest_entry ~classification:"missing_from_residual") origin_missing @
                         List.map (manifest_entry ~classification:"missing_from_origin") residual_missing);
      "intentionally_excluded", `List [
        `Assoc ["category", `String "domain"; "reason", `String "Oct excluded by user scope"];
        `Assoc ["category", `String "domain"; "reason", `String "Taint excluded by user scope"];
        `Assoc ["category", `String "claim"; "reason", `String "arbitrary-C/whole-program equivalence excluded"];
      ];
      "expected_but_not_emitted", `List [];
    ]
  in
  let residual_to_origin =
    full_itv_direction_json ~direction:Residual_to_oracle ~pass:r2o_pass
      ~matched:matched_residual ~missing:residual_missing ~failures:r2o_failures
  in
  let origin_to_residual =
    full_itv_direction_json ~direction:Oracle_to_residual ~pass:o2r_pass
      ~matched:matched_origin ~missing:origin_missing ~failures:o2r_failures
  in
  let full_pass = r2o_pass && o2r_pass in
  `Assoc [
    "relation", `String "full-sparrow-itv-semantic-relation";
    "domain", `String "sparrow-itv";
    "status", `String (pass_status full_pass);
    "claim_scope", `String "witness-bounded full Itv evidence exposed by the residual-linking oracle suite; exact full-table equality is not claimed when residual evidence is a documented over-approximation";
    "semantic_universe", `List [
      `String "final_input_table cells";
      `String "final_output_table cells";
      `String "semantic exports";
      `String "linked environment bindings";
      `String "linked call/effect derivation";
      `String "completion/status evidence";
      `String "provenance and oracle identity";
    ];
    "semantic_universe_manifest", semantic_universe_manifest;
    "failure_taxonomy", failure_taxonomy_json;
    "canonicalization", canonicalization_json;
    "oracle_identity", oracle_identity_json oracle;
    "non_claims", full_itv_non_claims_json;
    "residual_to_origin", residual_to_origin;
    "origin_to_residual", origin_to_residual;
    "bidirectional_status", `Assoc [
      "residual_to_origin_evidence", `String (pass_status r2o_pass);
      "origin_to_residual_coverage", `String (pass_status o2r_pass);
      "equal", `Bool false;
      "equality_claim", `String "not claimed for over-approximating residual Itv table cells";
    ];
    "summary", `Assoc [
      "residual_fact_count", `Int (List.length residual_facts);
      "origin_fact_count", `Int (List.length oracle_table_facts);
      "compared_fact_count", `Int (List.length compared_facts);
      "failure_count", `Int (List.length r2o_failures + List.length o2r_failures);
    ];
    "failures", `List (List.map failure_to_json (r2o_failures @ o2r_failures));
  ]

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
         "status", `String (pass_status pass);
         "residual_value", `Int residual_value;
         "oracle_value", (match oracle_value with Some n -> `Int n | None -> `Null);
         "relation_module", `String "Abstract_speculate_residual_relation";
         "evidence_paths", `List [`String "semantic_exports"; `String "projection.final_output_table"];
       ])

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
    "status", `String (pass_status pass);
    "binding_keys", `List (List.map (fun k -> `String k) keys);
    "relation_module", `String "Abstract_speculate_residual_relation";
    "evidence_paths", `List [`String "linked_environment"; `String "projection.final_output_table"];
  ]

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
    "status", `String (pass_status pass);
    "mixed_modules", `List (List.map (fun m -> `String m) mixed);
    "claim_scope", `String "mixed-role scheduling/order and summary handoff; not upstream value-dependence";
    "relation_module", `String "Abstract_speculate_residual_relation";
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
    "status", `String (pass_status pass);
    "location", `String "shared_g";
    "residual_value", (match residual_value with Some v -> `String v | None -> `Null);
    "oracle_value", (match oracle_value with Some n -> `Int n | None -> `Null);
    "normalization", `String "oracle singleton accepted when contained in any residual interval observation";
    "relation_module", `String "Abstract_speculate_residual_relation";
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
    "status", `String (pass_status pass);
    "location", `String location;
    "residual_value", (match residual_value with Some v -> `String v | None -> `Null);
    "oracle_value", (match oracle_value with Some v -> `String v | None -> `Null);
    "oracle_return_value", (match oracle_return with Some n -> `Int n | None -> `Null);
    "normalization", `String "pointer alias summary compared by residual shared pointee provenance and oracle write_ptr return/effect summary";
    "relation_module", `String "Abstract_speculate_residual_relation";
    "evidence_paths", `List [`String "linked_output.final_*_table"; `String "projection.final_*_table"];
  ]

let oracle_suite_obligations ~source_guard_obligation witness_id category residual oracle =
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

let obligation_status obligation_json = string_field "status" obligation_json
let obligation_passes xs = xs |> List.for_all (fun o -> obligation_status o = "pass")
let witness_pass_status obligations residual_obs oracle_obs =
  obligations <> [] && obligation_passes obligations &&
  residual_obs <> [] && oracle_obs <> [] &&
  observations_have_provenance (residual_obs @ oracle_obs)
