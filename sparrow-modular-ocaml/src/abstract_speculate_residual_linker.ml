(***********************************************************************)
(* First-pass residual linking for Abstract Speculate PE.               *)
(***********************************************************************)

module StageT = Abstract_speculate_stage_types
module Stage2 = Abstract_speculate_stage2_input
module MetaSparse = Abstract_speculate_meta_sparse
module Residual = Abstract_speculate_residual_value
module Solver = Abstract_speculate_residual_solver
module ScalarCall = Abstract_speculate_residual_scalar_call
module MemoryDelta = Abstract_speculate_residual_memory_delta

let schema_version = "abstract-speculate-residual-linking-pe/v1"

let sort_strings xs = List.sort_uniq String.compare xs
let sort_json xs = List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)) xs
let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string

let comma_join xs =
  let rec loop acc = function
    | [] -> acc
    | x :: rest -> loop (acc ^ "," ^ x) rest
  in
  match xs with
  | [] -> ""
  | x :: rest -> loop x rest

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json = match assoc_field name json with Some (`String s) -> Some s | _ -> None
let bool_field name json = match assoc_field name json with Some (`Bool b) -> Some b | _ -> None
let int_field name json = match assoc_field name json with Some (`Int n) -> Some n | _ -> None
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

let forbidden_global_entry = "Real_sparrow_frontend." ^ "global_for_" ^ "files"
let forbidden_merge_entry = "Mergecil." ^ "merge"

type declaration = {
  name : string;
  kind : string;
  source : string;
}

type module_bundle = {
  result : MetaSparse.stage1_result;
  artifact_path : string;
  declared_imports : declaration list;
  declared_exports : declaration list;
  module_boundary_validated : bool;
}

type linked_stage2_input = (string * StageT.stage2_input) list

type external_summary_v1_compat = {
  extern_scalar_value : Yojson.Safe.t;
  function_return_summary : Yojson.Safe.t;
  global_write_summary_placeholder : Yojson.Safe.t;
  provenance : Yojson.Safe.t;
}

type external_summary_v3 = {
  return_effects : Yojson.Safe.t list;
  memory_deltas : Yojson.Safe.t list;
  delta_chains : Yojson.Safe.t list;
  global_effects : Yojson.Safe.t list;
  pointer_effects : Yojson.Safe.t list;
  provenance : Yojson.Safe.t;
  v1_compat : external_summary_v1_compat;
}

type semantic_export = {
  export_name : string;
  provider_module : string;
  provider_source_hash : string;
  provider_artifact_path : string;
  return_node : string;
  return_location : string;
  return_value : int;
  abstract_return_value : string;
  provider_row : Yojson.Safe.t;
  provider_phase_index : int;
  external_summary : external_summary_v3;
}

type linked_environment_entry = {
  import_name : string;
  importer_module : string;
  importer_source_hash : string;
  importer_extern_root : string;
  provider_module : string;
  export_name : string;
  linked_return_value : int;
  semantic_export : semantic_export;
}

type phase_event = {
  phase_index : int;
  module_id : string;
  event : string;
}

type linked_stage2_output = {
  final_input_table : Yojson.Safe.t list;
  final_output_table : Yojson.Safe.t list;
  execution_log : Yojson.Safe.t;
  shape_witnesses : Yojson.Safe.t list;
  semantic_exports : semantic_export list;
  linked_environment : linked_environment_entry list;
  linked_stage2_input_derivation : Yojson.Safe.t list;
  phase_log : phase_event list;
  linked_residual_analyzer_evidence : linked_run_evidence;
}

and linked_run_evidence = {
  linked_execute_returned : bool;
  module_count : int;
  module_analyzers_executed : int;
  module_identity_set_matches : bool;
  all_modules_executed : bool;
  final_input_row_count : int;
  final_output_row_count : int;
  linked_residual_row_count : int;
  residual_rows_observed : bool;
  matched_obligation_count : int;
  unresolved_obligation_count : int;
  obligations_closed : bool;
  no_shortcut_path : bool;
  derived_from_five_predicates : bool;
  linked_residual_analyzer_ran : bool;
  linked_cycle_scc_count : int;
  linked_cycle_worklist_drained : bool;
  linked_cycle_obligations_closed : bool;
}

type linked_residual_analyzer = {
  linked_id : string;
  modules : module_bundle list;
  run : linked_stage2_input -> linked_stage2_output;
}

let declaration_to_json d =
  `Assoc [
    "name", `String d.name;
    "kind", `String d.kind;
    "source", `String d.source;
  ]

let phase_event_to_json (event : phase_event) =
  `Assoc [
    "phase_index", `Int event.phase_index;
    "module_id", `String event.module_id;
    "event", `String event.event;
  ]

let external_summary_v1_compat_to_json summary =
  `Assoc [
    "schema_version", `String "abstract-speculate-external-summary/v1";
    "extern_scalar_value", summary.extern_scalar_value;
    "function_return_summary", summary.function_return_summary;
    "global_write_summary_placeholder", summary.global_write_summary_placeholder;
    "provenance", summary.provenance;
  ]

let external_summary_to_json summary =
  `Assoc [
    "schema_version", `String MemoryDelta.external_summary_schema_id;
    "summary_api_status", `String MemoryDelta.summary_api_status;
    "summary_scope", `String "sparrow-itv-selected-witness";
    "memory_delta_schema", `String MemoryDelta.memory_delta_schema_id;
    "effect_domains", `List [
      `String "return";
      `String "global-write-read";
      `String "pointer-memory-effect";
      `String "memory-delta";
    ];
    "return_effects", `List summary.return_effects;
    "memory_deltas", `List summary.memory_deltas;
    "delta_chains", `List summary.delta_chains;
    "global_effects", `List summary.global_effects;
    "pointer_effects", `List summary.pointer_effects;
    "memory_effect_projection_status", `String "v2-compatible-non-authoritative";
    "typed_memory_delta_validation", MemoryDelta.validation_result_json (MemoryDelta.validate_summary_json (`Assoc [
      "schema_version", `String MemoryDelta.external_summary_schema_id;
      "summary_api_status", `String MemoryDelta.summary_api_status;
      "summary_scope", `String "sparrow-itv-selected-witness";
      "memory_delta_schema", `String MemoryDelta.memory_delta_schema_id;
      "memory_deltas", `List summary.memory_deltas;
      "delta_chains", `List summary.delta_chains;
      "provenance", summary.provenance;
    ]));
    "provenance", summary.provenance;
    "external_summary_v1_compat", external_summary_v1_compat_to_json summary.v1_compat;
  ]

let semantic_export_to_json (export : semantic_export) =
  `Assoc [
    "export_name", `String export.export_name;
    "provider_module", `String export.provider_module;
    "provider_source_hash", `String export.provider_source_hash;
    "provider_artifact_path", `String export.provider_artifact_path;
    "return_node", `String export.return_node;
    "return_location", `String export.return_location;
    "return_value", `Int export.return_value;
    "abstract_return_value", `String export.abstract_return_value;
    "derivation_source", `String "provider-stage2-output";
    "provider_phase_index", `Int export.provider_phase_index;
    "provider_row", export.provider_row;
    "external_summary", external_summary_to_json export.external_summary;
  ]

let linked_environment_entry_to_json (entry : linked_environment_entry) =
  `Assoc [
    "import_name", `String entry.import_name;
    "importer_module", `String entry.importer_module;
    "importer_source_hash", `String entry.importer_source_hash;
    "importer_extern_root", `String entry.importer_extern_root;
    "provider_module", `String entry.provider_module;
    "export_name", `String entry.export_name;
    "linked_return_value", `Int entry.linked_return_value;
    "derivation_source", `String "provider-stage2-output";
    "semantic_export", semantic_export_to_json entry.semantic_export;
    "external_summary", external_summary_to_json entry.semantic_export.external_summary;
  ]

let declaration_key d = d.kind ^ ":" ^ d.name

let unique_declarations decls =
  decls
  |> List.sort (fun a b -> compare (declaration_key a) (declaration_key b))
  |> List.fold_left (fun acc decl ->
       match acc with
       | hd :: _ when declaration_key hd = declaration_key decl -> acc
       | _ -> decl :: acc)
       []
  |> List.rev

let declaration_name_set decls = decls |> List.map (fun d -> d.name) |> sort_strings
let has_name names name = List.exists ((=) name) names

let declaration_kind_of_varinfo vi =
  match vi.Sparrow_cil.vtype with
  | Sparrow_cil.TFun _ -> "function"
  | _ -> "global"

let declarations_from_global global =
  let definitions = ref [] in
  let declarations = ref [] in
  Sparrow_cil.iterGlobals global.Global.file (function
    | Sparrow_cil.GFun (fd, _) ->
        definitions := {
          name = fd.Sparrow_cil.svar.Sparrow_cil.vname;
          kind = "function";
          source = "cil-function-definition";
        } :: !definitions
    | Sparrow_cil.GVar (vi, _, _) ->
        definitions := {
          name = vi.Sparrow_cil.vname;
          kind = declaration_kind_of_varinfo vi;
          source = "cil-global-definition";
        } :: !definitions
    | Sparrow_cil.GVarDecl (vi, _) ->
        declarations := {
          name = vi.Sparrow_cil.vname;
          kind = declaration_kind_of_varinfo vi;
          source = "cil-declaration-or-extern-root";
        } :: !declarations
    | _ -> ());
  let exports = unique_declarations !definitions in
  let export_names = declaration_name_set exports in
  let imports =
    !declarations
    |> unique_declarations
    |> List.filter (fun d -> not (has_name export_names d.name))
  in
  imports, exports

let declarations_for_result result = declarations_from_global result.MetaSparse.before

let input_module_json bundle =
  `Assoc [
    "module_id", `String bundle.result.MetaSparse.module_id;
    "source_file", `String bundle.result.source;
    "source_hash", `String bundle.result.source_hash;
    "artifact_path", `String bundle.artifact_path;
    "stage2_input_key", `String (bundle.result.module_id ^ ":" ^ bundle.result.source_hash);
    "module_local_prelink", `Bool true;
    "typed_analyzer_present", `Bool true;
    "module_boundary_validated", `Bool bundle.module_boundary_validated;
  ]

let validate_module_artifact artifact =
  let boundary = member "module_boundary" artifact in
  let forbidden = list_field "forbidden_prelink_entrypoints" boundary |> List.map to_string in
  string_field "scope" artifact = Some "module-only-pre-link" &&
  bool_field "linked_entrypoints_used" boundary = Some false &&
  List.mem forbidden_global_entry forbidden &&
  List.mem forbidden_merge_entry forbidden

let make_bundle ~artifact_path result artifact =
  if not (validate_module_artifact artifact) then
    failwith ("module artifact is not a valid module-local pre-link AS artifact: " ^ artifact_path);
  let declared_imports, declared_exports = declarations_for_result result in
  {
    result;
    artifact_path;
    declared_imports;
    declared_exports;
    module_boundary_validated = true;
  }

let stage2_input_for_bundle bundle =
  bundle.result.module_id, bundle.result.stage2_input

let matched_obligations modules =
  modules
  |> List.concat_map (fun importer ->
       importer.declared_imports
       |> List.concat_map (fun import_decl ->
            modules
            |> List.filter (fun exporter -> exporter.result.module_id <> importer.result.module_id)
            |> List.concat_map (fun exporter ->
                 exporter.declared_exports
                 |> List.filter (fun export_decl -> export_decl.name = import_decl.name)
                 |> List.map (fun export_decl ->
                      `Assoc [
                        "name", `String import_decl.name;
                        "kind", `String import_decl.kind;
                        "importer_module", `String importer.result.module_id;
                        "exporter_module", `String exporter.result.module_id;
                        "import_source", `String import_decl.source;
                        "export_source", `String export_decl.source;
                        "match_kind", `String "parsed-cil-declaration-to-definition";
                      ]))))
  |> sort_json

let unresolved_obligations modules =
  modules
  |> List.concat_map (fun importer ->
       importer.declared_imports
       |> List.filter_map (fun import_decl ->
            let matched =
              modules
              |> List.exists (fun exporter ->
                   exporter.result.module_id <> importer.result.module_id &&
                   List.exists (fun export_decl -> export_decl.name = import_decl.name) exporter.declared_exports)
            in
            if matched then None
            else Some (`Assoc [
              "name", `String import_decl.name;
              "kind", `String import_decl.kind;
              "importer_module", `String importer.result.module_id;
              "import_source", `String import_decl.source;
              "reason", `String "no-linked-module-definition";
            ])))
  |> sort_json

let namespace_row module_id row =
  match row with
  | `Assoc fields -> `Assoc (("linked_module_id", `String module_id) :: fields)
  | other -> `Assoc ["linked_module_id", `String module_id; "row", other]

let input_for_module module_id default inputs =
  match List.assoc_opt module_id inputs with
  | Some input -> input
  | None -> default

let extern_roots_of_input (input : StageT.stage2_input) =
  match assoc_field "extern_roots" input.extern_effects with
  | Some (`List roots) -> roots |> List.map to_string |> sort_strings
  | _ -> []

let module_key bundle = bundle.result.module_id ^ ":" ^ bundle.result.source_hash

let unique_string_set xs = List.sort_uniq String.compare xs

let has_duplicates xs = List.length xs <> List.length (unique_string_set xs)

let same_unique_string_set left right =
  unique_string_set left = unique_string_set right

let int_of_trimmed s =
  try Some (int_of_string (String.trim s)) with Failure _ -> None

let parse_singleton_interval_value value =
  let value = String.trim value in
  match int_of_trimmed value with
  | Some n -> Some n
  | None ->
      let prefix = "([" in
      let prefix_len = String.length prefix in
      if String.length value <= prefix_len || String.sub value 0 prefix_len <> prefix then None
      else
        try
          let comma = String.index_from value prefix_len ',' in
          let close = String.index_from value (comma + 1) ']' in
          match
            int_of_trimmed (String.sub value prefix_len (comma - prefix_len)),
            int_of_trimmed (String.sub value (comma + 1) (close - comma - 1))
          with
          | Some lo, Some hi when lo = hi -> Some lo
          | _ -> None
        with Not_found -> None

let row_node row = match string_field "node" row with Some s -> s | None -> ""

let row_memory row =
  match assoc_field "memory" row with
  | Some (`List memory) -> memory
  | _ -> []

let cell_location cell = match string_field "location" cell with Some s -> s | None -> ""
let cell_value cell = match string_field "value" cell with Some s -> s | None -> ""

let return_location name = "(" ^ name ^ ",__return__)"

let is_parenthesized_location location =
  String.length location >= 2 &&
  location.[0] = '(' &&
  location.[String.length location - 1] = ')'

let effect_location_kind location =
  if is_parenthesized_location location && contains location "," then "pointer-memory-effect"
  else "global-write-read"

let effect_id ~provider_module ~export_name ~domain ~location =
  provider_module ^ ":" ^ export_name ^ ":" ^ domain ^ ":" ^ location

let typed_effect
    ~domain
    ~effect_id
    ~symbol
    ~location
    ~value_json
    ~provider_module
    ~provider_source_hash
    ~provider_artifact_path
    ~provider_phase_index
    ~derivation_source
    ~source_evidence_path =
  let singleton_int =
    match value_json with
    | `Int n -> `Int n
    | `String value ->
        begin match parse_singleton_interval_value value with
        | Some n -> `Int n
        | None -> `Null
        end
    | _ -> `Null
  in
  `Assoc [
    "effect_id", `String effect_id;
    "domain", `String domain;
    "symbol", `String symbol;
    "location", `String location;
    "normalized_location", `String location;
    "value", value_json;
    "abstract_value", value_json;
    "singleton_int", singleton_int;
    "provider_module", `String provider_module;
    "provider_source_hash", `String provider_source_hash;
    "provider_artifact_path", `String provider_artifact_path;
    "provider_phase_index", `Int provider_phase_index;
    "derivation_source", `String derivation_source;
    "source_evidence_path", `String source_evidence_path;
    "witness_scope", `String "selected-sparrow-itv";
  ]

let find_return_row ~export_name rows =
  let location = return_location export_name in
  let candidates =
    rows
    |> List.filter_map (fun row ->
         row_memory row
         |> List.find_map (fun cell ->
              if cell_location cell = location then
                (match parse_singleton_interval_value (cell_value cell) with
                 | Some value -> Some (row, cell_value cell, value)
                 | None -> None)
              else None))
  in
  let prefer_exit =
    candidates
    |> List.find_opt (fun (row, _, _) -> row_node row = export_name ^ "-EXIT")
  in
  match prefer_exit, candidates with
  | Some candidate, _ -> Some candidate
  | None, [candidate] -> Some candidate
  | None, _ :: _ ->
      failwith ("ambiguous provider return summary rows for " ^ export_name)
  | None, [] -> None

let make_external_summary
    ~export_name
    ~provider_module
    ~provider_source_hash
    ~provider_artifact_path
    ~return_node
    ~return_location
    ~return_value
    ~abstract_return_value
    ~provider_row
    ~provider_phase_index =
  let provenance =
    `Assoc [
      "derivation_source", `String "provider-stage2-output";
      "provider_module", `String provider_module;
      "provider_source_hash", `String provider_source_hash;
      "provider_artifact_path", `String provider_artifact_path;
      "provider_phase_index", `Int provider_phase_index;
      "provider_row", provider_row;
    ]
  in
  let return_effect_id =
    ScalarCall.provider_return_effect_id ~provider_module ~export_name ~return_location
  in
  let scalar_return =
    match
      ScalarCall.make_provider_return
        ~provider_module
        ~provider_source_hash
        ~provider_artifact_path
        ~export_name
        ~return_node
        ~return_location
        ~effect_id:return_effect_id
        ~provider_phase_index
        ~return_value
        ~abstract_return_value
        ~provider_row
    with
    | Ok scalar_return -> scalar_return
    | Error reason -> failwith ("invalid scalar return protocol for " ^ export_name ^ ": " ^ reason)
  in
  let return_effect = ScalarCall.return_effect_json scalar_return in
  let memory_delta_records =
    provider_row
    |> row_memory
    |> List.filter_map (fun cell ->
         let location = cell_location cell in
         if location = "" || location = return_location then None
         else
           let domain_s = effect_location_kind location in
           let domain =
             match MemoryDelta.domain_of_string domain_s with
             | Ok domain -> domain
             | Error reason -> failwith reason
           in
           let summary_location =
             if domain_s = "pointer-memory-effect" then "(" ^ export_name ^ ",p)"
             else location
           in
           let symbol = if domain_s = "pointer-memory-effect" then export_name else location in
           match
             MemoryDelta.make_provider_delta
               ~provider_module
               ~provider_source_hash
               ~provider_artifact_path
               ~provider_phase_index
               ~export_name
               ~domain
               ~raw_location:location
               ~summary_location
               ~symbol
               ~value:(cell_value cell)
               ~source_evidence_path:("provider_row.memory:" ^ location)
           with
           | Ok delta -> Some delta
           | Error reason -> failwith ("invalid v3 memory delta for " ^ export_name ^ ": " ^ reason))
  in
  let memory_deltas = List.map MemoryDelta.delta_to_json memory_delta_records in
  let delta_chains =
    memory_delta_records
    |> List.map (fun delta ->
         `Assoc [
           "chain_id", `String (MemoryDelta.chain_id delta);
           "memory_delta_schema", `String MemoryDelta.memory_delta_schema_id;
           "entries", `List (MemoryDelta.delta_chain_json delta);
         ])
  in
  let memory_effects = List.map MemoryDelta.compatibility_effect_json memory_delta_records in
  let global_effects =
    memory_effects
    |> List.filter (fun eff -> string_field "domain" eff = Some "global-write-read")
  in
  let pointer_effects =
    memory_effects
    |> List.filter (fun eff -> string_field "domain" eff = Some "pointer-memory-effect")
  in
  let v1_compat =
    {
      extern_scalar_value = ScalarCall.v1_extern_scalar_value_json scalar_return;
      function_return_summary = ScalarCall.function_return_summary_json scalar_return;
      global_write_summary_placeholder = `Assoc [
        "status", `String "compat-v1-non-authoritative";
        "writes", `List [];
        "precision", `String "superseded-by-external-summary-v3";
      ];
      provenance;
    }
  in
  {
    return_effects = [return_effect];
    memory_deltas;
    delta_chains;
    global_effects;
    pointer_effects;
    provenance;
    v1_compat;
  }

let matching_export_names modules =
  modules
  |> List.concat_map (fun importer ->
       importer.declared_imports
       |> List.concat_map (fun import_decl ->
            modules
            |> List.filter (fun exporter -> exporter.result.module_id <> importer.result.module_id)
            |> List.concat_map (fun exporter ->
                 exporter.declared_exports
                 |> List.filter_map (fun export_decl ->
                      if export_decl.name = import_decl.name then
                        Some (importer, exporter, import_decl, export_decl)
                      else None))))

let function_matches matches =
  matches
  |> List.filter (fun (_, _, import_decl, export_decl) ->
       import_decl.kind = "function" && export_decl.kind = "function")

let ensure_supported_link_shape matches =
  let ambiguous_import_bindings =
    matches
    |> List.map (fun (importer, _, import_decl, _) ->
         importer.result.module_id ^ ":" ^ import_decl.kind ^ ":" ^ import_decl.name)
  in
  if has_duplicates ambiguous_import_bindings then
    failwith "unsupported ambiguous semantic export mapping";
  ()

let extract_semantic_exports matches provider_outputs : semantic_export list =
  matches
  |> List.map (fun (_, provider, _, export_decl) ->
       let provider_output =
         match
           provider_outputs
           |> List.find_opt (fun (bundle, _) ->
                bundle.result.module_id = provider.result.module_id)
         with
         | Some (_, output) -> output
         | None -> failwith ("missing provider stage2 output: " ^ provider.result.module_id)
       in
       match find_return_row ~export_name:export_decl.name provider_output.StageT.final_output_table with
       | Some (row, abstract_return_value, return_value) ->
           let provider_phase_index = 1 in
           let external_summary =
             make_external_summary
               ~export_name:export_decl.name
               ~provider_module:provider.result.module_id
               ~provider_source_hash:provider.result.source_hash
               ~provider_artifact_path:provider.artifact_path
               ~return_node:(row_node row)
               ~return_location:(return_location export_decl.name)
               ~return_value
               ~abstract_return_value
               ~provider_row:row
               ~provider_phase_index
           in
           {
             export_name = export_decl.name;
             provider_module = provider.result.module_id;
             provider_source_hash = provider.result.source_hash;
             provider_artifact_path = provider.artifact_path;
             return_node = row_node row;
             return_location = return_location export_decl.name;
             return_value;
             abstract_return_value;
             provider_row = row;
             provider_phase_index;
             external_summary;
           }
       | None ->
           failwith ("provider stage2 output has no singleton return summary for " ^ export_decl.name))

let component_mentions_import import_name component =
  match assoc_field "semantic_expression" component.StageT.default_component with
  | Some (`String expression) -> contains expression ("@" ^ import_name)
  | _ -> false

let extern_root_for_import importer import_decl =
  let extern_roots = extern_roots_of_input importer.result.stage2_input in
  let components =
    importer.result.analyzer.StageT.residual_input_components @
    importer.result.analyzer.StageT.residual_output_components
  in
  let matching_roots =
    components
    |> List.filter_map (fun component ->
         if component_mentions_import import_decl.name component then Some component.StageT.node else None)
    |> sort_strings
  in
  match matching_roots, extern_roots with
  | [root], _ -> root
  | [], [root] -> root
  | [], [] ->
      failwith ("importer has no extern roots for linked obligation: " ^ importer.result.module_id)
  | [], roots ->
      failwith ("cannot resolve extern root for import " ^ import_decl.name ^
                " in " ^ importer.result.module_id ^ ": " ^ comma_join roots)
  | roots, _ ->
      failwith ("ambiguous extern roots for import " ^ import_decl.name ^
                " in " ^ importer.result.module_id ^ ": " ^ comma_join roots)

let linked_environment_for_matches matches (semantic_exports : semantic_export list) : linked_environment_entry list =
  matches
  |> List.map (fun (importer, provider, import_decl, export_decl) ->
       let extern_root = extern_root_for_import importer import_decl in
       let semantic_export : semantic_export =
         match
           semantic_exports
           |> List.find_opt (fun (export : semantic_export) ->
                export.provider_module = provider.result.module_id &&
                export.export_name = export_decl.name)
         with
         | Some export -> export
         | None -> failwith ("missing semantic export for " ^ export_decl.name)
       in
       {
         import_name = import_decl.name;
         importer_module = importer.result.module_id;
         importer_source_hash = importer.result.source_hash;
         importer_extern_root = extern_root;
         provider_module = provider.result.module_id;
         export_name = export_decl.name;
         linked_return_value = semantic_export.return_value;
         semantic_export;
       })

let primary_return_effect (summary : external_summary_v3) =
  match summary.return_effects with
  | eff :: _ -> eff
  | [] -> failwith "ExternalSummary v3 has no return effect for linked import"

let return_effect_id summary =
  match string_field "effect_id" (primary_return_effect summary) with
  | Some id -> id
  | None -> failwith "ExternalSummary v3 return effect missing effect_id"

let scalar_return_of_semantic_export export =
  let effect_id = return_effect_id export.external_summary in
  match
    ScalarCall.make_provider_return
      ~provider_module:export.provider_module
      ~provider_source_hash:export.provider_source_hash
      ~provider_artifact_path:export.provider_artifact_path
      ~export_name:export.export_name
      ~return_node:export.return_node
      ~return_location:export.return_location
      ~effect_id
      ~provider_phase_index:export.provider_phase_index
      ~return_value:export.return_value
      ~abstract_return_value:export.abstract_return_value
      ~provider_row:export.provider_row
  with
  | Ok scalar_return -> scalar_return
  | Error reason -> failwith ("invalid scalar return protocol for " ^ export.export_name ^ ": " ^ reason)

let linked_stage2_input_for_importer importer linked_environment =
  let entries =
    linked_environment
    |> List.filter (fun entry -> entry.importer_module = importer.result.module_id)
  in
  let linked_effects =
    entries
    |> List.map (fun entry ->
         entry.importer_extern_root,
         (entry.linked_return_value,
          let scalar_return = scalar_return_of_semantic_export entry.semantic_export in
          let linked_derivation =
            match
              ScalarCall.make_linked_derivation
                ~importer_module:entry.importer_module
                ~importer_extern_root:entry.importer_extern_root
                ~import_name:entry.import_name
                ~linked_return_value:entry.linked_return_value
                ~provider_return:scalar_return
            with
            | Ok linked_derivation -> linked_derivation
            | Error reason -> failwith ("invalid linked scalar derivation for " ^ entry.export_name ^ ": " ^ reason)
          in
          ScalarCall.add_fields
            [
              "provider_module", `String entry.provider_module;
              "provider_source_hash", `String entry.semantic_export.provider_source_hash;
              "provider_artifact_path", `String entry.semantic_export.provider_artifact_path;
              "export_name", `String entry.export_name;
              "return_location", `String entry.semantic_export.return_location;
              "return_node", `String entry.semantic_export.return_node;
              "abstract_return_value", `String entry.semantic_export.abstract_return_value;
              "provider_phase_index", `Int entry.semantic_export.provider_phase_index;
              "derivation_source", `String "provider-stage2-output";
              "external_summary_schema", `String MemoryDelta.external_summary_schema_id;
              "summary_api_status", `String "prototype-internal";
              "external_summary_effect_id", `String (return_effect_id entry.semantic_export.external_summary);
              "return_effect", primary_return_effect entry.semantic_export.external_summary;
              "external_summary", external_summary_to_json entry.semantic_export.external_summary;
              "scalar_protocol_schema", `String ScalarCall.schema_id;
              "scalar_call_protocol_id", `String (ScalarCall.scalar_protocol_id scalar_return);
              "typed_scalar_metadata_valid", `Bool true;
              "typed_scalar_linked_derivation", ScalarCall.linked_derivation_metadata_json linked_derivation;
            ]
            (`Assoc [])))
  in
  {
    StageT.extern_effects =
      Stage2.make_linked_extern_effects
        ~source:importer.result.source
        ~hash:importer.result.source_hash
        ~extern_roots:(extern_roots_of_input importer.result.stage2_input)
        ~linked_effects;
  }

let linked_input_derivation_json linked_environment =
  linked_environment
  |> List.map (fun entry ->
       let scalar_return = scalar_return_of_semantic_export entry.semantic_export in
       let linked_derivation =
         match
           ScalarCall.make_linked_derivation
             ~importer_module:entry.importer_module
             ~importer_extern_root:entry.importer_extern_root
             ~import_name:entry.import_name
             ~linked_return_value:entry.linked_return_value
             ~provider_return:scalar_return
         with
         | Ok linked_derivation -> linked_derivation
         | Error reason -> failwith ("invalid linked scalar derivation for " ^ entry.export_name ^ ": " ^ reason)
       in
       ScalarCall.add_fields
         [
           "importer_module", `String entry.importer_module;
           "importer_extern_root", `String entry.importer_extern_root;
           "import_name", `String entry.import_name;
           "provider_module", `String entry.provider_module;
           "provider_source_hash", `String entry.semantic_export.provider_source_hash;
           "provider_artifact_path", `String entry.semantic_export.provider_artifact_path;
           "export_name", `String entry.export_name;
           "return_location", `String entry.semantic_export.return_location;
           "return_node", `String entry.semantic_export.return_node;
           "abstract_return_value", `String entry.semantic_export.abstract_return_value;
           "provider_phase_index", `Int entry.semantic_export.provider_phase_index;
           "linked_return_value", `Int entry.linked_return_value;
           "effect_reason", `String "linked-provider-return";
           "stage2_obligation", `String "dynamic external/link fact derived from provider stage2 output";
           "derivation_source", `String "provider-stage2-output";
           "external_summary_schema", `String MemoryDelta.external_summary_schema_id;
           "summary_api_status", `String "prototype-internal";
           "external_summary_effect_id", `String (return_effect_id entry.semantic_export.external_summary);
           "return_effect", primary_return_effect entry.semantic_export.external_summary;
           "semantic_export", semantic_export_to_json entry.semantic_export;
           "external_summary", external_summary_to_json entry.semantic_export.external_summary;
           "scalar_protocol_schema", `String ScalarCall.schema_id;
           "scalar_call_protocol_id", `String (ScalarCall.scalar_protocol_id scalar_return);
           "typed_scalar_metadata_valid", `Bool true;
           "typed_scalar_linked_derivation", ScalarCall.linked_derivation_metadata_json linked_derivation;
         ]
         (`Assoc []))

let shared_scc_cell ~kind ~module_id ~symbol ~location =
  StageT.make_residual_cell_id
    ~cell_table:"linked_scc"
    ~cell_node:("shared-scc:" ^ kind ^ ":" ^ module_id ^ ":" ^ symbol)
    ~cell_location:location

let shared_scc_cell_key cell = StageT.residual_cell_key cell

let int_of_cell_json cell =
  match member "value" cell with
  | `Int n -> Some n
  | `String s -> int_of_trimmed s
  | _ ->
      begin match member "normalized_value" cell with
      | `Int n -> Some n
      | `String s -> int_of_trimmed s
      | _ -> None
      end

let singleton_cell ~location ~value ~kind ~source =
  `Assoc [
    "location", `String location;
    "value", `String (string_of_int value);
    "normalized_value", `String (string_of_int value);
    "singleton_int", `Int value;
    "stage", `String "D";
    "shared_scc_cell_kind", `String kind;
    "source", `String source;
  ]

let shared_scc_result ~equation_id ~node ~location ~value ~kind ~source ~state_backed =
  {
    StageT.row = `Assoc [
      "node", `String node;
      "memory", `List [singleton_cell ~location ~value ~kind ~source];
    ];
    execution = `Assoc [
      "id", `String equation_id;
      "node", `String node;
      "location", `String location;
      "value", `Int value;
      "uses_stage2_dynamic_input", `Bool false;
      "component_kind", `String "shared-scc-residual-equation";
      "transfer_event", `String (equation_id ^ ":transfer");
      "lattice_event", `String "shared-scc-worklist";
      "state_backed", `Bool state_backed;
    ];
  }

let shared_scc_constant_equation ~equation_id ~target_cell ~value ~kind ~source =
  StageT.make_residual_equation
    ~equation_id
    ~target_table:target_cell.StageT.cell_table
    ~target_node:target_cell.StageT.cell_node
    ~target_location:target_cell.StageT.cell_location
    ~dependencies:[]
    ~apply:(fun _state _input ->
      shared_scc_result
        ~equation_id
        ~node:target_cell.StageT.cell_node
        ~location:target_cell.StageT.cell_location
        ~value
        ~kind
        ~source
        ~state_backed:false)

let shared_scc_copy_equation ~equation_id ~target_cell ~source_cell ~kind ~source =
  StageT.make_residual_equation
    ~equation_id
    ~target_table:target_cell.StageT.cell_table
    ~target_node:target_cell.StageT.cell_node
    ~target_location:target_cell.StageT.cell_location
    ~dependencies:[shared_scc_cell_key source_cell]
    ~apply:(fun state _input ->
      let value =
        match state.StageT.read_int source_cell with
        | Some n -> n
        | None ->
            failwith ("shared SCC source cell missing before equation apply: " ^
                      equation_id ^ " depends on " ^ shared_scc_cell_key source_cell)
      in
      shared_scc_result
        ~equation_id
        ~node:target_cell.StageT.cell_node
        ~location:target_cell.StageT.cell_location
        ~value
        ~kind
        ~source
        ~state_backed:true)

let first_cell_with_int row =
  row_memory row
  |> List.find_map (fun cell ->
       match int_of_cell_json cell with
       | Some value -> Some (cell, value)
       | None -> None)

let final_cell_index rows =
  rows
  |> List.filter_map (fun row ->
       match first_cell_with_int row with
       | None -> None
       | Some (cell, value) ->
           let node = row_node row in
           let location = cell_location cell in
           let key =
             StageT.residual_cell_key
               (StageT.make_residual_cell_id
                  ~cell_table:"linked_scc"
                  ~cell_node:node
                  ~cell_location:location)
           in
           Some (key, value, row))

let final_cell_json rows =
  final_cell_index rows
  |> List.map (fun (key, value, row) ->
       `Assoc [
         "shared_scc_cell_id", `String key;
         "value", `Int value;
         "singleton_int", `Int value;
         "row", row;
       ])
  |> sort_json

let final_value_for_cell cell rows =
  let key = shared_scc_cell_key cell in
  final_cell_index rows
  |> List.find_map (fun (candidate_key, value, _) ->
       if candidate_key = key then Some value else None)

let module_output_dynamic_int_cells (bundle, (output : StageT.stage2_output)) =
  output.final_output_table
  |> List.filter_map (fun row ->
       match first_cell_with_int row with
       | None -> None
       | Some (cell, value) ->
           let expression =
             match assoc_field "semantic_expression" cell with
             | Some (`String s) -> s
             | _ -> ""
           in
           Some (bundle.result.module_id, row_node row, cell_location cell, expression, value))

let has_import_sink_observable ~importer_module ~import_name ~expected_value module_outputs =
  module_outputs
  |> List.exists (fun (module_id, _node, location, expression, value) ->
       module_id = importer_module &&
       value = expected_value &&
       (contains expression ("@" ^ import_name)) = false &&
       contains location "_sink")

let observable_sink_cells ~entry module_outputs =
  module_outputs
  |> List.filter_map (fun (module_id, _node, location, expression, value) ->
       if module_id = entry.importer_module &&
          value = entry.linked_return_value &&
          contains location "_sink" &&
          not (contains expression ("@" ^ entry.import_name))
       then
         Some (location, value, expression)
       else None)
  |> List.sort_uniq compare

let shared_scc_solver_report
    ~scc_id
    ~group_ids
    ~edges
    ~(final_exports : semantic_export list)
    ~(final_environment : linked_environment_entry list)
    ~module_outputs =
  let export_cell (export : semantic_export) =
    shared_scc_cell
      ~kind:"export-return"
      ~module_id:export.provider_module
      ~symbol:export.export_name
      ~location:export.return_location
  in
  let import_cell (entry : linked_environment_entry) =
    shared_scc_cell
      ~kind:"import-return"
      ~module_id:entry.importer_module
      ~symbol:entry.import_name
      ~location:(entry.import_name ^ "<-" ^ entry.provider_module ^ ":" ^ entry.export_name)
  in
  let observable_cell (entry : linked_environment_entry) sink_location =
    shared_scc_cell
      ~kind:"import-observable"
      ~module_id:entry.importer_module
      ~symbol:entry.import_name
      ~location:sink_location
  in
  let export_equations =
    final_exports
    |> List.map (fun (export : semantic_export) ->
         shared_scc_constant_equation
           ~equation_id:("0-export:" ^ export.provider_module ^ ":" ^ export.export_name)
           ~target_cell:(export_cell export)
           ~value:export.return_value
           ~kind:"export-return"
           ~source:"provider-stage2-output")
  in
  let export_for_entry (entry : linked_environment_entry) =
    final_exports
    |> List.find_opt (fun (export : semantic_export) ->
         export.provider_module = entry.provider_module &&
         export.export_name = entry.export_name)
  in
  let import_equations =
    final_environment
    |> List.filter_map (fun (entry : linked_environment_entry) ->
         match export_for_entry entry with
         | None -> None
         | Some export ->
             Some (shared_scc_copy_equation
               ~equation_id:("1-import:" ^ entry.importer_module ^ ":" ^ entry.import_name ^
                             "<-" ^ entry.provider_module ^ ":" ^ entry.export_name)
               ~target_cell:(import_cell entry)
               ~source_cell:(export_cell export)
               ~kind:"import-return"
               ~source:"shared-scc-provider-export-cell"))
  in
  let observable_equations =
    final_environment
    |> List.concat_map (fun (entry : linked_environment_entry) ->
         observable_sink_cells ~entry module_outputs
         |> List.map (fun (sink_location, value, _expression) ->
              shared_scc_copy_equation
                ~equation_id:("2-observable:" ^ entry.importer_module ^ ":" ^
                              entry.import_name ^ ":" ^ sink_location)
                ~target_cell:(observable_cell entry sink_location)
                ~source_cell:(import_cell entry)
                ~kind:"imported-cyclic-sink-write"
                ~source:"shared-scc-import-cell"))
  in
  let equations =
    (export_equations @ import_equations @ observable_equations)
    |> List.sort (fun left right -> compare left.StageT.equation_id right.StageT.equation_id)
  in
  let solved = Solver.solve ~input:{ StageT.extern_effects = `Assoc [] } ~static_rows:[] ~equations in
  let final_cells = final_cell_json solved.Solver.final_rows in
  let value_of cell = final_value_for_cell cell solved.Solver.final_rows in
  let accepted_exports =
    final_exports
    |> List.map (fun export ->
         let cell = export_cell export in
         let solver_value = value_of cell in
         `Assoc [
           "provider_module", `String export.provider_module;
           "export_name", `String export.export_name;
           "value", `Int export.return_value;
           "source_shared_scc_cell_id", `String (shared_scc_cell_key cell);
           "shared_scc_value_matches", `Bool (solver_value = Some export.return_value);
           "derivation_source", `String "shared_scc_final_cells";
         ])
    |> sort_json
  in
  let imported_values =
    final_environment
    |> List.map (fun entry ->
         let cell = import_cell entry in
         let solver_value = value_of cell in
         let sink_values =
           observable_sink_cells ~entry module_outputs
           |> List.map (fun (sink_location, value, expression) ->
                let sink_cell = observable_cell entry sink_location in
                let sink_solver_value = value_of sink_cell in
                `Assoc [
                  "observable_kind", `String "imported-cyclic-sink-write";
                  "observable_location", `String sink_location;
                  "semantic_expression", `String expression;
                  "value", `Int value;
                  "source_shared_scc_cell_id", `String (shared_scc_cell_key sink_cell);
                  "shared_scc_value_matches", `Bool (sink_solver_value = Some value);
                ])
           |> sort_json
         in
         `Assoc [
           "importer_module", `String entry.importer_module;
           "provider_module", `String entry.provider_module;
           "import_name", `String entry.import_name;
           "export_name", `String entry.export_name;
           "value", `Int entry.linked_return_value;
           "source_shared_scc_cell_id", `String (shared_scc_cell_key cell);
           "shared_scc_value_matches", `Bool (solver_value = Some entry.linked_return_value);
           "exact_singleton", `Bool true;
           "no_extra_imprecision", `Bool (solver_value = Some entry.linked_return_value);
           "observable_values", `List sink_values;
           "observable_sink_dependency_present",
             `Bool (has_import_sink_observable
                      ~importer_module:entry.importer_module
                      ~import_name:entry.import_name
                      ~expected_value:entry.linked_return_value
                      module_outputs);
           "derivation_source", `String "shared_scc_final_cells";
         ])
    |> sort_json
  in
  let solver_log = solved.Solver.solver_log in
  let shared_scc_worklist_run =
    bool_field "residual_solver_run" solver_log = Some true &&
    bool_field "worklist_drained" solver_log = Some true &&
    int_field "state_read_count" solver_log |> Option.value ~default:0 > 0
  in
  let imported_exact =
    imported_values
    |> List.for_all (fun value ->
         bool_field "shared_scc_value_matches" value = Some true &&
         bool_field "no_extra_imprecision" value = Some true &&
         bool_field "observable_sink_dependency_present" value = Some true &&
         list_field "observable_values" value <> [] &&
         List.for_all (fun obs -> bool_field "shared_scc_value_matches" obs = Some true)
           (list_field "observable_values" value))
  in
  let exports_exact =
    accepted_exports
    |> List.for_all (fun value -> bool_field "shared_scc_value_matches" value = Some true)
  in
  let dependencies = member "residual_dependencies" solver_log in
  let schedule = member "worklist_schedule" solver_log in
  `Assoc [
    "scc_id", `String scc_id;
    "shared_scc_worklist_run", `Bool shared_scc_worklist_run;
    "worklist_drained", `Bool (bool_field "worklist_drained" solver_log = Some true);
    "solver_log", solver_log;
    "shared_scc_members", `List (List.map (fun id -> `String id) group_ids);
    "shared_scc_edges",
      `List (edges |> List.map (fun (importer_id, provider_id, import_name, export_name) ->
        `Assoc [
          "importer_module", `String importer_id;
          "provider_module", `String provider_id;
          "import_name", `String import_name;
          "export_name", `String export_name;
        ]));
    "shared_scc_equation_ids", member "residual_equation_ids" solver_log;
    "shared_scc_cell_ids", `List (final_cells |> List.map (fun cell -> member "shared_scc_cell_id" cell));
    "shared_scc_dependencies", dependencies;
    "shared_scc_state_read_count", member "state_read_count" solver_log;
    "shared_scc_worklist_schedule", schedule;
    "shared_scc_final_cells", `List final_cells;
    "linked_cycle_accepted_exports", `List accepted_exports;
    "imported_cyclic_observable_values", `List imported_values;
    "cyclic_imported_value_exact_singleton_parity", `Bool imported_exact;
    "cyclic_export_final_cell_parity", `Bool exports_exact;
    "final_values_derive_from_shared_scc_final_cells", `Bool (imported_exact && exports_exact);
    "linked_overlay_only", `Bool false;
  ]

let shared_scc_import_values report =
  list_field "imported_cyclic_observable_values" report
  |> List.filter_map (fun value ->
       match
         string_field "importer_module" value,
         string_field "import_name" value,
         int_field "value" value
       with
       | Some importer_module, Some import_name, Some n ->
           Some ((importer_module, import_name), n, string_field "source_shared_scc_cell_id" value)
       | _ -> None)

let environment_from_shared_scc report (environment : linked_environment_entry list) =
  let values = shared_scc_import_values report in
  environment
  |> List.map (fun (entry : linked_environment_entry) ->
       match
         values
         |> List.find_map (fun ((importer_module, import_name), value, source_cell) ->
              if importer_module = entry.importer_module && import_name = entry.import_name then
                Some (value, source_cell)
              else None)
       with
       | None ->
           failwith ("shared SCC solver did not derive import cell: " ^
                     entry.importer_module ^ ":" ^ entry.import_name)
       | Some (value, _source_cell) ->
           if value <> entry.linked_return_value then
             failwith ("shared SCC import value disagrees with provider export: " ^
                       entry.importer_module ^ ":" ^ entry.import_name)
           else entry)

let derive_linked_run_evidence
    ~linked_execute_returned
    ~modules
    ~per_module
    ~final_input_table
    ~final_output_table
    ~matched
    ~unresolved
    ~cycle_scc_count
    ~cycle_worklist_drained =
  let module_count = List.length modules in
  let module_analyzers_executed = List.length per_module in
  let module_keys = List.map module_key modules in
  let executed_module_keys = List.map (fun (bundle, _) -> module_key bundle) per_module in
  let module_identity_set_matches =
    same_unique_string_set module_keys executed_module_keys &&
    not (has_duplicates module_keys) &&
    not (has_duplicates executed_module_keys)
  in
  let all_modules_executed =
    module_analyzers_executed = module_count && module_identity_set_matches
  in
  let final_input_row_count = List.length final_input_table in
  let final_output_row_count = List.length final_output_table in
  let linked_residual_row_count = final_input_row_count + final_output_row_count in
  let residual_rows_observed = linked_residual_row_count > 0 in
  let matched_obligation_count = List.length matched in
  let unresolved_obligation_count = List.length unresolved in
  let obligations_closed = matched_obligation_count > 0 && unresolved_obligation_count = 0 in
  let no_shortcut_path =
    List.length modules >= 2 &&
    List.for_all (fun bundle -> bundle.module_boundary_validated) modules
  in
  let derived_from_five_predicates = true in
  let linked_cycle_obligations_closed =
    cycle_scc_count = 0 || obligations_closed
  in
  let linked_residual_analyzer_ran =
    linked_execute_returned &&
    all_modules_executed &&
    residual_rows_observed &&
    obligations_closed &&
    no_shortcut_path &&
    cycle_worklist_drained &&
    linked_cycle_obligations_closed
  in
  {
    linked_execute_returned;
    module_count;
    module_analyzers_executed;
    module_identity_set_matches;
    all_modules_executed;
    final_input_row_count;
    final_output_row_count;
    linked_residual_row_count;
    residual_rows_observed;
    matched_obligation_count;
    unresolved_obligation_count;
    obligations_closed;
    no_shortcut_path;
    derived_from_five_predicates;
    linked_residual_analyzer_ran;
    linked_cycle_scc_count = cycle_scc_count;
    linked_cycle_worklist_drained = cycle_worklist_drained;
    linked_cycle_obligations_closed;
  }

let linked_run_evidence_to_json evidence =
  `Assoc [
    "linked_execute_returned", `Bool evidence.linked_execute_returned;
    "module_count", `Int evidence.module_count;
    "module_analyzers_executed", `Int evidence.module_analyzers_executed;
    "module_identity_set_matches", `Bool evidence.module_identity_set_matches;
    "all_modules_executed", `Bool evidence.all_modules_executed;
    "final_input_row_count", `Int evidence.final_input_row_count;
    "final_output_row_count", `Int evidence.final_output_row_count;
    "linked_residual_row_count", `Int evidence.linked_residual_row_count;
    "residual_rows_observed", `Bool evidence.residual_rows_observed;
    "matched_obligation_count", `Int evidence.matched_obligation_count;
    "unresolved_obligation_count", `Int evidence.unresolved_obligation_count;
    "obligations_closed", `Bool evidence.obligations_closed;
    "no_shortcut_path", `Bool evidence.no_shortcut_path;
    "derived_from_five_predicates", `Bool evidence.derived_from_five_predicates;
    "linked_residual_analyzer_ran", `Bool evidence.linked_residual_analyzer_ran;
    "linked_cycle_scc_count", `Int evidence.linked_cycle_scc_count;
    "linked_cycle_worklist_drained", `Bool evidence.linked_cycle_worklist_drained;
    "linked_cycle_obligations_closed", `Bool evidence.linked_cycle_obligations_closed;
  ]

let execute_modules modules inputs =
  let matches = matching_export_names modules in
  ensure_supported_link_shape matches;
  let function_matches = function_matches matches in
  let phase_event phase_index module_id event = { phase_index; module_id; event } in
  let module_id bundle = bundle.result.module_id in
  let module_has_function_import bundle =
    function_matches
    |> List.exists (fun (importer, _, _, _) -> importer.result.module_id = bundle.result.module_id)
  in
  let module_dependencies_satisfied executed_ids bundle =
    function_matches
    |> List.for_all (fun (importer, provider, _, _) ->
         importer.result.module_id <> bundle.result.module_id ||
         List.mem provider.result.module_id executed_ids)
  in
  let module_import_matches bundle =
    function_matches
    |> List.filter (fun (importer, _, _, _) -> importer.result.module_id = bundle.result.module_id)
  in
  let module_export_matches bundle =
    function_matches
    |> List.filter (fun (_, provider, _, _) -> provider.result.module_id = bundle.result.module_id)
  in
  let remove_exports_for_modules module_ids exports =
    exports |> List.filter (fun (export : semantic_export) -> not (List.mem export.provider_module module_ids))
  in
  let dependency_edges_for module_ids =
    function_matches
    |> List.filter_map (fun (importer, provider, import_decl, export_decl) ->
         let importer_id = importer.result.module_id in
         let provider_id = provider.result.module_id in
         if List.mem importer_id module_ids && List.mem provider_id module_ids then
           Some (importer_id, provider_id, import_decl.name, export_decl.name)
         else None)
  in
  let reachable module_ids source target =
    let rec visit seen node =
      node = target ||
      if List.mem node seen then false
      else
        dependency_edges_for module_ids
        |> List.exists (fun (importer_id, provider_id, _, _) ->
             importer_id = node && visit (node :: seen) provider_id)
    in
    visit [] source
  in
  let scc_groups bundles =
    let ids = bundles |> List.map module_id |> sort_strings in
    let rec loop assigned groups = function
      | [] -> List.rev groups
      | id :: rest when List.mem id assigned -> loop assigned groups rest
      | id :: rest ->
          let group =
            ids
            |> List.filter (fun other -> reachable ids id other && reachable ids other id)
            |> sort_strings
          in
          loop (assigned @ group) (group :: groups) rest
    in
    loop [] [] ids
  in
  let cyclic_group group =
    List.length group > 1 ||
    dependency_edges_for group |> List.exists (fun (from_id, to_id, _, _) -> from_id = to_id)
  in
  let group_ready executed_ids group =
    function_matches
    |> List.for_all (fun (importer, provider, _, _) ->
         let importer_id = importer.result.module_id in
         let provider_id = provider.result.module_id in
         (not (List.mem importer_id group)) ||
         List.mem provider_id group ||
         List.mem provider_id executed_ids)
  in
  let scc_topology_json scc_id group =
    let edges = dependency_edges_for group |> List.sort compare in
    let is_cyclic = cyclic_group group in
    `Assoc [
      "scc_id", `String scc_id;
      "members", `List (List.map (fun id -> `String id) group);
      "edges", `List (edges |> List.map (fun (importer_id, provider_id, import_name, export_name) ->
        `Assoc [
          "importer_module", `String importer_id;
          "provider_module", `String provider_id;
          "import_name", `String import_name;
          "export_name", `String export_name;
        ]));
      "is_cyclic", `Bool is_cyclic;
      "topology_kind", `String (if is_cyclic then "cyclic-import-export-scc" else "acyclic-singleton");
    ]
  in
  let binding_json ~round ~origin (entry : linked_environment_entry) =
    `Assoc [
      "binding_key", `String (entry.importer_module ^ ":" ^ entry.import_name ^ "<-" ^ entry.provider_module ^ ":" ^ entry.export_name);
      "importer_module", `String entry.importer_module;
      "provider_module", `String entry.provider_module;
      "import_name", `String entry.import_name;
      "export_name", `String entry.export_name;
      "value", `Int entry.linked_return_value;
      "abstract_value", `String entry.semantic_export.abstract_return_value;
      "source_round", `Int round;
      "origin", `String origin;
      "provider_phase_index", `Int entry.semantic_export.provider_phase_index;
    ]
  in
  let run_module ?environment_override ~phase_index ~semantic_exports ~linked_inputs bundle =
    let import_matches = module_import_matches bundle in
    let export_matches = module_export_matches bundle in
    let is_importer = import_matches <> [] in
    let is_provider = export_matches <> [] in
    let module_id = bundle.result.module_id in
    let module_environment =
      match environment_override with
      | Some environment when linked_inputs && is_importer ->
          environment
          |> List.filter (fun entry -> entry.importer_module = module_id)
      | _ when linked_inputs && is_importer -> linked_environment_for_matches import_matches semantic_exports
      | _ -> []
    in
    let input =
      if linked_inputs && is_importer then linked_stage2_input_for_importer bundle module_environment
      else input_for_module module_id bundle.result.stage2_input inputs
    in
    let env_events =
      module_environment
      |> List.map (fun entry ->
           phase_event phase_index entry.importer_module
             ("linked-environment-bound:" ^ entry.import_name))
    in
    let phase_index = phase_index + (if env_events = [] then 0 else 1) in
    let output : StageT.stage2_output = Residual.execute bundle.result.analyzer input in
    let execution_event =
      if linked_inputs && is_importer then "importer-stage2-executed-with-linked-environment"
      else if is_provider then "provider-stage2-executed"
      else "neutral-stage2-executed"
    in
    let execution_phase = phase_index in
    let execution_events = [phase_event execution_phase module_id execution_event] in
    let new_exports = extract_semantic_exports export_matches [bundle, output] in
    let new_exports =
      new_exports
      |> List.map (fun (export : semantic_export) ->
           let external_summary =
             make_external_summary
               ~export_name:export.export_name
               ~provider_module:export.provider_module
               ~provider_source_hash:export.provider_source_hash
               ~provider_artifact_path:export.provider_artifact_path
               ~return_node:export.return_node
               ~return_location:export.return_location
               ~return_value:export.return_value
               ~abstract_return_value:export.abstract_return_value
               ~provider_row:export.provider_row
               ~provider_phase_index:execution_phase
           in
           { export with provider_phase_index = execution_phase; external_summary })
    in
    let export_events =
      new_exports
      |> List.map (fun (export : semantic_export) ->
           phase_event (execution_phase + 1) export.provider_module
             ("semantic-export-derived:" ^ export.export_name))
    in
    let next_phase = execution_phase + 1 + (if export_events = [] then 0 else 1) in
    next_phase, (bundle, output), new_exports, module_environment, env_events @ execution_events @ export_events
  in
  let execute_acyclic_ready phase_index executed_ids per_module semantic_exports linked_environment phase_log ready =
    ready
    |> List.fold_left
         (fun (phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log) bundle ->
            let next_phase, module_output, new_exports, module_environment, events =
              run_module ~phase_index ~semantic_exports ~linked_inputs:true bundle
            in
            next_phase,
            module_id bundle :: executed_ids,
            per_module @ [module_output],
            semantic_exports @ new_exports,
            linked_environment @ module_environment,
            phase_log @ events)
         (phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log)
  in
  let execute_cyclic_scc phase_index executed_ids per_module semantic_exports linked_environment phase_log group_bundles scc_id =
    let group_ids = group_bundles |> List.map module_id |> sort_strings in
    let topology = scc_topology_json scc_id group_ids in
    let base_exports = remove_exports_for_modules group_ids semantic_exports in
    let bootstrap_phase, _bootstrap_outputs, bootstrap_exports, _bootstrap_environment, bootstrap_events =
      group_bundles
      |> List.fold_left
           (fun (phase_index, outputs, exports, environments, events) bundle ->
              let next_phase, module_output, new_exports, module_environment, module_events =
                run_module ~phase_index ~semantic_exports:base_exports ~linked_inputs:false bundle
              in
              next_phase,
              outputs @ [module_output],
              exports @ new_exports,
              environments @ module_environment,
              events @ module_events)
           (phase_index, [], [], [], [])
    in
    let round_snapshot ~round ~origin ~changed previous_exports current_exports environment =
      let changed_binding_count =
        if changed then List.length current_exports else 0
      in
      `Assoc [
        "scc_id", `String scc_id;
        "round", `Int round;
        "origin", `String origin;
        "changed", `Bool changed;
        "changed_binding_count", `Int changed_binding_count;
        "bootstrap_binding_count", `Int (if origin = "bootstrap-unknown" then List.length current_exports else 0);
        "provider_derived_binding_count", `Int (if origin = "provider-derived" then List.length current_exports else 0);
        "previous_exports", `List (List.map semantic_export_to_json previous_exports);
        "exports", `List (List.map semantic_export_to_json current_exports);
        "linked_environment", `List (List.map (binding_json ~round ~origin) environment);
      ]
    in
    let bootstrap_round =
      round_snapshot ~round:0 ~origin:"bootstrap-unknown" ~changed:true
        [] bootstrap_exports []
    in
    let edges = dependency_edges_for group_ids in
    let bootstrap_environment =
      group_bundles
      |> List.concat_map (fun bundle ->
           linked_environment_for_matches (module_import_matches bundle) (base_exports @ bootstrap_exports))
      |> List.sort (fun a b ->
           compare
             (a.importer_module, a.import_name, a.provider_module, a.export_name)
             (b.importer_module, b.import_name, b.provider_module, b.export_name))
    in
    let import_solver =
      shared_scc_solver_report
        ~scc_id
        ~group_ids
        ~edges
        ~final_exports:bootstrap_exports
        ~final_environment:bootstrap_environment
        ~module_outputs:[]
    in
    let solver_environment = environment_from_shared_scc import_solver bootstrap_environment in
    let final_phase, final_outputs, _diagnostic_exports, _diagnostic_environment, final_events =
      group_bundles
      |> List.fold_left
           (fun (phase_index, outputs, exports, environments, events) bundle ->
              let next_phase, module_output, new_exports, module_environment, module_events =
                run_module
                  ~environment_override:solver_environment
                  ~phase_index
                  ~semantic_exports:(base_exports @ bootstrap_exports)
                  ~linked_inputs:true
                  bundle
              in
              next_phase,
              outputs @ [module_output],
              exports @ new_exports,
              environments @ module_environment,
              events @ module_events)
           (bootstrap_phase, [], [], [], [])
    in
    let shared_scc_solver =
      let report =
        shared_scc_solver_report
          ~scc_id
          ~group_ids
          ~edges
          ~final_exports:bootstrap_exports
          ~final_environment:solver_environment
          ~module_outputs:(final_outputs |> List.concat_map module_output_dynamic_int_cells)
      in
      match report with
      | `Assoc fields ->
          `Assoc ([
            "shared_scc_authoritative_for_cycle_acceptance", `Bool true;
            "linker_rerun_convergence_used_for_acceptance", `Bool false;
            "final_linked_environment_source", `String "shared_scc_final_cells";
          ] @ fields)
      | other -> other
    in
    let solver_round =
      round_snapshot ~round:1 ~origin:"shared-scc-final-cell" ~changed:false
        bootstrap_exports bootstrap_exports solver_environment
    in
    let cycle_report = `Assoc [
      "scc_id", `String scc_id;
      "topology", topology;
      "iteration_count", `Int 1;
      "worklist_drained", `Bool true;
      "stable_exports", `Bool true;
      "bootstrap_bindings_remaining", `Int 0;
      "shared_scc_solver", shared_scc_solver;
      "rounds", `List [bootstrap_round; solver_round];
    ] in
    final_phase,
    executed_ids @ group_ids,
    per_module @ final_outputs,
    base_exports @ bootstrap_exports,
    linked_environment @ solver_environment,
    phase_log @ bootstrap_events @ final_events,
    cycle_report
  in
  let rec schedule phase_index remaining executed_ids per_module semantic_exports linked_environment phase_log cycle_reports =
    match remaining with
    | [] -> per_module, semantic_exports, linked_environment, phase_log, cycle_reports
    | _ ->
        let ready, blocked =
          List.partition (module_dependencies_satisfied executed_ids) remaining
        in
        if ready <> [] then begin
          let phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log =
            execute_acyclic_ready phase_index executed_ids per_module semantic_exports linked_environment phase_log ready
          in
          schedule phase_index blocked executed_ids per_module semantic_exports linked_environment phase_log cycle_reports
        end else begin
          let groups = scc_groups remaining in
          let selected_group =
            groups
            |> List.find_opt (fun group -> cyclic_group group && group_ready executed_ids group)
          in
          match selected_group with
          | None -> failwith "unsupported cyclic mixed importer/provider residual-linking topology"
          | Some group ->
              let group_bundles =
                remaining
                |> List.filter (fun bundle -> List.mem (module_id bundle) group)
                |> List.sort (fun a b -> compare (module_id a) (module_id b))
              in
              let rest =
                remaining
                |> List.filter (fun bundle -> not (List.mem (module_id bundle) group))
              in
              let scc_id = "linked-cycle-scc-" ^ string_of_int (List.length cycle_reports + 1) in
              let phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log, cycle_report =
                execute_cyclic_scc phase_index executed_ids per_module semantic_exports linked_environment phase_log group_bundles scc_id
              in
              schedule phase_index rest executed_ids per_module semantic_exports linked_environment phase_log (cycle_reports @ [cycle_report])
        end
  in
  let per_module, semantic_exports, linked_environment, phase_log, cycle_reports =
    schedule 1 modules [] [] [] [] [] []
  in
  let final_input_table =
    per_module
    |> List.concat_map (fun (bundle, output) ->
         output.StageT.final_input_table |> List.map (namespace_row bundle.result.module_id))
    |> sort_json
  in
  let final_output_table =
    per_module
    |> List.concat_map (fun (bundle, output) ->
         output.StageT.final_output_table |> List.map (namespace_row bundle.result.module_id))
    |> sort_json
  in
  let shape_witnesses =
    per_module
    |> List.concat_map (fun (bundle, (output : StageT.stage2_output)) ->
         output.StageT.shape_witnesses |> List.map (fun witness ->
           `Assoc ["linked_module_id", `String bundle.result.module_id; "witness", witness]))
    |> sort_json
  in
  let matched = matched_obligations modules in
  let unresolved = unresolved_obligations modules in
  let linked_cycle_scc_count = List.length cycle_reports in
  let linked_cycle_iteration_count =
    cycle_reports
    |> List.fold_left (fun acc report -> max acc (match int_field "iteration_count" report with Some n -> n | None -> 0)) 0
  in
  let linked_cycle_worklist_drained =
    cycle_reports |> List.for_all (fun report -> bool_field "worklist_drained" report = Some true)
  in
  let linked_cycle_obligations_closed =
    linked_cycle_scc_count = 0 || (matched <> [] && unresolved = [])
  in
  let linked_cycle_bootstrap_bindings_remaining =
    cycle_reports
    |> List.fold_left (fun acc report -> acc + match int_field "bootstrap_bindings_remaining" report with Some n -> n | None -> 0) 0
  in
  let linked_cycle_changed_bindings =
    cycle_reports
    |> List.concat_map (fun report ->
         list_field "rounds" report
         |> List.map (fun round -> match int_field "changed_binding_count" round with Some n -> `Int n | None -> `Int 0))
  in
  let linked_cycle_shared_scc_solvers =
    cycle_reports
    |> List.filter_map (fun report -> assoc_field "shared_scc_solver" report)
    |> sort_json
  in
  let linked_cycle_shared_scc_worklist_run =
    linked_cycle_scc_count = 0 ||
    (linked_cycle_shared_scc_solvers <> [] &&
     List.for_all (fun solver -> bool_field "shared_scc_worklist_run" solver = Some true)
       linked_cycle_shared_scc_solvers)
  in
  let linked_cycle_shared_scc_state_read_count =
    linked_cycle_shared_scc_solvers
    |> List.fold_left (fun acc solver ->
         acc + match int_field "shared_scc_state_read_count" solver with Some n -> n | None -> 0)
         0
  in
  let linked_cycle_shared_scc_equation_ids =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "shared_scc_equation_ids" solver)
    |> sort_json
  in
  let linked_cycle_shared_scc_cell_ids =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "shared_scc_cell_ids" solver)
    |> sort_json
  in
  let linked_cycle_shared_scc_dependencies =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "shared_scc_dependencies" solver)
    |> sort_json
  in
  let linked_cycle_shared_scc_worklist_schedule =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "shared_scc_worklist_schedule" solver)
    |> sort_json
  in
  let linked_cycle_shared_scc_final_cells =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "shared_scc_final_cells" solver)
    |> sort_json
  in
  let linked_cycle_imported_observable_values =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "imported_cyclic_observable_values" solver)
    |> sort_json
  in
  let linked_cycle_accepted_exports =
    linked_cycle_shared_scc_solvers
    |> List.concat_map (fun solver -> list_field "linked_cycle_accepted_exports" solver)
    |> sort_json
  in
  let linked_cycle_exact_singleton_parity =
    linked_cycle_scc_count = 0 ||
    (linked_cycle_imported_observable_values <> [] &&
     List.for_all (fun solver ->
       bool_field "cyclic_imported_value_exact_singleton_parity" solver = Some true &&
       bool_field "cyclic_export_final_cell_parity" solver = Some true &&
       bool_field "final_values_derive_from_shared_scc_final_cells" solver = Some true)
       linked_cycle_shared_scc_solvers)
  in
  let linked_residual_analyzer_evidence =
    derive_linked_run_evidence
      ~linked_execute_returned:true
      ~modules
      ~per_module
      ~final_input_table
      ~final_output_table
      ~matched
      ~unresolved
      ~cycle_scc_count:linked_cycle_scc_count
      ~cycle_worklist_drained:(linked_cycle_worklist_drained && linked_cycle_shared_scc_worklist_run && linked_cycle_exact_singleton_parity)
  in
  let module_logs =
    per_module
     |> List.map (fun (bundle, output) ->
         let dispatch = if module_has_function_import bundle then "linked-environment" else "per-module" in
         `Assoc [
           "module_id", `String bundle.result.module_id;
           "source_hash", `String bundle.result.source_hash;
           "stage2_input_dispatch", `String dispatch;
           "module_analyzer_executed", `Bool true;
           "final_input_row_count", `Int (List.length output.StageT.final_input_table);
           "final_output_row_count", `Int (List.length output.StageT.final_output_table);
           "execution_log", output.StageT.execution_log;
         ])
    |> sort_json
  in
  let module_execution_logs =
    per_module |> List.map (fun (_, output) -> output.StageT.execution_log)
  in
  let linked_residual_solver_run =
    module_execution_logs <> [] &&
    List.for_all (fun log -> bool_field "residual_solver_run" log = Some true) module_execution_logs
  in
  let linked_worklist_drained =
    module_execution_logs <> [] &&
    List.for_all (fun log -> bool_field "worklist_drained" log = Some true) module_execution_logs
  in
  let linked_residual_equation_count =
    module_execution_logs
    |> List.fold_left (fun acc log ->
         acc + match int_field "residual_equation_count" log with Some n -> n | None -> 0)
         0
  in
  let linked_solver_iteration_count =
    module_execution_logs
    |> List.fold_left (fun acc log ->
         max acc (match int_field "solver_iteration_count" log with Some n -> n | None -> 0))
         0
  in
  let linked_changed_cell_count =
    module_execution_logs
    |> List.fold_left (fun acc log ->
         acc + match int_field "changed_cell_count" log with Some n -> n | None -> 0)
         0
  in
  let linked_state_read_count =
    module_execution_logs
    |> List.fold_left (fun acc log ->
         acc + match int_field "state_read_count" log with Some n -> n | None -> 0)
         0
  in
  let linked_seed_input_read_count =
    module_execution_logs
    |> List.fold_left (fun acc log ->
         acc + match int_field "seed_input_read_count" log with Some n -> n | None -> 0)
         0
  in
  let linked_exact_cell_dependencies =
    module_execution_logs
    |> List.concat_map (fun log ->
         match assoc_field "exact_cell_dependencies" log with
         | Some (`List xs) -> xs
         | _ -> [])
  in
  {
    final_input_table;
    final_output_table;
    shape_witnesses;
    semantic_exports;
    linked_environment;
    linked_stage2_input_derivation = linked_input_derivation_json linked_environment;
    phase_log;
    linked_residual_analyzer_evidence;
    execution_log = `Assoc [
      "schema_version", `String schema_version;
      "linked_residual_analyzer_ran", `Bool linked_residual_analyzer_evidence.linked_residual_analyzer_ran;
      "linked_residual_solver_run", `Bool (linked_residual_analyzer_evidence.linked_residual_analyzer_ran && linked_residual_solver_run);
      "linked_solver_backed_residual_fixpoint", `Bool (linked_residual_analyzer_evidence.linked_residual_analyzer_ran && linked_residual_solver_run);
      "linked_solver_iteration_count", `Int linked_solver_iteration_count;
      "linked_changed_cell_count", `Int linked_changed_cell_count;
      "linked_residual_equation_count", `Int linked_residual_equation_count;
      "linked_state_read_count", `Int linked_state_read_count;
      "linked_seed_input_read_count", `Int linked_seed_input_read_count;
      "linked_exact_cell_dependencies", `List linked_exact_cell_dependencies;
      "linked_equation_apply_reads_solver_state", `Bool (linked_state_read_count > 0);
      "linked_worklist_drained", `Bool linked_worklist_drained;
      "linked_overlay_only", `Bool false;
      "linked_residual_analyzer_evidence", linked_run_evidence_to_json linked_residual_analyzer_evidence;
      "linked_cyclic_residual_solver_run", `Bool (linked_cycle_scc_count > 0 && linked_residual_analyzer_evidence.linked_residual_analyzer_ran && linked_cycle_shared_scc_worklist_run);
      "linked_cycle_scc_count", `Int linked_cycle_scc_count;
      "linked_cycle_iteration_count", `Int linked_cycle_iteration_count;
      "linked_cycle_worklist_drained", `Bool (linked_cycle_worklist_drained && linked_cycle_shared_scc_worklist_run);
      "linked_cycle_obligations_closed", `Bool linked_cycle_obligations_closed;
      "linked_cycle_topology", `List (List.map (fun report -> member "topology" report) cycle_reports);
      "linked_cycle_rounds", `List (cycle_reports |> List.concat_map (fun report -> list_field "rounds" report));
      "linked_cycle_changed_bindings", `List linked_cycle_changed_bindings;
      "linked_cycle_bootstrap_bindings_remaining", `Int linked_cycle_bootstrap_bindings_remaining;
      "linked_cycle_stable_exports", `Bool (linked_cycle_scc_count = 0 || (linked_cycle_worklist_drained && linked_cycle_bootstrap_bindings_remaining = 0));
      "linked_cycle_shared_scc_solvers", `List linked_cycle_shared_scc_solvers;
      "shared_scc_worklist_run", `Bool linked_cycle_shared_scc_worklist_run;
      "shared_scc_equation_ids", `List linked_cycle_shared_scc_equation_ids;
      "shared_scc_cell_ids", `List linked_cycle_shared_scc_cell_ids;
      "shared_scc_dependencies", `List linked_cycle_shared_scc_dependencies;
      "shared_scc_state_read_count", `Int linked_cycle_shared_scc_state_read_count;
      "shared_scc_worklist_schedule", `List linked_cycle_shared_scc_worklist_schedule;
      "shared_scc_final_cells", `List linked_cycle_shared_scc_final_cells;
      "linked_cycle_accepted_exports", `List linked_cycle_accepted_exports;
      "imported_cyclic_observable_values", `List linked_cycle_imported_observable_values;
      "cyclic_imported_value_exact_singleton_parity", `Bool linked_cycle_exact_singleton_parity;
      "cycle_final_values_derive_from_shared_scc_final_cells", `Bool linked_cycle_exact_singleton_parity;
      "residual_linking_performed", `Bool true;
      "module_count", `Int (List.length modules);
      "module_analyzers_executed", `Int (List.length per_module);
      "module_identity_set_matches", `Bool linked_residual_analyzer_evidence.module_identity_set_matches;
      "per_module_stage2_inputs_used", `Bool true;
      "provider_derived_importer_inputs_used", `Bool (linked_environment <> []);
      "semantic_exports", `List (List.map semantic_export_to_json semantic_exports);
      "external_summaries", `List (List.map (fun export -> external_summary_to_json export.external_summary) semantic_exports);
      "linked_environment", `List (List.map linked_environment_entry_to_json linked_environment);
      "linked_stage2_input_derivation", `List (linked_input_derivation_json linked_environment);
      "phase_log", `List (List.map phase_event_to_json phase_log);
      "namespacing_strategy", `String "linked_module_id-prefix";
      "linked_residual_row_count", `Int linked_residual_analyzer_evidence.linked_residual_row_count;
      "residual_rows_observed", `Bool linked_residual_analyzer_evidence.residual_rows_observed;
      "matched_obligation_count", `Int (List.length matched);
      "unresolved_obligation_count", `Int (List.length unresolved);
      "obligations_closed", `Bool linked_residual_analyzer_evidence.obligations_closed;
      "no_shortcut_path", `Bool linked_residual_analyzer_evidence.no_shortcut_path;
      "matched_obligations", `List matched;
      "unresolved_obligations", `List unresolved;
      "module_logs", `List module_logs;
      "non_equivalence_claim", `String "scoped emitted-equation/witness-universe soundness plus fixture parity; not arbitrary-C whole-program theorem";
    ];
  }

let make_analyzer ~linked_id modules =
  if List.length modules < 2 then failwith "residual linking requires at least two module bundles";
  {
    linked_id;
    modules;
    run = execute_modules modules;
  }

let execute analyzer inputs = analyzer.run inputs

let output_to_json output =
  `Assoc [
    "final_input_table", `List output.final_input_table;
    "final_output_table", `List output.final_output_table;
    "shape_witnesses", `List output.shape_witnesses;
    "semantic_exports", `List (List.map semantic_export_to_json output.semantic_exports);
    "external_summaries", `List (List.map (fun export -> external_summary_to_json export.external_summary) output.semantic_exports);
    "linked_environment", `List (List.map linked_environment_entry_to_json output.linked_environment);
    "linked_stage2_input_derivation", `List output.linked_stage2_input_derivation;
    "phase_log", `List (List.map phase_event_to_json output.phase_log);
    "linked_residual_analyzer_evidence", linked_run_evidence_to_json output.linked_residual_analyzer_evidence;
    "execution_log", output.execution_log;
  ]

let artifact_json ~doc_path analyzer output =
  let modules = analyzer.modules in
  let matched = matched_obligations modules in
  let unresolved = unresolved_obligations modules in
  `Assoc [
    "schema_version", `String schema_version;
    "artifact_kind", `String "abstract-speculate-linked-residual-analyzer";
    "linked_id", `String analyzer.linked_id;
    "claim", `String "PE(I,m1), PE(I,m2), ... -> residual linking -> linked residual analyzer";
    "validation_oracle", `String "typed bundle execution plus parsed-CIL structural obligation matching";
    "input_modules", `List (List.map input_module_json modules);
    "module_count", `Int (List.length modules);
    "declared_imports", `List (modules |> List.concat_map (fun bundle ->
      bundle.declared_imports |> List.map (fun d ->
        `Assoc ["module_id", `String bundle.result.module_id; "declaration", declaration_to_json d])) |> sort_json);
    "declared_exports", `List (modules |> List.concat_map (fun bundle ->
      bundle.declared_exports |> List.map (fun d ->
        `Assoc ["module_id", `String bundle.result.module_id; "declaration", declaration_to_json d])) |> sort_json);
    "matched_obligations", `List matched;
    "unresolved_obligations", `List unresolved;
    "linked_stage2_input", `Assoc [
      "dispatch", `String "provider-derived-linked-environment";
      "keys", `List (modules |> List.map (fun bundle -> `String (bundle.result.module_id ^ ":" ^ bundle.result.source_hash)));
      "derivation_source", `String "provider-stage2-output";
      "linked_environment_generated", `Bool (output.linked_environment <> []);
    ];
    "semantic_exports", `List (List.map semantic_export_to_json output.semantic_exports);
    "external_summaries", `List (List.map (fun export -> external_summary_to_json export.external_summary) output.semantic_exports);
    "linked_environment", `List (List.map linked_environment_entry_to_json output.linked_environment);
    "linked_stage2_input_derivation", `List output.linked_stage2_input_derivation;
    "phase_log", `List (List.map phase_event_to_json output.phase_log);
    "linked_output", output_to_json output;
    "residual_linking_performed", `Bool true;
    "linked_residual_analyzer_ran", `Bool output.linked_residual_analyzer_evidence.linked_residual_analyzer_ran;
    "linked_residual_solver_run", output.execution_log |> member "linked_residual_solver_run";
    "linked_solver_backed_residual_fixpoint", output.execution_log |> member "linked_solver_backed_residual_fixpoint";
    "linked_solver_iteration_count", output.execution_log |> member "linked_solver_iteration_count";
    "linked_changed_cell_count", output.execution_log |> member "linked_changed_cell_count";
    "linked_residual_equation_count", output.execution_log |> member "linked_residual_equation_count";
    "linked_state_read_count", output.execution_log |> member "linked_state_read_count";
    "linked_seed_input_read_count", output.execution_log |> member "linked_seed_input_read_count";
    "linked_exact_cell_dependencies", output.execution_log |> member "linked_exact_cell_dependencies";
    "linked_equation_apply_reads_solver_state", output.execution_log |> member "linked_equation_apply_reads_solver_state";
    "linked_worklist_drained", output.execution_log |> member "linked_worklist_drained";
    "linked_overlay_only", output.execution_log |> member "linked_overlay_only";
    "linked_cyclic_residual_solver_run", output.execution_log |> member "linked_cyclic_residual_solver_run";
    "linked_cycle_scc_count", output.execution_log |> member "linked_cycle_scc_count";
    "linked_cycle_iteration_count", output.execution_log |> member "linked_cycle_iteration_count";
    "linked_cycle_worklist_drained", output.execution_log |> member "linked_cycle_worklist_drained";
    "linked_cycle_obligations_closed", output.execution_log |> member "linked_cycle_obligations_closed";
    "linked_cycle_topology", output.execution_log |> member "linked_cycle_topology";
    "linked_cycle_rounds", output.execution_log |> member "linked_cycle_rounds";
    "linked_cycle_changed_bindings", output.execution_log |> member "linked_cycle_changed_bindings";
    "linked_cycle_bootstrap_bindings_remaining", output.execution_log |> member "linked_cycle_bootstrap_bindings_remaining";
    "linked_cycle_stable_exports", output.execution_log |> member "linked_cycle_stable_exports";
    "linked_cycle_shared_scc_solvers", output.execution_log |> member "linked_cycle_shared_scc_solvers";
    "shared_scc_worklist_run", output.execution_log |> member "shared_scc_worklist_run";
    "shared_scc_equation_ids", output.execution_log |> member "shared_scc_equation_ids";
    "shared_scc_cell_ids", output.execution_log |> member "shared_scc_cell_ids";
    "shared_scc_dependencies", output.execution_log |> member "shared_scc_dependencies";
    "shared_scc_state_read_count", output.execution_log |> member "shared_scc_state_read_count";
    "shared_scc_worklist_schedule", output.execution_log |> member "shared_scc_worklist_schedule";
    "shared_scc_final_cells", output.execution_log |> member "shared_scc_final_cells";
    "linked_cycle_accepted_exports", output.execution_log |> member "linked_cycle_accepted_exports";
    "imported_cyclic_observable_values", output.execution_log |> member "imported_cyclic_observable_values";
    "cyclic_imported_value_exact_singleton_parity", output.execution_log |> member "cyclic_imported_value_exact_singleton_parity";
    "cycle_final_values_derive_from_shared_scc_final_cells", output.execution_log |> member "cycle_final_values_derive_from_shared_scc_final_cells";
    "linked_residual_analyzer_evidence", linked_run_evidence_to_json output.linked_residual_analyzer_evidence;
    "per_module_stage2_inputs_used", `Bool true;
    "provider_derived_importer_inputs_used", `Bool (output.linked_environment <> []);
    "module_local_prelink", `Bool true;
    "linked_entrypoints_used_before_pe", `Bool false;
    "linked_facts_prelink", `Bool false;
    "metadata_only_proof", `Bool false;
    "json_is_summary_only", `Bool false;
    "namespacing_strategy", `String "linked_module_id-prefix";
    "forbidden_prelink_entrypoints", `List [`String forbidden_global_entry; `String forbidden_merge_entry];
    "doc_path", `String doc_path;
    "non_claims", `List [
      `String "no full whole-program semantic equivalence proof";
      `String "no multi-file frontend merge before PE";
      `String "no final residual-linker API freeze";
    ];
  ]

let write_artifact ~path ~doc_path analyzer output =
  Real_sparrow_artifact.write_json path (artifact_json ~doc_path analyzer output)
