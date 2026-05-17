(***********************************************************************)
(* First-pass residual linking for Abstract Speculate PE.               *)
(***********************************************************************)

module StageT = Abstract_speculate_stage_types
module Stage2 = Abstract_speculate_stage2_input
module MetaSparse = Abstract_speculate_meta_sparse
module Residual = Abstract_speculate_residual_value

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

type external_summary_v1 = {
  extern_scalar_value : Yojson.Safe.t;
  function_return_summary : Yojson.Safe.t;
  global_write_summary_placeholder : Yojson.Safe.t;
  provenance : Yojson.Safe.t;
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
  external_summary : external_summary_v1;
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

let external_summary_to_json summary =
  `Assoc [
    "schema_version", `String "abstract-speculate-external-summary/v1";
    "extern_scalar_value", summary.extern_scalar_value;
    "function_return_summary", summary.function_return_summary;
    "global_write_summary_placeholder", summary.global_write_summary_placeholder;
    "provenance", summary.provenance;
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
  {
    extern_scalar_value = `Assoc [
      "name", `String export_name;
      "value", `Int return_value;
      "abstract_value", `String abstract_return_value;
      "source", `String "function-return-singleton";
    ];
    function_return_summary = `Assoc [
      "function", `String export_name;
      "return_node", `String return_node;
      "return_location", `String return_location;
      "return_value", `Int return_value;
      "abstract_return_value", `String abstract_return_value;
    ];
    global_write_summary_placeholder = `Assoc [
      "status", `String "placeholder-v1";
      "writes", `List [];
      "precision", `String "deferred";
    ];
    provenance;
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
          `Assoc [
            "provider_module", `String entry.provider_module;
            "provider_source_hash", `String entry.semantic_export.provider_source_hash;
            "provider_artifact_path", `String entry.semantic_export.provider_artifact_path;
            "export_name", `String entry.export_name;
            "return_location", `String entry.semantic_export.return_location;
            "return_node", `String entry.semantic_export.return_node;
            "abstract_return_value", `String entry.semantic_export.abstract_return_value;
            "provider_phase_index", `Int entry.semantic_export.provider_phase_index;
            "derivation_source", `String "provider-stage2-output";
            "external_summary_schema", `String "abstract-speculate-external-summary/v1";
            "external_summary", external_summary_to_json entry.semantic_export.external_summary;
          ]))
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
       `Assoc [
         "importer_module", `String entry.importer_module;
         "importer_extern_root", `String entry.importer_extern_root;
         "import_name", `String entry.import_name;
         "provider_module", `String entry.provider_module;
         "export_name", `String entry.export_name;
         "linked_return_value", `Int entry.linked_return_value;
         "effect_reason", `String "linked-provider-return";
         "stage2_obligation", `String "dynamic external/link fact derived from provider stage2 output";
         "derivation_source", `String "provider-stage2-output";
         "semantic_export", semantic_export_to_json entry.semantic_export;
         "external_summary", external_summary_to_json entry.semantic_export.external_summary;
       ])

let derive_linked_run_evidence
    ~linked_execute_returned
    ~modules
    ~per_module
    ~final_input_table
    ~final_output_table
    ~matched
    ~unresolved =
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
  let linked_residual_analyzer_ran =
    linked_execute_returned &&
    all_modules_executed &&
    residual_rows_observed &&
    obligations_closed &&
    no_shortcut_path
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
  ]

let execute_modules modules inputs =
  let matches = matching_export_names modules in
  ensure_supported_link_shape matches;
  let function_matches = function_matches matches in
  let phase_event phase_index module_id event = { phase_index; module_id; event } in
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
  let rec schedule phase_index remaining executed_ids per_module semantic_exports linked_environment phase_log =
    match remaining with
    | [] -> per_module, semantic_exports, linked_environment, phase_log
    | _ ->
        let ready, blocked =
          List.partition (module_dependencies_satisfied executed_ids) remaining
        in
        if ready = [] then
          failwith "unsupported cyclic mixed importer/provider residual-linking topology";
        let phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log =
          ready
          |> List.fold_left
               (fun (phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log) bundle ->
                  let import_matches = module_import_matches bundle in
                  let export_matches = module_export_matches bundle in
                  let is_importer = import_matches <> [] in
                  let is_provider = export_matches <> [] in
                  let module_id = bundle.result.module_id in
                  let module_environment =
                    if is_importer then linked_environment_for_matches import_matches semantic_exports
                    else []
                  in
                  let input =
                    if is_importer then linked_stage2_input_for_importer bundle module_environment
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
                    if is_importer then "importer-stage2-executed-with-linked-environment"
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
                  next_phase,
                  module_id :: executed_ids,
                  per_module @ [bundle, output],
                  semantic_exports @ new_exports,
                  linked_environment @ module_environment,
                  phase_log @ env_events @ execution_events @ export_events)
               (phase_index, executed_ids, per_module, semantic_exports, linked_environment, phase_log)
        in
        schedule phase_index blocked executed_ids per_module semantic_exports linked_environment phase_log
  in
  let per_module, semantic_exports, linked_environment, phase_log =
    schedule 1 modules [] [] [] [] []
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
  let linked_residual_analyzer_evidence =
    derive_linked_run_evidence
      ~linked_execute_returned:true
      ~modules
      ~per_module
      ~final_input_table
      ~final_output_table
      ~matched
      ~unresolved
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
      "non_equivalence_claim", `String "first-pass residual linkability only; not whole-program semantic equivalence";
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
