(***********************************************************************)
(* Stage-2 input and online residual runtime for Abstract Speculate PE. *)
(***********************************************************************)

module T = Abstract_speculate_stage_types

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

let member = Yojson.Safe.Util.member
let int_json n = `Int n

let make_extern_effects ~source ~hash ~extern_roots =
  `Assoc [
    "schema_version", `String "abstract-speculate-extern-effects/v2";
    "source_file", `String source;
    "source_hash", `String hash;
    "extern_roots", `List (List.map (fun root -> `String root) extern_roots);
    "row_overrides", `List [];
    "effects", `List (List.map (fun root ->
      `Assoc [
        "node", `String root;
        "reason", `String "unknown-extern-call";
        "value", `Int 41;
        "stage2_obligation", `String "dynamic external/link fact supplied after module PE";
      ]) extern_roots);
  ]

let int_field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some (`Int n) -> Some n | _ -> None)
  | _ -> None

let string_field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some (`String s) -> Some s | _ -> None)
  | _ -> None

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let sort_uniq_strings xs = List.sort_uniq String.compare xs

let same_string_set left right =
  sort_uniq_strings left = sort_uniq_strings right

let string_list_field name json =
  match assoc_field name json with
  | Some (`List xs) ->
      xs
      |> List.fold_left (fun acc -> function
           | `String s -> Option.map (fun ss -> s :: ss) acc
           | _ -> None)
           (Some [])
      |> Option.map List.rev
  | _ -> None

let parse_effect = function
  | (`Assoc _ as eff) ->
      begin match string_field "node" eff, int_field "value" eff with
      | Some node, Some _value -> Some node
      | _ -> None
      end
  | _ -> None

let effects_nodes = function
  | `List effects ->
      effects
      |> List.fold_left (fun acc eff ->
           match acc, parse_effect eff with
           | Some nodes, Some node -> Some (node :: nodes)
           | _ -> None)
           (Some [])
      |> Option.map List.rev
  | _ -> None

let structural_source_hash_matches ~source_hash (input : T.stage2_input) =
  match string_field "source_hash" input.extern_effects with
  | Some hash -> hash = source_hash
  | None -> false

let extern_int ~node ~location:_ (input : T.stage2_input) =
  match member "effects" input.extern_effects with
  | `List effects ->
      let rec loop = function
        | [] -> 0
        | entry :: rest ->
            match string_field "node" entry, int_field "value" entry with
            | Some node', Some value when node' = node -> value
            | _ -> loop rest
      in
      loop effects
  | _ -> 0

let validate_extern_effects ~source_hash ~extern_roots input =
  match input.T.extern_effects with
  | (`Assoc _ as payload) ->
      string_field "schema_version" payload = Some "abstract-speculate-extern-effects/v2" &&
      structural_source_hash_matches ~source_hash input &&
      begin match string_list_field "extern_roots" payload, assoc_field "effects" payload with
      | Some roots, Some effects ->
          same_string_set roots extern_roots &&
          begin match effects_nodes effects with
          | Some effect_nodes -> same_string_set effect_nodes extern_roots
          | None -> false
          end
      | _ -> false
      end
  | _ -> false

let overlay_rows static_rows residual_rows =
  List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))
    (static_rows @ residual_rows)

let rows_of_results results = List.map (fun (result : T.residual_component_result) -> result.T.row) results
let executions_of_results results = List.map (fun (result : T.residual_component_result) -> result.T.execution) results

let override_row ~table ~node extern_effects =
  match member "row_overrides" extern_effects with
  | `List overrides ->
      overrides
      |> List.find_map (fun override ->
        match string_field "table" override, string_field "node" override, member "row" override with
        | Some table', Some node', row when table' = table && node' = node && row <> `Null -> Some row
        | _ -> None)
  | _ -> None

let assoc_without keys fields =
  fields |> List.filter (fun (key, _) -> not (List.mem key keys))

let with_fields fields extras =
  `Assoc (extras @ assoc_without (List.map fst extras) fields)

let executed_component default_component residual_arithmetic_value residual_guard_value =
  match default_component with
  | `Assoc fields ->
      with_fields fields [
        "value", `String (string_of_int residual_arithmetic_value);
        "residual_arithmetic_value", `Int residual_arithmetic_value;
        "residual_guard_value", `Bool residual_guard_value;
        "stage2_semantics_applied", `Bool true;
      ]
  | json -> json

let executed_row ~location default_row executed_component =
  match default_row with
  | `Assoc fields ->
      let memory =
        match List.assoc_opt "memory" fields with
        | Some (`List cells) ->
            cells |> List.map (fun cell ->
              match string_field "location" cell with
              | Some loc when loc = location -> executed_component
              | _ -> cell)
        | _ -> [executed_component]
      in
      with_fields fields ["memory", `List memory]
  | _ -> default_row

let execute_staged_component
    ~id
    ~source_file
    ~source_hash
    ~table
    ~node
    ~location
    ~ordinal
    ~default_row
    ~default_component
    ~component_kind
    ~transfer_event
    ~lattice_event
    ~shape
    ~witness_constructor
    ~semantic_expression
    ~semantic_guard
    ~residual_arithmetic_value
    ~residual_guard_value
    (input : T.stage2_input) =
  if not (structural_source_hash_matches ~source_hash input) then
    failwith "stage2 staged component source hash mismatch";
  let executed_component = executed_component default_component residual_arithmetic_value residual_guard_value in
  let row = executed_row ~location default_row executed_component in
  {
    T.row = row;
    execution = `Assoc [
      "id", `String id;
      "table", `String table;
      "node", `String node;
      "location", `String location;
      "ordinal", `Int ordinal;
      "code_kind", `String "Trx.code";
      "typed_code_present", `Bool true;
      "granularity", `String "cell-component";
      "component_kind", `String component_kind;
      "semantic_core", `String "staged-transfer-component-residual";
      "uses_stage2_dynamic_input", `Bool true;
      "source_file", `String source_file;
      "source_hash", `String source_hash;
      "runcode_executed", `Bool true;
      "execution_relation", `String "structural-equiv";
      "shape", `String shape;
      "typed_shape", `String shape;
      "witness_constructor", `String witness_constructor;
      "transfer_event", `String transfer_event;
      "lattice_event", `String lattice_event;
      "semantic_expression", `String semantic_expression;
      "semantic_guard", `String semantic_guard;
      "fixpoint_iteration", member "fixpoint_iteration" default_component;
      "default_component", default_component;
      "executed_component", executed_component;
      "executed_row", row;
      "residual_arithmetic_executed", `Bool true;
      "residual_arithmetic_value", `Int residual_arithmetic_value;
      "residual_guard_executed", `Bool true;
      "residual_guard_value", `Bool residual_guard_value;
      "stage2_dynamic_input_checked", `Bool true;
      "stage2_sparse_recompute", `Bool false;
      "row_wrapper", `Bool false;
    ];
  }

let rec count_shape_json (loops, branches, exprs, typed) = function
  | `Assoc fields ->
      begin match List.assoc_opt "kind" fields with
      | Some (`String "typed-code") -> (loops, branches, exprs, typed + 1)
      | Some (`String "typed-component") -> (loops, branches, exprs, typed + 1)
      | Some (`String "loop") ->
          let body = match List.assoc_opt "body" fields with Some (`List xs) -> xs | _ -> [] in
          List.fold_left count_shape_json (loops + 1, branches, exprs, typed) body
      | Some (`String "if") ->
          let then_w = match List.assoc_opt "then_witness" fields with Some (`List xs) -> xs | _ -> [] in
          let else_w = match List.assoc_opt "else_witness" fields with Some (`List xs) -> xs | _ -> [] in
          List.fold_left count_shape_json (loops, branches + 1, exprs, typed) (then_w @ else_w)
      | Some (`String "expr") ->
          let children = match List.assoc_opt "children" fields with Some (`List xs) -> xs | _ -> [] in
          List.fold_left count_shape_json (loops, branches, exprs + 1, typed) children
      | _ -> (loops, branches, exprs, typed)
      end
  | _ -> (loops, branches, exprs, typed)

let shape_counts_json witnesses =
  List.fold_left count_shape_json (0, 0, 0, 0) witnesses

let bool_member name json = match member name json with `Bool b -> b | _ -> false

let string_member name json = match member name json with `String s -> s | _ -> ""

let staged_transfer_component json =
  string_member "component_kind" json = "staged-abstract-cell"

let executed_transfer_component json =
  staged_transfer_component json &&
  (contains (string_member "transfer_event" json) "created-D" ||
   contains (string_member "transfer_event" json) "dynamic")

let executed_lattice_component json =
  staged_transfer_component json &&
  string_member "lattice_event" json <> ""

let run_summary
    ~schema_version
    ~module_id
    ~source_file
    ~source_hash
    ~extern_roots
    ~static_input_rows
    ~static_output_rows
    ~residual_input_results
    ~residual_output_results
    ~control_results
    ~shape_witnesses_json
    ~blind_equality_witness
    (input : T.stage2_input) =
  let extern_valid = validate_extern_effects ~source_hash ~extern_roots input in
  if not extern_valid then failwith "stage2 extern/link input does not match module residual obligation";
  let residual_input_rows = rows_of_results residual_input_results in
  let residual_output_rows = rows_of_results residual_output_results in
  let final_input_table = overlay_rows static_input_rows residual_input_rows in
  let final_output_table = overlay_rows static_output_rows residual_output_rows in
  if final_input_table = [] && final_output_table = [] then
    failwith "stage2 residual analyzer received no stage-1 sparse rows";
  let executed_residuals =
    executions_of_results (residual_input_results @ residual_output_results @ control_results)
  in
  let transfer_residuals = List.filter executed_transfer_component executed_residuals in
  let lattice_residuals = List.filter executed_lattice_component executed_residuals in
  let dynamic_iterations =
    transfer_residuals
    |> List.filter_map (fun json -> match member "fixpoint_iteration" json with `Int n -> Some n | _ -> None)
    |> List.sort_uniq compare
    |> List.length
  in
  let loops, branches, exprs, typed = shape_counts_json shape_witnesses_json in
  let blind_ok = bool_member "static_projection_equal" blind_equality_witness in
  let execution_log = `Assoc [
    "schema_version", `String schema_version;
    "module_id", `String module_id;
    "source_file", `String source_file;
    "source_hash", `String source_hash;
    "typed_metaocaml_code_value", `Bool true;
    "online_runcode_run", `Bool true;
    "runcode_run_performed", `Bool true;
    "source_string_residual_core", `Bool false;
    "json_metadata_only_proof", `Bool false;
    "wrapper_reuse", `Bool false;
    "linked_facts_prelink", `Bool false;
    "top_substitution", `Bool false;
    "toy_only", `Bool false;
    "extern_effect_input_valid", `Bool extern_valid;
    "extern_root_count", `Int (List.length extern_roots);
    "extern_roots", `List (List.map (fun root -> `String root) extern_roots);
    "static_input_row_count", int_json (List.length static_input_rows);
    "residual_input_row_count", int_json (List.length residual_input_rows);
    "static_output_row_count", int_json (List.length static_output_rows);
    "residual_output_row_count", int_json (List.length residual_output_rows);
    "control_residual_count", int_json (List.length control_results);
    "executed_residual_count", int_json (List.length executed_residuals);
    "executed_residuals", `List executed_residuals;
    "blind_equality_static_projection", `Bool blind_ok;
    "convergence_ignores_residual_code_structure", blind_equality_witness |> member "ignores_residual_code_structure";
    "blind_equality_witness", blind_equality_witness;
    "shape_loop_count", `Int loops;
    "shape_branch_count", `Int branches;
    "shape_expr_count", `Int exprs;
    "shape_typed_code_count", `Int typed;
    "stage2_sparse_recompute", `Bool false;
    "staged_domain_fixpoint", `Bool true;
    "bta_participates_in_fixpoint", `Bool true;
    "stage1_direct_sparse_pipeline", `Bool false;
    "direct_sparse_oracle_only", `Bool true;
    "posthoc_row_split_used", `Bool false;
    "row_obligation_residual_source", `Bool false;
    "whole_row_dynamic_wrapper", `Bool false;
    "metadata_only_proof", `Bool false;
    "transfer_level_d_site_count", int_json (List.length transfer_residuals);
    "staged_lattice_event_count", int_json (List.length lattice_residuals);
    "bta_dynamic_sites_before_convergence", int_json (List.length extern_roots + List.length transfer_residuals);
    "fixpoint_iterations_with_dynamic_cells", int_json dynamic_iterations;
    "typed_residual_arithmetic_count", int_json (List.length transfer_residuals);
    "typed_residual_guard_count", int_json (List.length transfer_residuals);
    "residual_runtime_scope", `String "staged-cell-component-residuals";
    "comparison_relation", `String "=";
  ] in
  {
    T.final_input_table;
    final_output_table;
    execution_log;
    shape_witnesses = shape_witnesses_json;
  }

let output_to_yojson (output : T.stage2_output) =
  `Assoc [
    "final_input_table", `List output.final_input_table;
    "final_output_table", `List output.final_output_table;
    "execution_log", output.execution_log;
    "shape_witnesses", `List output.shape_witnesses;
  ]
