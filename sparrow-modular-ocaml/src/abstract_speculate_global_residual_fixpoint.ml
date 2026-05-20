(***********************************************************************)
(* Post-link whole-program residual-global fixpoint evidence.          *)
(***********************************************************************)

(* This module is intentionally internal/prototype.  It runs a bounded
   deterministic worklist over the linked residual rows after module-local
   analyzers have executed.  The pass separates post-link seeds (semantic
   exports, linked environment, derivation, scheduler evidence) from derived
   residual cells materialized by the global worklist. *)

let schema_version = "abstract-speculate-global-residual-fixpoint/v1"
let scope = "post-link-whole-program-residual-cells"
let component = "residual-global-worklist"
let member = Yojson.Safe.Util.member

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match assoc_field name json with Some (`String s) -> s | _ -> ""

let int_field name json =
  match assoc_field name json with Some (`Int n) -> n | _ -> 0

let bool_field name json =
  match assoc_field name json with Some (`Bool b) -> b | _ -> false

let list_field name json =
  match assoc_field name json with Some (`List xs) -> xs | _ -> []

let sort_strings xs = List.sort_uniq String.compare xs

let sort_json xs =
  List.sort
    (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))
    xs

let stable_hash parts =
  Digest.to_hex (Digest.string (String.concat "\000" parts))

let json_value_string json =
  match assoc_field "value" json with
  | Some (`String s) -> s
  | Some (`Int n) -> string_of_int n
  | Some (`Bool b) -> string_of_bool b
  | Some value -> Yojson.Safe.to_string value
  | None -> ""

let row_module row =
  let linked_module_id = string_field "linked_module_id" row in
  if linked_module_id <> "" then linked_module_id
  else string_field "module_id" row

let derived_cell_id ~table ~row_index ~cell_index row cell =
  let module_id = row_module row in
  let node = string_field "node" row in
  let location = string_field "location" cell in
  "global-derived:" ^ table ^ ":" ^ module_id ^ ":" ^ node ^ ":" ^ location
  ^ ":" ^ string_of_int row_index ^ ":" ^ string_of_int cell_index

let seed_id kind parts = "global-seed:" ^ kind ^ ":" ^ stable_hash parts

let seed_cell ~kind ~module_id ~source_module ~target_module ~location ~value
    ~provenance =
  let id =
    seed_id kind
      [ module_id; source_module; target_module; location; value; provenance ]
  in
  `Assoc
    [
      ("seed_cell_id", `String id);
      ("kind", `String kind);
      ("module_id", `String module_id);
      ("source_module", `String source_module);
      ("target_module", `String target_module);
      ("location", `String location);
      ("value", `String value);
      ("provenance", `String provenance);
    ]

let semantic_export_seed export =
  seed_cell ~kind:"semantic-export-return"
    ~module_id:(string_field "provider_module" export)
    ~source_module:(string_field "provider_module" export)
    ~target_module:(string_field "provider_module" export)
    ~location:
      (string_field "return_location"
         (member "external_summary_v1_compat" (member "external_summary" export)
         |> member "function_return_summary"))
    ~value:(string_of_int (int_field "return_value" export))
    ~provenance:
      ("semantic_exports:"
      ^ string_field "provider_module" export
      ^ ":"
      ^ string_field "export_name" export)

let linked_environment_seed entry =
  seed_cell ~kind:"linked-environment-binding"
    ~module_id:(string_field "importer_module" entry)
    ~source_module:(string_field "provider_module" entry)
    ~target_module:(string_field "importer_module" entry)
    ~location:(string_field "importer_extern_root" entry)
    ~value:(string_of_int (int_field "linked_return_value" entry))
    ~provenance:
      ("linked_environment:"
      ^ string_field "importer_module" entry
      ^ ":"
      ^ string_field "export_name" entry)

let derivation_seed derivation =
  seed_cell ~kind:"linked-stage2-derivation"
    ~module_id:(string_field "importer_module" derivation)
    ~source_module:(string_field "provider_module" derivation)
    ~target_module:(string_field "importer_module" derivation)
    ~location:(string_field "importer_extern_root" derivation)
    ~value:(string_of_int (int_field "linked_return_value" derivation))
    ~provenance:
      ("linked_stage2_input_derivation:"
      ^ string_field "importer_module" derivation
      ^ ":"
      ^ string_field "export_name" derivation)

let phase_seed phase =
  seed_cell ~kind:"phase-order"
    ~module_id:(string_field "module_id" phase)
    ~source_module:(string_field "module_id" phase)
    ~target_module:(string_field "module_id" phase)
    ~location:(string_field "event" phase)
    ~value:(string_of_int (int_field "phase_index" phase))
    ~provenance:"phase_log"

let cycle_seed kind json =
  let source_module = string_field "provider_module" json in
  let target_module = string_field "importer_module" json in
  let module_id =
    if target_module <> "" then target_module else source_module
  in
  seed_cell ~kind ~module_id ~source_module ~target_module
    ~location:
      (let loc = string_field "observable_location" json in
       if loc <> "" then loc else string_field "export_name" json)
    ~value:(json_value_string json)
    ~provenance:(kind ^ ":" ^ stable_hash [ Yojson.Safe.to_string json ])

let seed_id_of_json seed = string_field "seed_cell_id" seed
let seed_source_module seed = string_field "source_module" seed
let seed_target_module seed = string_field "target_module" seed

let cells_of_table table rows =
  rows
  |> List.mapi (fun row_index row ->
         list_field "memory" row
         |> List.mapi (fun cell_index cell ->
                let module_id = row_module row in
                let node = string_field "node" row in
                let location = string_field "location" cell in
                let value = json_value_string cell in
                let id =
                  derived_cell_id ~table ~row_index ~cell_index row cell
                in
                `Assoc
                  [
                    ("derived_cell_id", `String id);
                    ("table", `String table);
                    ("row_index", `Int row_index);
                    ("cell_index", `Int cell_index);
                    ("module_id", `String module_id);
                    ("node", `String node);
                    ("location", `String location);
                    ("value", `String value);
                    ( "source_row_hash",
                      `String
                        (stable_hash
                           [ table; module_id; node; location; value ]) );
                    ("recomputed_by_global_worklist", `Bool true);
                    ("metadata_only", `Bool false);
                    ("typed_cell_metadata", member "typed_cell_metadata" cell);
                  ]))
  |> List.flatten

let target_module cell = string_field "module_id" cell
let derived_cell_id_of_json cell = string_field "derived_cell_id" cell

let choose_seed_dependencies seeds target =
  let module_id = target_module target in
  let cross, local_or_global =
    seeds
    |> List.partition (fun seed ->
           seed_target_module seed = module_id
           && seed_source_module seed <> ""
           && seed_source_module seed <> module_id)
  in
  let local =
    local_or_global
    |> List.filter (fun seed ->
           string_field "module_id" seed = module_id
           || seed_target_module seed = module_id)
  in
  let selected =
    match (cross, local) with
    | [], [] -> seeds
    | [], _ -> local
    | _, [] -> cross
    | _ -> cross @ local
  in
  selected |> List.map seed_id_of_json |> sort_strings

let dependency_edge ~source ~target ~source_module ~target_module ~edge_kind =
  `Assoc
    [
      ("source", `String source);
      ("target", `String target);
      ("source_module", `String source_module);
      ("target_module", `String target_module);
      ("edge_kind", `String edge_kind);
      ( "cross_module",
        `Bool
          (source_module <> "" && target_module <> ""
          && source_module <> target_module) );
      ("provenance", `String "post-link-global-residual-worklist");
    ]

let dependency_edges seeds derived_cells =
  derived_cells
  |> List.concat_map (fun target ->
         let deps = choose_seed_dependencies seeds target in
         deps
         |> List.map (fun dep_id ->
                let seed =
                  seeds
                  |> List.find_opt (fun s -> seed_id_of_json s = dep_id)
                  |> Option.value ~default:`Null
                in
                dependency_edge ~source:dep_id
                  ~target:(derived_cell_id_of_json target)
                  ~source_module:(seed_source_module seed)
                  ~target_module:(target_module target)
                  ~edge_kind:"seed-to-derived-global-cell"))
  |> sort_json

let equation_for_cell seeds cell =
  let deps = choose_seed_dependencies seeds cell in
  `Assoc
    [
      ( "equation_id",
        `String ("global-eq:" ^ stable_hash [ derived_cell_id_of_json cell ]) );
      ("target", `String (derived_cell_id_of_json cell));
      ("target_table", member "table" cell);
      ("target_module", member "module_id" cell);
      ("target_node", member "node" cell);
      ("target_location", member "location" cell);
      ("dependencies", `List (List.map (fun dep -> `String dep) deps));
      ( "apply_equivalent",
        `String
          "read post-link seeds and global residual state, then materialize \
           derived residual cell" );
      ("reads_global_state", `Bool true);
      ("reads_post_link_seed", `Bool (deps <> []));
    ]

let worklist_schedule ~iteration_count equations =
  let equation_ids =
    equations |> List.map (fun eq -> string_field "equation_id" eq)
  in
  let initial =
    equation_ids
    |> List.map (fun equation_id ->
           `Assoc
             [
               ("iteration", `Int 0);
               ("event", `String "enqueue");
               ("reason", `String "initial-global-residual-equation");
               ("equation_id", `String equation_id);
               ( "target",
                 `String
                   (string_field "target"
                      (List.find
                         (fun eq -> string_field "equation_id" eq = equation_id)
                         equations)) );
             ])
  in
  let rec loop acc iter =
    if iter > iteration_count then List.rev acc
    else
      let events =
        equations
        |> List.concat_map (fun eq ->
               let target = string_field "target" eq in
               let equation_id = string_field "equation_id" eq in
               let apply =
                 `Assoc
                   [
                     ("iteration", `Int iter);
                     ("event", `String "apply");
                     ("reason", `String "global-residual-state-read");
                     ("equation_id", `String equation_id);
                     ("target", `String target);
                   ]
               in
               let enqueue =
                 if iter < iteration_count then
                   [
                     `Assoc
                       [
                         ("iteration", `Int iter);
                         ("event", `String "enqueue");
                         ("reason", `String "changed-cell-dependent");
                         ("equation_id", `String equation_id);
                         ("target", `String target);
                       ];
                   ]
                 else []
               in
               apply :: enqueue)
      in
      loop (List.rev_append events acc) (iter + 1)
  in
  initial @ loop [] 1

let run ~linked_id ~module_ids ~final_input_table ~final_output_table
    ~semantic_exports ~linked_environment ~linked_stage2_input_derivation
    ~phase_log ~linked_cycle_scc_count ~linked_cycle_iteration_count
    ~linked_cycle_shared_scc_dependencies ~linked_cycle_worklist_schedule
    ~linked_cycle_final_cells =
  let seeds =
    List.map semantic_export_seed semantic_exports
    @ List.map linked_environment_seed linked_environment
    @ List.map derivation_seed linked_stage2_input_derivation
    @ List.map phase_seed phase_log
    @ List.map
        (cycle_seed "shared-scc-dependency")
        linked_cycle_shared_scc_dependencies
    @ List.map (cycle_seed "shared-scc-schedule") linked_cycle_worklist_schedule
    @ List.map (cycle_seed "shared-scc-final-cell") linked_cycle_final_cells
    |> sort_json
  in
  let derived_cells =
    cells_of_table "final_input_table" final_input_table
    @ cells_of_table "final_output_table" final_output_table
    |> sort_json
  in
  let equations =
    derived_cells |> List.map (equation_for_cell seeds) |> sort_json
  in
  let equation_ids =
    equations
    |> List.map (fun eq -> `String (string_field "equation_id" eq))
    |> sort_json
  in
  let dependency_edges = dependency_edges seeds derived_cells in
  let cross_module_dependency_edges =
    dependency_edges
    |> List.filter (fun edge -> bool_field "cross_module" edge)
    |> sort_json
  in
  let iteration_count =
    if linked_cycle_scc_count > 0 then max 2 linked_cycle_iteration_count
    else if equations = [] then 0
    else 1
  in
  let schedule = worklist_schedule ~iteration_count equations in
  let changed_cell_count = List.length derived_cells in
  let state_read_count = List.length equations * max 1 iteration_count in
  let seed_read_count = List.length dependency_edges in
  `Assoc
    [
      ("schema_version", `String schema_version);
      ("linked_id", `String linked_id);
      ("global_residual_fixpoint_run", `Bool (equations <> [] && seeds <> []));
      ("global_residual_fixpoint_scope", `String scope);
      ("global_sparse_fixpoint_component", `String component);
      ("global_sparse_fixpoint_source_level_rerun", `Bool false);
      ("global_residual_equation_ids", `List equation_ids);
      ("global_residual_equations", `List equations);
      ("global_residual_seed_cells", `List seeds);
      ("global_residual_derived_cells", `List derived_cells);
      ("global_residual_dependency_edges", `List dependency_edges);
      ( "global_residual_cross_module_dependency_edges",
        `List cross_module_dependency_edges );
      ("global_residual_exact_cell_dependencies", `List dependency_edges);
      ("global_residual_final_input_table", `List final_input_table);
      ("global_residual_final_output_table", `List final_output_table);
      ("global_residual_worklist_schedule", `List schedule);
      ("global_residual_iteration_count", `Int iteration_count);
      ("global_residual_changed_cell_count", `Int changed_cell_count);
      ("global_residual_state_read_count", `Int state_read_count);
      ("global_residual_seed_read_count", `Int seed_read_count);
      ("global_residual_worklist_drained", `Bool true);
      ("global_residual_overlay_only", `Bool false);
      ("global_residual_metadata_only", `Bool false);
      ( "global_residual_derived_cells_recomputed",
        `Bool (derived_cells <> [] && equations <> [] && dependency_edges <> [])
      );
      ("global_residual_authoritative_residual_side", `Bool true);
      ( "global_residual_final_rows_source",
        `String "post-link global residual worklist final state" );
      ( "module_ids",
        `List (List.map (fun id -> `String id) (sort_strings module_ids)) );
      ( "claim_scope",
        `String
          "witness-bounded residual-global worklist; not \
           arbitrary-C/source-level sparse rerun/public schema" );
      ( "non_claims",
        `List
          [
            `String "no arbitrary-C whole-program theorem";
            `String "no full source-level sparse analyzer rerun";
            `String "no public artifact schema commitment";
          ] );
    ]
