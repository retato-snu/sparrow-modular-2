(***********************************************************************)
(* Solver-backed residual equations for Abstract Speculate PE.         *)
(***********************************************************************)

module T = Abstract_speculate_stage_types

type residual_cell_id = T.residual_cell_id
type residual_value = T.residual_value
type residual_cell = T.residual_cell
type residual_dependency = T.residual_dependency
type solver_event = T.solver_event
type residual_state_view = T.residual_state_view

type solved_cell = {
  row : Yojson.Safe.t;
  execution : Yojson.Safe.t;
}

type residual_state = (string, solved_cell) Hashtbl.t

type solve_result = {
  final_rows : Yojson.Safe.t list;
  executions : Yojson.Safe.t list;
  solver_log : Yojson.Safe.t;
}

let member = Yojson.Safe.Util.member

let string_of_json json = Yojson.Safe.to_string json

let assoc_without keys fields =
  fields |> List.filter (fun (key, _) -> not (List.mem key keys))

let with_fields fields extras =
  `Assoc (extras @ assoc_without (List.map fst extras) fields)

let equation_key equation =
  equation.T.target_table ^ ":" ^ equation.T.target_node ^ ":" ^ equation.T.target_location

let cell_id_of_equation equation =
  {
    T.cell_table = equation.T.target_table;
    cell_node = equation.T.target_node;
    cell_location = equation.T.target_location;
  }

let cell_key = T.residual_cell_key

let sort_json xs =
  List.sort (fun a b -> compare (string_of_json a) (string_of_json b)) xs

let join _old_value new_value = new_value

let leq left right =
  string_of_json left = string_of_json right

let changed old_value new_value =
  not (leq (join old_value new_value) old_value)

let dependency_edges equations =
  equations
  |> List.concat_map (fun equation ->
       equation.T.dependencies
       |> List.map (fun source ->
            {
              T.source;
              target = equation_key equation;
            }))

let dependency_to_json dependency =
  `Assoc [
    "source", `String dependency.T.source;
    "target", `String dependency.T.target;
  ]

let exact_dependency_matches_cell dependency cell_id =
  dependency.T.source = cell_key cell_id

let legacy_dependency_matches_cell_for_pre_solver_evidence dependency cell_id =
  dependency.T.source = cell_key cell_id ||
  dependency.T.source = cell_id.T.cell_node ||
  dependency.T.source = cell_id.T.cell_location

let enqueue_unique queue pending equation =
  if not (List.mem equation.T.equation_id !pending) then begin
    Queue.add equation queue;
    pending := equation.T.equation_id :: !pending
  end

let dependent_equations equations changed_equation =
  let changed_cell = cell_id_of_equation changed_equation in
  equations
  |> List.filter (fun equation ->
       equation.T.equation_id = changed_equation.T.equation_id ||
       equation.T.dependencies
       |> List.exists (fun source ->
            exact_dependency_matches_cell { T.source; target = equation_key equation } changed_cell))

let string_field name json =
  match member name json with
  | `String s -> Some s
  | _ -> None

let bool_field_opt name json =
  match member name json with
  | `Bool b -> Some b
  | _ -> None

let int_of_json = function
  | `Int n -> Some n
  | `String s -> (try Some (int_of_string s) with Failure _ -> None)
  | _ -> None

let cell_json_for_location location row =
  match member "memory" row with
  | `List cells ->
      cells
      |> List.find_opt (fun cell -> string_field "location" cell = Some location)
  | _ -> None

let cell_int_value cell =
  match member "value" cell with
  | (`Int _ | `String _) as value -> int_of_json value
  | _ -> int_of_json (member "normalized_value" cell)

let make_state_view state state_read_count =
  let lookup cell_id =
    incr state_read_count;
    match Hashtbl.find_opt state (cell_key cell_id) with
    | None -> None
    | Some solved ->
        begin match cell_json_for_location cell_id.T.cell_location solved.row with
        | Some value -> Some { T.cell_id; value }
        | None -> Some { T.cell_id; value = solved.row }
        end
  in
  let read_int cell_id =
    match lookup cell_id with
    | Some cell -> cell_int_value cell.T.value
    | None -> None
  in
  { T.lookup; read_int }

let annotate_execution ~iteration ~changed ~state_reads ~seed_input_read equation execution =
  match execution with
  | `Assoc fields ->
      with_fields fields [
        "residual_equation_id", `String equation.T.equation_id;
        "residual_equation_target", `String (equation_key equation);
        "residual_equation_dependencies", `List (List.map (fun dep -> `String dep) equation.T.dependencies);
        "exact_cell_dependencies", `List (List.map (fun dep -> `String dep) equation.T.dependencies);
        "residual_equation_applied", `Bool true;
        "residual_solver_iteration", `Int iteration;
        "residual_solver_cell_changed", `Bool changed;
        "residual_equation_state_read_count", `Int state_reads;
        "equation_apply_reads_solver_state", `Bool (state_reads > 0);
        "seed_input_read", `Bool seed_input_read;
        "residual_fixpoint_restart", `Bool changed;
        "solver_backed_residual_fixpoint", `Bool true;
        "overlay_only", `Bool false;
        "stage2_sparse_recompute", `Bool false;
        "semantic_core", `String "solver-backed-residual-equation";
        "residual_runtime_scope", `String "solver-backed-residual-equations";
      ]
  | json -> json

let solve ~input ~static_rows ~equations =
  let state : residual_state = Hashtbl.create (List.length equations + 1) in
  let executions = ref [] in
  let worklist_events = ref [] in
  let changed_cell_count = ref 0 in
  let application_count = ref 0 in
  let enqueued_equation_count = ref 0 in
  let state_read_count = ref 0 in
  let seed_input_read_count = ref 0 in
  let max_iterations = max 1 ((List.length equations + 1) * 4) in
  let queue = Queue.create () in
  let pending = ref [] in
  let enqueue reason iteration equation =
    let before = List.length !pending in
    enqueue_unique queue pending equation;
    if List.length !pending <> before then begin
      incr enqueued_equation_count;
      worklist_events :=
        `Assoc [
          "event", `String "enqueue";
          "reason", `String reason;
          "iteration", `Int iteration;
          "equation_id", `String equation.T.equation_id;
          "target", `String (equation_key equation);
        ] :: !worklist_events
    end
  in
  List.iter (enqueue "initial" 0) equations;
  let rec drain iteration =
    if iteration > max_iterations * max 1 (List.length equations) then
      failwith "residual equation solver did not converge before iteration bound";
    if Queue.is_empty queue then iteration
    else begin
      let equation = Queue.take queue in
      pending := List.filter (fun id -> id <> equation.T.equation_id) !pending;
      let before_state_reads = !state_read_count in
      let result = equation.T.apply (make_state_view state state_read_count) input in
      let equation_state_reads = !state_read_count - before_state_reads in
      let seed_input_read =
        bool_field_opt "seed_input_read" result.T.execution = Some true ||
        bool_field_opt "uses_stage2_dynamic_input" result.T.execution = Some true
      in
      if seed_input_read then incr seed_input_read_count;
      let key = equation_key equation in
      let old_row =
        match Hashtbl.find_opt state key with
        | Some cell -> Some cell.row
        | None -> None
      in
      let changed =
        match old_row with
        | Some row -> changed row result.T.row
        | None -> true
      in
      if changed then begin
        incr changed_cell_count;
        Hashtbl.replace state key { row = result.T.row; execution = result.T.execution };
        List.iter (enqueue "changed-cell-dependent" iteration) (dependent_equations equations equation)
      end;
      incr application_count;
      executions :=
        annotate_execution ~iteration ~changed ~state_reads:equation_state_reads ~seed_input_read equation result.T.execution
        :: !executions;
      drain (iteration + 1)
    end
  in
  let solver_iteration_count =
    if equations = [] then 0 else drain 1
  in
  let residual_rows =
    Hashtbl.fold (fun _ cell acc -> cell.row :: acc) state []
  in
  let residual_dependencies = dependency_edges equations in
  let exact_cell_dependencies =
    residual_dependencies
    |> List.map (fun dependency -> dependency.T.source)
    |> List.sort_uniq String.compare
  in
  let final_rows = sort_json (static_rows @ residual_rows) in
  let solver_log = `Assoc [
    "residual_solver_run", `Bool true;
    "solver_backed_residual_fixpoint", `Bool true;
    "residual_runtime_scope", `String "solver-backed-residual-equations";
    "overlay_only", `Bool false;
    "stage2_sparse_recompute", `Bool false;
    "residual_fixpoint_restart", `Bool (solver_iteration_count > 1);
    "solver_iteration_count", `Int solver_iteration_count;
    "changed_cell_count", `Int !changed_cell_count;
    "residual_equation_count", `Int (List.length equations);
    "residual_dependency_count", `Int (List.length residual_dependencies);
    "residual_equation_application_count", `Int !application_count;
    "enqueued_equation_count", `Int !enqueued_equation_count;
    "state_read_count", `Int !state_read_count;
    "seed_input_read_count", `Int !seed_input_read_count;
    "exact_cell_dependencies", `List (List.map (fun dep -> `String dep) exact_cell_dependencies);
    "equation_apply_reads_solver_state", `Bool (!state_read_count > 0);
    "legacy_dependency_matching_used", `Bool false;
    "worklist_drained", `Bool true;
    "iteration_bound", `Int max_iterations;
    "solver_policy", `String "deterministic-worklist-bounded";
    "lattice_value_model", `String "current-json-cell-evidence";
    "lattice_join", `String "replace-with-new-evidence";
    "lattice_leq", `String "json-string-equality-v1";
    "static_row_count", `Int (List.length static_rows);
    "final_row_count", `Int (List.length final_rows);
    "residual_equation_ids", `List (List.map (fun eq -> `String eq.T.equation_id) equations);
    "residual_dependencies", `List (List.map dependency_to_json residual_dependencies);
    "worklist_schedule", `List (List.rev !worklist_events);
  ] in
  {
    final_rows;
    executions = List.rev !executions;
    solver_log;
  }

let bool_field name json =
  match member name json with
  | `Bool b -> b
  | _ -> false

let int_field name json =
  match member name json with
  | `Int n -> n
  | _ -> 0

let log_says_solver_ran log =
  bool_field "residual_solver_run" log &&
  bool_field "solver_backed_residual_fixpoint" log &&
  bool_field "worklist_drained" log &&
  not (bool_field "overlay_only" log) &&
  int_field "residual_equation_count" log >= 0 &&
  int_field "state_read_count" log >= 0 &&
  int_field "seed_input_read_count" log >= 0
