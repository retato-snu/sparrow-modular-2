module StageT = Sparrow_modular_ocaml.Abstract_speculate_stage_types
module Stage2 = Sparrow_modular_ocaml.Abstract_speculate_stage2_input
module Solver = Sparrow_modular_ocaml.Abstract_speculate_residual_solver

let expect cond msg = if not cond then failwith msg

let member = Yojson.Safe.Util.member

let int_field name json =
  match member name json with
  | `Int n -> n
  | _ -> 0

let bool_field name json =
  match member name json with
  | `Bool b -> b
  | _ -> false

let list_field name json =
  match member name json with
  | `List xs -> xs
  | _ -> []

let string_field name json =
  match member name json with
  | `String s -> s
  | _ -> ""

let cell ~location ~value =
  `Assoc [
    "location", `String location;
    "value", `String (string_of_int value);
    "normalized_value", `String (string_of_int value);
  ]

let row ~node ~location ~value =
  `Assoc [
    "node", `String node;
    "memory", `List [cell ~location ~value];
  ]

let result ~id ~node ~location ~value ~uses_stage2_dynamic_input =
  {
    StageT.row = row ~node ~location ~value;
    execution = `Assoc [
      "id", `String id;
      "node", `String node;
      "location", `String location;
      "value", `Int value;
      "uses_stage2_dynamic_input", `Bool uses_stage2_dynamic_input;
      "component_kind", `String "solver-unit-equation";
      "transfer_event", `String (id ^ ":transfer");
      "lattice_event", `String "unit-lattice";
    ];
  }

let cell_id ~node ~location =
  {
    StageT.cell_table = "output";
    cell_node = node;
    cell_location = location;
  }

let equation ~id ~target_node ~target_location ~dependencies ~uses_stage2_dynamic_input ~eval =
  StageT.make_residual_equation
    ~equation_id:id
    ~target_table:"output"
    ~target_node
    ~target_location
    ~dependencies
    ~apply:(fun state input ->
      result ~id ~node:target_node ~location:target_location ~uses_stage2_dynamic_input
        ~value:(eval state input))

let extern_effects value =
  `Assoc [
    "schema_version", `String "abstract-speculate-extern-effects/v2";
    "source_file", `String "solver-unit.c";
    "source_hash", `String "solver-unit-hash";
    "extern_roots", `List [`String "n"];
    "row_overrides", `List [];
    "effects", `List [`Assoc [
      "node", `String "n";
      "reason", `String "solver-unit-seed";
      "value", `Int value;
      "stage2_obligation", `String "unit dynamic seed";
    ]];
  ]

let cell_value location rows =
  rows
  |> List.find_map (fun row ->
       match member "memory" row with
       | `List cells ->
           cells |> List.find_map (fun cell ->
             if string_field "location" cell = location then Some (string_field "value" cell) else None)
       | _ -> None)

let schedule_has reason log =
  list_field "worklist_schedule" log
  |> List.exists (fun event -> string_field "reason" event = reason)

let execution_for id solved =
  solved.Solver.executions
  |> List.find (fun execution -> string_field "residual_equation_id" execution = id)

let () =
  let input = { StageT.extern_effects = extern_effects 7 } in
  let n input = Stage2.extern_int ~node:"n" ~location:"n" input in
  let x_cell = cell_id ~node:"x" ~location:"x" in
  let y_cell = cell_id ~node:"y" ~location:"y" in
  let x_key = StageT.residual_cell_key x_cell in
  let y_key = StageT.residual_cell_key y_cell in
  let read_required_int state cell_id label =
    match state.StageT.read_int cell_id with
    | Some n -> n
    | None -> failwith ("missing solver state cell: " ^ label)
  in
  let equations = [
    equation ~id:"x=n" ~target_node:"x" ~target_location:"x" ~dependencies:[]
      ~uses_stage2_dynamic_input:true
      ~eval:(fun _state input -> n input);
    equation ~id:"y=x+1" ~target_node:"y" ~target_location:"y" ~dependencies:[x_key]
      ~uses_stage2_dynamic_input:false
      ~eval:(fun state _input -> read_required_int state x_cell "x" + 1);
    equation ~id:"ret=y" ~target_node:"ret" ~target_location:"ret" ~dependencies:[y_key]
      ~uses_stage2_dynamic_input:false
      ~eval:(fun state _input -> read_required_int state y_cell "y");
  ] in
  let solved = Solver.solve ~input ~static_rows:[] ~equations in
  expect (cell_value "x" solved.Solver.final_rows = Some "7") "x did not read seed n=7";
  expect (cell_value "y" solved.Solver.final_rows = Some "8") "y did not compute x+1 evidence";
  expect (cell_value "ret" solved.Solver.final_rows = Some "8") "ret did not receive propagated value";
  expect (bool_field "residual_solver_run" solved.Solver.solver_log) "solver did not run";
  expect (bool_field "worklist_drained" solved.Solver.solver_log) "worklist did not drain";
  expect (int_field "residual_equation_count" solved.Solver.solver_log = 3) "wrong equation count";
  expect (int_field "changed_cell_count" solved.Solver.solver_log >= 3) "dependent cells did not change";
  expect (int_field "solver_iteration_count" solved.Solver.solver_log > 3) "solver did not perform propagation iterations";
  expect (int_field "state_read_count" solved.Solver.solver_log > 0) "dependent equations did not read solver state";
  expect (int_field "seed_input_read_count" solved.Solver.solver_log > 0) "seed equation did not read dynamic input";
  expect (bool_field "equation_apply_reads_solver_state" solved.Solver.solver_log) "solver log did not record state-reading equations";
  expect (List.mem (`String x_key) (list_field "exact_cell_dependencies" solved.Solver.solver_log)) "missing exact x dependency";
  expect (List.mem (`String y_key) (list_field "exact_cell_dependencies" solved.Solver.solver_log)) "missing exact y dependency";
  let y_execution = execution_for "y=x+1" solved in
  let ret_execution = execution_for "ret=y" solved in
  expect (bool_field "equation_apply_reads_solver_state" y_execution) "y equation did not read solver state";
  expect (bool_field "equation_apply_reads_solver_state" ret_execution) "ret equation did not read solver state";
  expect (not (bool_field "seed_input_read" y_execution)) "y equation read stage2 input directly";
  expect (not (bool_field "seed_input_read" ret_execution)) "ret equation read stage2 input directly";
  expect (schedule_has "changed-cell-dependent" solved.Solver.solver_log) "no dependent enqueue evidence";
  print_endline "abstract_speculate_residual_solver_unit: PASS"
