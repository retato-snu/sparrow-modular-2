(***********************************************************************)
(* Typed MetaOCaml residual analyzer construction.                      *)
(***********************************************************************)

module T = Abstract_speculate_stage_types
module Stage2 = Abstract_speculate_stage2_input
module Lift = Abstract_speculate_lift
module Blind = Abstract_speculate_blind_equal

let make_component
    ~module_id
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
    ?expression_code
    ?guard_code
    ?(semantic_expression = transfer_event)
    ?(semantic_guard = transfer_event)
    () =
  let residual_id =
    "residual-component:" ^ module_id ^ ":" ^ table ^ ":" ^ node ^ ":" ^ location ^ ":" ^ string_of_int ordinal
  in
  let id_c = Lift.lift_string residual_id in
  let source_file_c = Lift.lift_string source_file in
  let source_hash_c = Lift.lift_string source_hash in
  let table_c = Lift.lift_string table in
  let node_c = Lift.lift_string node in
  let location_c = Lift.lift_string location in
  let ordinal_c = .<ordinal>. in
  let row_c = Lift.lift_json default_row in
  let component_c = Lift.lift_json default_component in
  let component_kind_c = Lift.lift_string component_kind in
  let transfer_event_c = Lift.lift_string transfer_event in
  let lattice_event_c = Lift.lift_string lattice_event in
  let shape_c = Lift.lift_string shape in
  let witness_c = Lift.lift_string witness_constructor in
  let semantic_expression_c = Lift.lift_string semantic_expression in
  let semantic_guard_c = Lift.lift_string semantic_guard in
  let expression_code =
    match expression_code with
    | Some code -> code
    | None -> .<fun _ -> 0>.
  in
  let guard_code =
    match guard_code with
    | Some code -> code
    | None -> .<fun _ -> true>.
  in
  let component_code =
    .<fun input ->
      Stage2.execute_staged_component
        ~id:.~id_c
        ~source_file:.~source_file_c
        ~source_hash:.~source_hash_c
        ~table:.~table_c
        ~node:.~node_c
        ~location:.~location_c
        ~ordinal:.~ordinal_c
        ~default_row:.~row_c
        ~default_component:.~component_c
        ~component_kind:.~component_kind_c
        ~transfer_event:.~transfer_event_c
        ~lattice_event:.~lattice_event_c
        ~shape:.~shape_c
        ~witness_constructor:.~witness_c
        ~semantic_expression:.~semantic_expression_c
        ~semantic_guard:.~semantic_guard_c
        ~residual_arithmetic_value:(.~expression_code input)
        ~residual_guard_value:(.~guard_code input)
        input>.
  in
  {
    T.residual_id;
    table;
    node;
    location;
    component_kind;
    transfer_event;
    lattice_event;
    shape;
    witness_constructor;
    default_row;
    default_component;
    expression_code;
    guard_code;
    component_code;
  }

let rec residual_equations components =
  match components with
  | [] -> .<[]>.
  | component :: rest ->
      let equation_id_c = Lift.lift_string ("residual-equation:" ^ component.T.residual_id) in
      let table_c = Lift.lift_string component.T.table in
      let node_c = Lift.lift_string component.T.node in
      let location_c = Lift.lift_string component.T.location in
      let dependency_id =
        {
          T.cell_table = component.T.table;
          cell_node = component.T.node;
          cell_location = component.T.location;
        }
      in
      let dependency_key = T.residual_cell_key dependency_id in
      let dependencies_c = Lift.lift_string_list [dependency_key] in
      let apply = component.T.component_code in
      let tail = residual_equations rest in
      .<T.make_residual_equation
          ~equation_id:.~equation_id_c
          ~target_table:.~table_c
          ~target_node:.~node_c
          ~target_location:.~location_c
          ~dependencies:.~dependencies_c
          ~apply:(fun state input ->
            ignore (T.residual_state_lookup state
              (T.make_residual_cell_id
                ~cell_table:.~table_c
                ~cell_node:.~node_c
                ~cell_location:.~location_c));
            .~apply input)
        :: .~tail>.

let blind_witness ~static_input_rows ~static_output_rows ~residual_input_components ~residual_output_components ~control_components =
  let static_rows = static_input_rows @ static_output_rows in
  let components = residual_input_components @ residual_output_components @ control_components in
  let residual_values = List.map (fun component -> T.D component.T.component_code) components in
  let left = { Blind.static_rows; residual_values } in
  let right = { Blind.static_rows; residual_values = residual_values @ residual_values } in
  let witness = Blind.convergence_witness left right in
  if not (Blind.equal_static_projection left right) then
    failwith "static projection changed while growing residual component code";
  witness

let make_code
    ~schema_version
    ~module_id
    ~source_file
    ~source_hash
    ~extern_roots
    ~static_input_rows
    ~static_output_rows
    ~residual_input_components
    ~residual_output_components
    ~control_components
    ~shape_witnesses
    ~blind_equality_witness =
  let schema_version_c = Lift.lift_string schema_version in
  let module_id_c = Lift.lift_string module_id in
  let source_file_c = Lift.lift_string source_file in
  let source_hash_c = Lift.lift_string source_hash in
  let extern_roots_c = Lift.lift_string_list extern_roots in
  let static_input_rows_c = Lift.lift_json_list static_input_rows in
  let static_output_rows_c = Lift.lift_json_list static_output_rows in
  let shape_witnesses_json_c = Lift.lift_json_list (T.shape_witness_json_list shape_witnesses) in
  let blind_equality_witness_c = Lift.lift_json blind_equality_witness in
  .<fun input ->
    let residual_input_equations = .~(residual_equations residual_input_components) in
    let residual_output_equations = .~(residual_equations residual_output_components) in
    let control_equations = .~(residual_equations control_components) in
    Stage2.run_summary
      ~schema_version:.~schema_version_c
      ~module_id:.~module_id_c
      ~source_file:.~source_file_c
      ~source_hash:.~source_hash_c
      ~extern_roots:.~extern_roots_c
      ~static_input_rows:.~static_input_rows_c
      ~static_output_rows:.~static_output_rows_c
      ~residual_input_equations
      ~residual_output_equations
      ~control_equations
      ~shape_witnesses_json:.~shape_witnesses_json_c
      ~blind_equality_witness:.~blind_equality_witness_c
      input>.

let make_analyzer
    ~schema_version
    ~module_id
    ~source_file
    ~source_hash
    ~extern_roots
    ~static_input_rows
    ~static_output_rows
    ~residual_input_components
    ~residual_output_components
    ~control_components
    ~shape_witnesses =
  let blind_equality_witness =
    blind_witness
      ~static_input_rows
      ~static_output_rows
      ~residual_input_components
      ~residual_output_components
      ~control_components
  in
  let code =
    make_code
      ~schema_version
      ~module_id
      ~source_file
      ~source_hash
      ~extern_roots
      ~static_input_rows
      ~static_output_rows
      ~residual_input_components
      ~residual_output_components
      ~control_components
      ~shape_witnesses
      ~blind_equality_witness
  in
  {
    T.module_id;
    source_file;
    source_hash;
    code;
    shape_witnesses;
    static_input_rows;
    static_output_rows;
    residual_input_components;
    residual_output_components;
    control_components;
    blind_equality_witness;
  }

let execute analyzer input = Runcode.run analyzer.T.code input
