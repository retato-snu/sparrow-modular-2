(***********************************************************************)
(* Separately compiled staged types for Abstract Speculate PE.          *)
(***********************************************************************)

type stage2_input = {
  extern_effects : Yojson.Safe.t;
}

type residual_component_result = {
  row : Yojson.Safe.t;
  execution : Yojson.Safe.t;
}

type 'a ps =
  | S of 'a
  | D of 'a Trx.code

type residual_cell_id = {
  cell_table : string;
  cell_node : string;
  cell_location : string;
}

let make_residual_cell_id ~cell_table ~cell_node ~cell_location =
  { cell_table; cell_node; cell_location }

let residual_cell_key cell_id =
  cell_id.cell_table ^ ":" ^ cell_id.cell_node ^ ":" ^ cell_id.cell_location

type staged_residual_component = {
  residual_id : string;
  table : string;
  node : string;
  location : string;
  component_kind : string;
  transfer_event : string;
  lattice_event : string;
  shape : string;
  witness_constructor : string;
  default_row : Yojson.Safe.t;
  default_component : Yojson.Safe.t;
  residual_dependencies : residual_cell_id list;
  derive_from_first_dependency : bool;
  expression_code : (stage2_input -> int) Trx.code;
  guard_code : (stage2_input -> bool) Trx.code;
  component_code : (stage2_input -> residual_component_result) Trx.code;
}

type residual_value = Yojson.Safe.t

type residual_cell = {
  cell_id : residual_cell_id;
  value : residual_value;
}

type residual_state_view = {
  lookup : residual_cell_id -> residual_cell option;
  read_int : residual_cell_id -> int option;
}

let residual_state_lookup state cell_id = state.lookup cell_id
let residual_state_read_int state cell_id = state.read_int cell_id

type residual_equation = {
  equation_id : string;
  target_table : string;
  target_node : string;
  target_location : string;
  dependencies : string list;
  apply : residual_state_view -> stage2_input -> residual_component_result;
}

type residual_dependency = {
  source : string;
  target : string;
}

type solver_event = Yojson.Safe.t

type solver_result = {
  solver_final_rows : Yojson.Safe.t list;
  solver_events : solver_event list;
  solver_log : Yojson.Safe.t;
}

let make_residual_equation
    ~equation_id
    ~target_table
    ~target_node
    ~target_location
    ~dependencies
    ~apply =
  {
    equation_id;
    target_table;
    target_node;
    target_location;
    dependencies;
    apply;
  }

type shape_witness =
  | Typed_component of staged_residual_component
  | Loop_shape of string * string * shape_witness list
  | Branch_shape of string * string * shape_witness list * shape_witness list
  | Expr_shape of string * shape_witness list

type stage2_output = {
  final_input_table : Yojson.Safe.t list;
  final_output_table : Yojson.Safe.t list;
  execution_log : Yojson.Safe.t;
  shape_witnesses : Yojson.Safe.t list;
}

type residual_analyzer = {
  module_id : string;
  source_file : string;
  source_hash : string;
  code : (stage2_input -> stage2_output) Trx.code;
  shape_witnesses : shape_witness list;
  static_input_rows : Yojson.Safe.t list;
  static_output_rows : Yojson.Safe.t list;
  residual_input_components : staged_residual_component list;
  residual_output_components : staged_residual_component list;
  control_components : staged_residual_component list;
  blind_equality_witness : Yojson.Safe.t;
}

let residual_dependency_keys component =
  List.map residual_cell_key component.residual_dependencies

let component_to_yojson component =
  `Assoc [
    "id", `String component.residual_id;
    "table", `String component.table;
    "node", `String component.node;
    "location", `String component.location;
    "component_kind", `String component.component_kind;
    "transfer_event", `String component.transfer_event;
    "lattice_event", `String component.lattice_event;
    "shape", `String component.shape;
    "witness_constructor", `String component.witness_constructor;
    "code_kind", `String "Trx.code";
    "typed_code_present", `Bool true;
    "granularity", `String "cell-component";
    "row_wrapper", `Bool false;
    "residual_dependencies", `List (List.map (fun dep -> `String dep) (residual_dependency_keys component));
    "derive_from_first_dependency", `Bool component.derive_from_first_dependency;
    "default_component", component.default_component;
    "default_row", component.default_row;
  ]

let rec shape_witness_to_yojson = function
  | Typed_component component ->
      `Assoc [
        "kind", `String "typed-component";
        "id", `String component.residual_id;
        "table", `String component.table;
        "node", `String component.node;
        "location", `String component.location;
        "component_kind", `String component.component_kind;
        "shape", `String component.shape;
        "code_kind", `String "Trx.code";
        "typed_code_present", `Bool true;
        "granularity", `String "cell-component";
        "paired_executable_code", `Bool true;
        "row_wrapper", `Bool false;
        "residual_dependencies", `List (List.map (fun dep -> `String dep) (residual_dependency_keys component));
        "derive_from_first_dependency", `Bool component.derive_from_first_dependency;
      ]
  | Loop_shape (id, guard_kind, body) ->
      `Assoc [
        "kind", `String "loop";
        "id", `String id;
        "guard_kind", `String guard_kind;
        "body", `List (List.map shape_witness_to_yojson body);
      ]
  | Branch_shape (id, condition_kind, then_w, else_w) ->
      `Assoc [
        "kind", `String "if";
        "id", `String id;
        "condition_kind", `String condition_kind;
        "then_witness", `List (List.map shape_witness_to_yojson then_w);
        "else_witness", `List (List.map shape_witness_to_yojson else_w);
      ]
  | Expr_shape (id, children) ->
      `Assoc [
        "kind", `String "expr";
        "id", `String id;
        "children", `List (List.map shape_witness_to_yojson children);
      ]

let shape_witnesses_to_yojson witnesses =
  `List (List.map shape_witness_to_yojson witnesses)

let shape_witness_json_list witnesses =
  List.map shape_witness_to_yojson witnesses

let rec has_typed_code = function
  | Typed_component _ -> true
  | Loop_shape (_, _, body) | Expr_shape (_, body) -> List.exists has_typed_code body
  | Branch_shape (_, _, then_w, else_w) -> List.exists has_typed_code (then_w @ else_w)

let rec shape_kind = function
  | Typed_component _ -> "typed-component"
  | Loop_shape _ -> "loop"
  | Branch_shape _ -> "if"
  | Expr_shape _ -> "expr"

let witness_status witnesses =
  if List.for_all has_typed_code witnesses then "pass" else "fail"

(***********************************************************************)
(* Testable staged-domain lattice helpers.                              *)
(***********************************************************************)

type staged_cell = {
  cell_location : string;
  abstract_value : string;
  semantic_source : string;
  cell_value : (stage2_input -> int) ps;
}

type staged_lattice_event = {
  op : string;
  location : string;
  before_stage : string;
  after_stage : string;
  typed_code_present : bool;
  approximated_to_top : bool;
}

let stage_of_cell cell = match cell.cell_value with S _ -> "S" | D _ -> "D"
let typed_code_present cell = match cell.cell_value with S _ -> false | D _ -> true
let dynamic_code cell = match cell.cell_value with D code -> Some code | S _ -> None

let lattice_event op before after = {
  op;
  location = after.cell_location;
  before_stage = stage_of_cell before;
  after_stage = stage_of_cell after;
  typed_code_present = typed_code_present after;
  approximated_to_top = false;
}

let make_static_cell ~location ~abstract_value ~semantic_source ~ordinal =
  { cell_location = location; abstract_value; semantic_source; cell_value = S (fun _ -> ordinal) }

let make_dynamic_cell ~location ~abstract_value ~semantic_source ~code =
  { cell_location = location; abstract_value; semantic_source; cell_value = D code }

let staged_order left right =
  match left.cell_value, right.cell_value with
  | S _, S _ -> left.abstract_value <= right.abstract_value
  | D _, D _ -> true
  | D _, S _ -> false
  | S _, D _ -> false

let preserve_dynamic left right =
  match left.cell_value, right.cell_value with
  | D code, _ -> D code
  | _, D code -> D code
  | S x, S _ -> S x

let staged_join left right =
  let joined = {
    cell_location = left.cell_location;
    abstract_value =
      if typed_code_present left || typed_code_present right then
        "join(" ^ left.abstract_value ^ "," ^ right.abstract_value ^ ")"
      else left.abstract_value ^ " ⊔ " ^ right.abstract_value;
    semantic_source = "staged-join(" ^ left.semantic_source ^ "," ^ right.semantic_source ^ ")";
    cell_value = preserve_dynamic left right;
  } in
  joined, lattice_event "join" left joined

let staged_widen old_cell new_cell =
  let widened = {
    cell_location = old_cell.cell_location;
    abstract_value = "widen(" ^ old_cell.abstract_value ^ "," ^ new_cell.abstract_value ^ ")";
    semantic_source = "staged-widen(" ^ old_cell.semantic_source ^ "," ^ new_cell.semantic_source ^ ")";
    cell_value = preserve_dynamic old_cell new_cell;
  } in
  widened, lattice_event "widen" old_cell widened

let staged_narrow old_cell new_cell =
  let narrowed = {
    cell_location = old_cell.cell_location;
    abstract_value = "narrow(" ^ old_cell.abstract_value ^ "," ^ new_cell.abstract_value ^ ")";
    semantic_source = "staged-narrow(" ^ old_cell.semantic_source ^ "," ^ new_cell.semantic_source ^ ")";
    cell_value = preserve_dynamic old_cell new_cell;
  } in
  narrowed, lattice_event "narrow" old_cell narrowed

let staged_unstable old_cell new_cell =
  (not (staged_order new_cell old_cell)), lattice_event "unstable" old_cell new_cell

let staged_propagate cell =
  { cell with semantic_source = "staged-propagate(" ^ cell.semantic_source ^ ")" },
  lattice_event "propagate" cell cell

let staged_lattice_event_to_yojson event =
  `Assoc [
    "op", `String event.op;
    "location", `String event.location;
    "before_stage", `String event.before_stage;
    "after_stage", `String event.after_stage;
    "typed_code_present", `Bool event.typed_code_present;
    "approximated_to_top", `Bool event.approximated_to_top;
  ]
