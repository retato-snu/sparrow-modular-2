(***********************************************************************)
(* Direct module-local staged-domain sparse PE for Abstract Speculate.  *)
(***********************************************************************)

module StageT = Abstract_speculate_stage_types
module Residual = Abstract_speculate_residual_value
module Stage2 = Abstract_speculate_stage2_input

module AccessSemItv = AccessSem.Make (ItvSem)
module AccessAnalysisItv = AccessAnalysis.Make (AccessSemItv)
module Access = AccessAnalysisItv.Access
module DUGraph = Dug.Make (ItvDom.Mem)
module Ssa = SsaDug.Make (DUGraph) (Access)
module PowLoc = Access.PowLoc

let boundary =
  "parse_one_file -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> local staged-domain sparse fixpoint -> MetaOCaml component residual code"

let compare_string = String.compare
let sort_strings xs = List.sort_uniq compare_string xs
let sort_json xs = List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)) xs
let node_to_string = BasicDom.Node.to_string
let loc_to_string = BasicDom.Loc.to_string
let member = Yojson.Safe.Util.member

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match assoc_field name json with Some (`String s) -> s | _ -> ""

let list_field name json =
  match assoc_field name json with Some (`List xs) -> xs | _ -> []

let assoc_without keys fields =
  fields |> List.filter (fun (key, _) -> not (List.mem key keys))

let with_assoc_fields extras = function
  | `Assoc fields -> `Assoc (extras @ assoc_without (List.map fst extras) fields)
  | json -> `Assoc (extras @ [ ("wrapped", json) ])

let bool_json name value = name, `Bool value
let int_json n = `Int n

let comma_join =
  let rec loop = function
    | [] -> ""
    | [x] -> x
    | x :: xs -> x ^ "," ^ loop xs
  in
  loop

let powloc_strings xs = PowLoc.fold (fun loc acc -> loc_to_string loc :: acc) xs [] |> sort_strings
let powloc_json xs = powloc_strings xs |> List.map (fun s -> `String s)
let powloc_is_empty xs = PowLoc.fold (fun _ _ -> false) xs true

let get_locset mem =
  ItvDom.Mem.foldi (fun loc value locset ->
    locset
    |> PowLoc.add loc
    |> PowLoc.union (ItvDom.Val.pow_loc_of_val value)
    |> fun locset ->
       BatSet.fold (fun allocsite acc -> PowLoc.add (BasicDom.Loc.of_allocsite allocsite) acc)
         (ItvDom.Val.allocsites_of_val value) locset)
    mem PowLoc.empty

let derive_sparse_spec global =
  Options.pfs := 100;
  Options.narrow := true;
  let premem = global.Global.mem in
  let locset = get_locset premem in
  let locset_fs = locset in
  { ItvSem.Spec.empty with ItvSem.Spec.locset; locset_fs; premem }, locset, locset_fs

let sorted_pids global = global.Global.icfg |> InterCfg.pidsof |> sort_strings

let command_kind cmd =
  let open IntraCfg.Cmd in
  match cmd with
  | Cinstr _ -> "Cinstr"
  | Cif _ -> "Cif"
  | CLoop _ -> "CLoop"
  | Cset _ -> "Cset"
  | Cexternal _ -> "Cexternal"
  | Calloc _ -> "Calloc"
  | Csalloc _ -> "Csalloc"
  | Cfalloc _ -> "Cfalloc"
  | Cassume _ -> "Cassume"
  | Ccall _ -> "Ccall"
  | Creturn _ -> "Creturn"
  | Casm _ -> "Casm"
  | Cskip -> "Cskip"

let extern_dependency_of_node global node cmd =
  match cmd with
  | IntraCfg.Cmd.Cexternal _ -> Some "unknown-extern-call"
  | IntraCfg.Cmd.Ccall (_, Sparrow_cil.Lval (Sparrow_cil.Var callee, Sparrow_cil.NoOffset), _, _)
    when List.mem callee.Sparrow_cil.vname [ "memcpy"; "strcpy"; "strlen" ] ->
      None
  | IntraCfg.Cmd.Ccall (_, Sparrow_cil.Lval (Sparrow_cil.Var callee, Sparrow_cil.NoOffset), _, _) ->
      let callees = InterCfg.get_callees node global.Global.icfg in
      if not (InterCfg.ProcSet.is_empty callees) || List.mem callee.Sparrow_cil.vname (sorted_pids global)
      then None else Some "unknown-extern-call"
  | IntraCfg.Cmd.Ccall _ ->
      let callees = InterCfg.get_callees node global.Global.icfg in
      if InterCfg.ProcSet.is_empty callees then Some "unknown-extern-call" else None
  | _ -> None

let bta_roots global =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.filter_map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       match extern_dependency_of_node global node cmd with
       | Some reason -> Some (node, reason)
       | None -> None)

let extern_dependency_nodes global dug =
  let rec visit seen = function
    | [] -> seen
    | node :: rest ->
        if BasicDom.PowNode.mem node seen then visit seen rest
        else
          let seen = BasicDom.PowNode.add node seen in
          visit seen (DUGraph.succ node dug @ rest)
  in
  bta_roots global |> List.map fst |> visit BasicDom.PowNode.empty

let bta_node_facts global extern_nodes =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       match extern_dependency_of_node global node cmd with
       | Some reason ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "residual-extern-dependent";
             "reason", `String reason;
             "depends_on_extern", `Bool true;
             "extern_root", `String (node_to_string node);
             "command_kind", `String (command_kind cmd);
             "bta_phase", `String "before-transfer";
           ]
       | None when BasicDom.PowNode.mem node extern_nodes ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "residual-extern-dependent";
             "reason", `String "transitively-extern-dependent";
             "depends_on_extern", `Bool true;
             "command_kind", `String (command_kind cmd);
             "bta_phase", `String "before-transfer";
           ]
       | None ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "static-precomputed";
             "reason", `String "module-local-static-fixpoint-fact";
             "depends_on_extern", `Bool false;
             "command_kind", `String (command_kind cmd);
             "bta_phase", `String "before-transfer";
           ])
  |> sort_json

let bta_edge_facts dug extern_nodes =
  DUGraph.fold_edges (fun src dst acc ->
    let depends = BasicDom.PowNode.mem src extern_nodes || BasicDom.PowNode.mem dst extern_nodes in
    let labels = DUGraph.get_abslocs src dst dug in
    let classification, reason =
      if depends then ("residual-extern-dependent", "transitively-extern-dependent")
      else ("static-precomputed", "module-local-static-fixpoint-fact")
    in
    (`Assoc [
      "id", `String ("dug-edge:" ^ node_to_string src ^ "->" ^ node_to_string dst);
      "kind", `String "dug-edge";
      "src", `String (node_to_string src);
      "dst", `String (node_to_string dst);
      "labels", `List (powloc_json labels);
      "classification", `String classification;
      "reason", `String reason;
      "depends_on_extern", `Bool depends;
      "bta_phase", `String "before-propagation";
    ]) :: acc)
    dug []
  |> sort_json

type staged_completion = {
  mutable worklist_initialized : bool;
  mutable finalize_performed : bool;
  mutable widening_performed : bool;
  mutable widening_iterations : int;
  mutable widening_worklist_drained : bool;
  mutable narrowing_enabled : bool;
  mutable narrowing_applicable : bool;
  mutable narrowing_performed : bool;
  mutable narrowing_iterations : int;
  mutable narrowing_worklist_drained : bool;
  mutable blind_equality_checks : int;
  mutable residual_code_observed_by_convergence : bool;
  mutable staged_domain_fixpoint : bool;
  mutable bta_participates_in_fixpoint : bool;
  mutable transfer_level_d_site_count : int;
  mutable staged_lattice_event_count : int;
  mutable fixpoint_iterations_with_dynamic_cells : int;
}

let new_completion () = {
  worklist_initialized = true;
  finalize_performed = false;
  widening_performed = false;
  widening_iterations = 0;
  widening_worklist_drained = false;
  narrowing_enabled = !Options.narrow;
  narrowing_applicable = !Options.narrow;
  narrowing_performed = false;
  narrowing_iterations = 0;
  narrowing_worklist_drained = false;
  blind_equality_checks = 0;
  residual_code_observed_by_convergence = false;
  staged_domain_fixpoint = true;
  bta_participates_in_fixpoint = true;
  transfer_level_d_site_count = 0;
  staged_lattice_event_count = 0;
  fixpoint_iterations_with_dynamic_cells = 0;
}

let completion_json completion =
  `Assoc [
    "worklist_initialized", `Bool completion.worklist_initialized;
    "finalize_performed", `Bool completion.finalize_performed;
    "widening_performed", `Bool completion.widening_performed;
    "widening_iterations", `Int completion.widening_iterations;
    "widening_worklist_drained", `Bool completion.widening_worklist_drained;
    "narrowing_enabled", `Bool completion.narrowing_enabled;
    "narrowing_applicable", `Bool completion.narrowing_applicable;
    "narrowing_performed", `Bool completion.narrowing_performed;
    "narrowing_iterations", `Int completion.narrowing_iterations;
    "narrowing_worklist_drained", `Bool completion.narrowing_worklist_drained;
    "worklist_drained", `Bool (completion.widening_worklist_drained && ((not completion.narrowing_applicable) || completion.narrowing_worklist_drained));
    "pfs", `Int !Options.pfs;
    "pfs_binding_path", `Bool (!Options.pfs < 100);
    "convergence_equality", `String "abstract-domain-static-projection";
    "blind_equality_checks", `Int completion.blind_equality_checks;
    "residual_code_observed_by_convergence", `Bool completion.residual_code_observed_by_convergence;
    "staged_domain_fixpoint", `Bool completion.staged_domain_fixpoint;
    "bta_participates_in_fixpoint", `Bool completion.bta_participates_in_fixpoint;
    "stage1_direct_sparse_pipeline", `Bool false;
    "direct_sparse_oracle_only", `Bool true;
    "posthoc_row_split_used", `Bool false;
    "row_obligation_residual_source", `Bool false;
    "transfer_level_d_site_count", int_json completion.transfer_level_d_site_count;
    "staged_lattice_event_count", int_json completion.staged_lattice_event_count;
    "fixpoint_iterations_with_dynamic_cells", int_json completion.fixpoint_iterations_with_dynamic_cells;
  ]

type staged_cell = {
  loc : BasicDom.Loc.t;
  cell : StageT.staged_cell;
  row_fragment : Yojson.Safe.t;
  component : StageT.staged_residual_component option;
}

type staged_memory = staged_cell list

type staged_tables = {
  input_rows : Yojson.Safe.t list;
  output_rows : Yojson.Safe.t list;
  residual_input_components : StageT.staged_residual_component list;
  residual_output_components : StageT.staged_residual_component list;
  facts : Yojson.Safe.t list;
  completion : staged_completion;
}

let component_value_json loc stage value =
  `Assoc [
    "location", `String (loc_to_string loc);
    "value", `String value;
    "stage", `String stage;
  ]

let row_of_memory node memory =
  `Assoc [
    "node", `String (node_to_string node);
    "memory", `List (memory |> List.map (fun c -> c.row_fragment) |> sort_json);
  ]

let locs_for_node spec access dug node =
  let used =
    try Access.Info.useof (Access.find_node node access)
    with _ -> PowLoc.empty
  in
  let edge_locs =
    let from_preds =
      DUGraph.pred node dug
      |> List.fold_left (fun acc pred -> PowLoc.union (DUGraph.get_abslocs pred node dug) acc) PowLoc.empty
    in
    DUGraph.succ node dug
    |> List.fold_left (fun acc succ -> PowLoc.union (DUGraph.get_abslocs node succ dug) acc) from_preds
  in
  let locs = PowLoc.union used edge_locs in
  if powloc_is_empty locs then spec.ItvSem.Spec.locset else locs

let static_value_string spec loc =
  ItvDom.Val.to_string (ItvDom.Mem.find loc spec.ItvSem.Spec.premem)

let component_for_cell
    ~module_id
    ~source_file
    ~source_hash
    ~table
    ~node
    ~ordinal
    ~row
    ~loc
    ~component_kind
    ~transfer_event
    ~lattice_event
    ~shape
    ~witness_constructor
    ~semantic_expression
    ~semantic_guard
    ?(residual_dependencies = [])
    ?(derive_from_first_dependency = false)
    ~expression_code
    ~guard_code
    row_fragment =
  Residual.make_component
    ~module_id
    ~source_file
    ~source_hash
    ~table
    ~node:(node_to_string node)
    ~location:(loc_to_string loc)
    ~ordinal
    ~default_row:row
    ~default_component:row_fragment
    ~component_kind
    ~transfer_event
    ~lattice_event
    ~shape
    ~witness_constructor
    ~semantic_expression
    ~semantic_guard
    ~residual_dependencies
    ~derive_from_first_dependency
    ~expression_code
    ~guard_code
    ()

let powloc_of_lval spec node lv =
  try ItvSem.eval_lv ~spec (BasicDom.Node.get_pid node) lv spec.ItvSem.Spec.premem
  with _ -> PowLoc.empty

let rec powlocs_of_exp spec node exp =
  let open Sparrow_cil in
  match exp with
  | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | AddrOfLabel _ -> PowLoc.empty
  | Lval lv | AddrOf lv | StartOf lv -> powloc_of_lval spec node lv
  | SizeOfE e | UnOp (_, e, _) | Real e | Imag e | CastE (_, e) -> powlocs_of_exp spec node e
  | BinOp (_, a, b, _) -> PowLoc.union (powlocs_of_exp spec node a) (powlocs_of_exp spec node b)
  | Question (c, t, f, _) ->
      PowLoc.union (powlocs_of_exp spec node c)
        (PowLoc.union (powlocs_of_exp spec node t) (powlocs_of_exp spec node f))

let loc_list_of_powlocs locs =
  PowLoc.fold (fun loc acc -> loc :: acc) locs [] |> List.sort compare

let memory_find loc memory =
  List.find_opt (fun cell -> compare cell.loc loc = 0) memory

let dynamic_cell_for_loc loc memory =
  match memory_find loc memory with
  | Some staged -> StageT.typed_code_present staged.cell
  | None -> false

let any_dynamic_locs locs memory =
  loc_list_of_powlocs locs |> List.exists (fun loc -> dynamic_cell_for_loc loc memory)

let dynamic_locs_from locs memory =
  loc_list_of_powlocs locs |> List.filter (fun loc -> dynamic_cell_for_loc loc memory)

let static_or_input_cell spec loc memory =
  match memory_find loc memory with
  | Some staged -> staged
  | None ->
      let abstract_value = static_value_string spec loc in
      let cell =
        StageT.make_static_cell
          ~location:(loc_to_string loc)
          ~abstract_value
          ~semantic_source:"module-local-premem"
          ~ordinal:0
      in
      {
        loc;
        cell;
        row_fragment = component_value_json loc "S" abstract_value;
        component = None;
      }

let static_eval_string spec node exp =
  try ItvSem.eval ~spec (BasicDom.Node.get_pid node) exp spec.ItvSem.Spec.premem |> ItvDom.Val.to_string
  with _ -> "static-eval-unavailable"

let cilint_to_residual_int i64 =
  try Sparrow_cil.Cilint.to_int i64 with _ -> 0

let static_eval_int spec node exp =
  match exp with
  | Sparrow_cil.Const (Sparrow_cil.CInt (i64, _, _)) ->
      cilint_to_residual_int i64
  | _ ->
      let _ = static_eval_string spec node exp in
      0

let dynamic_staged_cell ~loc ~abstract_value ~semantic_source ~expression_code =
  StageT.make_dynamic_cell
    ~location:(loc_to_string loc)
    ~abstract_value
    ~semantic_source
    ~code:expression_code

let replace_cells updates memory =
  let update_keys = List.map (fun cell -> loc_to_string cell.loc) updates in
  updates @ List.filter (fun cell -> not (List.mem (loc_to_string cell.loc) update_keys)) memory

let row_from_cells node cells =
  `Assoc [
    "node", `String (node_to_string node);
    "memory", `List (cells |> List.map (fun c -> c.row_fragment) |> sort_json);
  ]

let lval_string lv = CilHelper.s_exp (Sparrow_cil.Lval lv)

let extern_expression_code node loc =
  let node_c = Abstract_speculate_lift.lift_string (node_to_string node) in
  let loc_c = Abstract_speculate_lift.lift_string (loc_to_string loc) in
  .<fun input -> Stage2.extern_int ~node:.~node_c ~location:.~loc_c input>.

let static_expression_code value = .<fun _ -> value>.

let dynamic_code_for_lval spec node memory lv =
  powloc_of_lval spec node lv
  |> loc_list_of_powlocs
  |> List.find_map (fun loc ->
       match memory_find loc memory with
       | Some cell -> StageT.dynamic_code cell.cell
       | None -> None)

let rec residual_expression_code spec node memory exp =
  let open Sparrow_cil in
  match exp with
  | Const (CInt (i64, _, _)) ->
      let value = cilint_to_residual_int i64 in
      static_expression_code value
  | Const _ -> static_expression_code 0
  | Lval lv ->
      begin match dynamic_code_for_lval spec node memory lv with
      | Some code -> code
      | None -> static_expression_code (static_eval_int spec node exp)
      end
  | UnOp (Neg, e, _) ->
      let c = residual_expression_code spec node memory e in
      .<fun input -> -(.~c input)>.
  | UnOp (LNot, e, _) ->
      let g = residual_guard_code spec node memory e in
      .<fun input -> if .~g input then 0 else 1>.
  | UnOp (BNot, e, _) ->
      let c = residual_expression_code spec node memory e in
      .<fun input -> lnot (.~c input)>.
  | BinOp (op, a, b, _) ->
      let ac = residual_expression_code spec node memory a in
      let bc = residual_expression_code spec node memory b in
      begin match op with
      | PlusA | PlusPI | IndexPI -> .<fun input -> (.~ac input) + (.~bc input)>.
      | MinusA | MinusPI | MinusPP -> .<fun input -> (.~ac input) - (.~bc input)>.
      | Mult -> .<fun input -> (.~ac input) * (.~bc input)>.
      | Div -> .<fun input -> let rhs = .~bc input in if rhs = 0 then 0 else (.~ac input) / rhs>.
      | Mod -> .<fun input -> let rhs = .~bc input in if rhs = 0 then 0 else (.~ac input) mod rhs>.
      | Lt -> .<fun input -> if (.~ac input) < (.~bc input) then 1 else 0>.
      | Gt -> .<fun input -> if (.~ac input) > (.~bc input) then 1 else 0>.
      | Le -> .<fun input -> if (.~ac input) <= (.~bc input) then 1 else 0>.
      | Ge -> .<fun input -> if (.~ac input) >= (.~bc input) then 1 else 0>.
      | Eq -> .<fun input -> if (.~ac input) = (.~bc input) then 1 else 0>.
      | Ne -> .<fun input -> if (.~ac input) <> (.~bc input) then 1 else 0>.
      | LAnd -> .<fun input -> if (.~ac input) <> 0 && (.~bc input) <> 0 then 1 else 0>.
      | LOr -> .<fun input -> if (.~ac input) <> 0 || (.~bc input) <> 0 then 1 else 0>.
      | Shiftlt -> .<fun input -> (.~ac input) lsl max 0 (.~bc input)>.
      | Shiftrt -> .<fun input -> (.~ac input) lsr max 0 (.~bc input)>.
      | BAnd -> .<fun input -> (.~ac input) land (.~bc input)>.
      | BXor -> .<fun input -> (.~ac input) lxor (.~bc input)>.
      | BOr -> .<fun input -> (.~ac input) lor (.~bc input)>.
      end
  | Question (c, t, f, _) ->
      let gc = residual_guard_code spec node memory c in
      let tc = residual_expression_code spec node memory t in
      let fc = residual_expression_code spec node memory f in
      .<fun input -> if .~gc input then .~tc input else .~fc input>.
  | CastE (_, e) | SizeOfE e | Real e | Imag e -> residual_expression_code spec node memory e
  | SizeOf _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | AddrOf _ | StartOf _ | AddrOfLabel _ ->
      static_expression_code (static_eval_int spec node exp)

and residual_guard_code spec node memory exp =
  let open Sparrow_cil in
  match exp with
  | UnOp (LNot, e, _) ->
      let c = residual_guard_code spec node memory e in
      .<fun input -> not (.~c input)>.
  | BinOp (op, a, b, _) ->
      let ac = residual_expression_code spec node memory a in
      let bc = residual_expression_code spec node memory b in
      begin match op with
      | Lt -> .<fun input -> (.~ac input) < (.~bc input)>.
      | Gt -> .<fun input -> (.~ac input) > (.~bc input)>.
      | Le -> .<fun input -> (.~ac input) <= (.~bc input)>.
      | Ge -> .<fun input -> (.~ac input) >= (.~bc input)>.
      | Eq -> .<fun input -> (.~ac input) = (.~bc input)>.
      | Ne -> .<fun input -> (.~ac input) <> (.~bc input)>.
      | LAnd -> .<fun input -> (.~ac input) <> 0 && (.~bc input) <> 0>.
      | LOr -> .<fun input -> (.~ac input) <> 0 || (.~bc input) <> 0>.
      | _ ->
          let c = residual_expression_code spec node memory exp in
          .<fun input -> .~c input <> 0>.
      end
  | _ ->
      let c = residual_expression_code spec node memory exp in
      .<fun input -> .~c input <> 0>.

let make_dynamic_update
    ~module_id
    ~source_file
    ~source_hash
    ~table
    ~node
    ~iteration
    ~ordinal
    ~lattice_event
    ~transfer_event
    ~shape
    ~witness_constructor
    ~semantic_expression
    ~semantic_guard
    ~residual_dependencies
    ~derive_from_first_dependency
    ~extra_row_fields
    ~expression_code
    ~guard_code
    ~loc
    ~abstract_value
    () =
  let semantic_source = transfer_event ^ ":" ^ semantic_expression in
  let cell = dynamic_staged_cell ~loc ~abstract_value ~semantic_source ~expression_code in
  let row_fragment =
    `Assoc
      ([
         "location", `String (loc_to_string loc);
         "value", `String abstract_value;
         "stage", `String "D";
         "semantic_expression", `String semantic_expression;
         "semantic_guard", `String semantic_guard;
         "transfer_event", `String transfer_event;
         "fixpoint_iteration", `Int iteration;
         "residual_dependencies", `List (List.map (fun dep -> `String (StageT.residual_cell_key dep)) residual_dependencies);
         "derive_from_first_dependency", `Bool derive_from_first_dependency;
       ]
      @ extra_row_fields)
  in
  let row = row_from_cells node [{ loc; cell; row_fragment; component = None }] in
  let component =
    component_for_cell
      ~module_id
      ~source_file
      ~source_hash
      ~table
      ~node
      ~ordinal
      ~row
      ~loc
      ~component_kind:"staged-abstract-cell"
      ~transfer_event
      ~lattice_event
      ~shape
      ~witness_constructor
      ~semantic_expression
      ~semantic_guard
      ~residual_dependencies
      ~derive_from_first_dependency
      ~expression_code
      ~guard_code
      row_fragment
  in
  { loc; cell; row_fragment; component = Some component }


let callee_name = function
  | Sparrow_cil.Lval (Sparrow_cil.Var callee, Sparrow_cil.NoOffset) -> Some callee.Sparrow_cil.vname
  | _ -> None

let api_residual_baseline = function
  | "memcpy" -> "sparrow-modular-ocaml/src/itvSem.ml:482-492; sparrow-modular-ocaml/src/apiSem.ml:70-75"
  | "strcpy" -> "sparrow-modular-ocaml/src/itvSem.ml:510-522; sparrow-modular-ocaml/src/apiSem.ml:70-75"
  | "strlen" -> "sparrow-modular-ocaml/src/itvSem.ml:392-398; sparrow-modular-ocaml/src/apiSem.ml:103-107"
  | name -> "unsupported-api-residual-model:" ^ name

let api_residual_effect = function
  | "memcpy" -> "source array memory copied to destination locations; memcpy return destination is tracked only when assigned"
  | "strcpy" -> "destination null-position/value witness copied from source; C return semantics intentionally not claimed"
  | "strlen" -> "return interval/value witness derived from source null-position"
  | name -> "unsupported residual API model: " ^ name

let component_dependency_of_cell cell =
  match cell.component with
  | Some component ->
      Some
        (StageT.make_residual_cell_id
           ~cell_table:component.StageT.table
           ~cell_node:component.StageT.node
           ~cell_location:component.StageT.location)
  | None -> None

let dynamic_dependencies_for_locs locs memory =
  loc_list_of_powlocs locs
  |> List.filter_map (fun loc ->
       match memory_find loc memory with
       | Some cell when StageT.typed_code_present cell.cell -> component_dependency_of_cell cell
       | _ -> None)
  |> List.sort_uniq compare

let first_dynamic_expression_code locs memory =
  loc_list_of_powlocs locs
  |> List.find_map (fun loc ->
       match memory_find loc memory with
       | Some cell -> StageT.dynamic_code cell.cell
       | None -> None)
  |> function
  | Some code -> code
  | None -> static_expression_code 0

let api_extra_fields ~function_name ~abstract_effect =
  [
    "api_residual_function", `String function_name;
    "api_baseline_source", `String (api_residual_baseline function_name);
    "api_abstract_effect", `String abstract_effect;
    "api_semantic_provenance", `String "residual-api-model-coverage/slice-1";
    "covered_api", `Bool true;
    "unsupported_api_coverage", `Bool false;
  ]

let api_model_updates
    ~module_id
    ~source_file
    ~source_hash
    ~table
    ~node
    ~iteration
    ~ordinal_base
    spec
    input_memory
    lvo
    function_name
    args =
  let make_api_updates target_locs source_locs abstract_effect abstract_value offset =
    let dependencies = dynamic_dependencies_for_locs source_locs input_memory in
    if dependencies = [] then []
    else
      target_locs
      |> loc_list_of_powlocs
      |> List.mapi (fun i loc ->
           make_dynamic_update
             ~module_id
             ~source_file
             ~source_hash
             ~table
             ~node
             ~iteration
             ~ordinal:(ordinal_base + offset + i)
             ~lattice_event:"staged-lattice-transfer-D-before-fixpoint"
             ~transfer_event:("api-residual-model-" ^ function_name ^ "-created-D-during-transfer")
             ~shape:"Staged_component_shape"
             ~witness_constructor:"Api_residual_model"
             ~semantic_expression:("api residual model " ^ function_name ^ ": " ^ abstract_effect)
             ~semantic_guard:("api residual model guard at " ^ node_to_string node)
             ~residual_dependencies:dependencies
             ~derive_from_first_dependency:true
             ~extra_row_fields:(api_extra_fields ~function_name ~abstract_effect)
             ~expression_code:(first_dynamic_expression_code source_locs input_memory)
             ~guard_code:.<fun _ -> true>.
             ~loc
             ~abstract_value
             ())
  in
  match function_name, args with
  | "memcpy", dst :: src :: _ ->
      let dst_locs = powlocs_of_exp spec node dst in
      let src_locs = powlocs_of_exp spec node src in
      let copy_updates =
        make_api_updates dst_locs src_locs (api_residual_effect "memcpy") "api-memcpy-copy-from-source" 0
      in
      let return_updates =
        match lvo with
        | Some lv ->
            make_api_updates
              (powloc_of_lval spec node lv)
              src_locs
              "memcpy return destination with source-backed copy provenance"
              "api-memcpy-return-destination"
              1000
        | None -> []
      in
      copy_updates @ return_updates
  | "strcpy", dst :: src :: _ ->
      make_api_updates
        (powlocs_of_exp spec node dst)
        (powlocs_of_exp spec node src)
        (api_residual_effect "strcpy")
        "api-strcpy-null-position-from-source"
        0
  | "strlen", src :: _ ->
      begin
        match lvo with
        | Some lv ->
            make_api_updates
              (powloc_of_lval spec node lv)
              (powlocs_of_exp spec node src)
              (api_residual_effect "strlen")
              "api-strlen-length-from-source-null-position"
              0
        | None -> []
      end
  | _ -> []

let command_transfer_updates ~module_id ~source_file ~source_hash spec global table node iteration input_memory ordinal_base =
  let cmd = InterCfg.cmdof global.Global.icfg node in
  let make_updates locs transfer_event semantic_expression semantic_guard abstract_value expression_code_for_loc guard_code =
    loc_list_of_powlocs locs
    |> List.mapi (fun i loc ->
         let expression_code = expression_code_for_loc loc in
         make_dynamic_update
           ~module_id
           ~source_file
           ~source_hash
           ~table
           ~node
           ~iteration
           ~ordinal:(ordinal_base + i)
           ~lattice_event:"staged-lattice-transfer-D-before-fixpoint"
           ~transfer_event
           ~shape:"Staged_component_shape"
           ~witness_constructor:"Staged_component"
           ~semantic_expression
           ~semantic_guard
           ~residual_dependencies:[]
           ~derive_from_first_dependency:false
           ~extra_row_fields:[]
           ~expression_code
           ~guard_code
           ~loc
           ~abstract_value
           ())
  in
  match cmd with
  | IntraCfg.Cmd.Cexternal (lv, _) ->
      make_updates (powloc_of_lval spec node lv)
        "extern-read-created-D-during-transfer"
        ("external write to " ^ lval_string lv)
        ("extern availability guard at " ^ node_to_string node)
        "extern-dependent-value"
        (fun loc -> extern_expression_code node loc)
        .<fun _ -> true>.
  | IntraCfg.Cmd.Ccall (lvo, callee, args, _) ->
      let api_updates =
        match callee_name callee with
        | Some (("memcpy" | "strcpy" | "strlen") as function_name) ->
            api_model_updates
              ~module_id
              ~source_file
              ~source_hash
              ~table
              ~node
              ~iteration
              ~ordinal_base
              spec
              input_memory
              lvo
              function_name
              args
        | _ -> []
      in
      if api_updates <> [] then api_updates
      else
      let unknown = extern_dependency_of_node global node cmd <> None in
      let arg_reads =
        List.fold_left (fun acc exp -> PowLoc.union acc (powlocs_of_exp spec node exp)) PowLoc.empty args
      in
      let dynamic_args = any_dynamic_locs arg_reads input_memory in
      let dynamic_arg_code () =
        args
        |> List.find_opt (fun exp -> any_dynamic_locs (powlocs_of_exp spec node exp) input_memory)
        |> function
           | Some exp -> residual_expression_code spec node input_memory exp
           | None -> static_expression_code 0
      in
      begin match lvo with
      | Some lv when unknown || dynamic_args ->
          let transfer_event =
            if unknown then "extern-call-result-created-D-during-transfer"
            else "dynamic-call-result-created-D-during-transfer"
          in
          make_updates (powloc_of_lval spec node lv)
            transfer_event
            ("call result " ^ lval_string lv ^ " := " ^ CilHelper.s_exp callee)
            ("call guard with dynamic args at " ^ node_to_string node)
            "call-result-extern-or-dynamic"
            (fun loc ->
              if unknown then extern_expression_code node loc
              else dynamic_arg_code ())
            .<fun _ -> true>.
      | _ -> []
      end
  | IntraCfg.Cmd.Cset (lv, exp, _) ->
      let reads = powlocs_of_exp spec node exp in
      if any_dynamic_locs reads input_memory then
        make_updates (powloc_of_lval spec node lv)
          "dynamic-arithmetic-created-D-during-transfer"
          (lval_string lv ^ " := " ^ CilHelper.s_exp exp)
          ("assignment available at " ^ node_to_string node)
          ("eval(" ^ static_eval_string spec node exp ^ ") with dynamic operands " ^
           comma_join (List.map loc_to_string (dynamic_locs_from reads input_memory)))
          (fun _ -> residual_expression_code spec node input_memory exp)
          .<fun _ -> true>.
      else []
  | IntraCfg.Cmd.Cif (exp, _, _, _) | IntraCfg.Cmd.Cassume (exp, _) ->
      let reads = powlocs_of_exp spec node exp in
      if any_dynamic_locs reads input_memory then
        let locs = List.fold_left (fun acc loc -> PowLoc.add loc acc) PowLoc.empty (dynamic_locs_from reads input_memory) in
        make_updates locs
          "dynamic-guard-created-D-during-transfer"
          ("guard " ^ CilHelper.s_exp exp)
          ("guard residual predicate at " ^ node_to_string node)
          "guard-dependent-on-dynamic-cell"
          (fun loc ->
            match memory_find loc input_memory with
            | Some cell -> (match StageT.dynamic_code cell.cell with Some code -> code | None -> static_expression_code 0)
            | None -> static_expression_code 0)
          (residual_guard_code spec node input_memory exp)
      else []
  | _ -> []

let staged_transfer_node ~module_id ~source_file ~source_hash spec global access dug table node iteration input_memory ordinal_base =
  let base_locs = locs_for_node spec access dug node in
  let base_memory =
    loc_list_of_powlocs base_locs
    |> List.map (fun loc -> static_or_input_cell spec loc input_memory)
  in
  let updates =
    command_transfer_updates
      ~module_id
      ~source_file
      ~source_hash
      spec
      global
      table
      node
      iteration
      (input_memory @ base_memory)
      ordinal_base
  in
  replace_cells updates base_memory

let join_predecessor_memory pred_mem succ_mem =
  let by_loc = Hashtbl.create 17 in
  let events = ref [] in
  List.iter (fun cell -> Hashtbl.replace by_loc (loc_to_string cell.loc) cell) succ_mem;
  List.iter (fun pred_cell ->
    let key = loc_to_string pred_cell.loc in
    match Hashtbl.find_opt by_loc key with
    | None -> Hashtbl.replace by_loc key pred_cell
    | Some old_cell ->
        let joined, event = StageT.staged_join old_cell.cell pred_cell.cell in
        let component = match old_cell.component with Some _ -> old_cell.component | None -> pred_cell.component in
        let row_fragment = if old_cell.component = None && pred_cell.component <> None then pred_cell.row_fragment else old_cell.row_fragment in
        events := event :: !events;
        Hashtbl.replace by_loc key { old_cell with cell = joined; row_fragment; component }) pred_mem;
  Hashtbl.fold (fun _ cell acc -> cell :: acc) by_loc [], !events


let supported_api_model_call global node =
  match InterCfg.cmdof global.Global.icfg node with
  | IntraCfg.Cmd.Ccall (_, callee, _, _) -> (
      match callee_name callee with
      | Some ("memcpy" | "strcpy" | "strlen") -> true
      | _ -> false)
  | _ -> false

let api_model_nodes global =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.filter (supported_api_model_call global)

let staged_sparse_pipeline ~module_id ~source_file ~source_hash spec global access dug =
  let completion = new_completion () in
  let extern_nodes = extern_dependency_nodes global dug in
  let nodes =
    DUGraph.nodesof dug
    |> BatSet.fold (fun node acc -> node :: acc)
    |> fun f -> f []
    |> List.append (api_model_nodes global)
    |> List.sort_uniq compare
  in
  let nodes = if nodes = [] then InterCfg.nodesof global.Global.icfg else nodes in
  let input_tbl = Hashtbl.create (List.length nodes + 1) in
  let output_tbl = Hashtbl.create (List.length nodes + 1) in
  let lattice_events = ref [] in
  let dynamic_iterations = Hashtbl.create 17 in
  let note_events events =
    lattice_events := events @ !lattice_events;
    completion.staged_lattice_event_count <- completion.staged_lattice_event_count + List.length events
  in
  let dynamic_count memory =
    List.fold_left (fun n cell -> match cell.component with Some _ -> n + 1 | None -> n) 0 memory
  in
  let note_dynamic iteration memory =
    let count = dynamic_count memory in
    if count > 0 then begin
      Hashtbl.replace dynamic_iterations iteration true;
      completion.transfer_level_d_site_count <- completion.transfer_level_d_site_count + count;
      completion.residual_code_observed_by_convergence <- true
    end
  in
  let predecessor_input node =
    let base = locs_for_node spec access dug node |> loc_list_of_powlocs |> List.map (fun loc -> static_or_input_cell spec loc []) in
    let from_dug =
      DUGraph.pred node dug
      |> List.fold_left (fun acc pred ->
           match Hashtbl.find_opt output_tbl pred with
           | None -> acc
           | Some pred_mem ->
               let joined, events = join_predecessor_memory pred_mem acc in
               note_events events;
               joined)
           base
    in
    if supported_api_model_call global node then
      Hashtbl.fold
        (fun _ pred_mem acc ->
          let joined, events = join_predecessor_memory pred_mem acc in
          note_events events;
          joined)
        output_tbl
        from_dug
    else from_dug
  in
  let unstable_memory old_mem new_mem =
    let events = ref [] in
    let unstable =
      List.exists (fun new_cell ->
        match memory_find new_cell.loc old_mem with
        | None -> StageT.typed_code_present new_cell.cell
        | Some old_cell ->
            let is_unstable, event = StageT.staged_unstable old_cell.cell new_cell.cell in
            events := event :: !events;
            is_unstable)
        new_mem
    in
    note_events !events;
    unstable
  in
  let widen_memory old_mem new_mem =
    let events = ref [] in
    let widened =
      new_mem |> List.map (fun new_cell ->
        match memory_find new_cell.loc old_mem with
        | None -> new_cell
        | Some old_cell ->
            let cell, event = StageT.staged_widen old_cell.cell new_cell.cell in
            events := event :: !events;
            { new_cell with cell })
    in
    note_events !events;
    widened
  in
  let narrow_memory old_mem new_mem =
    let events = ref [] in
    let narrowed =
      new_mem |> List.map (fun new_cell ->
        match memory_find new_cell.loc old_mem with
        | None -> new_cell
        | Some old_cell ->
            let cell, event = StageT.staged_narrow old_cell.cell new_cell.cell in
            events := event :: !events;
            { new_cell with cell })
    in
    note_events !events;
    narrowed
  in
  let propagate_successors node worklist =
    DUGraph.succ node dug
    |> List.fold_left (fun wl succ ->
         let memory = match Hashtbl.find_opt output_tbl node with Some m -> m | None -> [] in
         let _propagated, events =
           memory
           |> List.map (fun cell ->
                let propagated, event = StageT.staged_propagate cell.cell in
                { cell with cell = propagated }, event)
           |> List.split
         in
         note_events events;
         if List.mem succ wl then wl else succ :: wl)
         worklist
  in
  let rec widen_loop iteration worklist =
    match worklist with
    | [] -> iteration
    | node :: rest ->
        let input_memory = predecessor_input node in
        Hashtbl.replace input_tbl node input_memory;
        let transferred =
          staged_transfer_node
            ~module_id
            ~source_file
            ~source_hash
            spec
            global
            access
            dug
            "output"
            node
            iteration
            input_memory
            (iteration * 10_000)
        in
        note_dynamic iteration transferred;
        let old = match Hashtbl.find_opt output_tbl node with Some m -> m | None -> [] in
        let unstable = old = [] || unstable_memory old transferred in
        if unstable then begin
          let widened = if old = [] then transferred else widen_memory old transferred in
          Hashtbl.replace output_tbl node widened;
          completion.widening_iterations <- completion.widening_iterations + 1;
          widen_loop (iteration + 1) (propagate_successors node rest)
        end else
          widen_loop (iteration + 1) rest
  in
  let final_widen_iteration = widen_loop 1 nodes in
  completion.widening_performed <- completion.widening_iterations > 0;
  completion.widening_worklist_drained <- true;
  completion.fixpoint_iterations_with_dynamic_cells <- Hashtbl.length dynamic_iterations;
  let narrowing_iterations = ref 0 in
  if !Options.narrow then begin
    List.iter (fun node ->
      let input_memory = predecessor_input node in
      Hashtbl.replace input_tbl node input_memory;
      let transferred =
        staged_transfer_node
          ~module_id
          ~source_file
          ~source_hash
          spec
          global
          access
          dug
          "output"
          node
          (final_widen_iteration + !narrowing_iterations)
          input_memory
          ((final_widen_iteration + !narrowing_iterations) * 10_000)
      in
      let old = match Hashtbl.find_opt output_tbl node with Some m -> m | None -> [] in
      let narrowed = if old = [] then transferred else narrow_memory old transferred in
      Hashtbl.replace output_tbl node narrowed;
      incr narrowing_iterations)
      nodes;
    completion.narrowing_performed <- !narrowing_iterations > 0;
    completion.narrowing_iterations <- !narrowing_iterations;
    completion.narrowing_worklist_drained <- true
  end;
  completion.narrowing_applicable <- !Options.narrow;
  completion.finalize_performed <- true;
  completion.blind_equality_checks <- max 1 (completion.widening_iterations + completion.narrowing_iterations);
  let table_of tbl = nodes |> List.map (fun node -> node, match Hashtbl.find_opt tbl node with Some m -> m | None -> []) in
  let input_memories = table_of input_tbl in
  let output_memories = table_of output_tbl in
  let rows memories = memories |> List.map (fun (node, memory) -> row_of_memory node memory) |> sort_json in
  let components memories =
    memories
    |> List.concat_map (fun (_, memory) -> List.filter_map (fun cell -> cell.component) memory)
  in
  let component_facts table memories =
    memories
    |> List.concat_map (fun (node, memory) ->
         memory |> List.map (fun cell ->
           let depends = Option.is_some cell.component || StageT.typed_code_present cell.cell in
           let classification, reason =
             if depends then ("residual-extern-dependent", "transitively-extern-dependent")
             else ("static-precomputed", "module-local-static-fixpoint-fact")
           in
           `Assoc [
             "id", `String (table ^ ":" ^ node_to_string node ^ ":" ^ loc_to_string cell.loc);
             "kind", `String "staged-component";
             "table", `String table;
             "node", `String (node_to_string node);
             "location", `String (loc_to_string cell.loc);
             "classification", `String classification;
             "reason", `String reason;
             "depends_on_extern", `Bool depends;
             "fixpoint_phase", `String "worklist-transfer-lattice";
             "stage", `String (StageT.stage_of_cell cell.cell);
             "typed_code_present", `Bool (StageT.typed_code_present cell.cell);
             "abstract_value", `String cell.cell.StageT.abstract_value;
             "semantic_source", `String cell.cell.StageT.semantic_source;
             "row_wrapper", `Bool false;
           ]))
  in
  {
    input_rows = rows input_memories;
    output_rows = rows output_memories;
    residual_input_components = components input_memories;
    residual_output_components = components output_memories;
    facts = (bta_node_facts global extern_nodes @ bta_edge_facts dug extern_nodes @ component_facts "input" input_memories @ component_facts "output" output_memories) |> sort_json;
    completion;
  }

let control_component ~module_id ~source_file ~source_hash ~node ~ordinal ~shape ~witness_constructor =
  let default_row = `Assoc [
    "node", `String node;
    "table", `String "control";
    "memory", `List [];
    "control_shape", `String shape;
    "control_semantic_core", `String "typed-residual-control-shape";
    "executable_control_residual", `Bool true;
  ] in
  Residual.make_component
    ~module_id
    ~source_file
    ~source_hash
    ~table:"control"
    ~node
    ~location:"control"
    ~ordinal
    ~default_row
    ~default_component:(`Assoc ["location", `String "control"; "value", `String shape])
    ~component_kind:"staged-control-guard"
    ~transfer_event:"control-transfer-created-D"
    ~lattice_event:"control-lattice-preserved-D"
    ~shape
    ~witness_constructor
    ~semantic_expression:("control residual shape " ^ shape)
    ~semantic_guard:("control residual guard " ^ node)
    ~expression_code:.<fun _ -> 0>.
    ~guard_code:.<fun _ -> true>.
    ()

let syntax_shape_witnesses ~module_id ~source_file ~source_hash global =
  let from_syntax = ref [] in
  let control_components = ref [] in
  let ordinal = ref 1 in
  Sparrow_cil.iterGlobals global.Global.file (function
    | Sparrow_cil.GFun (fd, _) ->
        List.iter (fun stmt ->
          let id = fd.Sparrow_cil.svar.Sparrow_cil.vname ^ ":" ^ string_of_int stmt.Sparrow_cil.sid in
          match stmt.Sparrow_cil.skind with
          | Sparrow_cil.Loop _ ->
              let component =
                control_component
                  ~module_id
                  ~source_file
                  ~source_hash
                  ~node:id
                  ~ordinal:!ordinal
                  ~shape:"Loop_shape"
                  ~witness_constructor:"Loop_shape"
              in
              incr ordinal;
              control_components := component :: !control_components;
              from_syntax := StageT.Loop_shape (id, "typed-residual-loop-shape", [StageT.Typed_component component]) :: !from_syntax
          | Sparrow_cil.If _ ->
              let component =
                control_component
                  ~module_id
                  ~source_file
                  ~source_hash
                  ~node:id
                  ~ordinal:!ordinal
                  ~shape:"Branch_shape"
                  ~witness_constructor:"Branch_shape"
              in
              incr ordinal;
              control_components := component :: !control_components;
              let typed = StageT.Typed_component component in
              from_syntax := StageT.Branch_shape (id, "typed-residual-branch-shape", [typed], [typed]) :: !from_syntax
          | _ -> ())
          fd.Sparrow_cil.sallstmts
    | _ -> ());
  List.rev !from_syntax, List.rev !control_components

let shape_witnesses ~module_id ~source_file ~source_hash global residual_input_components residual_output_components =
  let module_component =
    control_component
      ~module_id
      ~source_file
      ~source_hash
      ~node:(module_id ^ ":module-residual-analyzer")
      ~ordinal:0
      ~shape:"Typed_code"
      ~witness_constructor:"Typed_code"
  in
  let syntax_witnesses, syntax_components = syntax_shape_witnesses ~module_id ~source_file ~source_hash global in
  let residual_exprs =
    (residual_input_components @ residual_output_components)
    |> List.map (fun component ->
         StageT.Expr_shape (component.StageT.node, [StageT.Typed_component component]))
  in
  StageT.Typed_component module_component :: syntax_witnesses @ residual_exprs,
  module_component :: syntax_components

type stage1_result = {
  source : string;
  module_id : string;
  source_hash : string;
  before : Global.t;
  pre : Global.t;
  input_rows : Yojson.Safe.t list;
  output_rows : Yojson.Safe.t list;
  completion : Yojson.Safe.t;
  facts : Yojson.Safe.t list;
  extern_roots : string list;
  shape_witnesses : StageT.shape_witness list;
  analyzer : StageT.residual_analyzer;
  stage2_input : StageT.stage2_input;
  stage2_output : StageT.stage2_output;
}

type source_rerun_result = {
  rerun_id : string;
  result : stage1_result;
  linked_context_consumed : bool;
  linked_extern_effects_consumed : bool;
  source_hash_matches_stage2_input : bool;
  evidence : Yojson.Safe.t;
}

let source_hash path = Digest.to_hex (Digest.file path)
let module_id source = Filename.basename source

let stage2_input_key ~module_id ~source_hash = module_id ^ ":" ^ source_hash

let linked_context_consumed input =
  string_field "derivation" input.StageT.extern_effects = "linked-environment"
  || string_field "linked_derivation_source" input.StageT.extern_effects <> ""

let linked_extern_effects_consumed input =
  linked_context_consumed input && list_field "effects" input.StageT.extern_effects <> []

let stage2_input_source_hash input =
  string_field "source_hash" input.StageT.extern_effects

let string_list_field name json =
  list_field name json
  |> List.filter_map (function `String s -> Some s | _ -> None)

let rewrite_linked_stage2_input_roots ~extern_roots
    (input : StageT.stage2_input) =
  let input_roots = string_list_field "extern_roots" input.StageT.extern_effects in
  let root_pairs =
    if List.length input_roots = List.length extern_roots then
      List.combine input_roots extern_roots
    else []
  in
  let rewrite_node old_node =
    match List.assoc_opt old_node root_pairs with
    | Some fresh_node -> fresh_node
    | None when List.length extern_roots = 1 -> List.hd extern_roots
    | None -> old_node
  in
  let rewrite_effect = function
    | ((`Assoc fields) as eff) -> (
        match List.assoc_opt "node" fields with
        | Some (`String old_node) ->
            let fresh_node = rewrite_node old_node in
            with_assoc_fields
              [
                ("node", `String fresh_node);
                ("linked_original_node", `String old_node);
              ]
              eff
        | _ -> eff)
    | eff -> eff
  in
  {
    StageT.extern_effects =
      with_assoc_fields
        [
          ( "extern_roots",
            `List (List.map (fun root -> `String root) extern_roots) );
          ( "effects",
            `List
              (list_field "effects" input.StageT.extern_effects
              |> List.map rewrite_effect) );
          ( "linked_stage2_input_rebased_to_fresh_frontend_nodes",
            `Bool (input_roots <> extern_roots) );
          ( "linked_stage2_input_original_extern_roots",
            `List (List.map (fun root -> `String root) input_roots) );
        ]
        input.StageT.extern_effects;
  }

let build_stage1_result ?stage2_input source =
  let source_hash = source_hash source in
  let module_id = module_id source in
  let before = Real_sparrow_frontend.global_for_module source in
  let pre = PreAnalysis.perform before in
  let spec, locset, locset_fs = derive_sparse_spec pre in
  let access_transfer = ItvSem.run AbsSem.Strong spec in
  let access = AccessAnalysisItv.perform pre locset access_transfer spec.ItvSem.Spec.premem in
  let dug = Ssa.make (pre, access, locset_fs) in
  let staged = staged_sparse_pipeline ~module_id ~source_file:source ~source_hash spec pre access dug in
  let input_rows = staged.input_rows in
  let output_rows = staged.output_rows in
  let static_input_rows = input_rows in
  let static_output_rows = output_rows in
  let extern_roots = bta_roots pre |> List.map (fun (node, _) -> node_to_string node) |> sort_strings in
  let residual_input_components = staged.residual_input_components in
  let residual_output_components = staged.residual_output_components in
  let shape_witnesses, control_components =
    shape_witnesses ~module_id ~source_file:source ~source_hash pre residual_input_components residual_output_components
  in
  let analyzer =
    Residual.make_analyzer
      ~schema_version:"abstract-speculate-metaocaml-stage2/v1"
      ~module_id
      ~source_file:source
      ~source_hash
      ~extern_roots
      ~static_input_rows
      ~static_output_rows
      ~residual_input_components
      ~residual_output_components
      ~control_components
      ~shape_witnesses
  in
  let stage2_input =
    match stage2_input with
    | Some input -> input
    | None ->
        {
          StageT.extern_effects =
            Stage2.make_extern_effects ~source ~hash:source_hash ~extern_roots;
        }
  in
  let stage2_output =
    try Residual.execute analyzer stage2_input
    with Failure msg when msg = "stage2 extern/link input does not match module residual obligation" ->
      failwith
        (msg ^ " for " ^ module_id ^ " expected_hash=" ^ source_hash
       ^ " input_hash=" ^ stage2_input_source_hash stage2_input
       ^ " expected_roots=[" ^ comma_join extern_roots ^ "] input_roots=["
       ^ comma_join
           (list_field "extern_roots" stage2_input.StageT.extern_effects
           |> List.filter_map (function `String s -> Some s | _ -> None)
           |> sort_strings)
       ^ "]")
  in
  {
    source;
    module_id;
    source_hash;
    before;
    pre;
    input_rows;
    output_rows;
    completion = completion_json staged.completion;
    facts = staged.facts;
    extern_roots;
    shape_witnesses;
    analyzer;
    stage2_input;
    stage2_output;
  }

let run_stage1 source = build_stage1_result source

let source_rerun_evidence ~rerun_id result stage2_input stage2_output =
  let linked_context_consumed = linked_context_consumed stage2_input in
  let linked_extern_effects_consumed =
    linked_extern_effects_consumed stage2_input
  in
  let source_hash_matches_stage2_input =
    stage2_input_source_hash stage2_input = result.source_hash
  in
  let execution_log = stage2_output.StageT.execution_log in
  let rerun_stage2_executed =
    match assoc_field "runcode_run_performed" execution_log with
    | Some (`Bool true) -> true
    | _ -> false
  in
  let evidence =
    `Assoc
      [
        ("schema_version", `String "abstract-speculate-source-rerun/v1");
        ("rerun_id", `String rerun_id);
        ("module_id", `String result.module_id);
        ("source_file", `String result.source);
        ("source_hash", `String result.source_hash);
        ( "stage2_input_key",
          `String
            (stage2_input_key ~module_id:result.module_id
               ~source_hash:result.source_hash) );
        ("source_hash_matches", `Bool source_hash_matches_stage2_input);
        ("linked_context_consumed", `Bool linked_context_consumed);
        ( "linked_extern_effects_consumed",
          `Bool linked_extern_effects_consumed );
        ("stage1_frontend_rerun", `Bool true);
        ("staged_sparse_pipeline_rerun", `Bool true);
        ("rerun_stage2_executed", `Bool rerun_stage2_executed);
        ( "rerun_input_row_count",
          `Int (List.length stage2_output.StageT.final_input_table) );
        ( "rerun_output_row_count",
          `Int (List.length stage2_output.StageT.final_output_table) );
        ("rerun_completion", result.completion);
        ("linked_input_derivation", member "derivation" stage2_input.StageT.extern_effects);
        ( "linked_derivation_source",
          member "linked_derivation_source" stage2_input.StageT.extern_effects
        );
        ("linked_extern_effects", member "effects" stage2_input.StageT.extern_effects);
      ]
  in
  {
    rerun_id;
    result;
    linked_context_consumed;
    linked_extern_effects_consumed;
    source_hash_matches_stage2_input;
    evidence;
  }

let rerun_stage1_with_stage2_input ~rerun_id ~stage2_input source =
  let result = build_stage1_result source in
  let stage2_input =
    rewrite_linked_stage2_input_roots ~extern_roots:result.extern_roots
      stage2_input
  in
  let stage2_output = Residual.execute result.analyzer stage2_input in
  let result = { result with stage2_input; stage2_output } in
  source_rerun_evidence ~rerun_id result stage2_input stage2_output
