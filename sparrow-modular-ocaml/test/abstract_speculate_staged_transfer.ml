module T = Sparrow_modular_ocaml.Abstract_speculate_stage_types
module MetaSparse = Sparrow_modular_ocaml.Abstract_speculate_meta_sparse
module Residual = Sparrow_modular_ocaml.Abstract_speculate_residual_value

let expect cond msg = if not cond then failwith msg

let has_event needle components =
  List.exists (fun c -> c.T.component_kind = "staged-abstract-cell" && c.T.transfer_event = needle) components

let has_semantic_fragment needle components =
  List.exists (fun c ->
    let json = T.component_to_yojson c |> Yojson.Safe.to_string in
    let len = String.length json and n = String.length needle in
    let rec loop i = i + n <= len && (String.sub json i n = needle || loop (i + 1)) in
    n = 0 || loop 0)
    components

let output_entries output =
  match output.T.execution_log |> Yojson.Safe.Util.member "executed_residuals" with
  | `List xs -> xs
  | _ -> []

let execution_entries result =
  output_entries result.MetaSparse.stage2_output

let execution_log result =
  result.MetaSparse.stage2_output.T.execution_log

let string_field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let bool_field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some (`Bool b) -> b | _ -> false)
  | _ -> false

let bool_equals name expected = function
  | `Assoc fields -> List.assoc_opt name fields = Some (`Bool expected)
  | _ -> false

let int_field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some (`Int n) -> n | _ -> min_int)
  | _ -> min_int

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let set_field name value = function
  | `Assoc fields -> `Assoc ((name, value) :: List.remove_assoc name fields)
  | json -> json

let remove_field name = function
  | `Assoc fields -> `Assoc (List.remove_assoc name fields)
  | json -> json

let map_field name f = function
  | `Assoc fields ->
      let value = match List.assoc_opt name fields with Some json -> f json | None -> f `Null in
      `Assoc ((name, value) :: List.remove_assoc name fields)
  | json -> json

let mutate_extern_effects input f =
  { T.extern_effects = f input.T.extern_effects }

let set_source_hash input hash =
  mutate_extern_effects input (set_field "source_hash" (`String hash))

let remove_first_extern_root input =
  mutate_extern_effects input
    (map_field "extern_roots" (function
      | `List (_ :: roots) -> `List roots
      | json -> json))

let set_effects input effects =
  mutate_extern_effects input (set_field "effects" effects)

let map_effects input f =
  mutate_extern_effects input
    (map_field "effects" (function
      | `List effects -> `List (List.map f effects)
      | json -> json))

let set_all_effect_values input value =
  map_effects input (set_field "value" (`Int value))

let remove_first_effect_field input field =
  mutate_extern_effects input
    (map_field "effects" (function
      | `List effects ->
          begin match effects with
          | eff :: rest -> `List (remove_field field eff :: rest)
          | [] -> `List []
          end
      | json -> json))

let non_int_effect_values input =
  map_effects input (set_field "value" (`String "not-an-int"))

let expect_raises label f =
  let raised =
    try ignore (f ()); false with _ -> true
  in
  expect raised (label ^ " should fail closed")

let execute_mutated result input =
  Residual.execute result.MetaSparse.analyzer input

let require_executed_semantic ~event ~semantic entries =
  expect
    (List.exists (fun entry ->
       string_field "component_kind" entry = "staged-abstract-cell" &&
       string_field "transfer_event" entry = event &&
       contains (string_field "semantic_expression" entry) semantic &&
       bool_field "typed_code_present" entry &&
       bool_field "residual_arithmetic_executed" entry &&
       bool_field "residual_guard_executed" entry &&
       not (bool_field "row_wrapper" entry))
       entries)
    ("missing executed staged semantic transfer event: " ^ event ^ " / " ^ semantic)

let require_value ~semantic ~value entries =
  expect
    (List.exists (fun entry ->
       contains (string_field "semantic_expression" entry) semantic &&
       int_field "residual_arithmetic_value" entry = value &&
       bool_field "stage2_semantics_applied" (Yojson.Safe.Util.member "executed_component" entry))
       entries)
    ("residual arithmetic for " ^ semantic ^ " did not compute " ^ string_of_int value)

let require_guard ~semantic ~value entries =
  expect
    (List.exists (fun entry ->
       contains (string_field "semantic_expression" entry) semantic &&
       bool_equals "residual_guard_value" value entry)
       entries)
    ("residual guard for " ^ semantic ^ " did not compute " ^ string_of_bool value)

let require_stage2_input_mutation_gates call_result =
  let base_input = call_result.MetaSparse.stage2_input in
  expect_raises "wrong source hash"
    (fun () -> execute_mutated call_result (set_source_hash base_input "wrong-source-hash"));
  expect_raises "missing extern root"
    (fun () -> execute_mutated call_result (remove_first_extern_root base_input));
  expect_raises "empty effects for extern roots"
    (fun () -> execute_mutated call_result (set_effects base_input (`List [])));
  expect_raises "non-list effects"
    (fun () -> execute_mutated call_result (set_effects base_input (`String "not-a-list")));
  expect_raises "effect missing node"
    (fun () -> execute_mutated call_result (remove_first_effect_field base_input "node"));
  expect_raises "effect missing value"
    (fun () -> execute_mutated call_result (remove_first_effect_field base_input "value"));
  expect_raises "effect with non-int value"
    (fun () -> execute_mutated call_result (non_int_effect_values base_input));
  let changed_entries =
    execute_mutated call_result (set_all_effect_values base_input 7)
    |> output_entries
  in
  require_value ~semantic:"call result" ~value:7 changed_entries;
  require_value ~semantic:"x := tmp" ~value:7 changed_entries;
  require_value ~semantic:"y := x+1" ~value:8 changed_entries

let require_solver_backed_stage2 result =
  let log = execution_log result in
  expect (bool_field "residual_solver_run" log)
    "stage2 must run residual solver";
  expect (bool_field "solver_backed_residual_fixpoint" log)
    "stage2 must report solver-backed residual fixpoint";
  expect (not (bool_field "overlay_only" log))
    "stage2 must not be overlay-only";
  expect (bool_field "worklist_drained" log)
    "residual solver worklist must drain";
  expect (int_field "residual_equation_count" log > 0)
    "stage2 must materialize residual equations";
  expect (int_field "solver_iteration_count" log > 1)
    "stage2 residual solver must iterate to a stable fixpoint, not execute once";
  expect (int_field "state_read_count" log > 0)
    "stage2 residual equations must read solver state";
  expect (int_field "seed_input_read_count" log > 0)
    "stage2 residual equations must report dynamic input seed reads";
  expect (bool_field "equation_apply_reads_solver_state" log)
    "stage2 residual equations did not report solver-state reads"

let require_guard_mutation_gate branch_result =
  let changed_entries =
    execute_mutated branch_result (set_all_effect_values branch_result.MetaSparse.stage2_input (-7))
    |> output_entries
  in
  require_guard ~semantic:"guard x>0" ~value:false changed_entries;
  require_guard ~semantic:"guard !(x>0)" ~value:true changed_entries


let all_components result =
  result.MetaSparse.analyzer.T.residual_input_components @
  result.MetaSparse.analyzer.T.residual_output_components @
  result.MetaSparse.analyzer.T.control_components

let json_contains needle json =
  contains (Yojson.Safe.to_string json) needle

let fact_command_kind ~node ~command_kind result =
  List.exists (fun fact ->
    string_field "node" fact = node && string_field "command_kind" fact = command_kind)
    result.MetaSparse.facts

let has_command_kind command_kind result =
  List.exists (fun fact -> string_field "command_kind" fact = command_kind) result.MetaSparse.facts

let has_transfer_event event result =
  List.exists (fun component -> component.T.transfer_event = event) (all_components result)

let matching_entries ~event ~semantic entries =
  List.filter (fun entry ->
    string_field "component_kind" entry = "staged-abstract-cell" &&
    string_field "transfer_event" entry = event &&
    contains (string_field "semantic_expression" entry) semantic &&
    bool_field "typed_code_present" entry &&
    bool_field "residual_arithmetic_executed" entry &&
    bool_field "residual_guard_executed" entry &&
    not (bool_field "row_wrapper" entry))
    entries

let require_command_provenance ~label ~command_kind ~event ~semantic result =
  let entries = execution_entries result |> matching_entries ~event ~semantic in
  expect
    (List.exists (fun entry ->
       fact_command_kind ~node:(string_field "node" entry) ~command_kind result)
       entries)
    (label ^ " missing command provenance " ^ command_kind ^ " for " ^ event ^ " / " ^ semantic)

let require_component_provenance ~label ~fixture ~event ~semantic result =
  expect (result.MetaSparse.source = fixture)
    (label ^ " source fixture mismatch: " ^ fixture);
  expect
    (List.exists (fun component ->
       component.T.transfer_event = event &&
       json_contains semantic (T.component_to_yojson component))
       (all_components result))
    (label ^ " missing component provenance for fixture " ^ fixture ^ " / " ^ event ^ " / " ^ semantic)

let require_no_shortcuts result =
  let components = all_components result in
  expect (not (has_semantic_fragment "ordinal" components))
    "transfer residual semantics must not be ordinal-only witnesses";
  let log = execution_log result in
  expect (not (bool_field "metadata_only_proof" log))
    "matrix evidence must not be metadata-only";
  expect (not (bool_field "whole_row_dynamic_wrapper" log))
    "matrix evidence must not be whole-row-wrapper-only";
  expect (not (bool_field "wrapper_reuse" log))
    "matrix evidence must not be wrapper-reuse-only"

type evidence =
  | Value of string * int
  | Guard of string * bool

type matrix_row = {
  label : string;
  fixture : string;
  command_kind : string;
  event : string;
  semantic : string;
  evidence : evidence list;
  mutation_gate : MetaSparse.stage1_result -> unit;
}

let require_evidence entries = function
  | Value (semantic, value) -> require_value ~semantic ~value entries
  | Guard (semantic, value) -> require_guard ~semantic ~value entries

let run_matrix_row result row =
  let entries = execution_entries result in
  require_component_provenance ~label:row.label ~fixture:row.fixture ~event:row.event ~semantic:row.semantic result;
  require_command_provenance ~label:row.label ~command_kind:row.command_kind ~event:row.event ~semantic:row.semantic result;
  require_executed_semantic ~event:row.event ~semantic:row.semantic entries;
  List.iter (require_evidence entries) row.evidence;
  row.mutation_gate result

let require_dynamic_chain_mutation result =
  let changed_entries =
    execute_mutated result (set_all_effect_values result.MetaSparse.stage2_input 7)
    |> output_entries
  in
  require_value ~semantic:"x := tmp" ~value:7 changed_entries;
  require_value ~semantic:"y := x+1" ~value:8 changed_entries

let require_known_dynamic_call_mutation result =
  let changed_entries =
    execute_mutated result (set_all_effect_values result.MetaSparse.stage2_input 7)
    |> output_entries
  in
  require_value ~semantic:"call result" ~value:7 changed_entries

let require_feasibility_blocker ~label ~fixture ~command_kind ~event result =
  expect (not (has_command_kind command_kind result))
    (label ^ " feasibility changed: " ^ command_kind ^ " is now fixture-reachable in " ^ fixture);
  expect (not (has_transfer_event event result))
    (label ^ " feasibility changed: " ^ event ^ " is now executable in " ^ fixture)

let fixture_path path =
  if Sys.file_exists path then path else Filename.concat "test" path

let run_fixture path =
  let path = fixture_path path in
  let result = MetaSparse.run_stage1 path in
  require_solver_backed_stage2 result;
  require_no_shortcuts result;
  result

let () =
  Sparrow_modular_ocaml.Sparrow_cil.initCIL ();
  let call_fixture = "fixtures/abstract_speculate_pe/extern_dependent_call.c" in
  let call_result = run_fixture call_fixture in
  let components = all_components call_result in
  expect (has_event "extern-call-result-created-D-during-transfer" components)
    "production transfer must create D for unknown extern call result";
  expect (has_event "dynamic-arithmetic-created-D-during-transfer" components)
    "production transfer must create D for arithmetic depending on extern result";
  run_matrix_row call_result {
    label = "Ccall unknown/imported";
    fixture = fixture_path call_fixture;
    command_kind = "Ccall";
    event = "extern-call-result-created-D-during-transfer";
    semantic = "call result";
    evidence = [Value ("call result", 41); Value ("x := tmp", 41)];
    mutation_gate = require_stage2_input_mutation_gates;
  };
  run_matrix_row call_result {
    label = "Cset dynamic chain";
    fixture = fixture_path call_fixture;
    command_kind = "Cset";
    event = "dynamic-arithmetic-created-D-during-transfer";
    semantic = "y := x+1";
    evidence = [Value ("x := tmp", 41); Value ("y := x+1", 42)];
    mutation_gate = require_dynamic_chain_mutation;
  };

  let known_call_fixture = "fixtures/abstract_speculate_pe/known_dynamic_call.c" in
  let known_call_result = run_fixture known_call_fixture in
  run_matrix_row known_call_result {
    label = "Ccall known dynamic";
    fixture = fixture_path known_call_fixture;
    command_kind = "Ccall";
    event = "dynamic-call-result-created-D-during-transfer";
    semantic = "call result";
    evidence = [Value ("call result", 41)];
    mutation_gate = require_known_dynamic_call_mutation;
  };

  let branch_fixture = "fixtures/abstract_speculate_metaocaml_sparse/dynamic_branch.c" in
  let branch_result = run_fixture branch_fixture in
  run_matrix_row branch_result {
    label = "Cassume dynamic guard true polarity";
    fixture = fixture_path branch_fixture;
    command_kind = "Cassume";
    event = "dynamic-guard-created-D-during-transfer";
    semantic = "guard x>0";
    evidence = [Guard ("guard x>0", true)];
    mutation_gate = require_guard_mutation_gate;
  };
  run_matrix_row branch_result {
    label = "Cassume dynamic guard false polarity";
    fixture = fixture_path branch_fixture;
    command_kind = "Cassume";
    event = "dynamic-guard-created-D-during-transfer";
    semantic = "guard !(x>0)";
    evidence = [Guard ("guard !(x>0)", false)];
    mutation_gate = require_guard_mutation_gate;
  };
  expect (not (has_command_kind "Cif" branch_result))
    "Cif feasibility changed: frontend no longer lowers branch residual guards exclusively to Cassume";

  let external_fixture = "fixtures/abstract_speculate_pe/external_global_read.c" in
  let external_result = MetaSparse.run_stage1 (fixture_path external_fixture) in
  require_feasibility_blocker
    ~label:"Cexternal direct external read"
    ~fixture:(fixture_path external_fixture)
    ~command_kind:"Cexternal"
    ~event:"extern-read-created-D-during-transfer"
    external_result;

  print_endline "abstract_speculate_staged_transfer: PASS"
