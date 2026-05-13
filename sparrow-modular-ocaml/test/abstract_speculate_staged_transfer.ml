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

let require_guard_mutation_gate branch_result =
  let changed_entries =
    execute_mutated branch_result (set_all_effect_values branch_result.MetaSparse.stage2_input (-7))
    |> output_entries
  in
  require_guard ~semantic:"guard x>0" ~value:false changed_entries;
  require_guard ~semantic:"guard !(x>0)" ~value:true changed_entries

let () =
  Sparrow_modular_ocaml.Sparrow_cil.initCIL ();
  let call_result =
    MetaSparse.run_stage1 "fixtures/abstract_speculate_pe/extern_dependent_call.c"
  in
  let components =
    call_result.MetaSparse.analyzer.T.residual_input_components @
    call_result.MetaSparse.analyzer.T.residual_output_components
  in
  expect (has_event "extern-call-result-created-D-during-transfer" components)
    "production transfer must create D for unknown extern call result";
  expect (has_event "dynamic-arithmetic-created-D-during-transfer" components)
    "production transfer must create D for arithmetic depending on extern result";
  let entries = execution_entries call_result in
  require_executed_semantic
    ~event:"extern-call-result-created-D-during-transfer"
    ~semantic:"call result" entries;
  require_executed_semantic
    ~event:"dynamic-arithmetic-created-D-during-transfer"
    ~semantic:"y := x+1" entries;
  require_value ~semantic:"call result" ~value:41 entries;
  require_value ~semantic:"x := tmp" ~value:41 entries;
  require_value ~semantic:"y := x+1" ~value:42 entries;
  require_stage2_input_mutation_gates call_result;
  expect (not (has_semantic_fragment "ordinal" components))
    "transfer residual semantics must not be ordinal-only witnesses";
  let branch_result =
    MetaSparse.run_stage1 "fixtures/abstract_speculate_metaocaml_sparse/dynamic_branch.c"
  in
  require_executed_semantic
    ~event:"dynamic-guard-created-D-during-transfer"
    ~semantic:"guard" (execution_entries branch_result);
  require_guard ~semantic:"guard x>0" ~value:true (execution_entries branch_result);
  require_guard ~semantic:"guard !(x>0)" ~value:false (execution_entries branch_result);
  require_guard_mutation_gate branch_result;
  print_endline "abstract_speculate_staged_transfer: PASS"
