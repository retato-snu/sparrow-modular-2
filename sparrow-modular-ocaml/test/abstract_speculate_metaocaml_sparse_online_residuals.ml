let artifact_dir = ref "_build/real-sparrow/abstract-speculate-metaocaml-sparse/active"

let usage =
  "abstract_speculate_metaocaml_sparse_online_residuals --artifact-dir <dir>"

let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string

let failf fmt = Printf.ksprintf failwith fmt

let assoc_fields = function
  | `Assoc fields -> fields
  | json -> failf "expected JSON object, got %s" (Yojson.Safe.to_string json)

let field name json =
  match List.assoc_opt name (assoc_fields json) with
  | Some value -> value
  | None -> failf "missing field %s in %s" name (Yojson.Safe.to_string json)

let string_field name json = match field name json with `String s -> s | _ -> failf "field %s is not a string" name
let bool_field name json = match field name json with `Bool b -> b | _ -> failf "field %s is not a bool" name
let int_field name json = match field name json with `Int i -> i | _ -> failf "field %s is not an int" name
let list_field name json = match field name json with `List xs -> xs | _ -> failf "field %s is not a list" name
let opt_string name json = match List.assoc_opt name (assoc_fields json) with Some (`String s) -> Some s | _ -> None
let opt_int name json = match List.assoc_opt name (assoc_fields json) with Some (`Int n) -> Some n | _ -> None

let artifact_paths dir =
  let manifest = Yojson.Safe.from_file (Filename.concat dir "manifest.json") in
  match member "artifacts" manifest with
  | `List paths -> List.map to_string paths
  | _ ->
      Sys.readdir dir
      |> Array.to_list
      |> List.filter (fun name -> Filename.check_suffix name ".json" && name <> "manifest.json")
      |> List.map (Filename.concat dir)
      |> List.sort String.compare

let expect cond msg = if not cond then failwith msg

let residual_artifact artifact =
  artifact |> member "projection" |> member "residual_artifact"

let has_residual_with_shape shape residuals =
  List.exists (fun residual ->
    opt_string "shape" residual = Some shape ||
    opt_string "typed_shape" residual = Some shape ||
    opt_string "witness_constructor" residual = Some shape)
    residuals

let require_online_residual module_id residual =
  expect (string_field "artifact_kind" residual = "online-metaocaml-module-residual-analyzer")
    (module_id ^ ": residual artifact is not the online MetaOCaml analyzer");
  expect (bool_field "json_is_summary_only" residual = false)
    (module_id ^ ": residual JSON is still marked summary-only");
  expect (bool_field "metaocaml_online" residual)
    (module_id ^ ": residual artifact did not record online MetaOCaml generation");
  expect (string_field "residual_code_kind" residual = "Trx.code")
    (module_id ^ ": residual code is not recorded as Trx.code");
  expect (int_field "stage1_static_transfer_count" residual > 0)
    (module_id ^ ": no stage-1 static transfer evidence recorded");
  expect (bool_field "blind_equality_static_projection" residual)
    (module_id ^ ": no static-projection blind-equality evidence recorded");
  expect (bool_field "convergence_ignores_residual_code_structure" residual)
    (module_id ^ ": convergence evidence still depends on residual-code structure");
  let residuals = list_field "online_residuals" residual in
  expect (residuals <> []) (module_id ^ ": no online residual code entries recorded");
  expect (bool_field "staged_domain_fixpoint" residual)
    (module_id ^ ": residual artifact did not record staged-domain fixpoint");
  expect (not (bool_field "stage1_direct_sparse_pipeline" residual))
    (module_id ^ ": direct sparse pipeline is still accepted as PE proof");
  expect (bool_field "direct_sparse_oracle_only" residual)
    (module_id ^ ": direct sparse baseline was not restricted to oracle-only status");
  expect (not (bool_field "posthoc_row_split_used" residual))
    (module_id ^ ": post-hoc row split is still accepted");
  expect (not (bool_field "row_obligation_residual_source" residual))
    (module_id ^ ": row obligations are still residual source");
  expect (not (bool_field "whole_row_dynamic_wrapper" residual))
    (module_id ^ ": whole-row dynamic wrapper is still accepted");
  let execution_log = field "execution_log" residual in
  let extern_root_count = int_field "extern_root_count" execution_log in
  if extern_root_count > 0 then begin
    expect (int_field "transfer_level_d_site_count" residual > 0)
      (module_id ^ ": no transfer-level D evidence recorded");
    expect (int_field "staged_lattice_event_count" residual > 0)
      (module_id ^ ": no staged lattice evidence recorded");
    expect (int_field "typed_residual_arithmetic_count" residual > 0)
      (module_id ^ ": no typed residual arithmetic evidence recorded");
    expect (int_field "typed_residual_guard_count" residual > 0)
      (module_id ^ ": no typed residual guard evidence recorded")
  end;
  expect (not (bool_field "stage2_sparse_recompute" execution_log))
    (module_id ^ ": residual analyzer recomputed the sparse pipeline at stage 2");
  expect (bool_field "residual_solver_run" execution_log)
    (module_id ^ ": stage2 did not run residual equation solver");
  expect (bool_field "solver_backed_residual_fixpoint" execution_log)
    (module_id ^ ": stage2 is not solver-backed");
  expect (not (bool_field "overlay_only" execution_log))
    (module_id ^ ": stage2 used overlay-only shortcut");
  expect (bool_field "worklist_drained" execution_log)
    (module_id ^ ": residual solver worklist did not drain");
  expect (int_field "residual_equation_count" execution_log > 0)
    (module_id ^ ": no residual equations were materialized");
  expect (int_field "solver_iteration_count" execution_log > 1)
    (module_id ^ ": residual equations were not iterated to a fixpoint");
  expect (int_field "state_read_count" execution_log > 0)
    (module_id ^ ": dependent residual equations did not read solver state");
  expect (int_field "seed_input_read_count" execution_log > 0)
    (module_id ^ ": residual equations did not report dynamic input seed reads");
  expect (bool_field "equation_apply_reads_solver_state" execution_log)
    (module_id ^ ": solver log did not record state-reading equation applications");
  expect (list_field "exact_cell_dependencies" execution_log <> [])
    (module_id ^ ": residual solver did not report exact cell dependencies");
  if extern_root_count > 0 then begin
    expect (int_field "residual_input_obligation_count" residual = 0)
      (module_id ^ ": extern-dependent PE must not be proved by residual input row obligations");
    expect (int_field "residual_output_obligation_count" residual = 0)
      (module_id ^ ": extern-dependent PE must not be proved by residual output row obligations");
    expect (int_field "residual_input_component_count" residual + int_field "residual_output_component_count" residual > 0)
      (module_id ^ ": extern-dependent PE lacks staged component residuals")
  end;
  let executed_ids =
    list_field "executed_residuals" execution_log
    |> List.map (fun code -> string_field "id" code)
  in
  expect (opt_int "executed_residual_count" execution_log = Some (List.length executed_ids))
    (module_id ^ ": execution log did not count actual spliced residual executions");
  List.iter (fun code ->
    let id = string_field "id" code in
    expect (List.mem id executed_ids)
      (module_id ^ ": online residual " ^ id ^ " was not present in stage-2 executed_residuals");
    expect (string_field "code_kind" code = "Trx.code")
      (module_id ^ ": online residual entry is not Trx.code");
    expect (bool_field "typed_code_present" code)
      (module_id ^ ": online residual entry lacks typed code");
    expect (bool_field "runcode_executed" code)
      (module_id ^ ": online residual entry was not executed by Runcode.run");
    expect (string_field "execution_relation" code = "structural-equiv")
      (module_id ^ ": online residual execution did not match frozen Sparrow");
    expect (not (bool_field "stage2_sparse_recompute" code))
      (module_id ^ ": online residual component used stage-2 sparse recomputation");
    expect (string_field "granularity" code = "cell-component")
      (module_id ^ ": online residual is not cell/component granular");
    expect (not (bool_field "row_wrapper" code))
      (module_id ^ ": online residual used a row wrapper");
    expect (bool_field "residual_arithmetic_executed" code)
      (module_id ^ ": residual arithmetic did not execute");
    expect (bool_field "residual_guard_executed" code)
      (module_id ^ ": residual guard did not execute"))
    residuals;
  let base = Filename.basename module_id in
  if base = "dynamic_loop.c" || base = "widening_loop.c" then
    expect (has_residual_with_shape "Loop_shape" residuals)
      (module_id ^ ": loop fixture lacks typed Loop_shape residual");
  if base = "dynamic_branch.c" || base = "branch_join.c" then
    expect (has_residual_with_shape "Branch_shape" residuals)
      (module_id ^ ": branch fixture lacks typed Branch_shape residual")

let () =
  Arg.parse
    ["--artifact-dir", Arg.Set_string artifact_dir, "active artifact directory"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  let paths = artifact_paths !artifact_dir in
  expect (paths <> []) ("no artifacts found in " ^ !artifact_dir);
  List.iter (fun path ->
    let artifact = Yojson.Safe.from_file path in
    let module_id = string_field "module_id" artifact in
    require_online_residual module_id (residual_artifact artifact))
    paths;
  Printf.printf "abstract_speculate_metaocaml_sparse_online_residuals: PASS (%d artifacts)\n"
    (List.length paths)
