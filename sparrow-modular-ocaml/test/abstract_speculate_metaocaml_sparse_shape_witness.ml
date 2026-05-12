let artifact_dir = ref "_build/real-sparrow/abstract-speculate-metaocaml-sparse/active"

let usage =
  "abstract_speculate_metaocaml_sparse_shape_witness --artifact-dir <dir>"

let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string

let failf fmt = Printf.ksprintf failwith fmt

let assoc_fields = function
  | `Assoc fields -> fields
  | json -> failf "expected object, got %s" (Yojson.Safe.to_string json)

let field name json =
  match List.assoc_opt name (assoc_fields json) with
  | Some value -> value
  | None -> failf "missing %s in %s" name (Yojson.Safe.to_string json)

let string_field name json = match field name json with `String s -> s | _ -> failf "%s is not a string" name
let bool_field name json = match field name json with `Bool b -> b | _ -> failf "%s is not a bool" name
let list_field name json = match field name json with `List xs -> xs | _ -> failf "%s is not a list" name
let opt_bool name json = match List.assoc_opt name (assoc_fields json) with Some (`Bool b) -> Some b | _ -> None
let opt_string name json = match List.assoc_opt name (assoc_fields json) with Some (`String s) -> Some s | _ -> None
let expect cond msg = if not cond then failwith msg

let artifact_paths dir =
  let manifest = Yojson.Safe.from_file (Filename.concat dir "manifest.json") in
  match member "artifacts" manifest with
  | `List paths -> List.map to_string paths
  | _ -> failf "manifest missing artifacts list in %s" dir

let require_witness module_id online_code_ids witness =
  let constructor = string_field "constructor" witness in
  expect (constructor = "Loop_shape" || constructor = "Branch_shape")
    (module_id ^ ": unsupported residual witness constructor " ^ constructor);
  expect (string_field "serialization_source" witness = "typed-witness")
    (module_id ^ ": shape witness was not serialized from typed witness data");
  expect (bool_field "paired_executable_code" witness)
    (module_id ^ ": witness lacks paired executable Trx.code");
  expect (opt_bool "source_text_scan_used" witness <> Some true)
    (module_id ^ ": witness was inferred by scanning residual source text");
  expect (opt_bool "flat_dependency_marker" witness <> Some true)
    (module_id ^ ": flat dependency marker was accepted as shape evidence");
  let code_id = string_field "residual_code_id" witness in
  expect (List.mem code_id online_code_ids)
    (module_id ^ ": witness residual_code_id has no paired online residual code")

let require_shape_witness artifact =
  let module_id = string_field "module_id" artifact in
  let residual = artifact |> member "projection" |> member "residual_artifact" in
  let online_code_ids =
    list_field "online_residuals" residual
    |> List.map (fun code -> string_field "id" code)
  in
  let witnesses = list_field "residual_shape_witnesses" residual in
  if Filename.basename module_id = "dynamic_loop.c" || Filename.basename module_id = "widening_loop.c" then
    expect (List.exists (fun w -> opt_string "constructor" w = Some "Loop_shape") witnesses)
      (module_id ^ ": dynamic loop fixture lacks Loop_shape witness");
  if Filename.basename module_id = "dynamic_branch.c" || Filename.basename module_id = "branch_join.c" then
    expect (List.exists (fun w -> opt_string "constructor" w = Some "Branch_shape") witnesses)
      (module_id ^ ": dynamic branch fixture lacks Branch_shape witness");
  List.iter (require_witness module_id online_code_ids) witnesses

let () =
  Arg.parse
    ["--artifact-dir", Arg.Set_string artifact_dir, "active artifact directory"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  let paths = artifact_paths !artifact_dir in
  expect (paths <> []) ("no artifacts found in " ^ !artifact_dir);
  List.iter (fun path -> require_shape_witness (Yojson.Safe.from_file path)) paths;
  Printf.printf "abstract_speculate_metaocaml_sparse_shape_witness: PASS (%d artifacts)\n"
    (List.length paths)
