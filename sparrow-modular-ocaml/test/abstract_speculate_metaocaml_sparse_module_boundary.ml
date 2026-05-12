let artifact_dir = ref "_build/real-sparrow/abstract-speculate-metaocaml-sparse/active"

let usage =
  "abstract_speculate_metaocaml_sparse_module_boundary --artifact-dir <dir>"

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
let int_field name json = match field name json with `Int i -> i | _ -> failf "%s is not an int" name
let list_field name json = match field name json with `List xs -> xs | _ -> failf "%s is not a list" name
let expect cond msg = if not cond then failwith msg

let artifact_paths dir =
  let manifest = Yojson.Safe.from_file (Filename.concat dir "manifest.json") in
  match member "artifacts" manifest with
  | `List paths -> List.map to_string paths
  | _ -> failf "manifest missing artifacts list in %s" dir

let require_no_sibling_facts module_id source hash provenance =
  expect (string_field "status" provenance = "pass") (module_id ^ ": provenance did not pass");
  expect (int_field "forbidden_sibling_fact_count" provenance = 0)
    (module_id ^ ": sibling facts leaked into module-local pre-link proof");
  list_field "facts" provenance
  |> List.iter (fun fact ->
       expect (string_field "source_file" fact = source)
         (module_id ^ ": fact source_file is not the module source");
       expect (string_field "source_hash" fact = hash)
         (module_id ^ ": fact source_hash is not the module hash");
       expect (bool_field "depends_on_external_module" fact = false || string_field "origin" fact = "module-local-extern-obligation")
         (module_id ^ ": external dependency was not recorded as a module-local extern obligation"))

let require_module_local_boundary artifact =
  let module_id = string_field "module_id" artifact in
  let source = string_field "source" artifact in
  let hash = string_field "source_hash" artifact in
  let boundary = member "module_boundary" artifact in
  expect (string_field "status" boundary = "pass") (module_id ^ ": module boundary did not pass");
  expect (bool_field "single_module_input" boundary) (module_id ^ ": did not use single-module input");
  expect (string_field "parser_entrypoint" boundary = "Real_sparrow_frontend.parse_one_file")
    (module_id ^ ": parser entrypoint is not parse_one_file");
  expect (string_field "global_entrypoint" boundary = "Real_sparrow_frontend.global_for_module")
    (module_id ^ ": global entrypoint is not global_for_module");
  expect (bool_field "linked_entrypoints_used" boundary = false)
    (module_id ^ ": linked entrypoints were used before link");
  expect (list_field "sibling_module_paths" boundary = [])
    (module_id ^ ": sibling modules were admitted into pre-link boundary");
  let forbidden = list_field "forbidden_prelink_entrypoints" boundary |> List.map to_string in
  List.iter (fun name ->
    expect (List.mem name forbidden) (module_id ^ ": missing forbidden pre-link entrypoint " ^ name))
    ["Real_sparrow_frontend.global_for_files"; "Mergecil.merge"];
  require_no_sibling_facts module_id source hash (member "fact_provenance" artifact)

let () =
  Arg.parse
    ["--artifact-dir", Arg.Set_string artifact_dir, "active artifact directory"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  let paths = artifact_paths !artifact_dir in
  expect (paths <> []) ("no artifacts found in " ^ !artifact_dir);
  List.iter (fun path -> require_module_local_boundary (Yojson.Safe.from_file path)) paths;
  Printf.printf "abstract_speculate_metaocaml_sparse_module_boundary: PASS (%d artifacts)\n"
    (List.length paths)
