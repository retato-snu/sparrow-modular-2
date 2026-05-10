open Sparrow_modular_ocaml
let fixtures = [Pipeline.Pipeline_extern_loop; Pipeline.Pipeline_dynamic_if; Pipeline.Pipeline_static_local]
let require_field name = function `Assoc fields when List.mem_assoc name fields -> () | _ -> failwith ("dug missing field " ^ name)
let require_edges = function
  | `Assoc fields -> begin match List.assoc "edges" fields with `List [] -> failwith "DUG has no edges" | `List _ -> () | _ -> failwith "edges not a list" end
  | _ -> failwith "bad DUG json"
let () =
  let argv = Array.to_list Sys.argv in
  let preanalysis_dir = Cli.arg_value "--preanalysis-dir" argv "" in
  if preanalysis_dir = "" then failwith "dug audit requires --preanalysis-dir";
  let emit = Cli.arg_value "--emit-dug" argv "_build/pipeline/dug" in
  let paths = Pipeline.write_dug_from_preanalysis_dir ~preanalysis_dir ~emit in
  List.iter2 (fun fixture path ->
    let json = Yojson.Safe.from_file path in
    List.iter (fun f -> require_field f json) ["nodes"; "edges"];
    require_edges json;
    Pipeline.assert_dug_fixture fixture json;
    Printf.printf "dug: wrote %s\n" path) fixtures paths
