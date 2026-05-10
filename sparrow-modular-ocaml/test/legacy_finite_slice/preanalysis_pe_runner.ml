open Sparrow_modular_ocaml

let fixtures = [Pipeline.Pipeline_extern_loop; Pipeline.Pipeline_dynamic_if; Pipeline.Pipeline_static_local]
let require_field name = function `Assoc fields when List.mem_assoc name fields -> () | _ -> failwith ("preanalysis missing field " ^ name)
let () =
  let argv = Array.to_list Sys.argv in
  let emit = Cli.arg_value "--emit" argv "_build/pipeline/preanalysis" in
  let frontend_dir = Cli.arg_value "--frontend-dir" argv "" in
  if frontend_dir = "" then failwith "preanalysis requires --frontend-dir";
  let paths = Pipeline.write_preanalysis_from_frontend_dir ~frontend_dir ~emit in
  List.iter2 (fun fixture path ->
    let json = Yojson.Safe.from_file path in
    List.iter (fun f -> require_field f json) ["module"; "static_facts"; "dynamic_facts"; "residual_obligations"; "widening_events"];
    Pipeline.assert_preanalysis_fixture fixture json;
    Printf.printf "preanalysis: wrote %s\n" path) fixtures paths
