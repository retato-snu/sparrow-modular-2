open Sparrow_modular_ocaml
let fixtures = [Pipeline.Pipeline_extern_loop; Pipeline.Pipeline_dynamic_if; Pipeline.Pipeline_static_local]
let module_path fixture name =
  let rel = Filename.concat (Pipeline.fixture_name fixture) name in
  let candidates = [Filename.concat "test/fixtures" rel; Filename.concat "fixtures" rel] in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> List.hd candidates
let require_field name = function `Assoc fields when List.mem_assoc name fields -> () | _ -> failwith ("main-analysis missing field " ^ name)
let () =
  let argv = Array.to_list Sys.argv in
  let preanalysis_dir = Cli.arg_value "--preanalysis-dir" argv "" in
  let dug_dir = Cli.arg_value "--dug-dir" argv "" in
  if preanalysis_dir = "" || dug_dir = "" then failwith "main analysis requires --preanalysis-dir and --dug-dir";
  let frontend_dir = Cli.arg_value "--frontend-dir" argv (Filename.concat (Filename.dirname preanalysis_dir) "frontend") in
  let emit_summary = Cli.arg_value "--emit-summary" argv "_build/pipeline/stage1" in
  List.iter (fun fixture ->
    let report_dir = Filename.concat emit_summary (Pipeline.fixture_name fixture) in
    let _, bundle = Pipeline.write_main_from_pipeline ~frontend_dir ~preanalysis_dir ~dug_dir ~emit_report:report_dir fixture in
    let paths = Cases.stage1 ~requested_case:(Cases.case_of_string (Pipeline.fixture_name fixture)) ~pipeline_artifacts:bundle { Cases.module_a = module_path fixture "a.c"; module_b = module_path fixture "b.c" } report_dir in
    let report = Yojson.Safe.from_file (Filename.concat report_dir ((Pipeline.fixture_name fixture) ^ ".main-analysis.json")) in
    List.iter (fun f -> require_field f report) ["static_transfers"; "dynamic_residuals"; "product_state"; "unsupported_components"];
    List.iter (Printf.printf "stage1: wrote %s\n") paths) fixtures
