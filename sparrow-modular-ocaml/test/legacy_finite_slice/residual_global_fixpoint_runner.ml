open Sparrow_modular_ocaml
let fixtures = [Pipeline.Pipeline_extern_loop; Pipeline.Pipeline_dynamic_if; Pipeline.Pipeline_static_local]
let () =
  let argv = Array.to_list Sys.argv in
  let root = Cli.arg_value "--summaries" argv "_build/pipeline/stage1" in
  let report = Cli.arg_value "--emit-report" argv "_build/reports/residual-global-fixpoint.json" in
  let reports = List.map (fun fixture ->
    let dir = Filename.concat root (Pipeline.fixture_name fixture) in
    let summaries = Cases.load_summaries dir in
    let results = Cases.stage2 summaries in
    Pipeline.residual_global_fixpoint_report ~fixture ~results) fixtures in
  Cli.write_file report (Yojson.Safe.pretty_to_string (`List reports));
  Printf.printf "residual-global-fixpoint: wrote %s\n" report
