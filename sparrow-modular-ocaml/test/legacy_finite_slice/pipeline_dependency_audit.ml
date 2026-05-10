open Sparrow_modular_ocaml
let choose_existing paths = match List.find_opt Sys.file_exists paths with Some p -> p | None -> List.hd paths
let expect_failure label f = match (try f (); None with exn -> Some exn) with Some _ -> () | None -> failwith (label ^ " unexpectedly succeeded")
let () =
  let a = choose_existing ["test/fixtures/pipeline_extern_loop/a.c"; "fixtures/pipeline_extern_loop/a.c"] in
  let b = choose_existing ["test/fixtures/pipeline_extern_loop/b.c"; "fixtures/pipeline_extern_loop/b.c"] in
  let frontend = Pipeline.frontend_report ~fixture:Pipeline.Pipeline_extern_loop ~module_a:a ~module_b:b in
  let _, pre = Pipeline.preanalysis_report_from_frontend frontend in
  let _, dug = Pipeline.dug_report_from_preanalysis pre in
  let _, main = Pipeline.main_analysis_report_from_pipeline ~frontend ~preanalysis:pre ~dug in
  Pipeline.require_json_contains main "dynamic_residuals";
  let bad_frontend = `Assoc ["fixture", `String "pipeline_extern_loop"; "cfg_nodes", `List []; "externs", `List []; "unsupported", `List []] in
  expect_failure "preanalysis without frontend nodes/externs" (fun () -> ignore (Pipeline.preanalysis_report_from_frontend bad_frontend));
  let bad_pre = `Assoc ["fixture", `String "pipeline_extern_loop"; "static_facts", `List []; "dynamic_facts", `List []; "residual_obligations", `List []] in
  expect_failure "DUG without preanalysis facts" (fun () -> ignore (Pipeline.dug_report_from_preanalysis bad_pre));
  let bad_dug = `Assoc ["fixture", `String "pipeline_extern_loop"; "nodes", `List []; "edges", `List []] in
  expect_failure "main analysis without DUG edges" (fun () -> ignore (Pipeline.main_analysis_report_from_pipeline ~frontend ~preanalysis:pre ~dug:bad_dug));
  print_endline "pipeline_dependency_audit: PASS"
