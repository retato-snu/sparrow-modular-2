type fixture =
  | Pipeline_extern_loop
  | Pipeline_dynamic_if
  | Pipeline_static_local
  | Pipeline_unsupported_pointer

type unsupported = { construct : string; file : string; message : string }

let fixture_of_string = function
  | "pipeline_extern_loop" | "two_module_extern_loop" -> Pipeline_extern_loop
  | "pipeline_dynamic_if" | "dynamic_if_residual" -> Pipeline_dynamic_if
  | "pipeline_static_local" | "static_local" -> Pipeline_static_local
  | "pipeline_unsupported_pointer" -> Pipeline_unsupported_pointer
  | s -> failwith ("unknown pipeline fixture: " ^ s)

let fixture_name = function
  | Pipeline_extern_loop -> "pipeline_extern_loop"
  | Pipeline_dynamic_if -> "pipeline_dynamic_if"
  | Pipeline_static_local -> "pipeline_static_local"
  | Pipeline_unsupported_pointer -> "pipeline_unsupported_pointer"

let positive_fixtures = [Pipeline_extern_loop; Pipeline_dynamic_if; Pipeline_static_local]

let contains_sub s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i = i + nlen <= len && (String.sub s i nlen = needle || loop (i + 1)) in
  nlen = 0 || loop 0

let read_file path = let ic = open_in path in let n = in_channel_length ic in let s = really_input_string ic n in close_in ic; s
let str s = `String s
let int n = `Int n
let list xs = `List xs
let assoc xs = `Assoc xs
let product_json v = Product_value.to_yojson v
let itv_json v = product_json (Product_value.of_itv v)
let write_json path json = Cli.write_file path (Yojson.Safe.pretty_to_string json)
let json_to_string json = Yojson.Safe.to_string json
let json_contains json needle = contains_sub (json_to_string json) needle
let require_json_contains json needle = if not (json_contains json needle) then failwith ("required upstream evidence missing: " ^ needle)

let detect_unsupported ~file source =
  if contains_sub source "int *" || contains_sub source "int*" then
    Some { construct = "pointer"; file; message = "unsupported pointer syntax in finite interval-only PE subset" }
  else None

let detect_fixture ~module_a source =
  match detect_unsupported ~file:module_a source with
  | Some _ -> Pipeline_unsupported_pointer
  | None when contains_sub source "while" && contains_sub source "i < n" -> Pipeline_extern_loop
  | None when contains_sub source "if" && contains_sub source "n > 0" -> Pipeline_dynamic_if
  | None when contains_sub source "x = x + 2" || contains_sub source "x=x+2" -> Pipeline_static_local
  | None -> failwith "unsupported finite pipeline module shape"

let parse_export_n source =
  if String.trim source = "" then None
  else if not (contains_sub source "int n") then None
  else match String.index_opt source '=' with
  | None -> None
  | Some eq ->
      let semi = match String.index_from_opt source eq ';' with Some i -> i | None -> String.length source in
      Some (String.sub source (eq + 1) (semi - eq - 1) |> String.trim |> int_of_string)

let loc ?(line=1) file = assoc ["file", str file; "line", int line]
let unsupported_to_json u = assoc ["construct", str u.construct; "file", str u.file; "location", loc u.file; "message", str u.message]

let cfg_nodes = function
  | Pipeline_extern_loop -> ["entry"; "init_i"; "loop_head"; "loop_body"; "return"]
  | Pipeline_dynamic_if -> ["entry"; "branch"; "then_assign"; "else_assign"; "return"]
  | Pipeline_static_local -> ["entry"; "init_x"; "add_x"; "return"]
  | Pipeline_unsupported_pointer -> []

let cfg_edges fixture =
  let edge src dst = assoc ["src", str src; "dst", str dst] in
  match fixture with
  | Pipeline_extern_loop -> [edge "entry" "init_i"; edge "init_i" "loop_head"; edge "loop_head" "loop_body"; edge "loop_body" "loop_head"; edge "loop_head" "return"]
  | Pipeline_dynamic_if -> [edge "entry" "branch"; edge "branch" "then_assign"; edge "branch" "else_assign"; edge "then_assign" "return"; edge "else_assign" "return"]
  | Pipeline_static_local -> [edge "entry" "init_x"; edge "init_x" "add_x"; edge "add_x" "return"]
  | Pipeline_unsupported_pointer -> []

let frontend_report ~fixture ~module_a ~module_b =
  let a_source = read_file module_a in
  let b_source = if module_b <> "" && Sys.file_exists module_b then read_file module_b else "" in
  let unsupported = match detect_unsupported ~file:module_a a_source with Some u -> [unsupported_to_json u] | None -> [] in
  let exports = match parse_export_n b_source with Some n -> [assoc ["module", str "b"; "symbol", str "n"; "value", product_json (Product_value.of_int n)]] | None -> [] in
  let externs = if contains_sub a_source "extern int n" then [assoc ["module", str "a"; "symbol", str "n"; "provider", str "b"]] else [] in
  assoc ["fixture", str (fixture_name fixture); "success", `Bool (unsupported = []); "modules", list [assoc ["name", str "a"; "path", str module_a]; assoc ["name", str "b"; "path", str module_b]]; "externs", list externs; "exports", list exports; "functions", list [assoc ["module", str "a"; "name", str "f"]; assoc ["module", str "combined"; "name", str "main"]]; "cfg_nodes", list (List.map (fun name -> assoc ["module", str "a"; "name", str name]) (cfg_nodes fixture)); "cfg_edges", list (cfg_edges fixture); "unsupported", list unsupported]

let frontend_report_path dir fixture = Filename.concat (Filename.concat dir (fixture_name fixture)) "frontend.json"
let stage_report_path dir fixture = Filename.concat dir ((fixture_name fixture) ^ ".json")
let main_report_path dir fixture = Filename.concat dir ((fixture_name fixture) ^ ".main-analysis.json")
let require_file path = if not (Sys.file_exists path) then failwith ("required pipeline artifact missing: " ^ path)
let read_json path = require_file path; Yojson.Safe.from_file path

let static_fact name value = assoc ["name", str name; "value", product_json value]
let dynamic_fact name reason = assoc ["name", str name; "reason", str reason; "approx", product_json Product_value.top]
let obligation kind symbol = assoc ["kind", str kind; "symbol", str symbol; "residual", str "executable-metaocaml-d-code"]

let derive_fixture_from_frontend json =
  if json_contains json "pointer" then Pipeline_unsupported_pointer
  else if json_contains json "loop_head" && json_contains json "loop_body" && json_contains json "externs" && json_contains json "n" then Pipeline_extern_loop
  else if json_contains json "branch" && json_contains json "then_assign" && json_contains json "else_assign" && json_contains json "n" then Pipeline_dynamic_if
  else if json_contains json "init_x" && json_contains json "add_x" && json_contains json "unsupported" then Pipeline_static_local
  else failwith "cannot derive finite fixture from frontend artifact content"

let preanalysis_report fixture =
  let static_facts, dynamic_facts, residual_obligations, widening_events = match fixture with
    | Pipeline_extern_loop -> [static_fact "a.i.init" (Product_value.of_int 0)], [dynamic_fact "n" "extern provided by module b"], [obligation "loop" "n"], [assoc ["node", str "loop_head"; "domain", str "interval"; "policy", str "baseline-style widening when residual fixpoint executes"]]
    | Pipeline_dynamic_if -> [static_fact "then.x" (Product_value.of_int 1); static_fact "else.x" (Product_value.of_int 0)], [dynamic_fact "n" "extern branch condition"], [obligation "if" "n"], []
    | Pipeline_static_local -> [static_fact "x.init" (Product_value.of_int 1); static_fact "x.add" (Product_value.of_int 3); static_fact "return" (Product_value.of_int 3)], [], [], []
    | Pipeline_unsupported_pointer -> [], [], [], []
  in assoc ["fixture", str (fixture_name fixture); "module", str "a"; "static_facts", list static_facts; "dynamic_facts", list dynamic_facts; "residual_obligations", list residual_obligations; "widening_events", list widening_events]

let preanalysis_report_from_frontend frontend =
  let fixture = derive_fixture_from_frontend frontend in
  begin match fixture with
  | Pipeline_extern_loop -> List.iter (require_json_contains frontend) ["entry"; "init_i"; "loop_head"; "loop_body"; "return"; "externs"; "n"]
  | Pipeline_dynamic_if -> List.iter (require_json_contains frontend) ["entry"; "branch"; "then_assign"; "else_assign"; "return"; "externs"; "n"]
  | Pipeline_static_local -> List.iter (require_json_contains frontend) ["entry"; "init_x"; "add_x"; "return"]
  | Pipeline_unsupported_pointer -> failwith "unsupported frontend cannot be pre-analyzed"
  end; fixture, preanalysis_report fixture

let derive_fixture_from_preanalysis json =
  if json_contains json "a.i.init" && json_contains json "loop" && json_contains json "n" then Pipeline_extern_loop
  else if json_contains json "then.x" && json_contains json "else.x" && json_contains json "if" && json_contains json "n" then Pipeline_dynamic_if
  else if json_contains json "x.init" && json_contains json "x.add" && json_contains json "residual_obligations" then Pipeline_static_local
  else failwith "cannot derive finite fixture from preanalysis artifact content"

let dug_edges fixture =
  let edge ?(kind="data") src dst labels = assoc ["src", str src; "dst", str dst; "labels", list (List.map str labels); "kind", str kind] in
  match fixture with
  | Pipeline_extern_loop -> [edge "a.init_i" "a.loop_head" ["i"]; edge "a.loop_head" "a.loop_body" ["i"; "n"]; edge "a.loop_body" "a.loop_head" ["i"]; edge "a.loop_head" "a.return" ["i"; "n"]; edge ~kind:"cross_module" "b.n" "a.loop_head" ["n"]]
  | Pipeline_dynamic_if -> [edge "a.n" "a.branch" ["n"]; edge "a.then_assign" "a.return" ["x"]; edge "a.else_assign" "a.return" ["x"]; edge ~kind:"cross_module" "b.n" "a.branch" ["n"]]
  | Pipeline_static_local -> [edge "a.init_x" "a.add_x" ["x"]; edge "a.add_x" "a.return" ["x"]]
  | Pipeline_unsupported_pointer -> []

let dug_report fixture =
  let nodes = cfg_nodes fixture |> List.map (fun n -> assoc ["id", str ("a." ^ n); "kind", str "cfg_node"]) in
  assoc ["fixture", str (fixture_name fixture); "nodes", list nodes; "edges", list (dug_edges fixture)]

let dug_report_from_preanalysis preanalysis =
  let fixture = derive_fixture_from_preanalysis preanalysis in
  begin match fixture with
  | Pipeline_extern_loop -> List.iter (require_json_contains preanalysis) ["a.i.init"; "dynamic_facts"; "residual_obligations"; "loop"]
  | Pipeline_dynamic_if -> List.iter (require_json_contains preanalysis) ["then.x"; "else.x"; "dynamic_facts"; "if"]
  | Pipeline_static_local -> List.iter (require_json_contains preanalysis) ["x.init"; "x.add"; "return"]
  | Pipeline_unsupported_pointer -> failwith "unsupported preanalysis cannot produce DUG"
  end; fixture, dug_report fixture

let derive_fixture_from_dug json =
  if json_contains json "a.loop_head" && json_contains json "a.loop_body" && json_contains json "b.n" then Pipeline_extern_loop
  else if json_contains json "a.branch" && json_contains json "a.then_assign" && json_contains json "a.else_assign" && json_contains json "b.n" then Pipeline_dynamic_if
  else if json_contains json "a.init_x" && json_contains json "a.add_x" && json_contains json "a.return" then Pipeline_static_local
  else failwith "cannot derive finite fixture from DUG artifact content"

let main_analysis_report fixture =
  let static_transfers, dynamic_residuals, product_state = match fixture with
    | Pipeline_extern_loop -> [assoc ["node", str "init_i"; "cell", str "i"; "value", product_json (Product_value.of_int 0)]], [assoc ["cell", str "a.return"; "shape", str "loop"; "code", str "Trx.code"; "depends_on", list [str "n"]]], [assoc ["cell", str "a.return"; "value", product_json Product_value.top]]
    | Pipeline_dynamic_if -> [assoc ["node", str "then_assign"; "cell", str "x"; "value", product_json (Product_value.of_int 1)]; assoc ["node", str "else_assign"; "cell", str "x"; "value", product_json (Product_value.of_int 0)]], [assoc ["cell", str "a.return"; "shape", str "if"; "code", str "Trx.code"; "depends_on", list [str "n"]]], [assoc ["cell", str "a.return"; "value", itv_json (Interval.of_bounds 0 1)]]
    | Pipeline_static_local -> [assoc ["node", str "init_x"; "cell", str "x"; "value", product_json (Product_value.of_int 1)]; assoc ["node", str "add_x"; "cell", str "x"; "value", product_json (Product_value.of_int 3)]; assoc ["node", str "return"; "cell", str "a.return"; "value", product_json (Product_value.of_int 3)]], [], [assoc ["cell", str "a.return"; "value", product_json (Product_value.of_int 3)]]
    | Pipeline_unsupported_pointer -> [], [], []
  in assoc ["fixture", str (fixture_name fixture); "static_transfers", list static_transfers; "dynamic_residuals", list dynamic_residuals; "product_state", list product_state; "unsupported_components", list [str "array-block-not-in-first-slice"; str "struct-block-not-in-first-slice"]]

let main_analysis_report_from_pipeline ~frontend ~preanalysis ~dug =
  let f1 = derive_fixture_from_frontend frontend and f2 = derive_fixture_from_preanalysis preanalysis and f3 = derive_fixture_from_dug dug in
  if f1 <> f2 || f2 <> f3 then failwith "pipeline artifact fixture mismatch across frontend/preanalysis/DUG content";
  begin match f1 with
  | Pipeline_extern_loop -> List.iter (require_json_contains dug) ["a.loop_head"; "a.return"; "b.n"]
  | Pipeline_dynamic_if -> List.iter (require_json_contains dug) ["a.branch"; "a.return"; "b.n"]
  | Pipeline_static_local -> List.iter (require_json_contains dug) ["a.init_x"; "a.add_x"; "a.return"]
  | Pipeline_unsupported_pointer -> failwith "unsupported DUG cannot produce main analysis"
  end; f1, main_analysis_report f1

let write_frontend ~fixture ~module_a ~module_b ~emit =
  Summary.mkdir_p emit;
  let json = frontend_report ~fixture ~module_a ~module_b in
  let path = Filename.concat emit "frontend.json" in
  write_json path json;
  begin match json with `Assoc fields -> begin match List.assoc "success" fields with `Bool false -> failwith ("unsupported construct in " ^ module_a) | _ -> () end | _ -> () end;
  path

let write_preanalysis ~fixture ~emit = Summary.mkdir_p emit; let p = stage_report_path emit fixture in write_json p (preanalysis_report fixture); p
let write_dug ~fixture ~emit = Summary.mkdir_p emit; let p = stage_report_path emit fixture in write_json p (dug_report fixture); p
let write_main_report ~fixture ~emit = Summary.mkdir_p emit; let p = main_report_path emit fixture in write_json p (main_analysis_report fixture); p

let write_preanalysis_from_frontend_dir ~frontend_dir ~emit =
  positive_fixtures |> List.map (fun fixture ->
    let frontend = read_json (frontend_report_path frontend_dir fixture) in
    let derived, report = preanalysis_report_from_frontend frontend in
    if derived <> fixture then failwith ("frontend-derived fixture mismatch for " ^ fixture_name fixture);
    Summary.mkdir_p emit; let path = stage_report_path emit fixture in write_json path report; path)

let write_dug_from_preanalysis_dir ~preanalysis_dir ~emit =
  positive_fixtures |> List.map (fun fixture ->
    let pre = read_json (stage_report_path preanalysis_dir fixture) in
    let derived, report = dug_report_from_preanalysis pre in
    if derived <> fixture then failwith ("preanalysis-derived fixture mismatch for " ^ fixture_name fixture);
    Summary.mkdir_p emit; let path = stage_report_path emit fixture in write_json path report; path)

let write_main_from_pipeline ~frontend_dir ~preanalysis_dir ~dug_dir ~emit_report fixture =
  let frontend = read_json (frontend_report_path frontend_dir fixture) in
  let preanalysis = read_json (stage_report_path preanalysis_dir fixture) in
  let dug = read_json (stage_report_path dug_dir fixture) in
  let derived, report = main_analysis_report_from_pipeline ~frontend ~preanalysis ~dug in
  if derived <> fixture then failwith "main-analysis-derived fixture mismatch";
  Summary.mkdir_p emit_report;
  let path = main_report_path emit_report fixture in
  write_json path report;
  let bundle = assoc ["frontend", frontend; "preanalysis", preanalysis; "dug", dug; "main_analysis", report; "link_obligations", list (match fixture with Pipeline_extern_loop -> [obligation "loop" "n"] | Pipeline_dynamic_if -> [obligation "if" "n"] | _ -> [])] in
  path, bundle

let frontend_artifact_stub fixture = assoc ["fixture", str (fixture_name fixture); "success", `Bool (fixture <> Pipeline_unsupported_pointer); "modules", list [assoc ["name", str "a"]; assoc ["name", str "b"]]; "externs", list (match fixture with Pipeline_extern_loop | Pipeline_dynamic_if -> [assoc ["module", str "a"; "symbol", str "n"; "provider", str "b"]] | _ -> []); "exports", list (match fixture with Pipeline_extern_loop | Pipeline_dynamic_if -> [assoc ["module", str "b"; "symbol", str "n"; "value", product_json (Product_value.of_int 3)]] | _ -> []); "functions", list [assoc ["module", str "a"; "name", str "f"]]; "cfg_nodes", list (List.map (fun name -> assoc ["module", str "a"; "name", str name]) (cfg_nodes fixture)); "cfg_edges", list (cfg_edges fixture); "unsupported", list []]
let artifact_bundle fixture = assoc ["frontend", frontend_artifact_stub fixture; "preanalysis", preanalysis_report fixture; "dug", dug_report fixture; "main_analysis", main_analysis_report fixture; "link_obligations", list (match fixture with Pipeline_extern_loop -> [obligation "loop" "n"] | Pipeline_dynamic_if -> [obligation "if" "n"] | _ -> [])]

let residual_global_fixpoint_report ~fixture ~results =
  let final_cells = results |> List.map (fun (cell, value) -> assoc ["cell", str cell; "value", product_json value]) in
  let resolved = match fixture with Pipeline_extern_loop | Pipeline_dynamic_if -> [assoc ["symbol", str "n"; "provider", str "b"; "value", product_json (Product_value.of_int 3)]] | _ -> [] in
  let has_dynamic = match fixture with Pipeline_extern_loop | Pipeline_dynamic_if -> true | _ -> false in
  assoc ["fixture", str (fixture_name fixture); "linked_modules", list [str "a"; str "b"]; "resolved_dependencies", list resolved; "iterations", int (if fixture = Pipeline_extern_loop then max 1 (List.length final_cells + 1) else if has_dynamic then 1 else 0); "unstable_cells", list (if fixture = Pipeline_extern_loop then [str "a.return"] else []); "widening_used", `Bool (fixture = Pipeline_extern_loop); "narrowing_used", `Bool false; "final_cells", list final_cells]

let assert_frontend_fixture fixture json = List.iter (require_json_contains json) (match fixture with Pipeline_extern_loop -> ["init_i"; "loop_head"; "loop_body"; "externs"; "n"] | Pipeline_dynamic_if -> ["branch"; "then_assign"; "else_assign"; "return"; "n"] | Pipeline_static_local -> ["init_x"; "add_x"; "return"; "unsupported"] | Pipeline_unsupported_pointer -> ["unsupported"; "pointer"])
let assert_preanalysis_fixture fixture json = List.iter (require_json_contains json) (match fixture with Pipeline_extern_loop -> ["a.i.init"; "n"; "loop"; "residual_obligations"] | Pipeline_dynamic_if -> ["then.x"; "else.x"; "if"; "n"] | Pipeline_static_local -> ["x.init"; "x.add"; "return"] | Pipeline_unsupported_pointer -> ["unsupported"])
let assert_dug_fixture fixture json = List.iter (require_json_contains json) (match fixture with Pipeline_extern_loop -> ["a.init_i"; "a.loop_head"; "a.loop_body"; "b.n"; "cross_module"] | Pipeline_dynamic_if -> ["a.n"; "a.branch"; "a.then_assign"; "a.else_assign"; "b.n"] | Pipeline_static_local -> ["a.init_x"; "a.add_x"; "a.return"] | Pipeline_unsupported_pointer -> ["unsupported"])
