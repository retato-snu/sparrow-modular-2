type case = Two_module_extern_loop | Dynamic_if_residual | Static_local

let case_of_string = function
  | "two_module_extern_loop" | "pipeline_extern_loop" -> Two_module_extern_loop
  | "dynamic_if_residual" | "pipeline_dynamic_if" -> Dynamic_if_residual
  | "pipeline_static_local" | "static_local" -> Static_local
  | s -> failwith ("unknown case: " ^ s)

let case_name = function
  | Two_module_extern_loop -> "two_module_extern_loop"
  | Dynamic_if_residual -> "dynamic_if_residual"
  | Static_local -> "pipeline_static_local"

let pipeline_fixture = function
  | Two_module_extern_loop -> Pipeline.Pipeline_extern_loop
  | Dynamic_if_residual -> Pipeline.Pipeline_dynamic_if
  | Static_local -> Pipeline.Pipeline_static_local

let case_of_pipeline = function
  | Pipeline.Pipeline_extern_loop -> Two_module_extern_loop
  | Pipeline.Pipeline_dynamic_if -> Dynamic_if_residual
  | Pipeline.Pipeline_static_local -> Static_local
  | Pipeline.Pipeline_unsupported_pointer -> failwith "unsupported fixture has no analysis case"

type module_inputs = { module_a : string; module_b : string }

let read_file = Pipeline.read_file
let contains_sub = Pipeline.contains_sub

let detect_case ~module_a source =
  Pipeline.detect_fixture ~module_a source |> case_of_pipeline

let parse_export_n source = match Pipeline.parse_export_n source with
  | Some n -> n
  | None -> failwith "module B must define n with an initializer"

let stage1 ?requested_case ?pipeline_artifacts inputs out_dir =
  let a_source = read_file inputs.module_a in
  let b_source = if inputs.module_b <> "" && Sys.file_exists inputs.module_b then read_file inputs.module_b else "" in
  begin match Pipeline.detect_unsupported ~file:inputs.module_a a_source with
  | Some u -> failwith ("unsupported " ^ u.Pipeline.construct ^ " in " ^ u.file)
  | None -> ()
  end;
  let case = detect_case ~module_a:inputs.module_a a_source in
  begin match requested_case with
  | Some expected when expected <> case ->
      failwith ("--case " ^ case_name expected ^ " does not match module A source " ^ case_name case)
  | _ -> ()
  end;
  let fixture = pipeline_fixture case in
  let n_value = match case with Static_local -> None | _ -> Some (parse_export_n b_source) in
  let b = {
    Summary.module_name = "b";
    cells = [];
    exports = (match n_value with Some n -> ["n", Product_value.of_int n] | None -> []);
    dependencies = [];
    pipeline_artifacts = (match pipeline_artifacts with Some p -> Some p | None -> Some (Pipeline.artifact_bundle fixture));
  } in
  let a_cell = match case with
    | Two_module_extern_loop ->
        { Summary.name = "return"; value = Residual.D (Residual.make ~id:"a.return.loop" ~shape:Residual.Loop ~artifact:"residuals/a_return_loop.ml" ~approx:(Product_value.top)) }
    | Dynamic_if_residual ->
        { Summary.name = "return"; value = Residual.D (Residual.make ~id:"a.return.if" ~shape:Residual.If ~artifact:"residuals/a_return_if.ml" ~approx:(Product_value.of_itv (Interval.of_bounds 0 1))) }
    | Static_local ->
        { Summary.name = "return"; value = Residual.S (Product_value.of_int 3) }
  in
  let a = {
    Summary.module_name = "a";
    cells = [a_cell];
    exports = [];
    dependencies = (match case with Static_local -> [] | _ -> [{ Summary.symbol = "n"; provider = Some "b" }]);
    pipeline_artifacts = (match pipeline_artifacts with Some p -> Some p | None -> Some (Pipeline.artifact_bundle fixture));
  } in
  [Summary.write out_dir a; Summary.write out_dir b]

let load_summaries dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".summary")
  |> List.map (fun f -> Summary.read (Filename.concat dir f))

let case_of_summaries summaries =
  let shapes =
    summaries |> List.concat_map (fun s ->
      s.Summary.cells |> List.filter_map (fun c -> match c.Summary.value with
        | Residual.D d -> Some d.Residual.shape
        | _ -> None))
  in
  if List.exists ((=) Residual.If) shapes then Dynamic_if_residual
  else if List.exists ((=) Residual.Loop) shapes then Two_module_extern_loop
  else Static_local

let stage2 summaries =
  let n = Summary.find_export "n" summaries in
  let env = match n with Some v -> ["n", v] | None -> [] in
  summaries |> List.concat_map (fun s ->
    s.Summary.cells |> List.map (fun c ->
      let value = match c.Summary.value with
        | Residual.S v -> v
        | Residual.D d -> Residual.execute d env
      in
      s.Summary.module_name ^ "." ^ c.Summary.name, value))

let expected = function
  | Two_module_extern_loop -> ["a.return", Product_value.of_int 3]
  | Dynamic_if_residual -> ["a.return", Product_value.of_int 1]
  | Static_local -> ["a.return", Product_value.of_int 3]

let relation case results =
  if List.length results = List.length (expected case)
     && List.for_all (fun (k, v) -> match List.assoc_opt k results with Some got -> Product_value.equal got v | None -> false) (expected case)
  then "equiv" else "diverge"
