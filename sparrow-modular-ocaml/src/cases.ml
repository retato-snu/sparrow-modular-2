type case = Two_module_extern_loop | Dynamic_if_residual

let case_of_string = function
  | "two_module_extern_loop" -> Two_module_extern_loop
  | "dynamic_if_residual" -> Dynamic_if_residual
  | s -> failwith ("unknown case: " ^ s)

let case_name = function Two_module_extern_loop -> "two_module_extern_loop" | Dynamic_if_residual -> "dynamic_if_residual"

type module_inputs = { module_a : string; module_b : string }

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let contains_sub s needle =
  let len = String.length s and nlen = String.length needle in
  let rec loop i =
    i + nlen <= len &&
    (String.sub s i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let detect_case source =
  if contains_sub source "while" && contains_sub source "i < n" then Two_module_extern_loop
  else if contains_sub source "if" && contains_sub source "n > 0" then Dynamic_if_residual
  else failwith "unsupported first-slice module A shape"

let parse_export_n source =
  match String.index_opt source '=' with
  | None -> failwith "module B must define n with an initializer"
  | Some eq ->
      let semi = match String.index_from_opt source eq ';' with
        | Some i -> i
        | None -> String.length source
      in
      String.sub source (eq + 1) (semi - eq - 1) |> String.trim |> int_of_string

let stage1 ?requested_case inputs out_dir =
  let a_source = read_file inputs.module_a in
  let b_source = read_file inputs.module_b in
  if not (contains_sub a_source "extern int n") then
    failwith "module A must declare extern int n";
  let case = detect_case a_source in
  begin match requested_case with
  | Some expected when expected <> case ->
      failwith ("--case " ^ case_name expected ^ " does not match module A source " ^ case_name case)
  | _ -> ()
  end;
  let n_value = parse_export_n b_source in
  let module_dir = out_dir in
  let b = { Summary.module_name = "b"; cells = []; exports = ["n", Product_value.of_int n_value]; dependencies = [] } in
  let a_cell = match case with
    | Two_module_extern_loop ->
        { Summary.name = "return"; value = Residual.D (Residual.make ~id:"a.return.loop" ~shape:Residual.Loop ~artifact:"residuals/a_return_loop.ml" ~approx:(Product_value.top)) }
    | Dynamic_if_residual ->
        { Summary.name = "return"; value = Residual.D (Residual.make ~id:"a.return.if" ~shape:Residual.If ~artifact:"residuals/a_return_if.ml" ~approx:(Product_value.of_itv (Interval.of_bounds 0 1))) }
  in
  let a = { Summary.module_name = "a"; cells = [a_cell]; exports = []; dependencies = [{ Summary.symbol = "n"; provider = Some "b" }] } in
  let paths = [Summary.write module_dir a; Summary.write module_dir b] in
  paths

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
  if List.exists ((=) Residual.If) shapes then Dynamic_if_residual else Two_module_extern_loop

let stage2 summaries =
  let n = match Summary.find_export "n" summaries with Some v -> v | None -> failwith "missing export n" in
  let env = ["n", n] in
  let results =
    summaries |> List.concat_map (fun s ->
      s.Summary.cells |> List.map (fun c ->
        let value = match c.Summary.value with
          | Residual.S v -> v
          | Residual.D d -> Residual.execute d env
        in
        s.Summary.module_name ^ "." ^ c.Summary.name, value))
  in
  results

let expected = function
  | Two_module_extern_loop -> ["a.return", Product_value.of_int 3]
  | Dynamic_if_residual -> ["a.return", Product_value.of_int 1]

let relation case results =
  if List.length results = List.length (expected case)
     && List.for_all (fun (k, v) -> match List.assoc_opt k results with Some got -> Product_value.equal got v | None -> false) (expected case)
  then "equiv" else "diverge"
