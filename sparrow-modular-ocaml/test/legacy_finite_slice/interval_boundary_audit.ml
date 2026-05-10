open Sparrow_modular_ocaml
let check_summary path =
  let s = Summary.read path in
  List.iter (fun c ->
    let v = Residual.ps_value c.Summary.value in
    ignore v.Product_value.itv;
    match v.Product_value.array_blk, v.Product_value.struct_blk with
    | Product_value.Unsupported a, Product_value.Unsupported b when a <> "" && b <> "" -> ()
    | _ -> failwith ("non-interval component is not explicit unsupported in " ^ path)) s.Summary.cells
let rec walk dir =
  Sys.readdir dir |> Array.iter (fun f ->
    let p = Filename.concat dir f in
    if Sys.is_directory p then walk p else if Filename.check_suffix p ".summary" then check_summary p)
let () =
  let argv = Array.to_list Sys.argv in
  let dir = Cli.arg_value "--summary-dir" argv "_build/pipeline/stage1" in
  walk dir;
  print_endline "interval_boundary_audit: PASS"
