open Sparrow_modular_ocaml
let () =
  let argv = Array.to_list Sys.argv in
  let summary = Cli.arg_value "--summary" argv "" in
  let expect_loop = Cli.has_flag "--expect-loop-d-code" argv in
  let expect_if = Cli.has_flag "--expect-if-d-code" argv in
  let forbid_flat = Cli.has_flag "--forbid-flat-d" argv in
  let s = Summary.read summary in
  let dyn = Summary.dynamic_cells s in
  if dyn = [] then failwith "no dynamic D-code cells found";
  List.iter (fun c -> match c.Summary.value with
    | Residual.D d ->
        if forbid_flat && (d.source = "" || d.artifact = "") then failwith "flat D marker detected";
        if expect_loop && d.shape <> Residual.Loop then failwith "expected loop-shaped D code";
        if expect_if && d.shape <> Residual.If then failwith "expected if-shaped D code";
        if not (Sys.file_exists (Filename.concat (Filename.dirname summary) d.artifact)) then failwith "residual artifact missing"
    | _ -> ()) dyn;
  print_endline "residual_shape_audit: PASS"
