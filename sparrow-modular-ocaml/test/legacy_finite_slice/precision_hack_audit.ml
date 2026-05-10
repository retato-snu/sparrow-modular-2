let read path = let ic = open_in path in let n = in_channel_length ic in let s = really_input_string ic n in close_in ic; s
let contains s needle = Sparrow_modular_ocaml.Pipeline.contains_sub s needle
let choose_existing paths = match List.find_opt Sys.file_exists paths with Some p -> p | None -> List.hd paths
let () =
  let argv = Array.to_list Sys.argv in
  let report = Sparrow_modular_ocaml.Cli.arg_value "--report" argv "" in
  if report <> "" && not (Sys.file_exists report) then failwith ("missing report: " ^ report);
  let residual_path = choose_existing ["src/residual.ml"; "../src/residual.ml"] in
  let residual_source = read residual_path in
  if contains residual_source "hardcoded-n=3" || contains residual_source "fixed_extra_unroll" then
    failwith "precision hack marker found";
  print_endline "precision_hack_audit: PASS (no fixed unroll marker or hardcoded n=3 marker)"
