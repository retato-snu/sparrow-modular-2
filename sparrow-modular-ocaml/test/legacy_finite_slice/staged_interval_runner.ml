open Sparrow_modular_ocaml
let () =
  let argv = Array.to_list Sys.argv in
  let requested_case = Cli.arg_value "--case" argv "" |> Cases.case_of_string in
  let module_a = Cli.arg_value "--module-a" argv "" in
  let module_b = Cli.arg_value "--module-b" argv "" in
  if module_a = "" || module_b = "" then
    failwith "stage1 requires --module-a and --module-b source files";
  let out_dir = Cli.arg_value "--emit-summary" argv "_build/stage1" in
  let paths = Cases.stage1 ~requested_case { Cases.module_a = module_a; module_b } out_dir in
  List.iter (Printf.printf "wrote %s\n") paths
