open Sparrow_modular_ocaml
let json_of_results ?baseline_output relation results =
  `Assoc [
    "relation", `String relation;
    "baseline_output", (match baseline_output with
      | None -> `Null
      | Some path -> `Assoc ["path", `String path; "available", `Bool true]);
    "results", `List (List.map (fun (k,v) -> `Assoc ["cell", `String k; "value", Product_value.to_yojson v]) results)
  ]

let require_existing_file flag path =
  if path <> "" && not (Sys.file_exists path) then
    failwith (flag ^ " does not exist: " ^ path)

let () =
  let argv = Array.to_list Sys.argv in
  let dir = Cli.arg_value "--summaries" argv "" in
  let expect = Cli.arg_value "--expect-relation" argv "equiv" in
  let compare = Cli.arg_value "--compare" argv "" in
  let baseline_output = Cli.arg_value "--baseline-output" argv "" in
  require_existing_file "--baseline-output" baseline_output;
  let summaries = Cases.load_summaries dir in
  let case = Cases.case_of_summaries summaries in
  let results = Cases.stage2 summaries in
  let relation = Cases.relation case results in
  let baseline_output = if baseline_output = "" then None else Some baseline_output in
  if compare <> "" then Cli.write_file compare (Yojson.Safe.pretty_to_string (json_of_results ?baseline_output relation results));
  if relation <> expect then failwith ("relation " ^ relation ^ " != expected " ^ expect);
  print_endline ("stage2_linker: " ^ relation)
