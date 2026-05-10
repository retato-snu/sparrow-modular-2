open Sparrow_modular_ocaml

let require_field name = function
  | `Assoc fields when List.mem_assoc name fields -> ()
  | _ -> failwith ("frontend report missing field " ^ name)

let () =
  let argv = Array.to_list Sys.argv in
  let fixture = Cli.arg_value "--fixture" argv "" |> Pipeline.fixture_of_string in
  let module_a = Cli.arg_value "--module-a" argv "" in
  let module_b = Cli.arg_value "--module-b" argv "" in
  let emit = Cli.arg_value "--emit" argv "_build/pipeline/frontend" in
  if module_a = "" then failwith "modular_frontend_runner requires --module-a";
  let path =
    try Pipeline.write_frontend ~fixture ~module_a ~module_b ~emit with exn ->
      (* Unsupported fixtures still leave a structured report before failing. *)
      raise exn
  in
  let json = Yojson.Safe.from_file path in
  List.iter (fun f -> require_field f json) ["modules"; "externs"; "exports"; "functions"; "cfg_nodes"; "cfg_edges"; "unsupported"];
  Pipeline.assert_frontend_fixture fixture json;
  Printf.printf "frontend: wrote %s\n" path
