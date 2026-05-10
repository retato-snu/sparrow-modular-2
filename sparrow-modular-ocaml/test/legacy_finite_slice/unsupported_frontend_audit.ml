open Sparrow_modular_ocaml
let () =
  let argv = Array.to_list Sys.argv in
  let module_a = Cli.arg_value "--module-a" argv "fixtures/pipeline_unsupported_pointer/a.c" in
  let module_b = Cli.arg_value "--module-b" argv "fixtures/pipeline_unsupported_pointer/b.c" in
  let emit = Cli.arg_value "--emit" argv "../_build/pipeline/frontend/pipeline_unsupported_pointer" in
  let failed =
    try ignore (Pipeline.write_frontend ~fixture:Pipeline.Pipeline_unsupported_pointer ~module_a ~module_b ~emit); false
    with _ -> true
  in
  if not failed then failwith "unsupported pointer was accepted";
  let report = Filename.concat emit "frontend.json" in
  if not (Sys.file_exists report) then failwith "unsupported report was not written";
  let json = Yojson.Safe.from_file report in
  begin match json with
  | `Assoc fields -> begin match List.assoc "unsupported" fields with
      | `List (_::_) -> print_endline "unsupported_frontend_audit: PASS"
      | _ -> failwith "unsupported list is empty"
    end
  | _ -> failwith "bad unsupported report"
  end
