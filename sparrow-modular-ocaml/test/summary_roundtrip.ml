open Sparrow_modular_ocaml

let dynamic_payloads json =
  match json with
  | `Assoc fields ->
      begin match List.assoc "cells" fields with
      | `List cells ->
          cells |> List.filter_map (function
            | `Assoc cell_fields ->
                begin match List.assoc "value" cell_fields with
                | `Assoc value_fields ->
                    begin match List.assoc "kind" value_fields with
                    | `String "dynamic" ->
                        let shape = List.assoc "shape" value_fields in
                        let source = List.assoc "source" value_fields in
                        let checksum = List.assoc "artifact_source_checksum" value_fields in
                        Some (shape, source, checksum)
                    | _ -> None
                    end
                | _ -> None
                end
            | _ -> None)
      | _ -> failwith "bad cells"
      end
  | _ -> failwith "bad summary JSON"

let corrupt_first_dynamic_source json =
  let changed = ref false in
  let corrupt_value = function
    | `Assoc value_fields as value when not !changed ->
        begin match List.assoc_opt "kind" value_fields with
        | Some (`String "dynamic") ->
            changed := true;
            `Assoc (List.map (fun (k, v) ->
              if k = "source" then (k, `String "corrupted residual source")
              else (k, v)) value_fields)
        | _ -> value
        end
    | value -> value
  in
  match json with
  | `Assoc fields ->
      `Assoc (List.map (fun (k, v) ->
        if k = "cells" then
          (k, match v with
            | `List cells ->
                `List (List.map (function
                  | `Assoc cell_fields ->
                      `Assoc (List.map (fun (ck, cv) ->
                        if ck = "value" then (ck, corrupt_value cv) else (ck, cv)) cell_fields)
                  | other -> other) cells)
            | other -> other)
        else (k, v)) fields)
  | other -> other

let expect_read_failure path =
  match (try ignore (Summary.read path); None with exn -> Some exn) with
  | Some _ -> ()
  | None -> failwith "corrupted D-code source/checksum was accepted"

let () =
  let argv = Array.to_list Sys.argv in
  let summary = Cli.arg_value "--summary" argv "" in
  let out = Cli.arg_value "--out" argv "" in
  let expect = Cli.arg_value "--expect-d-code" argv "" |> Cli.split_comma in
  let execute = Cli.has_flag "--execute-sample" argv in
  let original_json = Yojson.Safe.from_file summary in
  let s = Summary.read summary in
  let dyn = Summary.dynamic_cells s in
  List.iter (fun shape ->
    let ok = List.exists (fun c -> match c.Summary.value with Residual.D d -> Residual.shape_to_string d.shape = shape | _ -> false) dyn in
    if not ok then failwith ("missing expected D-code shape: " ^ shape)) expect;
  Cli.write_file out (Yojson.Safe.pretty_to_string (Summary.to_yojson s));
  let roundtrip_json = Yojson.Safe.from_file out in
  if dynamic_payloads original_json <> dynamic_payloads roundtrip_json then
    failwith "D-code payload changed during roundtrip";
  let corrupt_path = out ^ ".corrupt" in
  Cli.write_file corrupt_path (Yojson.Safe.pretty_to_string (corrupt_first_dynamic_source original_json));
  expect_read_failure corrupt_path;
  let s2 = Summary.read out in
  if List.length (Summary.dynamic_cells s2) <> List.length dyn then failwith "D-code lost during roundtrip";
  if execute then ignore (Cases.stage2 [s2; { Summary.module_name="b"; cells=[]; exports=["n", Product_value.of_int 3]; dependencies=[] }]);
  print_endline "summary_roundtrip: PASS"
