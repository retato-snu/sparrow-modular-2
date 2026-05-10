open Sparrow_modular_ocaml

let object_fields = function
  | `Assoc fields -> List.map fst fields
  | _ -> failwith "expected object"

let require_field where field json =
  if not (List.mem field (object_fields json)) then
    failwith (where ^ ": missing field " ^ field)

let () =
  let argv = Array.to_list Sys.argv in
  let summary = Cli.arg_value "--summary" argv "" in
  let expected = Cli.arg_value "--expect-components" argv "" |> Cli.split_comma in
  let json = Yojson.Safe.from_file summary in
  require_field "summary" "cells" json;
  let cells = match json with `Assoc fields -> List.assoc "cells" fields | _ -> assert false in
  let cells = match cells with `List xs -> xs | _ -> failwith "cells is not a list" in
  if cells = [] then failwith "summary has no cells to audit";
  List.iter (fun cell_json ->
    let value_json = match cell_json with `Assoc fields -> List.assoc "value" fields | _ -> failwith "bad cell" in
    let product_json = match value_json with
      | `Assoc fields -> begin match List.assoc "kind" fields with
          | `String "static" -> List.assoc "value" fields
          | `String "dynamic" -> List.assoc "approx" fields
          | _ -> failwith "unknown cell kind"
        end
      | _ -> failwith "bad value"
    in
    let field_of_component = function
      | "array" -> "array_blk"
      | "struct" -> "struct_blk"
      | other -> other
    in
    List.iter (fun component -> require_field "product value" (field_of_component component) product_json) expected) cells;
  let parsed = Summary.read summary in
  List.iter (fun c -> ignore (Residual.ps_value c.Summary.value).Product_value.array_blk) parsed.Summary.cells;
  print_endline "product_state_audit: PASS"
