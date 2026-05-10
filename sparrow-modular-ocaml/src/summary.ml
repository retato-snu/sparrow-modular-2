type cell = { name : string; value : Residual.ps }
type dependency = { symbol : string; provider : string option }
type t = { module_name : string; cells : cell list; exports : (string * Product_value.t) list; dependencies : dependency list }

let rec mkdir_p path =
  if path <> "" && path <> "." && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let checksum s = Digest.to_hex (Digest.string s)
let write_file path content = let oc = open_out path in output_string oc content; close_out oc
let read_file path = let ic = open_in path in let n = in_channel_length ic in let s = really_input_string ic n in close_in ic; s

let dynamic_to_json d =
  let artifact_source_checksum = checksum d.Residual.source in
  `Assoc [
    "kind", `String "dynamic";
    "id", `String d.id;
    "shape", `String (Residual.shape_to_string d.shape);
    "approx", Product_value.to_yojson d.approx;
    "artifact", `String d.artifact;
    "artifact_source_checksum", `String artifact_source_checksum;
    "source", `String d.source
  ]

let cell_to_json c =
  let v = match c.value with
    | Residual.S v -> `Assoc ["kind", `String "static"; "value", Product_value.to_yojson v]
    | Residual.D d -> dynamic_to_json d
  in
  `Assoc ["name", `String c.name; "value", v]

let export_to_json (name, v) = `Assoc ["name", `String name; "value", Product_value.to_yojson v]
let dep_to_json d = `Assoc ["symbol", `String d.symbol; "provider", (match d.provider with None -> `Null | Some p -> `String p)]

let to_yojson s =
  `Assoc [
    "format", `String "sparrow-modular-stage1-summary-v1";
    "module", `String s.module_name;
    "cells", `List (List.map cell_to_json s.cells);
    "exports", `List (List.map export_to_json s.exports);
    "dependencies", `List (List.map dep_to_json s.dependencies)
  ]

let dynamic_of_json fields =
  let get name = List.assoc name fields in
  let id = match get "id" with `String s -> s | _ -> failwith "bad id" in
  let shape = match get "shape" with `String s -> Residual.shape_of_string s | _ -> failwith "bad shape" in
  let artifact = match get "artifact" with `String s -> s | _ -> failwith "bad artifact" in
  let source = match get "source" with `String s -> s | _ -> failwith "bad residual source" in
  let stored_checksum = match get "artifact_source_checksum" with `String s -> s | _ -> failwith "bad residual checksum" in
  let actual_checksum = checksum source in
  if actual_checksum <> stored_checksum then
    failwith ("residual source checksum mismatch for " ^ id);
  if source <> Residual.source_for_shape shape then
    failwith ("residual source is not supported by the executable generator for " ^ id);
  let approx = Product_value.of_yojson (get "approx") in
  Residual.D (Residual.make_with_source ~id ~shape ~artifact ~approx ~source)

let cell_of_json = function
  | `Assoc fields ->
      let name = match List.assoc "name" fields with `String s -> s | _ -> failwith "bad cell name" in
      let value = match List.assoc "value" fields with
        | `Assoc vf -> begin match List.assoc "kind" vf with
            | `String "static" -> Residual.S (Product_value.of_yojson (List.assoc "value" vf))
            | `String "dynamic" -> dynamic_of_json vf
            | _ -> failwith "bad value kind"
          end
        | _ -> failwith "bad cell value"
      in
      { name; value }
  | _ -> failwith "bad cell"

let export_of_json = function
  | `Assoc fields ->
      let name = match List.assoc "name" fields with `String s -> s | _ -> failwith "bad export name" in
      let value = Product_value.of_yojson (List.assoc "value" fields) in
      (name, value)
  | _ -> failwith "bad export"

let dep_of_json = function
  | `Assoc fields ->
      let symbol = match List.assoc "symbol" fields with `String s -> s | _ -> failwith "bad dep symbol" in
      let provider = match List.assoc "provider" fields with `Null -> None | `String s -> Some s | _ -> failwith "bad provider" in
      { symbol; provider }
  | _ -> failwith "bad dep"

let of_yojson = function
  | `Assoc fields ->
      let module_name = match List.assoc "module" fields with `String s -> s | _ -> failwith "bad module" in
      let list f = function `List xs -> List.map f xs | _ -> failwith "bad list" in
      { module_name;
        cells = list cell_of_json (List.assoc "cells" fields);
        exports = list export_of_json (List.assoc "exports" fields);
        dependencies = list dep_of_json (List.assoc "dependencies" fields) }
  | _ -> failwith "bad summary"

let write dir s =
  mkdir_p dir;
  let artifacts = Filename.concat dir "residuals" in
  mkdir_p artifacts;
  List.iter (fun c -> match c.value with
    | Residual.D d -> write_file (Filename.concat dir d.artifact) d.source
    | _ -> ()) s.cells;
  let path = Filename.concat dir (s.module_name ^ ".summary") in
  write_file path (Yojson.Safe.pretty_to_string (to_yojson s));
  path

let read path = Yojson.Safe.from_file path |> of_yojson

let dynamic_cells s = List.filter (fun c -> Residual.is_dynamic c.value) s.cells
let find_export name summaries =
  summaries |> List.find_map (fun s -> List.assoc_opt name s.exports)
