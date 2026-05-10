let active_dir = ref ""
let reference_dir = ref ""
let report = ref ""
let usage = "real_sparrow_access_dug_compare --active <dir> --reference <dir> --report <json>"

let read_manifest dir =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file (Filename.concat dir "manifest.json")
  |> member "modules" |> to_list |> List.map to_string

let member name json = Yojson.Safe.Util.member name json

let projection json =
  `Assoc [
    "schema_version", member "schema_version" json;
    "source_basename", `String (Filename.basename (json |> member "source" |> Yojson.Safe.Util.to_string));
    "scope", member "scope" json;
    "boundary", member "boundary" json;
    "lineage", member "lineage" json;
    "projection", member "projection" json;
    "non_claims", member "non_claims" json;
  ]

let projection_member name json =
  json |> member "projection" |> member name

let relation a b = if Yojson.Safe.equal a b then "structural-equiv" else "structural-diverge"

let bta_facts json =
  match projection_member "bta" json with
  | `List xs -> xs
  | _ -> []

let field name fields = List.assoc_opt name fields

let is_dug_kind = function
  | `String k -> String.length k >= 4 && String.sub k 0 4 = "dug-"
  | _ -> false

let classification = function
  | `Assoc fields -> field "classification" fields
  | _ -> None

let depends_on_extern = function
  | `Assoc fields -> field "depends_on_extern" fields = Some (`Bool true)
  | _ -> false

let bta_accepted_for_source source json =
  let facts = bta_facts json in
  let dug_edges_static =
    facts |> List.for_all (function
      | `Assoc fields when is_dug_kind (Option.value (field "kind" fields) ~default:`Null) ->
          classification (`Assoc fields) = Some (`String "static") && not (depends_on_extern (`Assoc fields))
      | _ -> true)
  in
  let extern_dependent_has_residual =
    if Filename.basename source = "extern_dependent.c" then
      facts |> List.exists (fun fact -> depends_on_extern fact && match classification fact with Some (`String "dynamic" | `String "residual") -> true | _ -> false)
    else true
  in
  let extern_independent_static =
    if Filename.basename source = "extern_independent_static.c" then
      facts |> List.for_all (fun fact -> not (depends_on_extern fact) && classification fact = Some (`String "static"))
    else true
  in
  dug_edges_static && extern_dependent_has_residual && extern_independent_static

let module_report active_path reference_path =
  let active_json = Yojson.Safe.from_file active_path in
  let reference_json = Yojson.Safe.from_file reference_path in
  let active_projection = projection active_json in
  let reference_projection = projection reference_json in
  let pre_rel = relation (projection_member "pre_dug_spec_inputs" active_json) (projection_member "pre_dug_spec_inputs" reference_json) in
  let access_rel = relation (projection_member "access_summaries" active_json) (projection_member "access_summaries" reference_json) in
  let dug_rel = relation (projection_member "dug_structure" active_json) (projection_member "dug_structure" reference_json) in
  let source = active_json |> member "source" |> Yojson.Safe.Util.to_string in
  let bta_ok = bta_accepted_for_source source active_json && bta_accepted_for_source source reference_json in
  let rel = if pre_rel = "structural-equiv" && access_rel = "structural-equiv" && dug_rel = "structural-equiv" && bta_ok && Yojson.Safe.equal active_projection reference_projection then "structural-equiv" else "structural-diverge" in
  `Assoc [
    "active", `String active_path;
    "reference", `String reference_path;
    "source_basename", `String (Filename.basename (active_json |> member "source" |> Yojson.Safe.Util.to_string));
    "relation", `String rel;
    "pre_dug_spec_inputs", `String pre_rel;
    "access_summaries", `String access_rel;
    "dug_structure", `String dug_rel;
    "bta", `String (if bta_ok then "accepted" else "rejected");
    "active_projection", active_projection;
    "reference_projection", reference_projection;
  ]

let relation_of_module = function
  | `Assoc fields -> (match List.assoc_opt "relation" fields with Some (`String r) -> r | _ -> "structural-diverge")
  | _ -> "structural-diverge"

let () =
  Arg.parse
    [
      "--active", Arg.Set_string active_dir, "active artifact directory";
      "--reference", Arg.Set_string reference_dir, "frozen reference artifact directory";
      "--report", Arg.Set_string report, "comparison report path";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !active_dir = "" || !reference_dir = "" || !report = "" then failwith usage;
  let active_paths = read_manifest !active_dir in
  let reference_paths = read_manifest !reference_dir in
  let modules = List.map2 module_report active_paths reference_paths in
  let all_equiv = List.for_all (fun m -> relation_of_module m = "structural-equiv") modules in
  let json = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_access_dug.schema_version;
    "claim", `String "module-only Access+DUG construction parity";
    "relation", `String (if all_equiv then "structural-equiv" else "structural-diverge");
    "modules", `List modules;
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if all_equiv then print_endline ("PASS " ^ !report)
  else failwith ("Access+DUG parity diverged; see " ^ !report)
