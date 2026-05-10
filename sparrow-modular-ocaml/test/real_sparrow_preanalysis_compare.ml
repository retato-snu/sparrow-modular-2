let active_dir = ref ""
let reference_dir = ref ""
let report = ref ""

let usage = "real_sparrow_preanalysis_compare --active <dir> --reference <dir> --report <json>"

let read_manifest dir =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file (Filename.concat dir "manifest.json")
  |> member "modules" |> to_list |> List.map to_string

let projection json =
  let open Yojson.Safe.Util in
  `Assoc [
    "schema_version", json |> member "schema_version";
    "source_basename", `String (Filename.basename (json |> member "source" |> to_string));
    "scope", json |> member "scope";
    "boundary", json |> member "boundary";
    "lineage", json |> member "lineage";
    "projection", json |> member "projection";
    "non_claims", json |> member "non_claims";
  ]

let relation a b = if Yojson.Safe.equal a b then "structural-equiv" else "structural-diverge"

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
  let pairs = List.combine active_paths reference_paths in
  let modules = pairs |> List.map (fun (a_path, r_path) ->
    let active = projection (Yojson.Safe.from_file a_path) in
    let reference = projection (Yojson.Safe.from_file r_path) in
    let rel = relation active reference in
    `Assoc [
      "active", `String a_path;
      "reference", `String r_path;
      "relation", `String rel;
      "active_projection", active;
      "reference_projection", reference;
    ])
  in
  let all_equiv = List.for_all (function
    | `Assoc fields -> (match List.assoc_opt "relation" fields with Some (`String "structural-equiv") -> true | _ -> false)
    | _ -> false) modules
  in
  let json = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_preanalysis.schema_version;
    "claim", `String "module-only PreAnalysis boundary projection parity";
    "relation", `String (if all_equiv then "structural-equiv" else "structural-diverge");
    "modules", `List modules;
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if all_equiv then print_endline ("PASS " ^ !report)
  else failwith ("preanalysis parity diverged; see " ^ !report)
