let rec mkdir_p path =
  if path <> "" && path <> "." && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end


let write_json path json =
  let dir = Filename.dirname path in
  mkdir_p dir;
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string json);
  output_char oc '\n';
  close_out oc

let read_json path = Yojson.Safe.from_file path

let module_artifact_path out_dir source =
  let base = Filename.basename source in
  Filename.concat out_dir (base ^ ".frontend-global.json")

let write_module_artifact ~out_dir source =
  let json = Real_sparrow_frontend.artifact_for_module source in
  let path = module_artifact_path out_dir source in
  write_json path json;
  path

let stable_projection json =
  let open Yojson.Safe.Util in
  let source = json |> member "source" |> to_string in
  let global = json |> member "global" in
  let global_json = global |> member "global_json" in
  let cfgs_json = global_json |> member "cfgs" in
  let cfg_entries =
    match cfgs_json with
    | `Assoc fields -> fields
    | _ -> []
  in
  let proc_projection (pid, cfg) =
    `Assoc [
      "pid", `String pid;
      "cfg", cfg;
    ]
  in
  `Assoc [
    "source_basename", `String (Filename.basename source);
    "parser", json |> member "parser";
    "cfgs", `List (List.map proc_projection cfg_entries);
    "callgraph", global_json |> member "callgraph";
  ]

let compare_active_to_reference ~active_paths ~reference_paths =
  let active = List.map (fun p -> p, stable_projection (read_json p)) active_paths in
  let reference = List.map (fun p -> p, stable_projection (read_json p)) reference_paths in
  let pairs = List.combine active reference in
  let module_results =
    pairs |> List.map (fun ((active_path, active_json), (reference_path, reference_json)) ->
      let equal = Yojson.Safe.equal active_json reference_json in
      `Assoc [
        "active", `String active_path;
        "reference", `String reference_path;
        "relation", `String (if equal then "structural-equiv" else "structural-diverge");
        "active_projection", active_json;
        "reference_projection", reference_json;
      ])
  in
  let all_equiv =
    List.for_all (function
      | `Assoc fields -> (match List.assoc_opt "relation" fields with Some (`String "structural-equiv") -> true | _ -> false)
      | _ -> false) module_results
  in
  `Assoc [
    "claim", `String "frontend/global structural comparison over real Sparrow boundary artifacts";
    "reference_kind", `String "caller-supplied reference artifacts; frozen Sparrow observer when reference path is frozen";
    "relation", `String (if all_equiv then "structural-equiv" else "structural-diverge");
    "modules", `List module_results;
  ]
