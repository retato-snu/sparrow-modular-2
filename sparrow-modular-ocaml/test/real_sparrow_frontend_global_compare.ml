let active_dir = ref ""
let reference_dir = ref ""
let report = ref ""

let usage = "real_sparrow_frontend_global_compare --active <dir> --reference <dir> --report <json>"

let json_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".frontend-global.json")
  |> List.sort String.compare
  |> List.map (Filename.concat dir)

let () =
  Arg.parse
    [
      "--active", Arg.Set_string active_dir, "active artifact directory";
      "--reference", Arg.Set_string reference_dir, "reference artifact directory";
      "--report", Arg.Set_string report, "output comparison report";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !active_dir = "" || !reference_dir = "" || !report = "" then failwith usage;
  let active_paths = json_files !active_dir in
  let reference_paths = json_files !reference_dir in
  if List.length active_paths = 0 then failwith "no active artifacts found";
  if List.length active_paths <> List.length reference_paths then failwith "active/reference artifact count mismatch";
  let comparison = Sparrow_modular_ocaml.Real_sparrow_artifact.compare_active_to_reference ~active_paths ~reference_paths in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report comparison;
  match comparison with
  | `Assoc fields ->
      begin match List.assoc_opt "relation" fields with
      | Some (`String "structural-equiv") -> print_endline ("PASS " ^ !report)
      | _ -> failwith ("frontend/global comparison failed; see " ^ !report)
      end
  | _ -> failwith "invalid comparison report"
