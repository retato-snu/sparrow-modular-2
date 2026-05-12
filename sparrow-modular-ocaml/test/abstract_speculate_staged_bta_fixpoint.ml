let artifact_dir = ref "_build/real-sparrow/abstract-speculate-metaocaml-sparse/active"
let usage = "abstract_speculate_staged_bta_fixpoint --artifact-dir <dir>"
let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string
let expect cond msg = if not cond then failwith msg
let bool_field name json = member name json = `Bool true
let false_field name json = member name json = `Bool false
let int_field name json = match member name json with `Int n -> n | _ -> 0
let artifact_paths dir =
  let manifest = Yojson.Safe.from_file (Filename.concat dir "manifest.json") in
  match member "artifacts" manifest with
  | `List paths -> List.map to_string paths
  | _ -> []
let residual artifact = artifact |> member "projection" |> member "residual_artifact"
let has_extern_root residual =
  match residual |> member "execution_log" |> member "extern_root_count" with `Int n -> n > 0 | _ -> false
let check path =
  let artifact = Yojson.Safe.from_file path in
  let residual = residual artifact in
  expect (bool_field "staged_domain_fixpoint" residual) "production PE must run a staged-domain fixpoint";
  expect (bool_field "bta_participates_in_fixpoint" residual) "BTA must participate in fixpoint construction";
  expect (false_field "stage1_direct_sparse_pipeline" residual) "direct sparse must not be PE proof";
  expect (false_field "posthoc_row_split_used" residual) "post-hoc row split must not be used";
  expect (false_field "row_obligation_residual_source" residual) "row obligations must not be residual source";
  if has_extern_root residual then begin
    expect (int_field "transfer_level_d_site_count" residual > 0) "extern artifact must contain transfer-level D evidence";
    expect (int_field "staged_lattice_event_count" residual > 0) "extern artifact must contain staged lattice evidence";
    expect (int_field "bta_dynamic_sites_before_convergence" residual > 0) "BTA dynamic sites must exist before convergence";
    expect (int_field "fixpoint_iterations_with_dynamic_cells" residual > 0) "fixpoint must iterate over dynamic cells"
  end else begin
    expect (int_field "transfer_level_d_site_count" residual = 0) "static artifact must not invent transfer-level D evidence"
  end
let () =
  Arg.parse ["--artifact-dir", Arg.Set_string artifact_dir, "artifact directory"] (fun arg -> raise (Arg.Bad arg)) usage;
  let paths = artifact_paths !artifact_dir in
  expect (paths <> []) "no artifacts found";
  List.iter check paths;
  Printf.printf "abstract_speculate_staged_bta_fixpoint: PASS (%d artifacts)\n" (List.length paths)
