let repo_root = ref "."
let artifact_dir = ref ""
let implementation = ref ""

let usage =
  "abstract_speculate_metaocaml_sparse_forbidden_shortcuts --repo-root <repo> [--artifact-dir <dir>] [--implementation <ml>]"

let member = Yojson.Safe.Util.member
let to_string = Yojson.Safe.Util.to_string

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let failf fmt = Printf.ksprintf failwith fmt
let expect cond msg = if not cond then failwith msg

let choose_existing paths =
  match List.find_opt Sys.file_exists paths with
  | Some path -> path
  | None -> List.hd paths

let project_path rel = Filename.concat !repo_root rel

let implementation_paths () =
  if !implementation <> "" then [!implementation]
  else
    let candidates = [
      "abstract_speculate_pe.ml";
      "abstract_speculate_meta_sparse.ml";
      "abstract_speculate_residual_value.ml";
      "abstract_speculate_stage2_input.ml";
      "abstract_speculate_stage_types.ml";
      "abstract_speculate_residual_linker.ml";
      "abstract_speculate_lift.ml";
      "abstract_speculate_blind_equal.ml";
    ] in
    let roots = [project_path "sparrow-modular-ocaml/src"; project_path "src"] in
    let existing =
      roots
      |> List.concat_map (fun root -> List.map (Filename.concat root) candidates)
      |> List.filter Sys.file_exists
    in
    let required =
      ["abstract_speculate_pe.ml"; "abstract_speculate_meta_sparse.ml";
       "abstract_speculate_residual_value.ml"; "abstract_speculate_stage2_input.ml";
       "abstract_speculate_stage_types.ml"]
    in
    required
    |> List.iter (fun name ->
         if not (List.exists (fun path -> Filename.basename path = name) existing) then
           failf "missing required implementation file in forbidden scan: %s" name);
    existing

let artifact_paths dir =
  if dir = "" || not (Sys.file_exists dir) then []
  else
    let manifest_path = Filename.concat dir "manifest.json" in
    if Sys.file_exists manifest_path then
      match Yojson.Safe.from_file manifest_path |> member "artifacts" with
      | `List paths -> List.map to_string paths
      | _ -> []
    else
      Sys.readdir dir
      |> Array.to_list
      |> List.filter (fun name -> Filename.check_suffix name ".json" && name <> "manifest.json")
      |> List.map (Filename.concat dir)

let residual_source_paths dir =
  artifact_paths dir
  |> List.filter_map (fun path ->
       let residual = Yojson.Safe.from_file path |> member "projection" |> member "residual_artifact" in
       match member "source_path" residual with `String p when Sys.file_exists p -> Some p | _ -> None)

let require_absent ~where source needles =
  needles
  |> List.filter (contains source)
  |> function
  | [] -> ()
  | hits -> failf "%s contains forbidden shortcut(s): %s" where (String.concat ", " hits)

let strip_report_literal_lines source =
  source
  |> String.split_on_char '\n'
  |> List.filter (fun line ->
       not (
         contains line "\"forbidden_prelink_entrypoints\"" ||
         contains line "`String \"Real_sparrow_frontend.parse\"" ||
         contains line "`String \"Real_sparrow_frontend.global_for_files\"" ||
         contains line "`String \"Mergecil.merge\""))
  |> String.concat "\n"

let () =
  Arg.parse
    ["--repo-root", Arg.Set_string repo_root, "repository root";
     "--artifact-dir", Arg.Set_string artifact_dir, "active artifact directory";
     "--implementation", Arg.Set_string implementation, "implementation file to scan"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  let impls = implementation_paths () in
  expect (impls <> []) "no implementation files selected for forbidden shortcut scan";
  impls
  |> List.iter (fun impl ->
       expect (Sys.file_exists impl) ("missing implementation file: " ^ impl);
       let impl_source = read_file impl |> strip_report_literal_lines in
       require_absent ~where:impl impl_source [
         "Real_sparrow_sparse_fixpoint_pe.artifact_for_module";
         "Real_sparrow_premerge_linked_observer";
         "real_sparrow_premerge_linked_observer";
         "Real_sparrow_frontend.global_for_files";
         "Mergecil.merge";
         "SparseItv.perform";
         "Stage2Sparse.perform";
         "recompute_sparse_rows";
         "String.concat";
         "Printf.sprintf";
         "Hashtbl.hash";
         "String.length semantic_guard >= 0";
         "ordinal + 1";
         "ordinal >= 0";
         "output_string";
         "source_ocaml_string_list";
         "source_json_list";
         "Yojson.Safe.to_string (`String";
         "row_code : (stage2_input -> residual_row_result) Trx.code";
         "T.D obligation.T.row_code";
         "stage1_direct_sparse_pipeline\", `Bool true";
         "residual_runtime_scope\", `String \"stage1-static-rows-plus-extern-dependent-row-code\"";
       ]);

  artifact_paths !artifact_dir
  |> List.iter (fun path ->
       let residual = Yojson.Safe.from_file path |> member "projection" |> member "residual_artifact" in
       expect (member "posthoc_row_split_used" residual = `Bool false)
         (path ^ ": posthoc row split accepted");
       expect (member "row_obligation_residual_source" residual = `Bool false)
         (path ^ ": row obligation residual source accepted");
       expect (member "whole_row_dynamic_wrapper" residual = `Bool false)
         (path ^ ": whole-row dynamic wrapper accepted");
       expect (member "metadata_only_proof" residual = `Bool false)
         (path ^ ": metadata-only proof accepted");
       expect (member "top_substitution" residual = `Bool false)
         (path ^ ": top substitution accepted"));
  residual_source_paths !artifact_dir
  |> List.iter (fun path ->
       let source = read_file path in
       require_absent ~where:path source [
         "Real_sparrow_sparse_fixpoint_pe.artifact_for_module";
         "Real_sparrow_frontend.global_for_files";
         "Mergecil.merge";
         "AccessAnalysis.perform";
         "SsaDug.make";
         "SparseItv.perform";
         "SparseAnalysis.perform";
         "Stage2Sparse.perform";
         "recompute_sparse_rows";
         "String.concat";
         "Printf.sprintf";
       ]);
  print_endline "abstract_speculate_metaocaml_sparse_forbidden_shortcuts: PASS"
