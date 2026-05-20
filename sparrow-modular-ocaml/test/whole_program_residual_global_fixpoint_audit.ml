let repo_root = ref ".."
let artifact_dir = ref ""
let report = ref ""

let usage =
  "whole_program_residual_global_fixpoint_audit --repo-root <path> [--artifact-dir <path>] --report <json>"

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

let lowercase_ascii = String.lowercase_ascii
let lines data = String.split_on_char '\n' data

let project_path root rel =
  let nested = Filename.concat (Filename.concat root "sparrow-modular-ocaml") rel in
  if Sys.file_exists nested then nested else Filename.concat root rel

let run cmd = Sys.command cmd = 0

let collect_files root =
  let dirs =
    [ project_path root "src"; project_path root "test"; project_path root "doc" ]
    |> List.filter Sys.file_exists
  in
  let command =
    Printf.sprintf
      "find %s -type f \\( -name '*.ml' -o -name '*.mli' -o -name dune -o -name '*.md' \\) 2>/dev/null"
      (String.concat " " (List.map Filename.quote dirs))
  in
  let ic = Unix.open_process_in command in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        ignore (Unix.close_process_in ic);
        List.rev acc
  in
  loop []

let is_legacy path =
  contains path "/legacy_finite_slice/"
  || contains path "/doc/legacy-finite-slice"
  || contains path "/doc/adr/archive/"
  || contains path "/doc/archive/"

let is_fixture path = contains path "/test/fixtures/"

let is_self path =
  Filename.basename path = "whole_program_residual_global_fixpoint_audit.ml"

let is_nonclaim_line line =
  let l = lowercase_ascii line in
  contains l "no " || contains l "not " || contains l "non-claim"
  || contains l "non_claim" || contains l "out of scope" || contains l "rejected"
  || contains l "forbidden" || contains l "never" || contains l "deferred"
  || contains l "future" || contains l "must not" || contains l "does not"
  || contains l "without" || contains l "missing" || contains l "unsupported"
  || contains l "prototype/non-public" || contains l "prototype-non-public"
  || contains l "no-overclaim" || contains l "audit"

let json_string_field name json =
  match Yojson.Safe.Util.member name json with `String s -> s | _ -> ""

let json_bool_field name json =
  match Yojson.Safe.Util.member name json with `Bool b -> Some b | _ -> None

let rec json_member_path path json =
  match path with
  | [] -> json
  | p :: rest -> json_member_path rest (Yojson.Safe.Util.member p json)

let json_list_field name json =
  match Yojson.Safe.Util.member name json with `List xs -> xs | _ -> []

let first_existing paths = List.find_opt Sys.file_exists paths

let whole_program_evidence_from_artifact_dir dir =
  if dir = "" || not (Sys.file_exists dir) then None
  else
    let candidates =
      [ Filename.concat dir "manifest.json";
        Filename.concat dir "abstract_speculate_residual_linking_oracle_suite.report.json" ]
    in
    match first_existing candidates with
    | None -> None
    | Some path -> (
        try
          let json = Yojson.Safe.from_file path in
          let witnesses = json_list_field "witnesses" json in
          let witness_has_global w =
            let residual =
              match Yojson.Safe.Util.member "residual_artifact" w with
              | `String p when Sys.file_exists p ->
                  (try Yojson.Safe.from_file p with _ -> `Null)
              | _ -> (
                  match Yojson.Safe.Util.member "residual_linked_artifact" w with
                  | `String p when Sys.file_exists p ->
                      (try Yojson.Safe.from_file p with _ -> `Null)
                  | _ -> w)
            in
            let log = Yojson.Safe.Util.member "linked_output" residual |> Yojson.Safe.Util.member "execution_log" in
            let top_or_log field =
              match Yojson.Safe.Util.member field residual with
              | `Null -> Yojson.Safe.Util.member field log
              | value -> value
            in
            json_bool_field "global_residual_fixpoint_run" residual = Some true
            || json_bool_field "global_residual_fixpoint_run" log = Some true
            || top_or_log "global_residual_fixpoint_scope" <> `Null
          in
          Some (List.exists witness_has_global witnesses)
        with _ -> Some false)

let no_flat_d_hits root =
  let source_and_test_files =
    collect_files root
    |> List.filter (fun path -> contains path "/src/" || contains path "/test/")
  in
  source_and_test_files
  |> List.filter (fun path -> not (is_legacy path || is_fixture path || is_self path))
  |> List.concat_map (fun path ->
         let data = read_file path in
         [ "Flat" ^ "D"; "Suspension" ^ "Marker"; "TODO" ^ "_D" ^ "_MARKER" ]
         |> List.filter (contains data)
         |> List.map (fun needle -> `Assoc [ "path", `String path; "needle", `String needle ]))

let overclaim_hits root has_global_evidence =
  let forbidden_needles =
    [ "arbitrary-C theorem";
      "full analyzer rewrite";
      "frozen sparrow analyzer edits";
      "public API/schema promise";
      "stable public residual-linker API";
      "full source-level sparse rerun";
      "all-domain generality";
      "all-domains-at-once" ]
  in
  let whole_program_positive_needles =
    [ "Whole-program residual global fixpoint | Implemented";
      "whole-program residual global fixpoint is implemented";
      "global_sparse_fixpoint_source_level_rerun = true";
      "global sparse fixpoint source-level rerun" ]
  in
  collect_files root
  |> List.filter (fun path -> not (is_legacy path || is_fixture path || is_self path))
  |> List.concat_map (fun path ->
         let data = read_file path in
         let line_hits needle =
           lines data
           |> List.filter (fun line -> contains line needle && not (is_nonclaim_line line))
           |> List.map (fun line ->
                  `Assoc [ "path", `String path; "needle", `String needle; "line", `String line ])
         in
         let forbidden_hits = List.concat_map line_hits forbidden_needles in
         let whole_program_hits =
           if has_global_evidence then []
           else List.concat_map line_hits whole_program_positive_needles
         in
         forbidden_hits @ whole_program_hits)

let required_non_goals_present root =
  let status = project_path root "doc/sparrow-pe-status.md" in
  if not (Sys.file_exists status) then []
  else
    let data = read_file status |> lowercase_ascii in
    [ ("arbitrary_c_excluded", contains data "arbitrary-c");
      ("schema_non_public_or_missing", contains data "prototype/non-public" || contains data "prototype-non-public" || contains data "stable public artifact schema");
      ("full_product_or_all_domain_excluded", contains data "full product-domain" || contains data "all-domain");
      ("mechanized_proof_nonclaim", contains data "mechanized proof") ]
    |> List.filter (fun (_, ok) -> not ok)
    |> List.map (fun (name, _) -> `String name)

let () =
  Arg.parse
    [ "--repo-root", Arg.Set_string repo_root, "repository root";
      "--artifact-dir", Arg.Set_string artifact_dir, "optional oracle-suite artifact directory";
      "--report", Arg.Set_string report, "audit report path" ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !report = "" then failwith "--report is required";
  let root = !repo_root in
  let baseline_clean =
    run (Printf.sprintf "git -C %s diff --exit-code -- sparrow >/dev/null" (Filename.quote root))
  in
  let evidence_probe = whole_program_evidence_from_artifact_dir !artifact_dir in
  let has_global_evidence = match evidence_probe with Some true -> true | _ -> false in
  let flat_hits = no_flat_d_hits root in
  let overclaim_hits = overclaim_hits root has_global_evidence in
  let missing_non_goals = required_non_goals_present root in
  let ok = baseline_clean && flat_hits = [] && overclaim_hits = [] && missing_non_goals = [] in
  let json =
    `Assoc
      [ "schema_version", `String "whole-program-residual-global-fixpoint-audit/v1";
        "status", `String (if ok then "pass" else "fail");
        "baseline_diff_clean", `Bool baseline_clean;
        "no_flat_d_source_policy", `Bool (flat_hits = []);
        "flat_dynamic_marker_hits", `List flat_hits;
        "artifact_dir", `String !artifact_dir;
        "global_residual_evidence_present", `Bool has_global_evidence;
        "global_residual_evidence_probe", (match evidence_probe with None -> `String "not-run" | Some b -> `Bool b);
        "overclaim_hits", `List overclaim_hits;
        "missing_required_non_goals", `List missing_non_goals;
        "claim_guard", `String "If no executable global-residual evidence is supplied, docs must retain the whole-program residual global-fixpoint non-claim; if evidence is supplied, it remains witness-bounded/prototype/non-public.";
        "non_goals", `List
          [ `String "no arbitrary-C theorem";
            `String "no full analyzer rewrite";
            `String "no frozen sparrow analyzer edits";
            `String "no new dependencies";
            `String "no all-domain generality";
            `String "no docs-only claim";
            `String "no public API/schema promise" ] ]
  in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if ok then print_endline ("PASS " ^ !report)
  else failwith ("whole-program residual global-fixpoint audit failed; see " ^ !report)
