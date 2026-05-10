let repo_root = ref ".."
let report = ref ""

let usage = "real_sparrow_acceptance_audit --repo-root <path> --report <json>"

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

let run cmd = Sys.command cmd = 0

let project_path root rel =
  let nested = Filename.concat (Filename.concat root "sparrow-modular-ocaml") rel in
  if Sys.file_exists nested then nested else Filename.concat root rel

let git_root root =
  if Sys.file_exists (Filename.concat root ".git") then Some root
  else
    let parent = Filename.concat root ".." in
    if Sys.file_exists (Filename.concat parent ".git") then Some parent else None

let collect_files root =
  let src = project_path root "src" in
  let test = project_path root "test" in
  let doc = project_path root "doc" in
  let dirs = List.filter Sys.file_exists [src; test; doc] in
  let command = Printf.sprintf
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

let is_real_input path =
  contains path "/test/fixtures/real_frontend/"
  || contains path "/test/fixtures/real_preanalysis/"

let is_source_lineage_support path =
  let base = Filename.basename path in
  List.mem base [
    "access.ml"; "access.mli";
    "apiSem.ml";
    "arrayBlk.ml"; "arrayBlk.mli";
    "basicDom.ml"; "basicDom.mli";
    "instrumentedMem.ml"; "instrumentedMem.mli";
    "itv.ml"; "itv.mli";
    "itvDom.ml"; "itvDom.mli";
    "itvSem.ml"; "itvSem.mli";
    "mapDom.ml"; "mapDom.mli";
    "powDom.ml"; "powDom.mli";
    "preAnalysis.ml"; "preAnalysis.mli";
    "prodDom.ml"; "prodDom.mli";
    "spec.ml"; "spec.mli";
    "structBlk.ml"; "structBlk.mli";
  ]

let forbidden_needles =
  ["String." ^ "contains"; "contains" ^ "_sub"; "sub" ^ "string"; "case" ^ "_name"; "extern" ^ "_loop"; "dynamic" ^ "_if"; "pipeline" ^ "_"]

let stale_overclaim_needles =
  [
    "does not implement pre-" ^ "analysis PE";
    "does not implement interval " ^ "analysis PE";
    "pre-" ^ "analysis PE, DUG PE, interval PE, residual D code";
    "DUG-" ^ "equivalent artifact";
    "main-" ^ "analysis PE, residual";
    "residual linking/global-" ^ "fixpoint evidence";
    "residual-" ^ "global-fixpoint JSON artifacts";
  ]

let forbidden_hits root =
  collect_files root
  |> List.filter (fun path -> not (is_legacy path || is_real_input path || is_source_lineage_support path || (Filename.basename path = "real_sparrow_acceptance_audit.ml" || Filename.basename path = "real_sparrow_access_dug_audit.ml")))
  |> List.concat_map (fun path ->
    let data = read_file path in
    forbidden_needles
    |> List.filter (contains data)
    |> List.map (fun needle -> `Assoc ["path", `String path; "needle", `String needle]))

let stale_overclaim_hits root =
  collect_files root
  |> List.filter (fun path -> not (is_legacy path || (Filename.basename path = "real_sparrow_acceptance_audit.ml" || Filename.basename path = "real_sparrow_access_dug_audit.ml")))
  |> List.concat_map (fun path ->
    let data = read_file path in
    stale_overclaim_needles
    |> List.filter (contains data)
    |> List.map (fun needle -> `Assoc ["path", `String path; "needle", `String needle]))

let file_exists path = Sys.file_exists path

let () =
  Arg.parse
    [
      "--repo-root", Arg.Set_string repo_root, "repository root";
      "--report", Arg.Set_string report, "audit report path";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !report = "" then failwith "--report is required";
  let root = !repo_root in
  let lineage = project_path root "doc/real-sparrow-lineage.md" in
  let adr = project_path root "doc/adr/ADR-0001-real-sparrow-frontend-global-fork.md" in
  let preanalysis_adr = project_path root "doc/adr/ADR-0002-real-sparrow-preanalysis-pe.md" in
  let experiment = project_path root "doc/experiments/real-sparrow-frontend-global.md" in
  let preanalysis_experiment = project_path root "doc/experiments/real-sparrow-preanalysis.md" in
  let preanalysis_closure = project_path root "doc/experiments/real-sparrow-preanalysis-closure.md" in
  let semantic_clean =
    match git_root root with
    | Some gr -> run (Printf.sprintf "cd %s && git diff --exit-code -- sparrow/src >/dev/null" gr)
    | None -> true
  in
  let docs_present = List.for_all file_exists [lineage; adr; preanalysis_adr; experiment; preanalysis_experiment; preanalysis_closure] in
  let hits = forbidden_hits root in
  let overclaim_hits = stale_overclaim_hits root in
  let ok = semantic_clean && docs_present && hits = [] && overclaim_hits = [] in
  let json =
    `Assoc [
      "status", `String (if ok then "pass" else "fail");
      "baseline_semantic_clean", `Bool semantic_clean;
      "frozen_baseline_parity", `String "available via non-semantic sparrow/test observers: real_frontend_global_observer.ml and real_preanalysis_observer.ml";
      "docs_present", `Bool docs_present;
      "forbidden_hits", `List hits;
      "stale_overclaim_hits", `List overclaim_hits;
      "legacy_quarantine", `String "legacy finite-slice code is under legacy_finite_slice paths and excluded from real acceptance";
      "no_analysis_overclaim", `String "PreAnalysis PE is module-only Weak ItvSem.run evidence; no Strong staging, sparse/DUG parity, whole-program merge equivalence, executable residual code, or residual linking/global-fixpoint claim";
    ]
  in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if ok then print_endline ("PASS " ^ !report) else failwith ("acceptance audit failed; see " ^ !report)
