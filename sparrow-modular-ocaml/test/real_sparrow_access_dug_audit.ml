let repo_root = ref ".."
let report = ref ""
let usage = "real_sparrow_access_dug_audit --repo-root <path> --report <json>"

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic; data

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let project_path root rel =
  let nested = Filename.concat (Filename.concat root "sparrow-modular-ocaml") rel in
  if Sys.file_exists nested then nested else Filename.concat root rel

let run cmd = Sys.command cmd = 0

let collect_files root =
  let dirs = [project_path root "src"; project_path root "test"; project_path root "doc"] |> List.filter Sys.file_exists in
  let command = Printf.sprintf "find %s -type f \\( -name '*.ml' -o -name '*.mli' -o -name dune -o -name '*.md' \\) 2>/dev/null" (String.concat " " (List.map Filename.quote dirs)) in
  let ic = Unix.open_process_in command in
  let rec loop acc = match input_line ic with line -> loop (line :: acc) | exception End_of_file -> ignore (Unix.close_process_in ic); List.rev acc in
  loop []

let is_legacy path = contains path "/legacy_finite_slice/" || contains path "/doc/adr/archive/" || contains path "/doc/archive/"
let is_fixture path = contains path "/test/fixtures/"
let is_source_lineage_support path =
  let base = Filename.basename path in
  List.mem base [
    "access.ml"; "access.mli"; "accessSem.ml"; "accessSem.mli";
    "accessAnalysis.ml"; "accessAnalysis.mli"; "dug.ml"; "dug.mli";
    "ssaDug.ml"; "ssaDug.mli"; "profiler.ml"; "profiler.mli";
    "itvSem.ml"; "itvSem.mli"; "preAnalysis.ml"; "preAnalysis.mli";
  ]

let forbidden_needles = [
  "implements Worklist.init";
  "accepts Worklist parity";
  "accepts SparseAnalysis.perform";
  "implements SparseAnalysis.perform";
  "proves Strong sparse fixpoint";
  "accepts Strong sparse fixpoint";
  "proves PartialFlowSensitivity staging";
  "proves PartialFlowSensitivity ranking";
  "proves fixpoint convergence";
  "proves whole-program merge";
  "generates executable residual";
  "links executable residual";
]

let stale_overclaim_needles = [
  "interval analysis PE is required";
  "main-analysis PE is required";
  "residual-global-fixpoint JSON artifacts are required";
]

let lowercase_ascii s = String.lowercase_ascii s

let is_nonclaim_line line =
  let l = lowercase_ascii line in
  contains l "no " || contains l "not " || contains l "non-claim" ||
  contains l "non_claim" || contains l "out of scope" || contains l "rejected" ||
  contains l "forbidden" || contains l "never" || contains l "deferred" ||
  contains l "future" || contains l "must not" || contains l "does not" ||
  contains l "without"

let lines data = String.split_on_char '\n' data

let hits root needles =
  collect_files root
  |> List.filter (fun path -> not (is_legacy path || is_fixture path || is_source_lineage_support path || Filename.basename path = "real_sparrow_access_dug_audit.ml"))
  |> List.concat_map (fun path ->
       let data = read_file path in
       needles
       |> List.filter (fun needle ->
            lines data |> List.exists (fun line -> contains line needle && not (is_nonclaim_line line)))
       |> List.map (fun needle -> `Assoc ["path", `String path; "needle", `String needle]))

let file_exists = Sys.file_exists

let () =
  Arg.parse ["--repo-root", Arg.Set_string repo_root, "repository root"; "--report", Arg.Set_string report, "report path"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !report = "" then failwith "--report is required";
  let root = !repo_root in
  let docs = [
    project_path root "doc/real-sparrow-lineage.md";
    project_path root "doc/adr/ADR-0003-real-sparrow-access-dug.md";
    project_path root "doc/experiments/real-sparrow-access-dug.md";
    project_path root "doc/experiments/real-sparrow-access-dug-closure.md";
  ] in
  let semantic_clean =
    run (Printf.sprintf "git -C %s diff --exit-code -- src >/dev/null" (Filename.quote (Filename.concat root "../sparrow"))) &&
    run (Printf.sprintf "test -z \"$(git -C %s status --porcelain -- src)\"" (Filename.quote (Filename.concat root "../sparrow")))
  in
  let docs_present = List.for_all file_exists docs in
  let forbidden_hits = hits root forbidden_needles in
  let stale_hits = hits root stale_overclaim_needles in
  let ok = semantic_clean && docs_present && forbidden_hits = [] && stale_hits = [] in
  let json = `Assoc [
    "status", `String (if ok then "pass" else "fail");
    "baseline_semantic_clean", `Bool semantic_clean;
    "docs_present", `Bool docs_present;
    "forbidden_hits", `List forbidden_hits;
    "stale_overclaim_hits", `List stale_hits;
    "claim", `String "module-only Access+DUG construction parity; residual is classification-only";
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if ok then print_endline ("PASS " ^ !report) else failwith ("Access+DUG audit failed; see " ^ !report)
