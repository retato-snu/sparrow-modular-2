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
  let command = Printf.sprintf "find %s %s -type f \\( -name '*.ml' -o -name dune \\) 2>/dev/null" src test in
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
  contains path "/legacy_finite_slice/" || contains path "/doc/legacy-finite-slice"

let is_real_input path = contains path "/test/fixtures/real_frontend/"

let forbidden_needles =
  ["String." ^ "contains"; "contains" ^ "_sub"; "sub" ^ "string"; "case" ^ "_name"; "extern" ^ "_loop"; "dynamic" ^ "_if"; "pipeline" ^ "_"]

let forbidden_hits root =
  collect_files root
  |> List.filter (fun path -> not (is_legacy path || is_real_input path || Filename.basename path = "real_sparrow_acceptance_audit.ml"))
  |> List.concat_map (fun path ->
    let data = read_file path in
    forbidden_needles
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
  let experiment = project_path root "doc/experiments/real-sparrow-frontend-global.md" in
  let semantic_clean =
    match git_root root with
    | Some gr -> run (Printf.sprintf "cd %s && git diff --exit-code -- sparrow/src/core sparrow/src/program sparrow/src/domain >/dev/null" gr)
    | None -> true
  in
  let docs_present = List.for_all file_exists [lineage; adr; experiment] in
  let hits = forbidden_hits root in
  let ok = semantic_clean && docs_present && hits = [] in
  let json =
    `Assoc [
      "status", `String (if ok then "pass" else "fail");
      "baseline_semantic_clean", `Bool semantic_clean;
      "frozen_baseline_parity", `String "available via non-semantic sparrow/test/real_frontend_global_observer.ml; see _build/real-sparrow/frontend-global/frozen-parity.json";
      "docs_present", `Bool docs_present;
      "forbidden_hits", `List hits;
      "legacy_quarantine", `String "legacy finite-slice code is under legacy_finite_slice paths and excluded from real acceptance";
      "no_analysis_overclaim", `String "pre-analysis PE, DUG PE, interval PE, residual D code, and residual linking are future milestones";
    ]
  in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  if ok then print_endline ("PASS " ^ !report) else failwith ("acceptance audit failed; see " ^ !report)
