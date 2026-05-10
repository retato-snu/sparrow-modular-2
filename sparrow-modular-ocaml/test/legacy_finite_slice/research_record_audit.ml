open Sparrow_modular_ocaml
let read_file path = let ic = open_in path in let n = in_channel_length ic in let s = really_input_string ic n in close_in ic; s
let contains s needle = Pipeline.contains_sub s needle
let require cond msg = if not cond then failwith msg
let require_heading content h = require (contains content ("## " ^ h)) ("missing heading: " ^ h)
let adr_headings = ["Date"; "Status"; "Context / problem"; "Options considered"; "Decision"; "Rejected alternatives"; "Relation to Sparrow baseline pipeline"; "Relation to PE / staging principles"; "Soundness / parity / precision impact"; "Validation evidence"; "Paper relevance"]
let exp_headings = ["Date/time"; "Hypothesis"; "Setup / commands / files"; "Observation / result"; "Interpretation"; "Decision / follow-up"; "Paper impact"]
let required_adrs = [
  "ADR-001-pipeline-shadow-vs-baseline-modification.md";
  "ADR-002-interval-only-boundary.md";
  "ADR-003-finite-subset-golden-fixture-contract.md";
  "ADR-004-frontend-preanalysis-dug-boundary.md";
  "ADR-005-residual-global-fixpoint-strategy.md"]
let () =
  let argv = Array.to_list Sys.argv in
  let adr_dir = Cli.arg_value "--adr-dir" argv "doc/adr" in
  let exp_dir = Cli.arg_value "--experiment-dir" argv "doc/experiments" in
  let traceability = Cli.arg_value "--traceability" argv "doc/traceability.md" in
  List.iter (fun file ->
    let path = Filename.concat adr_dir file in
    require (Sys.file_exists path) ("missing ADR: " ^ path);
    let content = read_file path in
    List.iter (require_heading content) adr_headings;
    require (contains content "sparrow/src/") ("ADR missing baseline reference: " ^ path);
    require (contains content "C-" || contains content "O-") ("ADR missing PE/staging principle reference: " ^ path);
    require (contains content "dune exec" || contains content "Validation evidence") ("ADR missing validation command: " ^ path)) required_adrs;
  let exp = Filename.concat exp_dir "experiment-001-timeboxed-sparrow-lib-dependency-reuse.md" in
  require (Sys.file_exists exp) ("missing experiment: " ^ exp);
  let exp_content = read_file exp in
  List.iter (require_heading exp_content) exp_headings;
  require (contains exp_content "sparrow_lib") "experiment missing sparrow_lib";
  require (Sys.file_exists traceability) ("missing traceability: " ^ traceability);
  let trace = read_file traceability in
  List.iter (fun fixture -> require (contains trace fixture) ("traceability missing " ^ fixture)) ["pipeline_extern_loop"; "pipeline_dynamic_if"; "pipeline_static_local"; "pipeline_unsupported_pointer"];
  require (contains trace "sparrow/src/core/frontend.ml:35-68") "traceability missing baseline anchors";
  print_endline "research_record_audit: PASS"
