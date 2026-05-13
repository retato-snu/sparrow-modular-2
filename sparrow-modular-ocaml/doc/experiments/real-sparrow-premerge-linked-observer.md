# Experiment Log — Real Sparrow Premerge Linked Observer

Date: 2026-05-11

## Goal

Prove linked multi-module ItvDom sparse final-table parity before any
Alarm/Report PE work.  The test-gated alias is
`@real_sparrow_premerge_linked_observer`.

## Implemented evidence

- Active linked path: `Real_sparrow_frontend.global_for_files` uses the real
  multi-file parse/Mergecil path and sets `Mergecil.ignore_merge_conflicts := true`
  for baseline-compatible multi-file merge behavior.
- Frozen observer: `sparrow/test/real_staged_linking_observer.ml` runs the frozen
  linked `Frontend.parse -> makeCFGinfo -> Global.init -> PreAnalysis.perform ->
  ItvAnalysis.do_analysis` path.
- Residual artifact: each linked fixture writes standalone OCaml residual source,
  compiles it with `ocamlc`, runs it, and captures output JSON.
- Comparator: validates linked Global summary parity, residual-vs-frozen final
  input/output table parity, completion evidence, BTA proof, and forbidden-call
  absence in residual source.

## Fixture groups

- `cross_file_direct_call`
- `cross_file_global_update`
- `cross_file_branch_join`
- `extern_boundary_linked`
- `extern_independent_all_static`
- `linked_loop_widen_narrow`

## Non-goals

No Alarm/Report PE, PFS staging/ranking, octDom/domain-generic PE, or baseline
`sparrow/src` semantic edit is implemented by this milestone.
