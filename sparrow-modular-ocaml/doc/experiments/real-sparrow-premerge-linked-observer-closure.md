# Closure Inventory — Real Sparrow Premerge Linked Observer

Date: 2026-05-11

## Source-lineage closure

- `sparrow/src/core/frontend.ml` — multi-file `Frontend.parse` / `Mergecil.merge`.
- `sparrow/src/program/global.ml` — `Global.init` boundary.
- `sparrow/src/core/preAnalysis.ml` — pre-analysis callgraph and memory setup.
- `sparrow/src/sparse/accessAnalysis.ml`, `accessSem.ml`, `dug.ml`, `ssaDug.ml` — sparse access/DUG boundary.
- `sparrow/src/sparse/sparseAnalysis.ml`, `worklist.ml` — Itv sparse fixpoint driver.
- Active predecessor: `sparrow-modular-ocaml/src/real_sparrow_sparse_fixpoint_pe.ml`.

## Baseline scope

`sparrow/src` semantics remain frozen.  The only baseline-side premerge linked observer
addition is `sparrow/test/real_staged_linking_observer.ml`, which links
`sparrow_lib` as a non-semantic observer.

## Residual closure

The residual artifact embeds staged linked final-table entries and emits a
recomposition JSON at runtime.  Entries are classified as:

- `staged-static-table` for extern-independent linked facts.
- `residual-extern-closure` for unknown extern roots and transitive dependent facts.

The residual source is intentionally standalone OCaml and is audited to avoid
frontend parsing, Mergecil merging, `Global.init`, or full sparse-analysis calls.
