# Experiment — Timeboxed sparrow_lib dependency reuse

## Date/time

2026-05-10T13:55:00Z. Timebox: stop after the first hard Dune/opam/API incompatibility or 20 minutes of inspection.

## Hypothesis

`sparrow-modular-ocaml` might safely depend on or reuse `sparrow_lib` under `MetaOCaml-full`, reducing semantic drift from `sparrow/`.

## Setup / commands / files

Files checked: `sparrow/`, `sparrow-modular-ocaml/`, `sparrow/src/core/main.ml:85-113`, `sparrow/src/core/preAnalysis.ml:19-34`, `sparrow/src/sparse/sparseAnalysis.ml:249-260`, `sparrow-modular-ocaml/dune-project`, and `sparrow-modular-ocaml/sparrow_modular_ocaml.opam`. Planned commands: `opam exec --switch MetaOCaml-full -- dune build @runtest` and `dune exec test/research_record_audit.bc`.

## Observation / result

The active project is a separate BER MetaOCaml project with quoted residual code. The frozen Sparrow code is concrete and not staging-parametric. Safe direct reuse was not established inside the timebox.

## Interpretation

For this milestone, semantic fidelity must come from traceable mirroring plus baseline parity reports, not a hard dependency on `sparrow_lib`.

## Decision / follow-up

Do not block the pass on `sparrow_lib`. Do not edit `sparrow/`. Revisit dependency reuse only after the finite pipeline evidence is green.

## Paper impact

The paper should state that this milestone validates a traceable staged slice rather than proving direct mechanized reuse of all baseline Sparrow modules.
