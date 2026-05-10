# ADR-002 — Interval-only boundary

## Date

2026-05-10

## Status

Accepted for the interval-only faithful pipeline PE milestone.

## Context / problem

The project must partially evaluate the original Sparrow pipeline module by module while keeping `sparrow/` frozen. Baseline anchors: sparrow/src/core/main.ml:85-113; sparrow/src/core/frontend.ml:35-68; sparrow/src/core/preAnalysis.ml:19-34; sparrow/src/sparse/ssaDug.ml:176-352; sparrow/src/semantics/itvSem.ml:811-899.

## Options considered

1. Active pipeline-shadow implementation with traceability. 2. Direct `sparrow_lib` dependency reuse. 3. Baseline staging-parametric rewrite. 4. Standalone packaging first.

## Decision

Use interval unconditionally and keep non-interval product components explicit unsupported fields.

## Rejected alternatives

Baseline semantic edits are rejected by O-4. Standalone packaging first is rejected because it can package an unfaithful fixture recognizer. Blocking on `sparrow_lib` is rejected unless the timeboxed experiment proves low-risk reuse.

## Relation to Sparrow baseline pipeline

This decision maps active artifacts to the frozen pipeline in sparrow/src/core/main.ml:85-113; sparrow/src/core/frontend.ml:35-68; sparrow/src/core/preAnalysis.ml:19-34; sparrow/src/sparse/ssaDug.ml:176-352; sparrow/src/semantics/itvSem.ml:811-899. The implementation cites these anchors instead of editing them.

## Relation to PE / staging principles

References: C-1 PE model; C-2 baseline parity; C-3 abstract-interpreter fixpoint; O-1 maximal partial execution; O-2 executable D code; O-3 blind equality; O-4 frozen baseline. The decision preserves executable residuals and stage-1/static vs stage-2/dynamic separation.

## Soundness / parity / precision impact

Claims are parity-oriented (`equiv`) for the golden fixtures. Any divergence must be reported with relation and cause, not hidden as a precision improvement.

## Validation evidence

Validation evidence: dune exec test/modular_frontend_runner.bc; dune exec test/preanalysis_pe_runner.bc; dune exec test/dug_artifact_audit.bc; dune exec test/main_analysis_pe_runner.bc; dune exec test/residual_global_fixpoint_runner.bc; dune exec test/research_record_audit.bc. Gate T11 also checks this ADR content.

## Paper relevance

This records the research claim, fixture example boundary, and threat to validity: the milestone is finite and interval-only, not a complete global proof.
