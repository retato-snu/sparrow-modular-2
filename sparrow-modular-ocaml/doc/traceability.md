# Traceability — Real Sparrow module-boundary evidence

## Current accepted scope

This repository currently accepts source-lineage evidence for:

- real frontend/global construction through `Global.init`
- module-only real PreAnalysis through `Global.init -> PreAnalysis.perform`
- Weak `ItvSem.run AbsSem.Weak ItvSem.Spec.empty` transfer evidence
- module-only fixture parity against frozen `sparrow/test` observers

## Baseline anchors

- `sparrow/src/core/frontend.ml:35-68`
- `sparrow/src/program/global.ml`
- `sparrow/src/core/preAnalysis.ml:19-74`
- `sparrow/src/semantics/itvSem.ml:811-899`

## Active artifacts

- `src/real_sparrow_frontend.ml` emits frontend/global boundary JSON.
- `src/real_sparrow_preanalysis.ml` emits PreAnalysis boundary JSON for
  weak-step lineage, callgraph parity, pruning parity, memory summary parity,
  and static/dynamic/residual BTA classification.
- `test/real_sparrow_acceptance_audit.ml` checks baseline `sparrow/src`
  cleanliness, required documentation, forbidden toy shortcuts, and stale
  overclaim wording.

## Non-claims

The current real Sparrow milestones do not claim Strong-mode staging,
sparse/DUG parity, whole-program merge equivalence, executable residual code,
residual linking, or link-time/global-fixpoint behavior.
