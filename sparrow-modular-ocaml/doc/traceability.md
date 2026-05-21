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
- `src/abstract_speculate_global_residual_fixpoint.ml` emits post-link
  residual-global seed/derived cell, equation, dependency, worklist, and
  non-source-level-rerun evidence for bounded residual-linking witnesses.
- `src/abstract_speculate_residual_relation.ml` derives the bounded
  `global_residual_equivalence_status` in the oracle-suite relation.
- `.omx/plans/prd-external-summary-effect-algebra.md` and
  `.omx/plans/test-spec-external-summary-effect-algebra.md` define the
  approved typed effect algebra migration contract and verification boundary;
  the current review slice should treat the planned `.mli` interfaces as future
  boundaries, not existing APIs.

## Non-claims

The current real Sparrow milestones do not claim Strong-mode staging,
sparse/DUG parity, whole-program merge equivalence, arbitrary-C preservation, a
source-level analyzer rerun inside the residual linker, or public schema
stability.  Residual linking and post-link residual-global fixpoint evidence are
accepted only for the bounded `sparrow-modular-ocaml` oracle-suite witnesses.
