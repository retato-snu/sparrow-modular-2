# ADR-0005 — Real Sparrow Staged Linking PE

Date: 2026-05-11
Status: accepted for staged-linking milestone implementation
Primary reference: `Doc/FOUNDATIONS.md`
Plan: `.omx/plans/plan-real-sparrow-staged-linking-pe.md`

## Decision

Add a sibling staged-linking milestone for linked multi-module ItvDom sparse
analysis.  The active path uses the real Sparrow frontend merge lineage:

`Frontend.parse / Mergecil.merge -> makeCFGinfo -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> SparseAnalysis.perform`

It then emits an executable residual recomposition artifact that embeds staged
static linked facts and exposes only the extern-dependent residual closure at
runtime.

## Drivers

1. Alarm/Report PE is premature until linked sparse final tables are proven.
2. Linked internal calls must be staged/static, not misclassified as externs.
3. Residual evidence must be executable; JSON-only summaries are insufficient.
4. Frozen `sparrow/src` remains the program-input `I` and is not edited.

## Rejected alternatives

- Re-run the full analyzer in the residual executable: rejected because it would
  residualize non-extern Mergecil/global/fixpoint work.
- Stop at linked Global/PreAnalysis parity: rejected because sparse final-table
  parity is the milestone boundary.
- Add Alarm/Report, PFS ranking, octDom, or domain-generic PE now: rejected as
  explicit scope creep.

## Consequences

- A non-semantic frozen observer is added under `sparrow/test` only.
- The active Dune alias is a sibling of predecessor aliases and must keep them
  green.
- Residual source is audited for forbidden calls: `Frontend.parse`,
  `Mergecil.merge`, `Global.init`, and full sparse analysis entry points.
