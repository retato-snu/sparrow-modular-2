# ADR-0005 — Real Sparrow Premerge Linked Observer

Date: 2026-05-11
Status: accepted as premerge linked observer / contrast lane
Primary reference: `Doc/FOUNDATIONS.md`

## Decision

Add a sibling premerge linked observer lane for linked multi-module ItvDom sparse
analysis. This lane is a whole-program reference/contrast path, not Abstract
Speculate module-local PE evidence.  The active path uses the real Sparrow frontend merge lineage:

`Frontend.parse / Mergecil.merge -> makeCFGinfo -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> SparseAnalysis.perform`

After premerge analysis it emits an executable residual recomposition artifact that embeds staged
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
  parity is the observer boundary.
- Add Alarm/Report, PFS ranking, octDom, or domain-generic PE now: rejected as
  explicit scope creep.

## Consequences

- A non-semantic frozen observer is added under `sparrow/test` only.
- The active Dune alias is a sibling observer alias and must keep predecessor
  aliases green.
- Residual source is audited for forbidden calls: `Frontend.parse`,
  `Mergecil.merge`, `Global.init`, and full sparse analysis entry points.
