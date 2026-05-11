# ADR-0004 — Real Sparrow Sparse Fixpoint PE Boundary

## Decision

Advance the real-Sparrow modular milestone from Access-DUG into the ItvDom sparse fixpoint boundary:

```ocaml
Global.init
-> PreAnalysis.perform
-> AccessAnalysis.perform
-> SsaDug.make
-> Worklist.init
-> widening
-> finalize
-> narrowing when enabled/applicable
```

The accepted evidence is module-only ItvDom/ItvSem sparse-fixpoint parity against a frozen Sparrow observer, plus BTA and residual-artifact inspection showing that dynamic/residual facts are extern-dependent only.

## Drivers

- The Worklist is an execution mechanism for the fixpoint; it is not accepted as a standalone product.
- The first sparse fixpoint PE is limited to the ItvDom/ItvSem instance.
- JSON reports may summarize evidence, but a typed MetaOCaml staged artifact or generated residual source must exist.
- Baseline analyzer semantics under `sparrow/src` remain frozen.

## Alternatives considered

- Worklist-only parity: rejected because it omits widening/finalize/narrowing behavior.
- Final-table parity without staged residual evidence: rejected because it is not PE evidence.
- Domain-generic sparse PE: deferred because the selected scope is the ItvDom instance only.
- Larger analyzer scope: rejected; no whole-program merge, no PartialFlowSensitivity staging/ranking parity, no residual linker, and no residual global-fixpoint runtime claim.

## Consequences

- Active code extracts source-lineage `SparseAnalysis`, `Worklist`, and `StepManager` support into `sparrow-modular-ocaml/src`.
- Completion evidence records that widening and narrowing ran where configured, and that the worklists drained.
- StepManager text labels are not an acceptance surface; the evidence boundary is phase completion and final table parity.
- BTA reports allow only `unknown-extern-call` and `transitively-extern-dependent` reasons for residual/dynamic facts.

## Follow-ups

Future milestones may broaden domains or residual execution, but this ADR does not authorize octagon integration, PFS staging, whole-program linking, or a residual global fixpoint runtime.
