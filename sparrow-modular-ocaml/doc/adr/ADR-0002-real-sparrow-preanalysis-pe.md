# ADR-0002 — Real Sparrow PreAnalysis PE Boundary

## Decision

Advance the real-Sparrow modular PE milestone from frontend/global into exactly:

```ocaml
Global.init -> PreAnalysis.perform
```

The accepted PreAnalysis path must include Weak interval semantics:

```ocaml
ItvSem.run AbsSem.Weak ItvSem.Spec.empty
```

## Drivers

- Preserve source-lineage fidelity with `sparrow/src/core/preAnalysis.ml`.
- Prove callgraph, pruning, memory-summary, and BTA/static-completion parity on module-only fixtures.
- Avoid overclaiming sparse/DUG, Strong interval semantics, whole-program merge equivalence, or residual linker/global-fixpoint behavior.

## Alternatives rejected

- Observer-only JSON: insufficient because it does not stage active Weak pre-analysis.
- Finite-slice/stub extension: current support stubs cannot justify real memory/callgraph/pruning parity.
- Baseline semantic edits: violate frozen-baseline evidence.
- Sparse/DUG extraction: belongs to a later milestone after `PreAnalysis.perform`.

## Consequences

- The real domain closure (`ItvDom`, `BasicDom`, `MapDom`, `InstrumentedMem`, `ApiSem`, etc.) is brought into the modular project as support for PreAnalysis.
- `residual` in BTA artifacts remains a classification label only, not executable residual code.
- Future sparse/DUG work requires a separate plan and separate acceptance criteria.
