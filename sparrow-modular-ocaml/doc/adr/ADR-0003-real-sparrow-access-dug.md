# ADR-0003 — Real Sparrow Access-DUG Construction Boundary

## Decision

Advance the real-Sparrow modular milestone from PreAnalysis into exactly:

```ocaml
Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make / Dug construction
```

The accepted claim is strict active-vs-frozen structural parity for pre-DUG sparse-spec inputs, access summaries, and DUG nodes/edges/labels on module-only fixtures.

## Drivers

- The user selected Access+DUG as the next sparse boundary.
- DUG evidence must be structural, not only lineage text.
- The milestone must preserve the frozen `sparrow/src` baseline and avoid Worklist/fixpoint overclaims.

## Alternatives considered

- Access-only parity: safer but insufficient for requested DUG evidence.
- Full sparse pipeline: rejected because it includes Worklist and sparse fixpoint scope.
- Observer-only DUG JSON: rejected because it would not be an active modular extraction milestone.

## Why chosen

Access+DUG construction is the narrowest source-lineage boundary that proves real sparse graph construction while excluding `Worklist.init`, `SparseAnalysis.perform`, widening, narrowing, convergence, PartialFlowSensitivity staging/ranking parity, and residual linker/global-fixpoint behavior.

## Consequences

- `Spec.premem`, `Spec.locset`, `Spec.locset_fs`, and `Options.pfs` mode are first-class pre-DUG parity inputs.
- `access_collection_transfer` must be documented as access instrumentation only, even if it uses the source Strong-mode transfer passed by baseline AccessAnalysis.
- Artifact normalization is mandatory because raw graph and set iteration order is not stable evidence.

## Follow-ups

Worklist order, sparse Strong fixpoint, and PFS ranking/staging require separate deep-interview and ralplan scopes.
