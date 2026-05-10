# Real Sparrow Access-DUG Closure Inventory

Status: **APPROVED for extraction**  
Parent plan: `.omx/plans/plan-real-sparrow-sparse-access-dug.md`  
Boundary: `Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make / Dug construction`

## Scope lock

This milestone extracts and wraps real Sparrow AccessAnalysis and DUG construction only. It does not claim or execute `Worklist.init`, `SparseAnalysis.perform`, sparse Strong fixpoint iteration, widening, narrowing, convergence, PartialFlowSensitivity staging/ranking parity, whole-program merge, executable residual linking, or residual global-fixpoint behavior.

The baseline analyzer under `sparrow/src` remains frozen. The only allowed baseline-side addition is a non-semantic observer under `sparrow/test`.

## Dependency matrix

| Component | Source anchor | Classification | Handling |
|---|---|---|---|
| `AccessSem.Make` | `sparrow/src/sparse/accessSem.ml:25-33` | copied/extracted | Use exact source lineage in `sparrow-modular-ocaml/src/accessSem.ml{,i}`. |
| `AccessAnalysis.Make` / `perform` | `sparrow/src/sparse/accessAnalysis.ml:25-131` | copied/extracted | Use exact source access pre-analysis pipeline. |
| `Access.Info`, proc/reach/local/def/use indexes | `sparrow/src/sparse/access.ml` | already present | Existing modular `access.ml{,i}` already matches source and exposes required indexes. |
| `Dug.Make` | `sparrow/src/sparse/dug.ml:55-173` | copied/extracted | Use source graph representation and labels. Normalize artifacts outside raw graph order. |
| `SsaDug.Make` | `sparrow/src/sparse/ssaDug.ml:27-398` | copied/extracted | Use source DUG construction functions through `make`; no Worklist handoff. |
| `Profiler` calls from `SsaDug` | `sparrow/src/sparse/ssaDug.ml:237-302` | wrapped | Provide no-op profiler support for construction timing hooks only. |
| `StepManager` | `sparrow/src/sparse/sparseAnalysis.ml:252-255` | deferred | Full phase orchestration belongs to `SparseAnalysis.perform`, out of scope. |
| `Worklist` | `sparrow/src/sparse/sparseAnalysis.ml:254` | deferred | Explicitly excluded. |
| Sparse widening/narrowing/fixpoint | `sparrow/src/sparse/sparseAnalysis.ml:256-260` | deferred | Explicitly excluded. |
| `Options.unsound_recursion` | `sparrow/src/sparse/ssaDug.ml:60-111` | already present | Existing options support recursion flag used by DUG construction. |
| `Options.pfs` | `sparrow/src/core/options.ml:22`, `sparrow/src/strategy/partialFlowSensitivity.ml:718-722` | wrapped input contract | Acceptance fixtures pin default `pfs=100`, making `locset_fs=locset`; no PFS ranking/staging parity claim. |
| `Spec.premem` | `sparrow/src/instance/itvAnalysis.ml:335-336` | wrapped | Derived as post-PreAnalysis `global.mem`. |
| `Spec.locset` | `sparrow/src/instance/itvAnalysis.ml:319-335` | wrapped | Derived by the source `get_locset` rule over `premem`. |
| `Spec.locset_fs` | `sparrow/src/instance/itvAnalysis.ml:331-336` | wrapped | Pinned to `locset` under `Options.pfs=100` for accepted fixtures. |
| Frozen observer | `sparrow/test/real_access_dug_observer.ml` | wrapped | New non-semantic test harness only; no `sparrow/src` edits. |

## Sparse-spec derivation contract

The active and frozen paths must emit these pre-DUG inputs before Access/DUG parity is accepted:

1. `premem`: `global.mem` after `PreAnalysis.perform`.
2. `locset`: fold `premem`, add each memory key, union pointer locations from values, and add allocation-site locations from values, matching `ItvAnalysis.get_locset`.
3. `locset_fs`: acceptance mode pins `Options.pfs = 100`, so source `PartialFlowSensitivity.select global locset` is equivalent to `locset`.
4. `Options.pfs`: emitted as `100` with derivation mode `default-full-flow-sensitive-input-contract`.

If an implementation requires `Options.pfs < 100`, PFS ranking, `SparseAnalysis.initialize/finalize`, Worklist, or sparse fixpoint behavior to make the DUG claim pass, stop and return to planning.

## `access_collection_transfer` call-context note

Allowed call path:

```ocaml
AccessAnalysis.perform -> AccessSem.accessof -> access_collection_transfer
```

The transfer may use the same source Strong-mode transfer function that baseline access collection passes into `AccessAnalysis.perform`, but only as per-node access instrumentation. Forbidden call paths and claims: `Worklist.init`, `SparseAnalysis.perform`, widening, narrowing, convergence, and Strong sparse fixpoint parity.

## Review record

- Ralph Phase 0 result: **CONTINUE**.
- Reason: AccessAnalysis and SsaDug can be constructed from source-lineage modules after PreAnalysis using explicit sparse-spec inputs, without Worklist or sparse fixpoint execution.
