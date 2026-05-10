# Real Sparrow PreAnalysis PE — Phase 0 Closure Inventory

Status: **APPROVED — Phase 0 closed; continue to Phase 1 artifact schema**  
Parent plan: `.omx/plans/plan-real-sparrow-preanalysis-pe.md`  
Scope gate: **no extraction or PE implementation may begin until this checklist is reviewed**.

## Scope lock

This closure inventory covers only the real Sparrow boundary:

```ocaml
Global.init -> PreAnalysis.perform
```

The active milestone must stage the Weak pre-analysis transfer:

```ocaml
ItvSem.run AbsSem.Weak ItvSem.Spec.empty
```

Out of scope for this inventory and for the first acceptance claim:

- `ItvSem.run Strong` staging or Strong-mode acceptance
- sparse analysis parity (`AccessAnalysis.perform`, `SsaDug.make`, `Worklist.init`, sparse widening/narrowing)
- DUG structural equivalence
- semantic edits under `sparrow/src`
- whole-program merge equivalence
- executable residual code, residual linking, or link-time/global-fixpoint evidence

## Source evidence anchors

- `sparrow/src/core/preAnalysis.ml:19-23` — `onestep_transfer` runs `ItvSem.run AbsSem.Weak ItvSem.Spec.empty` over nodes.
- `sparrow/src/core/preAnalysis.ml:25-33` — fixpoint uses `Mem.widen`, `Mem.le`, and `Dump.le`.
- `sparrow/src/core/preAnalysis.ml:35-42` — callee extraction uses `ItvSem.eval` and `Val.pow_proc_of_val`.
- `sparrow/src/core/preAnalysis.ml:44-53` — direct call edges are added to `global.icfg`.
- `sparrow/src/core/preAnalysis.ml:55-64` — callgraph edges are added and transitive calls computed.
- `sparrow/src/core/preAnalysis.ml:66-74` — `perform` writes `global.mem`, draws edges/graph, and removes unreachable functions.
- `sparrow/src/semantics/itvSem.ml:12-24` — `ItvSem` opens/uses `Vocab`, `Sparrow_cil`, `IntraCfg`, `AbsSem`, `BasicDom`, `ItvDom`, `Global`, `ArrayBlk`, and `BatTuple`; `Spec = Spec.Make(Dom)`.
- `sparrow/src/semantics/itvSem.ml:811-899` — `run` dispatch covers assignments, external values, allocation, assumptions/pruning, calls, returns, return-node propagation, skips, and asm.
- `sparrow/src/domain/itvDom.ml:14-98` — real `Val`, `Mem`, and `Table` definitions.
- `sparrow/src/domain/basicDom.ml:16-172` — node/procedure/location/dump domains needed by the weak path.
- `sparrow/src/sparse/sparseAnalysis.ml:249-260` — sparse/DUG starts later and remains excluded.
- `sparrow-modular-ocaml/src/itvDom.ml:9-11` and `sparrow-modular-ocaml/src/basicDom.ml:9-26` — current modular domains are support-only stubs.

## Dependency checklist

| Item | Baseline evidence | Why needed for PreAnalysis PE | Current modular status | Classification | Phase 0 decision |
|---|---|---|---|---|---|
| `PreAnalysis.perform` | `preAnalysis.ml:66-74` | Boundary entry and exit sequence: nodes, fixpoint, mem write, call edges, callgraph, pruning. | Missing in active modular code. | `copied/extracted` | Extract source-lineage module after review. |
| `PreAnalysis.onestep_transfer` | `preAnalysis.ml:19-23` | Required Weak-step loop over all ICFG nodes. | Missing. | `copied/extracted` | Extract exactly; keep Weak call visible in lineage. |
| `PreAnalysis.fixpt` | `preAnalysis.ml:25-33` | Drives convergence through `Mem.widen`, `Mem.le`, `Dump.le`. | Missing. | `copied/extracted` | Extract with real domains, not stubs. |
| `PreAnalysis.callees_of` | `preAnalysis.ml:35-42` | Resolves call targets from abstract values. | Missing. | `copied/extracted` | Extract; depends on real `ItvSem.eval` and `ItvDom.Val`. |
| `PreAnalysis.draw_call_edges` | `preAnalysis.ml:44-53` | Mutates ICFG call edges from abstract callees. | InterCfg add/get call edge primitives already extracted. | `wrapped` | Reuse extracted InterCfg; preserve baseline logic. |
| `PreAnalysis.draw_callgraph` | `preAnalysis.ml:55-64` | Adds direct callgraph edges and transitive closure. | CallGraph is extracted. | `wrapped` | Reuse existing `CallGraph.add_edge` / `compute_trans_calls`. |
| `Global.remove_unreachable_functions` | `global.ml:52-65` | Final PreAnalysis pruning acceptance evidence. | Already extracted in modular `global.ml:52-65`. | `already present` | Use unchanged; artifact must compare effects. |
| `ItvSem.run` | `itvSem.ml:811-899` | Required weak transfer semantics for every command kind. | Missing in active modular code. | `copied/extracted` | Extract full real implementation needed by fixtures. |
| `ItvSem.eval` / `eval_lv` | `itvSem.mli:14-15`; `itvSem.ml:55-194` | Needed by call-target resolution and command semantics. | Missing. | `copied/extracted` | Extract with supporting value/location domains. |
| `ItvSem.Spec.empty` / `Spec.Make` | `itvSem.ml:24`; `sparse/spec.ml:28-53` | PreAnalysis passes empty spec; fields still shape semantic behavior. | Missing `Spec` module in modular active code. | `copied/extracted` | Extract `Spec` even though sparse analyses remain out of acceptance. |
| `AbsSem.update_mode` | `absSem.mli:13-15` | Provides `Weak`/`Strong` constructors; PreAnalysis must use `Weak`. | `absSem.mli` only exists as support interface in modular; no full closure checked in active path. | `copied/extracted` | Extract interface; audit must forbid accepted Strong staging. |
| `AbsSem.S` | `absSem.mli:17-25` | Signature implemented by `ItvSem`; needed by module typing. | Not active for real ItvSem. | `copied/extracted` | Extract interface only; no semantic edit. |
| `ItvDom.Val` | `itvDom.ml:14-67` | Holds interval, locations, arrays, structs, procedure values; used by `ItvSem.eval`. | Current modular `ItvDom` has no `Val`. | `replaced` | Replace support stub with real source-lineage domain. |
| `ItvDom.Mem` | `itvDom.ml:69-96` | Real memory lattice, lookup, strong/weak update, widening/le through MapDom. | Current modular `Mem.t = unit`. | `replaced` | Replace stub; required for mem summary parity. |
| `ItvDom.Table` | `itvDom.ml:98`; `global.ml:21-23` | Per-node memory table and global field type. | Current modular `Table.t = unit`. | `replaced` | Replace stub with real MapDom table. |
| `BasicDom.Proc`, `Node`, `PowProc` | `basicDom.ml:16-19` | Procedure/node sets for callgraph and PreAnalysis traversal. | Present as reduced aliases. | `already present` | Keep or align with real BasicDom extraction. |
| `BasicDom.Allocsite`, `Loc`, `PowLoc` | `basicDom.ml:21-170` | Required by ItvDom values, memory locations, allocation, returns, dump. | Mostly missing from modular `basicDom.ml`. | `replaced` | Replace/extend with real BasicDom. |
| `BasicDom.Dump` | `basicDom.ml:172-184`; `itvSem.ml:876-877` | Fixpoint convergence and call-return summary propagation. | Current modular `Dump.t = unit`. | `replaced` | Replace with real `MapDom.MakeCPO (Proc) (PowLoc)`. |
| `InstrumentedMem` | `instrumentedMem.ml:11-49` | Base memory wrapper used by `ItvDom.Mem`; provides weak updates and access tracking hooks. | Missing in modular active code. | `copied/extracted` | Extract as support closure. |
| `MapDom` | `mapDom.ml:15-202`, `301-438` | Map lattice/functor for Mem, Table, Dump, ArrayBlk, StructBlk. | Missing in modular active code. | `copied/extracted` | Extract support closure. |
| `AbsDom` | `absDom.mli:13-50` | Lattice and set signatures for domain functors. | Only modular `absDom.mli` support may exist; real closure not wired. | `copied/extracted` | Extract/align interface. |
| `PowDom` | `powDom.ml:58-129`, `147-251` | Set domains for procedures, locations, struct sets. | Missing in modular active code. | `copied/extracted` | Extract support closure. |
| `ProdDom` | `prodDom.ml:15-82` | Product domain for `ItvDom.Val`. | Missing in modular active code. | `copied/extracted` | Extract support closure. |
| `Itv` | `itv.ml:24-250+` | Numeric interval lattice used in values, array offsets/sizes, pruning. | Missing in modular active code. | `copied/extracted` | Extract support closure. |
| `ArrayBlk` | `arrayBlk.ml:16-190` | Array component of `ItvDom.Val` and allocation/string semantics. | Missing in modular active code. | `copied/extracted` | Extract support closure; no sparse claim. |
| `StructBlk` | `structBlk.ml:16-46` | Struct component of `ItvDom.Val` and struct allocation/API models. | Missing in modular active code. | `copied/extracted` | Extract support closure. |
| `ApiSem` / undefined-function models | `itvSem.ml:583-790`; `apiSem.ml:64-66` | External/library call behavior affects memory and dynamic/residual classification. | Missing in modular active code. | `copied/extracted` | Extract for semantic fidelity; classify extern-dependent facts dynamic/residual in artifacts. |
| `Options.scaffold`, `unsound_lib`, `top_location`, `bugfinder`, `verbose` | `options.ml:46-56`, `82-83`, `111-118`; `global.ml:58`; `itvSem.ml:287-297`, `757`, `852` | Behavior-affecting flags for library models, pruning/logging, unknown locations, unreachable function pruning. | Some modular options exist but need parity check. | `wrapped` | Use defaults matching frozen observer; document differences. |
| `InterCfg` command/node helpers | `interCfg.ml:96-151` | `cmdof`, `nodesof`, call/return node helpers, arg list. | Already source-lineage extracted. | `already present` | Reuse; verify API sufficient for ItvSem. |
| `IntraCfg.Cmd` constructors | `itvSem.ml:814-899`; `intraCfg.ml` | Command variants consumed by `ItvSem.run`. | Already extracted for frontend/global. | `already present` | Reuse; verify constructor shape. |
| `CallGraph` | `callGraph.ml:44-56` | Direct/transitive callgraph updates. | Already extracted. | `already present` | Reuse; normalize output ordering in artifacts. |
| Frozen reference generator | Plan requires `Global.init -> PreAnalysis.perform`. | Needed for active-vs-frozen parity. | Existing observer is frontend/global only. | `copied/extracted` | Add non-semantic `sparrow/test` observer later; no baseline semantic edits. |
| `@real_sparrow_preanalysis` alias | Test spec T2. | Required acceptance target. | Missing. | `deferred` | Add after closure review with fixtures/artifacts. |

## Current modular gap register

1. `sparrow-modular-ocaml/src/itvDom.ml:9-11` is a unit-bottom stub and cannot support real `Val`, `Mem`, `Table`, lookup, widening, or weak update.
2. `sparrow-modular-ocaml/src/basicDom.ml:9-26` is a frontend/global support surface; it lacks real `Allocsite`, `Loc`, `PowLoc`, and `Dump` semantics needed by PreAnalysis.
3. No active `PreAnalysis`, `ItvSem`, `Spec`, `ApiSem`, or domain functor closure exists under `sparrow-modular-ocaml/src`.
4. `sparrow-modular-ocaml/test/dune:10-32` only wires `real_sparrow_frontend_global`; there is no `real_sparrow_preanalysis` alias.
5. The current acceptance audit still documents analysis-stage work as future; it must be updated only when real PreAnalysis evidence exists.
6. The existing frozen observer `sparrow/test/real_frontend_global_observer.ml:25-32` stops at `Global.init`; a new non-semantic reference generator must run `Global.init -> PreAnalysis.perform`.

## Fixture shortlist for later phases

These are proposed for Phase 3/4 after closure review; they are not implemented by this Phase 0 report.

1. **direct internal call** — proves call target evaluation, direct ICFG call edge, and callgraph edge.
2. **unreachable function** — proves `Global.remove_unreachable_functions` parity.
3. **branch/pruning case** — proves `Cassume`/prune lineage and branch effect projection.
4. **global/local memory update** — proves stable memory summary projection after Weak transfers.
5. **extern-dependent case** — proves dynamic/residual classification without guessing external callees/values.
6. **extern-independent calculation** — proves staging-time completion of calculations independent of externs.

## BTA boundary decision

BTA labels are accepted only as artifact classifications for this milestone:

- `static`: calculation completed by the PreAnalysis PE boundary for selected module fixtures.
- `dynamic`: depends on external/runtime facts and must not be guessed.
- `residual`: classification-only marker for facts left to later work; it is not executable residual code, residual linking, or a link-time/global-fixpoint claim.

## Scope violation audit

- **Strong staging:** present as a type constructor and branch in source semantics, but PreAnalysis calls `AbsSem.Weak`; accepted artifacts must not claim Strong staging.
- **Sparse/DUG:** `sparrow/src/sparse/sparseAnalysis.ml:249-260` begins later with access analysis, DUG, worklist, widening/narrowing; all deferred.
- **Baseline semantic edits:** forbidden; any `sparrow/src` change must be observer/test harness only and must pass `git diff --exit-code -- sparrow/src` at final acceptance unless the test harness is intentionally committed and separately documented as non-semantic.
- **Whole-program merge/linker/global-fixpoint:** no acceptance claim in this milestone.

## Continue / stop recommendation

**Recommendation: CONTINUE after architect review.**

The closure is large but bounded for a source-lineage extraction milestone. It does not require sparse/DUG acceptance, Strong-mode staging, baseline semantic edits, or whole-program linker/global-fixpoint behavior. The main implementation risk is that current modular support stubs must be replaced or expanded with real source-lineage support modules before any PreAnalysis parity claim can be made.

Required next step before implementation:

1. Architect review of this closure report.
2. If approved, proceed to Phase 1 artifact schema and Phase 2 extraction in `sparrow-modular-ocaml`.
3. If rejected, return to ralplan with the closure report and a narrowed extraction strategy.

## Review record

- Ralph Phase 0 created this closure report before implementation.
- Architect review: **APPROVE** (2026-05-10 UTC / 2026-05-11 KST).
- Must-fix notes carried forward:
  1. Phase 1 artifact schema must be completed before extraction.
  2. Use unambiguous full source paths in docs/schema references.
  3. Do not extend support stubs for parity; replace/extract real source-lineage domains.
  4. Preserve the no Strong/sparse/DUG/whole-program/residual-linker scope lock.
