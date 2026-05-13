# Real Sparrow Frontend/Global Lineage

Primary reference: `Doc/FOUNDATIONS.md`.

This milestone starts the active modular implementation from real Sparrow
frontend/global components.  The program-input `I` remains the frozen Sparrow
analyzer; this directory records the active extraction used to build the first
per-module frontend/global artifacts.

## Component map

| Active file | Original source | Original anchors | Lineage classification | Notes |
|---|---|---:|---|---|
| `src/sparrow_cil.ml` | `sparrow/src/sparrow_cil.ml` | full file | exact namespace fork | Keeps Sparrow's aliases to Goblint-CIL modules and MakeCFG. |
| `src/vocab.ml{,i}` | `sparrow/src/util/vocab.ml{,i}` | full file | mechanical compatibility adaptation | `Base.compare_*` replaced with stdlib compares for the active switch. |
| `src/options.ml{,i}` | `sparrow/src/core/options.ml{,i}` | full file | exact/support fork | Required by frontend/CFG modules; not an analysis claim. |
| `src/cilHelper.ml{,i}` | `sparrow/src/util/cilHelper.ml{,i}` | full file | mechanical compatibility adaptation | `CastE` patterns adapted to installed Goblint-CIL three-argument constructor. |
| `src/intraCfg.ml{,i}` | `sparrow/src/program/intraCfg.ml{,i}` | `Node`/`Cmd` definitions, `init` lines ~796-831, `generate_global_proc` lines ~833-862 | mechanical compatibility adaptation | Preserves Sparrow command conversion, `_G_` generation, and CFG transformations; `CastE` updated for API drift; deriving compare replaced with explicit compare. |
| `src/interCfg.ml{,i}` | `sparrow/src/program/interCfg.ml{,i}` | types lines ~16-50, CFG generation lines ~69-78, `init` lines ~254-258, JSON surface lines ~175-183 | mechanical compatibility adaptation | Preserves `InterCfg.init`, `IntraCfg.init`, global proc, optimization/dom/SCC path, and JSON surface. |
| `src/callGraph.ml{,i}` | `sparrow/src/program/callGraph.ml{,i}` | full file | support fork | Needed because `Global.t` contains `CallGraph.t`; callgraph remains empty at `Global.init`. |
| `src/global.ml{,i}` | `sparrow/src/program/global.ml{,i}` | `type t` lines ~16-23, `init` lines ~67-75, `to_json` lines ~87-92 | near-exact fork plus interface exposure | Preserves `Global.init`: `InterCfg.init`, `CallGraph.empty`, support bottoms, and `remove_unreachable_nodes`; `to_json` exposed for observer output. |
| `src/basicDom.ml` | `sparrow/src/domain/basicDom.ml` | names required by `Global.t` and `CallGraph` | support-only extraction | Provides only Proc/Node/PowProc/Dump names needed at this boundary; not analysis PE evidence. |
| `src/itvDom.ml{,i}` | `sparrow/src/domain/itvDom.ml{,i}` | `Mem.bot`, `Table.bot` names required by `Global.t` | support-only extraction | Bottom markers only; not interval-analysis implementation. |
| `src/real_sparrow_frontend.ml` | `sparrow/src/core/frontend.ml` | `parseOneFile` lines ~35-43, `parse` lines ~45-55, `makeCFGinfo` lines ~57-67 | frontend wrapper over forked primitives | Per-module path is `parseOneFile -> makeCFGinfo -> Global.init`; merge remains reference-only. |
| `src/real_sparrow_global.ml` | `sparrow/src/program/global.ml` | `Global.init` and `to_json` anchors above | observer wrapper | Calls extracted `Global.init` and emits lineage metadata. |
| `src/real_sparrow_artifact.ml` | active artifact layer | n/a | observer/serialization support | Emits stable active-vs-frozen structural comparisons; not part of Sparrow `I`. |

## Boundary facts

- Per-module acceptance path: `parseOneFile -> makeCFGinfo -> Global.init`.
- The active `Global.init` now calls the extracted Sparrow `InterCfg.init`, which
  calls extracted `IntraCfg.init`, creates `_G_`, applies Sparrow CFG conversion,
  and runs the original `remove_unreachable_nodes` boundary.
- `Global.init` initializes an empty callgraph.  Callgraph construction and
  unreachable-function pruning remain later `PreAnalysis.perform` behavior.

## Compatibility adaptations

The installed MetaOCaml-full switch uses Goblint-CIL where `CastE` has shape
`castkind * typ * exp`.  Active copied files were mechanically adapted at the
constructor sites needed for frontend/global.  No baseline `sparrow/src` analyzer
semantics were edited.

## Verification status

- Active source-lineage extraction builds and emits real Sparrow-shaped CFGs.
- Frozen executable observer parity now runs in the MetaOCaml-full switch via
  `sparrow/test/real_frontend_global_observer.ml`, a non-semantic test harness
  outside `sparrow/src`.
- `sparrow-modular-ocaml/_build/default/test/real_sparrow_frontend_global.source-lineage-check.json`
  records active-vs-frozen structural equivalence at the `Global.init` boundary
  when `dune build @real_sparrow_frontend_global` is rerun.  The alias writes
  compared active and frozen module artifacts under
  `_build/real-sparrow/frontend-global/{active,frozen}`.

## Frontend/global-only boundary record

The original frontend/global milestone stopped at `Global.init`; it did not claim
later analysis-boundary behavior. ADR-0002 supersedes that older boundary for the
current PreAnalysis milestone, which now records module-only Weak
`PreAnalysis.perform` evidence. Still out of scope: Strong staging, sparse/DUG
parity, whole-program merge equivalence, executable MetaOCaml `D code`, residual
linking, and the residual global fixpoint.

## PreAnalysis PE lineage extension

The PreAnalysis milestone extends the accepted path from:

```ocaml
parse_one_file -> make_cfg_info -> Global.init
```

to:

```ocaml
parse_one_file -> make_cfg_info -> Global.init -> PreAnalysis.perform
```

Source anchors:

- `sparrow/src/core/preAnalysis.ml:19-23` — `onestep_transfer` applies `ItvSem.run AbsSem.Weak ItvSem.Spec.empty`.
- `sparrow/src/core/preAnalysis.ml:25-33` — fixpoint over `Mem`/`Dump`.
- `sparrow/src/core/preAnalysis.ml:35-64` — call edge and callgraph construction.
- `sparrow/src/core/preAnalysis.ml:66-74` — write `global.mem`, draw edges/callgraph, remove unreachable functions.

Non-claims: no Strong staging, no sparse/DUG parity, no whole-program merge equivalence, and no executable residual linker/global-fixpoint behavior.

## Access-DUG lineage extension

The Access-DUG milestone extends the accepted path from:

```ocaml
parse_one_file -> make_cfg_info -> Global.init -> PreAnalysis.perform
```

to:

```ocaml
parse_one_file -> make_cfg_info -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make
```

Source anchors:

- `sparrow/src/sparse/accessSem.ml:25-33` — access instrumentation wrapper.
- `sparrow/src/sparse/accessAnalysis.ml:36-131` — access summary construction and indexes.
- `sparrow/src/sparse/dug.ml:55-173` — DUG graph and labels.
- `sparrow/src/sparse/ssaDug.ml:176-352` — def/use preparation and intra/inter DUG construction.
- `sparrow/src/instance/itvAnalysis.ml:319-336` — sparse-spec input derivation for interval sparse analysis.

Non-claims: no Worklist order, no `SparseAnalysis.perform`, no Strong sparse fixpoint, no widening/narrowing/convergence, no PartialFlowSensitivity staging/ranking parity, no whole-program merge equivalence, and no executable residual linker/global-fixpoint behavior.

## Sparse fixpoint PE lineage extension

The sparse-fixpoint PE milestone extends the accepted path from:

```ocaml
parse_one_file -> make_cfg_info -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make
```

to:

```ocaml
parse_one_file -> make_cfg_info -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> Worklist.init -> widening -> finalize -> narrowing
```

Source anchors:

- `sparrow/src/sparse/worklist.ml:154-238` — workorder, loop heads, worklist set/order operations, and `Worklist.init`.
- `sparrow/src/sparse/sparseAnalysis.ml:116-184` — widening/narrowing node transfer and worklist iteration.
- `sparrow/src/sparse/sparseAnalysis.ml:232-260` — initialize/finalize and source phase order.
- `sparrow/src/util/stepManager.ml:27-37` — phase wrapper compatibility; labels are not parity evidence.
- `sparrow/src/instance/itvAnalysis.ml:319-342` — Itv sparse-spec derivation and call to sparse analysis.

Non-claims: no Worklist-only product boundary, no standalone interval-analysis PE pipeline, no reusable interval-domain specializer, no octDom integration, no PartialFlowSensitivity staging/ranking parity, no whole-program merge equivalence, no executable residual linker, no residual global-fixpoint runtime, and no strict StepManager label parity requirement.

## Premerge linked observer lineage extension

The premerge linked observer lane intentionally leaves the module-only sparse
boundary and observes linked multi-file inputs:

```ocaml
Frontend.parse / Mergecil.merge
-> Frontend.makeCFGinfo
-> Global.init
-> PreAnalysis.perform
-> AccessAnalysis.perform
-> SsaDug.make
-> SparseAnalysis.perform
-> executable residual recomposition
```

Active anchors:

- `src/real_sparrow_frontend.ml` — `global_for_files` uses the existing real
  multi-file parser and sets `Mergecil.ignore_merge_conflicts := true` for
  baseline-compatible linked merge behavior.
- `src/real_sparrow_premerge_linked_observer.ml` — linked ItvDom sparse final-table
  artifacts, extern-only BTA, and standalone executable residual recomposition.
- `sparrow/test/real_staged_linking_observer.ml` — frozen observer with legacy upstream filename; used only as premerge linked reference output
  for linked final-table parity.

This is a contrast/reference lane only; it does not supersede Abstract
Speculate module-local PE or post-PE residual-linking claims.  Predecessor module-only aliases and their schemas remain
unchanged and still make no executable residual linker or residual global-fixpoint
claim.  Still out of scope: Alarm/Report PE, PFS staging/ranking,
octDom/domain-generic PE, and any baseline `sparrow/src` semantic edit.
