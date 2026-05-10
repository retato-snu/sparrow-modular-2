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
| `src/real_sparrow_artifact.ml` | active artifact layer | n/a | observer/serialization support | Emits stable source-lineage structural self-checks; not part of Sparrow `I`. |

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
- `_build/real-sparrow/frontend-global/frozen-parity.json` records active-vs-frozen
  structural equivalence at the `Global.init` boundary when the observer command
  is rerun.

## Out-of-scope statement

This milestone does not implement pre-analysis PE, DUG PE, interval analysis PE,
executable MetaOCaml `D code`, residual linking, or the residual global fixpoint.
Those are future milestones after this real frontend/global boundary is accepted.
