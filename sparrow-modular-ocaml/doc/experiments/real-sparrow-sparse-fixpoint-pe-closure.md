# Real Sparrow Sparse Fixpoint PE Closure Inventory

Status: **APPROVED for Ralph implementation**  
Parent plan: `.omx/plans/plan-real-sparrow-sparse-fixpoint-pe.md`  
Boundary: `Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> Worklist.init -> widening -> finalize -> narrowing`

## Scope lock

This milestone extracts and wraps the real Sparrow sparse fixpoint for the ItvDom/ItvSem instance only. It includes Worklist initialization, widening, finalization, and narrowing when enabled/applicable.

The milestone does not claim a standalone interval-analysis PE pipeline, a reusable interval-domain specializer, octDom support, PFS staging/ranking parity, whole-program merge equivalence, executable residual linking, or residual global-fixpoint runtime behavior. The baseline analyzer under `sparrow/src` remains frozen; the baseline-side addition is a non-semantic observer under `sparrow/test`.

## Dependency matrix

| Component | Source anchor | Classification | Handling |
|---|---|---|---|
| `SparseAnalysis.Make` | `sparrow/src/sparse/sparseAnalysis.ml:116-260` | copied/extracted | Preserve source phase order and add active-only completion counters. |
| `Worklist` | `sparrow/src/sparse/worklist.ml:154-238` | copied/extracted | Used only as the fixpoint execution worklist. |
| `StepManager` | `sparrow/src/util/stepManager.ml:27-37` | copied/extracted | Compatibility wrapper; label text is not parity evidence. |
| `ItvSem` / `ItvDom.Mem` | `sparrow/src/semantics/itvSem.ml`, `sparrow/src/domain/itvDom.ml` | existing active instance | First concrete sparse-fixpoint domain. |
| `AccessAnalysis` / `SsaDug` | Access-DUG milestone | predecessor dependency | Reused to construct sparse-spec and DUG inputs. |
| Frozen observer | `sparrow/test/real_sparse_fixpoint_observer.ml` | test-only wrapper | Emits final tables and coarse completion evidence from baseline. |
| Residual artifact | active MetaOCaml wrapper | PE evidence support | A typed staged value plus inspection source prove JSON is summary-only. |

## Static/dynamic boundary

Static work includes module-local sparse-fixpoint facts and final tables not rooted in unknown extern calls. Dynamic/residual facts must be rooted in unknown extern calls or transitively depend on those roots. Any other residual reason is a scope breach and must fail the BTA check.

## Verification reports

The alias `@real_sparrow_sparse_fixpoint_pe` writes:

- `real_sparrow_sparse_fixpoint_pe.source-lineage-check.json`
- `real_sparrow_sparse_fixpoint_pe.bta-report.json`
- `real_sparrow_sparse_fixpoint_pe.residual-inspection.json`
- `real_sparrow_sparse_fixpoint_pe.audit.json`
