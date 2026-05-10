# Traceability — Faithful Sparrow-Pipeline PE Coverage

## Scope

Interval-only finite PE milestone for modular frontend, pre-analysis, DUG-equivalent artifact, main-analysis PE, residual linking/global-fixpoint evidence, and parity reports.

## Baseline pipeline anchors

- `sparrow/src/core/frontend.ml:35-68`
- `sparrow/src/program/global.ml:67-75`
- `sparrow/src/core/preAnalysis.ml:19-34`
- `sparrow/src/core/preAnalysis.ml:66-74`
- `sparrow/src/sparse/accessAnalysis.ml:121-132`
- `sparrow/src/sparse/ssaDug.ml:176-352`
- `sparrow/src/sparse/dug.ml:18-53`
- `sparrow/src/semantics/itvSem.ml:811-899`
- `sparrow/src/sparse/worklist.ml:223-245`
- `sparrow/src/sparse/sparseAnalysis.ml:85-184`

## Active implementation mapping

- `src/pipeline.ml` emits frontend, pre-analysis, DUG, main-analysis, and residual-global-fixpoint JSON artifacts.
- `src/cases.ml` writes Stage-1 summaries with executable `Residual.D` code or static interval values.
- `src/residual.ml` stores executable MetaOCaml `Trx.code` for loop/if residuals.
- `src/summary.ml` serializes residual source/checksum and pipeline artifacts.

## Fixture matrix

- `pipeline_extern_loop`: residual loop, final `a.return=[3,3]`.
- `pipeline_dynamic_if`: residual if, final `a.return=[1,1]`.
- `pipeline_static_local`: fully static, final `a.return=[3,3]`.
- `pipeline_unsupported_pointer`: explicit unsupported diagnostic.

## Stage evidence

T2 frontend reports; T3 pre-analysis reports; T4 DUG reports; T5 main-analysis PE reports/summaries; T6 residual global-fixpoint report; T7 parity reports.

## ADR coverage

ADR-001 through ADR-005 cover pipeline shadowing, interval boundary, fixture contract, staged artifact boundaries, and residual global-fixpoint strategy.

## Experiment coverage

`experiment-001-timeboxed-sparrow-lib-dependency-reuse.md` records the dependency reuse trial and decision to proceed with traceable mirroring.

## Validation gates

`dune exec test/research_record_audit.bc -- --adr-dir doc/adr --experiment-dir doc/experiments --traceability doc/traceability.md` plus T0–T13 from `.omx/plans/test-spec-faithful-sparrow-pipeline-pe-interval.md`.

## Known gaps / threats to validity

The milestone is finite, interval-only, and not a complete global proof. Unsupported constructs fail explicitly rather than being analyzed.
