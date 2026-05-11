# Real Sparrow Sparse Fixpoint PE Experiment

## Accepted claim

The active modular path executes real Sparrow source-lineage sparse fixpoint code for module-only ItvDom/ItvSem fixtures and emits staged sparse-fixpoint PE evidence. The accepted boundary is:

```ocaml
Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> Worklist.init -> widening -> finalize -> narrowing
```

## Fixtures

The verification alias covers loop widening, branch/join propagation, a narrowing-enabled loop, an extern-dependent call, an extern-independent static case, and local/global memory.

## Evidence shape

- Final input/output table parity is checked against a frozen Sparrow observer.
- Completion evidence records `worklist_initialized`, `widening_performed`, `widening_iterations`, `finalize_performed`, `narrowing_enabled`, `narrowing_applicable`, `narrowing_performed`, and drained worklists.
- Reports pin `pfs = 100` and `pfs_binding_path = false`.
- BTA reports permit residual/dynamic facts only for `unknown-extern-call` roots or `transitively-extern-dependent` facts.
- Residual inspection requires a typed MetaOCaml residual artifact path; JSON is summary-only.

## Non-claims

This experiment is not a Worklist-only milestone, not a domain-generic sparse PE, and not a reusable interval-domain specializer. It does not cover octagon analysis, PartialFlowSensitivity staging/ranking parity, whole-program merge equivalence, executable residual linking, strict StepManager label parity, or residual global-fixpoint runtime behavior.

## Run

```bash
(cd sparrow-modular-ocaml && dune build @real_sparrow_sparse_fixpoint_pe)
```
