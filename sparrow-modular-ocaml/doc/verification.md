# Verification notes

This file is the canonical verification checklist for the active
`sparrow-modular-ocaml` slice.  Use it when validating local changes before
leader integration.

## Command matrix

Run commands from the repository root unless a command explicitly changes
working directory.

| Check | Command | Expected evidence |
| --- | --- | --- |
| Active build, typecheck, and test suite | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @runtest` | Dune completes successfully; this covers the aggregated active aliases in `test/dune`. |
| Focused MetaOCaml sparse PE gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_metaocaml_sparse_pe` | Sparse PE source-lineage, module-boundary, provenance, BTA, residual, forbidden-shortcut, and audit reports are regenerated. |
| Focused residual-linking prototype gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe` | The residual-linking prototype report is regenerated. |
| Typed scalar-call protocol unit gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune exec test/abstract_speculate_residual_scalar_call_unit.bc` | Unit checks validate typed scalar constructors, full-Itv scalar normalization, v1/v2 JSON compatibility, and mismatch rejection. |
| Focused residual-linking oracle-suite gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite` | The oracle-suite report is regenerated, including positive and negative witnesses. |
| Documentation build | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @doc` | Package documentation builds successfully when documentation tooling is installed. |
| Frozen baseline regression | `cd sparrow && opam exec --switch sparrow -- dune runtest --force` | The frozen baseline/oracle passes under its own switch. |
| Frozen baseline no-edit audit | `git diff --exit-code -- sparrow` | No active change modified the frozen baseline tree. |
| Dynamic-cell source-policy audit | `grep -R "FlatD\|SuspensionMarker\|TODO_D_MARKER" sparrow-modular-ocaml/src sparrow-modular-ocaml/test && exit 1 || true` | No forbidden flat dynamic markers or TODO marker strings are present in active sources/tests. |

There is no dedicated lint alias in the current Dune configuration.  For docs
changes, run the available Markdown linter directly on the touched file when it
is installed; otherwise record the missing tool and rely on Dune/build checks
plus review of the rendered Markdown table.



## Residual API model coverage checklist

Use this focused checklist when validating residual API model coverage for
`memcpy`, `strcpy`, and `strlen` only:

1. Confirm each coverage-matrix row names the baseline `ItvSem` / `ApiSem`
   anchor, abstract effect, residual equation/cell target, upstream source
   dependencies, stage-2 seed/provenance, positive fixture, and negative
   mutation.
2. Confirm residual equations read semantically upstream cells through solver
   state; empty, target/self-only, or instrumentation-only dependencies fail.
3. Confirm positive PE/oracle witnesses exist for all three APIs and expose
   `api_baseline_source`, `api_abstract_effect`,
   `api_semantic_provenance=residual-api-model-coverage/slice-1`, final cell
   provenance, seed reads, state reads, and exact dependencies.
4. Confirm negative mutations reject missing state reads, missing seed reads,
   empty dependencies, target/self-only dependencies, metadata-only API rows,
   unsupported optional APIs marked as covered, and corrupted provenance.
5. Reconfirm unsupported APIs remain non-claims: `memmove`, `strncpy`, `strcat`,
   `strdup` / `xstrdup`, input-buffer APIs, `fgets`, `scanf`, generic
   allocation APIs, broad `ApiSem` entries, disabled `memset`, arbitrary-C API
   semantics, non-Itv product domains, and whole-program equivalence.

## Typed residual scalar-call protocol checklist

Use this focused checklist when validating changes for
`.omx/plans/prd-residual-call-protocol-scalar.md`:

1. Confirm scalar return/call evidence is constructed through the shared typed
   scalar-call protocol before JSON encoding.
2. Confirm legacy compatibility fields remain present: `extern_scalar_value`,
   `function_return_summary`, `return_effects`, `external_summary`, and
   `linked_stage2_input_derivation`.
3. Confirm additive metadata (`scalar_protocol_schema`,
   `scalar_call_protocol_id`, `scalar_value_kind`, and
   `typed_scalar_metadata_valid`) is deterministic wherever the same return
   effect is duplicated.
4. Run the scalar unit gate, residual-linking PE gate, oracle-suite gate, and
   proof alias.  Record any environment-only blocker with the focused alias that
   did pass.
5. Review negative evidence for value, location, provider hash, effect id, and
   metadata mismatch rejection; the oracle relation should surface
   `typed_scalar_protocol_mismatch` for scalar protocol violations.
6. Reconfirm non-goals: no Oct/Taint semantics, no broad call-graph rewrite, no
   proof-system expansion, and no fixture-only proof.

## Fresh Task 4 evidence

Evidence collected for the docs/verification task on 2026-05-18:

- Active build/typecheck/test: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @runtest` passed.
- Focused residual-linking prototype gate: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe` passed.
- Focused residual-linking oracle-suite gate: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite` passed.
- Documentation build: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @doc` passed.
- Baseline regression: `cd sparrow && opam exec --switch sparrow -- dune runtest --force` passed.
- Baseline no-edit audit: `git diff --exit-code -- sparrow` passed.
- No-flat-D source audit: `grep -R "FlatD\|SuspensionMarker\|TODO_D_MARKER" sparrow-modular-ocaml/src sparrow-modular-ocaml/test && exit 1 || true` passed.

## Current semantic verification notes

- Stage 1 reads `--module-a` and `--module-b`: module A source determines the supported loop/if residual shape, and module B source supplies the exported `n` value. A mismatched `--case` is rejected.
- Summary load validates residual `source` against `artifact_source_checksum` and the executable generator before reconstructing `Trx.code`; the round-trip test also verifies a deliberately corrupted source/checksum is rejected.
- The executed MetaOCaml quotes contain the residual loop iteration (`let rec iterate ... if next = header ...`) and dynamic branch selection (`if then_path ... else if ...`) instead of delegating the whole residual to flat helper calls.

One environment note: running the frozen baseline under the current
`MetaOCaml-full` switch fails due to a `goblint-cil` constructor-shape mismatch.
The baseline passes under the existing `sparrow` switch, so no baseline source
change was made.
