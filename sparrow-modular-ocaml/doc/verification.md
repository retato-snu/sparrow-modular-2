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
| Focused residual-linking oracle-suite gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite` | The oracle-suite report is regenerated, including positive and negative witnesses. |
| Documentation build | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @doc` | Package documentation builds successfully when documentation tooling is installed. |
| Frozen baseline regression | `cd sparrow && opam exec --switch sparrow -- dune runtest --force` | The frozen baseline/oracle passes under its own switch. |
| Frozen baseline no-edit audit | `git diff --exit-code -- sparrow` | No active change modified the frozen baseline tree. |
| Dynamic-cell source-policy audit | `grep -R "FlatD\|SuspensionMarker\|TODO_D_MARKER" sparrow-modular-ocaml/src sparrow-modular-ocaml/test && exit 1 || true` | No forbidden flat dynamic markers or TODO marker strings are present in active sources/tests. |

There is no dedicated lint alias in the current Dune configuration.  For docs
changes, run the available Markdown linter directly on the touched file when it
is installed; otherwise record the missing tool and rely on Dune/build checks
plus review of the rendered Markdown table.

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
