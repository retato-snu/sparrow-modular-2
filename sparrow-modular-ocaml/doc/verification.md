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
| Focused residual-linking oracle-suite gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite` | The oracle-suite report is regenerated, including positive and negative witnesses plus the post-link `global_residual_*` fixpoint/equivalence gate. |
| Documentation build | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @doc` | Package documentation builds successfully when documentation tooling is installed. |
| Frozen baseline regression | `cd sparrow && opam exec --switch sparrow -- dune runtest --force` | The frozen baseline/oracle passes under its own switch. |
| Frozen baseline no-edit audit | `git diff --exit-code -- sparrow` | No active change modified the frozen baseline tree. |
| Dynamic-cell source-policy audit | `grep -R "FlatD\|SuspensionMarker\|TODO_D_MARKER" sparrow-modular-ocaml/src sparrow-modular-ocaml/test && exit 1 || true` | No forbidden flat dynamic markers or TODO marker strings are present in active sources/tests. |
| Whole-program residual-global no-overclaim audit | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @whole_program_residual_global_fixpoint_verification` | The audit report passes: frozen `sparrow/` diff is clean, no flat dynamic marker strings are present in active source/test paths, required non-goals remain documented, and no implemented whole-program residual-global claim appears without executable global-residual evidence. |

There is no dedicated lint alias in the current Dune configuration.  For docs
changes, run the available Markdown linter directly on the touched file when it
is installed; otherwise record the missing tool and rely on Dune/build checks
plus review of the rendered Markdown table.


## Whole-program residual-global fixpoint verification guard

The `@whole_program_residual_global_fixpoint_verification` alias is an
automated guard for the plan in
`.omx/plans/ralplan-whole-program-residual-global-fixpoint.md`. It is
intentionally a claim-hygiene and evidence audit, not a substitute for the
implementation lanes. Until executable global-residual fixpoint artifacts exist,
the audit requires the status documents to keep whole-program residual global
fixpoint as a non-claim. Once an oracle-suite artifact directory is supplied to
the audit, any positive implementation wording still has to remain
witness-bounded, prototype/non-public, and explicit about the hard non-goals.

The audit preserves these hard non-goals: no arbitrary-C theorem, no full
analyzer rewrite, no frozen `sparrow/` analyzer edit, no new dependency, no
all-domain generality, no docs-only implementation claim, and no public
API/schema promise.


## Whole-program residual-global fixpoint checklist

Use this checklist when validating the post-link residual-global worklist slice:

1. Confirm linked artifacts expose `global_residual_fixpoint_run=true`,
   `global_residual_fixpoint_scope=post-link-whole-program-residual-cells`,
   `global_sparse_fixpoint_component=residual-global-worklist`, and
   `global_sparse_fixpoint_source_level_rerun=false`.
2. Confirm the global report has non-empty seed cells, derived cells, equations,
   dependency edges, cross-module dependency edges, worklist schedule, state
   reads, and seed reads, with `global_residual_worklist_drained=true` and
   `global_residual_overlay_only=false`.
3. For cycle/SCC witnesses, confirm `global_residual_iteration_count > 1` and a
   repeated `changed-cell-dependent` schedule entry.
4. Confirm `full_itv_semantic_relation.global_residual_equivalence_status` is
   derived by the checker/reporting layer, not asserted by the linker module.
5. Confirm negative cases reject metadata-only reports, non-recomputed derived
   cells, missing dependencies/state reads, and undrained worklists.
6. Reconfirm non-goals: no arbitrary-C theorem, no full source-level sparse
   analyzer rerun, no Oct/OctImpact/product-domain generality, and no public
   schema guarantee.


## Whole-program residual-global fixpoint checklist

Use this checklist when validating the post-link residual-global worklist slice:

1. Confirm linked artifacts expose `global_residual_fixpoint_run=true`,
   `global_residual_fixpoint_scope=post-link-whole-program-residual-cells`,
   `global_sparse_fixpoint_component=residual-global-worklist`, and
   `global_sparse_fixpoint_source_level_rerun=false`.
2. Confirm the global report has non-empty seed cells, derived cells, equations,
   dependency edges, cross-module dependency edges, worklist schedule, state
   reads, and seed reads, with `global_residual_worklist_drained=true` and
   `global_residual_overlay_only=false`.
3. For cycle/SCC witnesses, confirm `global_residual_iteration_count > 1` and a
   repeated `changed-cell-dependent` schedule entry.
4. Confirm `full_itv_semantic_relation.global_residual_equivalence_status` is
   derived by the checker/reporting layer, not asserted by the linker module.
5. Confirm negative cases reject metadata-only reports, non-recomputed derived
   cells, missing dependencies/state reads, and undrained worklists.
6. Reconfirm non-goals: no arbitrary-C theorem, no full source-level sparse
   analyzer rerun, no Oct/OctImpact/product-domain generality, and no public
   schema guarantee.

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
6. Reconfirm non-goals: no Oct semantics, no general Taint/product-domain
   parity beyond named bounded product evidence, no broad call-graph rewrite,
   no proof-system expansion, and no fixture-only proof.

## ExternalSummary v3 memory-delta checklist

Use this focused checklist when validating changes for the v3 memory-delta
contract:

1. Confirm global/pointer memory evidence is constructed through
   `Abstract_speculate_residual_memory_delta` before JSON encoding.
2. Confirm `memory_deltas`, `delta_chains`, and `memory_delta_validation` are
   present in generated ExternalSummary v3 reports; legacy `global_effects` and
   `pointer_effects` must be marked compatibility projections only.
3. Confirm PE and oracle-suite negative cases reject role swaps, wrong raw or
   normalized locations, wrong value transitions, missing/wrong provenance, and
   missing/corrupted chains even when legacy projection fields remain plausible.
4. Run the memory-delta unit gate, residual-linking PE gate, oracle-suite gate,
   `@check`, and `git diff --check`.
5. Reconfirm non-goals: no solver rewrite, no proof-system expansion, no Oct
   semantics, no general Taint/product-domain parity, no broad call/link
   scheduler rewrite, and no public API/schema promise.

## Fresh Task 4 evidence

Evidence collected for the verification/no-overclaim task on 2026-05-20:

- Active typecheck: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @check` passed.
- Focused whole-program residual-global no-overclaim audit: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @whole_program_residual_global_fixpoint_verification` passed.
- Focused residual-linking prototype gate: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe` passed.
- Focused residual-linking oracle-suite gate: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite` passed.
- Documentation build: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @doc` passed with pre-existing odoc include-resolution warnings from `src/sparrow_cil.ml`.
- Baseline no-edit audit: `git diff --exit-code -- sparrow` passed.
- No-flat-D source audit: `grep -R "FlatD\|SuspensionMarker\|TODO_D_MARKER" sparrow-modular-ocaml/src sparrow-modular-ocaml/test && exit 1 || true` passed.
- Active aggregate test note: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @runtest` was blocked in this detached worker worktree because the frozen `sparrow` submodule observer executables were not checked out (`test/real_frontend_global_observer.exe`, `test/real_sparse_fixpoint_observer.exe`, `test/real_access_dug_observer.exe`, `test/real_staged_linking_observer.exe`). Focused active residual gates above passed without broadening the claim.
- Formatting/lint note: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @fmt` is not an available formatter gate in this worktree because no `.ocamlformat` is present; Dune reports ocamlformat disabled outside a detected project.

## Current semantic verification notes

- Stage 1 reads `--module-a` and `--module-b`: module A source determines the supported loop/if residual shape, and module B source supplies the exported `n` value. A mismatched `--case` is rejected.
- Summary load validates residual `source` against `artifact_source_checksum` and the executable generator before reconstructing `Trx.code`; the round-trip test also verifies a deliberately corrupted source/checksum is rejected.
- The executed MetaOCaml quotes contain the residual loop iteration (`let rec iterate ... if next = header ...`) and dynamic branch selection (`if then_path ... else if ...`) instead of delegating the whole residual to flat helper calls.

One environment note: running the frozen baseline under the current
`MetaOCaml-full` switch fails due to a `goblint-cil` constructor-shape mismatch.
The baseline passes under the existing `sparrow` switch, so no baseline source
change was made.
