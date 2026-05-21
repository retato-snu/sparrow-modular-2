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
| Focused residual API model coverage gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_api_model_coverage` | The Slice 1 API coverage report is regenerated with schema `abstract-speculate-residual-api-model-coverage/v1`, covering exactly `memcpy`, `strcpy`, and `strlen` with upstream dependency, state-read, seed-read, and negative mutation checks. |
| Typed scalar-call protocol unit gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune exec test/abstract_speculate_residual_scalar_call_unit.bc` | Unit checks validate typed scalar constructors, full-Itv scalar normalization, legacy v1 compatibility payloads, ExternalSummary v3 derivation compatibility, and mismatch rejection. |
| ExternalSummary v3 memory-delta unit gate | `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune exec test/abstract_speculate_residual_memory_delta_unit.bc` | Unit checks validate `abstract-speculate-external-summary/v3`, `abstract-speculate-external-summary-memory-delta/v3`, selected global/pointer memory deltas, chain validation, and negative corruption cases. |
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
   `global_sparse_fixpoint_source_level_rerun=true`.
2. Confirm the report has validated rerun provenance:
   `global_source_rerun_ready_for_relation_gate=true`,
   `global_source_rerun_linked_context_consumed=true`, and non-empty
   `global_source_rerun_validated_evidence` with module id, source file,
   source hash, artifact path, linked-context consumption, and final-row counts.
3. Confirm the global report has non-empty seed cells, derived cells, equations,
   dependency edges, cross-module dependency edges, worklist schedule, state
   reads, and seed reads, with `global_residual_worklist_drained=true` and
   `global_residual_overlay_only=false`.
4. For cycle/SCC witnesses, confirm `global_residual_iteration_count > 1` and a
   repeated `changed-cell-dependent` schedule entry.
5. Confirm `full_itv_semantic_relation.global_residual_equivalence_status` is
   derived by the checker/reporting layer, not asserted by the linker module.
6. Confirm negative cases reject metadata-only reports, missing rerun provenance,
   source-rerun claims without linked-context consumption, missing dependencies/
   state reads, and undrained worklists.
7. Reconfirm non-goals: no arbitrary-C theorem, no Oct/OctImpact/product-domain
   generality, no mechanized proof, and no public schema guarantee.





## Typed effect algebra migration review slice

Use this checklist when reviewing the approved PRD/test-spec pair for the
typed effect algebra redesign. This is a review/docs slice, not an
implementation claim.

1. Confirm the authoritative contract is the approved PRD plus test spec:
   `.omx/plans/prd-external-summary-effect-algebra.md` and
   `.omx/plans/test-spec-external-summary-effect-algebra.md`.
2. Confirm the review scope stays on the current authority surfaces:
   `abstract_speculate_residual_linker.ml`,
   `abstract_speculate_residual_memory_delta.ml`,
   `abstract_speculate_residual_scalar_call.ml`, and
   `abstract_speculate_residual_relation.ml`.
3. Confirm the docs slice names the intended typed effect algebra `.mli`
   boundaries as planned interfaces only.
4. Confirm the migration does not claim success until typed effect algebra
   constructors, projection-only JSON, and no-v3-authority gates are evidenced
   by tests.
5. Confirm review notes preserve the hard non-goals: no full verifier rewrite,
   no whole analysis rewrite, no new dependencies, no proof assistant, and no
   v3 JSON compatibility effort.
6. Confirm documentation updates distinguish current prototype/non-public
   evidence from any future stable schema claim.

## Residual API model coverage checklist

Use this focused checklist when validating residual API model coverage for
`memcpy`, `strcpy`, and `strlen` only.  The report schema is
`abstract-speculate-residual-api-model-coverage/v1`; row provenance is
`residual-api-model-coverage/slice-1`.

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
5. Confirm the generated report records
   `schema_version=abstract-speculate-residual-api-model-coverage/v1`,
   `scope=exactly memcpy,strcpy,strlen core memory/string copy/length residual
   semantics`, and an empty `unsupported_api_residual_coverage` list.
6. Reconfirm unsupported APIs remain non-claims: `memmove`, `strncpy`, `strcat`,
   `strdup` / `xstrdup`, input-buffer APIs, `fgets`, `scanf`, generic
   allocation APIs, broad `ApiSem` entries, disabled `memset`, arbitrary-C API
   semantics, non-Itv product domains, and whole-program equivalence.

## Initial PE ItvDom.Mem final-cell coverage checklist

Use this focused checklist when validating full final-table ItvDom.Mem coverage
for `abstract_speculate_residual_linking_pe/importer.c + provider.c` only:

1. Run `@abstract_speculate_residual_linking_pe` and inspect
   `_build/real-sparrow/abstract-speculate-residual-linking-pe/active/abstract-speculate-residual-linking-pe.linked.json`.
2. Confirm the linked JSON contains `itv_mem_coverage_gate=pass`,
   `itv_mem_total_cell_count`, `itv_mem_residual_equation_cell_count`,
   `itv_mem_static_projection_cell_count`, `itv_mem_uncovered_cell_count=0`,
   and an empty `itv_mem_uncovered_cells` list for the final Itv cell audit.
3. Confirm every covered final-table cell has stable identity fields
   (`table`, `node`, `location`, and value), a classification, a
   `residual_equation_id` when dynamic, and `typed_cell_metadata` when covered
   as a static projection/equation.
4. Confirm the PE and oracle relation outputs include
   `full_itv_relation_contract` and that negative cases reject missing coverage
   evidence, typed metadata/value mutations, and added uncovered final-table
   cells even when selected-observation diagnostics still look plausible.
5. Reconfirm non-goals: this is not an arbitrary-C theorem, not Oct/OctImpact
   support, not general Taint/product-domain parity, not general API/model
   coverage, and not a broad effect algebra for all Sparrow cells.

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
   present in generated ExternalSummary v3 reports with schema ids
   `abstract-speculate-external-summary/v3` and
   `abstract-speculate-external-summary-memory-delta/v3`; legacy
   `global_effects` and `pointer_effects` must be marked compatibility
   projections only.
3. Confirm PE and oracle-suite negative cases reject role swaps, wrong raw or
   normalized locations, wrong value transitions, missing/wrong provenance, and
   missing/corrupted chains even when legacy projection fields remain plausible.
4. Run the memory-delta unit gate, residual-linking PE gate, oracle-suite gate,
   `@check`, and `git diff --check`.
5. Reconfirm non-goals: no solver rewrite, no proof-system expansion, no Oct
   semantics, no general Taint/product-domain parity, no broad call/link
   scheduler rewrite, and no public API/schema promise.

## ExternalSummary effect-algebra migration checklist

Use this focused checklist when reviewing `.omx/plans/prd-external-summary-effect-algebra.md`
and `.omx/plans/test-spec-external-summary-effect-algebra.md`:

1. Review the authority-inversion boundary first: the typed effect algebra must
   be the construction authority, while `external_summary_v3`-style JSON and
   compat fields are projections only.
2. Treat these files as the highest-value review slices for linker/memory/
   scalar/relation migration risk:
   - `src/abstract_speculate_residual_linker.ml`
   - `src/abstract_speculate_residual_memory_delta.ml`
   - `src/abstract_speculate_residual_scalar_call.ml`
   - `src/abstract_speculate_residual_relation.ml`
   - `src/abstract_speculate_global_residual_fixpoint.ml`
3. Treat these tests as the no-v3-authority gate surface:
   - `test/abstract_speculate_residual_linking_pe_check.ml`
   - `test/abstract_speculate_residual_linking_oracle_suite_check.ml`
   - `test/abstract_speculate_residual_scalar_call_unit.ml`
   - `test/abstract_speculate_residual_memory_delta_unit.ml`
4. Confirm the migration does not turn compat projections into authority:
   `external_summary_v1_compat`, `v2-compatible-non-authoritative`,
   `compat-v1-non-authoritative`, and similar fields must remain diagnostic.
5. Confirm the relation/oracle checks still reject missing v3 memory authority,
   missing `delta_chains`, and authority inversions that only satisfy legacy
   selected-observation paths.
6. Reconfirm non-goals: no full verifier rewrite, no whole-analysis rewrite,
   no new dependencies, no proof assistant, and no v3 JSON compatibility
   effort.

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

## External summary effect algebra boundary

See `doc/effect-algebra-soundness.md` for the soundness taxonomy.  Typed effects
and projections are the authority boundary; legacy ExternalSummary v3-shaped
fields are adapter projections and are not derivation truth.
