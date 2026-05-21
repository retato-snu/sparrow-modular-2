# Sparrow PE implementation status

This note fixes the current research claim boundary for the active
`sparrow-modular-ocaml` implementation.  The baseline analyzer semantics live
in `../sparrow/` and remain the frozen oracle; the active implementation in this
directory is the staged/modular experiment.

## One-sentence claim

The current implementation is a MetaOCaml staged, module-local PE prototype for
a real-Sparrow ItvDom sparse-fixpoint slice, with executable `Trx.code`
residual equations solved by a stage-2 residual fixpoint kernel, plus
witness-bounded residual-linking evidence.

It is not a complete partial evaluator for all of Sparrow.

## PE framing

Use the three-role framing from `../../Doc/FOUNDATIONS.md`:

- Program input `I`: frozen real Sparrow analyzer semantics under `../sparrow/`.
- Static input: one C module parsed and analyzed by the active module-local
  pipeline.
- Dynamic input: extern/link facts supplied after stage 1 and consumed by the
  generated residual analyzer at stage 2.

The intended end-to-end research target remains:

```text
link(PE(I, m), d) ~= I(m + d)
```

Current evidence supports this for the witness-bounded full Sparrow-Itv evidence
universe emitted by the residual-linking oracle suite, not arbitrary-C programs,
Oct/OctImpact semantics, general Taint product semantics, or the full
Sparrow product analyzer.  The only Taint claim is the named bounded
`taint_product_pair` product evidence.

## Implemented

| Area | Status | Evidence surface |
| --- | --- | --- |
| Frozen baseline/oracle split | Implemented | `../sparrow/` is checked by `git diff --exit-code -- sparrow`. |
| Real Sparrow frontend/global lineage | Implemented | `src/real_sparrow_frontend.ml`, `@real_sparrow_frontend_global`. |
| Real PreAnalysis boundary | Implemented | `src/real_sparrow_preanalysis.ml`, `@real_sparrow_preanalysis`. |
| Access/DUG boundary | Implemented | `src/real_sparrow_access_dug.ml`, `@real_sparrow_access_dug`. |
| Real ItvDom sparse-fixpoint boundary | Implemented | `src/real_sparrow_sparse_fixpoint_pe.ml`, `@real_sparrow_sparse_fixpoint_pe`. |
| Staged cell representation | Implemented | `src/abstract_speculate_stage_types.ml` uses `S` / `D of Trx.code`. |
| Module-local staged sparse PE | Implemented for the current slice | `src/abstract_speculate_meta_sparse.ml`. |
| Executable residual analyzer values | Implemented | `src/abstract_speculate_residual_value.ml` builds `Trx.code`; stage 2 uses `Runcode.run`. |
| Solver-backed residual equations (Option A) | Implemented for current staged-cell slice | `src/abstract_speculate_stage_types.ml` defines residual equations; `src/abstract_speculate_residual_solver.ml` runs a deterministic bounded worklist and reports convergence evidence. |
| Stage-2 dynamic input validation | Implemented | `src/abstract_speculate_stage2_input.ml` validates source hash and extern roots before solving residual equations. |
| BTA/fact provenance reports | Implemented | `abstract_speculate_metaocaml_sparse.*.json` reports. |
| Blind static-projection convergence guard | Implemented | Residual code growth is not used as the fixpoint convergence criterion. |
| First residual-linking prototype | Implemented for bounded witnesses | `src/abstract_speculate_residual_linker.ml`, `@abstract_speculate_residual_linking_pe`. |
| Checked cyclic residual scheduling | Implemented only for bounded function-import SCC witnesses with explicit scheduler provenance | `src/abstract_speculate_residual_linker.ml`, `@abstract_speculate_residual_linking_oracle_suite`; reports may call scheduling callgraph-backed only when every scheduler edge carries direct callgraph or residual call-binding provenance. |
| Residual-linking oracle suite | Implemented as prototype full Sparrow-Itv evidence | `@abstract_speculate_residual_linking_oracle_suite`. |
| Post-link residual-global fixpoint | Implemented for bounded residual-linking witnesses | `src/abstract_speculate_global_residual_fixpoint.ml` runs a post-link whole-program residual-cell worklist; `src/abstract_speculate_residual_linker.ml` emits `global_residual_*` evidence; `@abstract_speculate_residual_linking_oracle_suite` gates positive, cycle, equivalence, and negative cases. |
| Selected Sparrow-Itv final-cell coverage gate | Implemented for the selected residual-linking PE fixture, the oracle-suite load/store, pointer/alias, branch/join, loop/fixpoint witnesses, and Slice 1 API/model fixtures | Linked JSON reports `itv_mem_*` coverage counters, strict per-cell classifications, producer evidence, `typed_cell_metadata` consistency booleans, and `full_itv_relation_contract`; the global/relation gate requires source-rerun coverage to exist, `itv_mem_coverage_gate=pass`, `itv_mem_uncovered_cell_count=0`, final-cell-set agreement, valid typed metadata, and selected-observation masking negatives so diagnostics cannot pass alone. |
| Bounded Taint-first product witness | Implemented only for the named `taint_product_pair` oracle-suite witness | ExternalSummary v3 reports `taint_components` and `product_pair_evidence` for one Itv+Taint product-pair; this is not general Taint/product-domain parity. |
| Residual summary language | Implemented as a prototype Sparrow-Itv fixture language, not a public API | ExternalSummary reports `abstract-speculate-sparrow-itv-summary-language/v1`, summary scope, claim boundary, return/memory/pointer/branch-loop/API expressible semantics, and unsupported-semantic non-claims; validators reject missing language, corrupted claim boundary, and malformed memory-effect operations. |
| Residual API model coverage Slice 1 | Implemented only for `memcpy`, `strcpy`, and `strlen` ItvDom.Mem-adjacent witnesses | `@abstract_speculate_residual_api_model_coverage` emits `abstract-speculate-residual-api-model-coverage/v1` plus `api_model_summary` and an API semantic-universe manifest for `api-model-memcpy`, `api-model-strcpy`, and `api-model-strlen`; each path gates final-cell, typed metadata, source-rerun, selected-observation masking, and unsupported-API negative evidence. |

## Not implemented / not claimed

| Area | Missing capability | Why it matters |
| --- | --- | --- |
| Full Sparrow PE | Only selected real-Sparrow boundaries and the ItvDom sparse slice are staged. | Avoid claiming a complete PE of Sparrow. |
| Full product-domain staging | The accepted staged semantics remain generally Itv-focused; only a bounded named Taint product-pair witness is staged. | Product-domain fidelity is required before broad Sparrow claims. |
| Oct/OctImpact and general Taint semantic preservation | Oct/OctImpact remain unsupported; Taint support is limited to `taint_product_pair` bounded evidence and is not general Taint parity. | Product-domain claims require separate evidence and tests. |
| Arbitrary-C semantic preservation | Fixtures and oracle-suite witnesses bound the evidence. | Current results are not a theorem for all C modules. |
| General ItvDom.Mem solver / arbitrary fixture coverage | The expanded fixture set now covers selected load/store, pointer/alias, branch/join, loop/fixpoint, and Slice 1 API/model paths with hard final-cell gates; a general ItvDom.Mem solver for every Sparrow fixture or arbitrary C program is not implemented. | Broader fixtures still need the same residual identity/classification audit, source-rerun coverage, semantic-universe manifest, and negative coverage gates before claiming parity. |
| General residual summary language | The current summary language is explicit enough for the expanded Sparrow-Itv fixture set, but it remains prototype/internal and fixture-evidence-only. | Broader linking still needs a general effect algebra beyond selected Sparrow-Itv witnesses and the planned `abstract-speculate-effect-algebra/v1` authority migration. |
| General API/model coverage | Only `memcpy`, `strcpy`, and `strlen` have residual API model coverage rows. | Broader `ApiSem`, allocation/input APIs, arbitrary aliasing, and general ItvDom.Mem semantics still need separate evidence beyond Slice 1. |
| General cyclic residual linking | Only checked function-import SCC witnesses are supported, and callgraph-backed scheduling is a provenance-gated report claim. Arbitrary recursive call/memory cycles, dependency-only schedules labeled as callgraph-backed, and cyclic effects outside the selected witnesses remain unsupported. | Broader cyclic linking still needs a general call/effect semantics plus oracle evidence beyond the current Sparrow-Itv witness slice. |
| Full source-level whole-program sparse rerun | Implemented for bounded residual-linking witnesses through a post-link source rerun/provenance adapter plus linked-context global residual evidence. | The claim is witness-bounded and evidence-gated; it is not an arbitrary-C theorem, performance claim, mechanized proof, or stable public schema. |
| Full dynamic control residualization | Loop/branch shape witnesses exist, but statement-level dynamic control coverage is limited. | Needed for larger program classes. |
| Mechanized proof | Verification is test/audit/oracle based, not proof-assistant mechanized. | Formal PL claims need a theorem statement and proof. |
| Stable public artifact schema | The oracle suite is prototype/non-public. | External users should not depend on the current JSON schema. |

## Typed effect algebra migration boundary

The approved redesign tracked by
`.omx/plans/prd-external-summary-effect-algebra.md` is a breaking, local
authority inversion plan: typed effects must become the construction authority,
and JSON / relation witness categories / return-global-pointer artifacts must
become projections or observations.

For review and documentation purposes, keep the migration slice narrow:

- review the current authority paths in
  `src/abstract_speculate_residual_linker.ml`,
  `src/abstract_speculate_residual_memory_delta.ml`,
  `src/abstract_speculate_residual_scalar_call.ml`, and
  `src/abstract_speculate_residual_relation.ml`;
- validate the replacement contract against
  `.omx/plans/test-spec-external-summary-effect-algebra.md`;
- treat the new typed effect algebra `.mli` boundaries as planned interfaces,
  not existing public APIs;
- preserve the current non-goals: no full verifier rewrite, no whole analysis
  rewrite, no new dependencies, no proof assistant, and no v3 JSON
  compatibility effort.

Primary hazards for reviewers:

1. v3 authority leakage through `external_summary_v3` or structural equality on
   `return_effects`;
2. in-place “typed” wrappers that do not enforce private constructors or
   projection-only JSON;
3. docs accidentally promoting prototype/non-public artifacts to stable schema
   status; and
4. taxonomy drift that broadens the scope beyond the bounded memory / alias /
   heap / struct / array / taint / product observations described in the PRD.

## Residual-linking claim boundary

The residual-linking prototype composes independently produced module-local
residual analyzers.  The primary stage-2 path is now Option A: module-local
residual equations are generated from executable staged cells, initialized from
validated dynamic extern/link facts, solved to a bounded worklist fixpoint, and
then materialized as final input/output rows.  The older component-overlay view is
kept only as compatibility evidence inside equation bodies; solver-backed reports
must say `residual_solver_run=true`, `solver_backed_residual_fixpoint=true`,
`worklist_drained=true`, and `overlay_only=false`.  After residual linking,
the prototype also runs a post-link source-level rerun/provenance adapter and
global residual-cell worklist.  Accepted reports must include
`global_residual_fixpoint_run=true`,
`global_residual_fixpoint_scope=post-link-whole-program-residual-cells`,
`global_sparse_fixpoint_source_level_rerun=true`,
`global_source_rerun_ready_for_relation_gate=true`, linked-context consumption,
validated rerun evidence, non-empty equations/dependencies/cross-module edges,
state/seed reads, and a drained worklist.

### Review-locked Option A obligations

This document intentionally does **not** claim that any stage-2 component
closure execution is sufficient PE evidence.  The Option A claim is locked to
the solver-state contract:

1. `residual_equation.apply` must receive a `residual_state_view` as well as
   validated `stage2_input`;
2. dependent residual equations must read prior residual cells through that
   state view rather than recomputing only from dynamic input;
3. reports must reject the claim when `state_read_count = 0`,
   `seed_input_read_count = 0`, `equation_apply_reads_solver_state=false`, or
   `exact_cell_dependencies=[]`;
4. unit/review fixtures must fail if a chain such as `n -> x -> y -> ret`
   computes `y` or `ret` directly from `stage2_input` instead of through solver
   state; and
5. compatibility component execution inside an equation body may explain the
   current implementation bridge, but it cannot by itself support the Option A
   residual-fixpoint claim.

The strongest implementation statement remains `solve(E_m, d) ⊒ I(m⊕d)` for
the checked witness universe.  Equality, arbitrary-C coverage, general cyclic
linked residual solving beyond the checked witness SCCs, and full
product-domain Sparrow PE remain out of scope.  A bounded `taint_product_pair` witness now records named Taint component evidence, but it does not broaden this claim to general product-domain staging.

It currently supports deterministic acyclic function bindings, including
multiple provider/import bindings and a mixed importer / provider role chain,
plus checked cyclic function-import SCC witnesses.  Cyclic scheduling evidence
is accepted as callgraph-backed only when the emitted scheduler edges include
stable edge ids, edge kind/source, and direct callgraph or residual call-binding
provenance.  Dependency-only edges may be scheduled for compatibility, but they
must be reported as residual dependency scheduling rather than callgraph-backed
scheduling.  Ambiguous provider choices still fail.

The oracle-suite relation is now a witness-bounded full Sparrow-Itv relation:

```text
origin_itv(premerge_oracle(m1 + ... + mn))
  <= residual_itv(link(PE(I, m1), ..., PE(I, mn)))
```

Residual-to-origin evidence/provenance is checked separately; exact full-table
equality is not claimed when residual Itv cells are documented
over-approximations.

The report gate is `full_itv_semantic_relation.status` for every witness plus
the named proof obligations.  The legacy `selected_observation_relation` remains
only under diagnostics/compatibility and is not the suite pass gate.

Covered positive witness categories:

- global write/read;
- pointer memory effect;
- multiple providers/imports;
- mixed-role scheduling and summary handoff;
- cycle topology, callgraph-backed loop scheduling, and branch/join loop value
  flow through the oracle suite; and
- Slice 1 API/model paths `api-model-memcpy`, `api-model-strcpy`, and
  `api-model-strlen` through the focused API coverage report.

Covered negative cases include mismatched returns/effects, missing global or
pointer observations, non-selected Itv cell removal that fails the full relation
while selected diagnostics still pass, selected-observation masking of missing
full-Itv/source-rerun coverage, missing/corrupted typed metadata, producer-only
or metadata-only coverage, ambiguous provider acceptance, invalid mixed-role
phase ordering, forbidden premerge shortcut leakage, missing oracle artifacts,
witness identity mismatch, missing provenance, cycle/scheduler/topology
corruption, and mixed-role dependency cycles.  The suite also rejects
ExternalSummary v1-only/compat-only artifacts, missing v3 summaries,
schema/status downgrades, missing typed return effects, missing authoritative
memory deltas/chains, and corrupted selected return/global/pointer effect
value, location, chain, or provenance.  The API coverage gate separately
rejects missing state/seed reads, empty or target/self-only dependencies,
metadata-only API rows, unsupported optional API coverage, corrupted API
provenance, and baseline-source corruption.

## Verification commands

Use the MetaOCaml BER switch for active verification:

```sh
cd sparrow-modular-ocaml
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_metaocaml_sparse_pe
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_api_model_coverage
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite
opam exec --switch MetaOCaml-full -- dune build @runtest
```

Baseline cleanliness remains a separate root-level check:

```sh
git diff --exit-code -- sparrow
```

## Recommended next milestone

Next work should not broaden beyond the Itv witness universe first.  It should
make the current prototype easier to evaluate:

1. stabilize and commit the residual-linking oracle-suite artifacts;
2. keep the full Sparrow-Itv relation schema documented and prototype/non-public;
3. keep the prototype selected-witness ExternalSummary v3 rules stable while
   treating them as non-public, then replace them with a general typed effect
   algebra only after the witness-bounded contract is stable;
4. keep residual API model coverage schema-stable for the current Slice 1
   (`memcpy`, `strcpy`, `strlen`) while treating it as prototype/internal and
   non-public;
5. continue factoring residual-linking scheduling into explicit dependency and
   call-binding provenance evidence and keep the stage-2 residual solver as the
   primary runtime;
6. keep any broader cyclic call/memory semantics out of scope until the reports
   and oracle suite can prove them with the same provenance and non-overclaim
   gates.

Only after that should the implementation broaden beyond the current ItvDom
sparse-fixpoint slice.

## External summary authority status

External summary authority is moving to `abstract-speculate-effect-algebra/v1`.
Legacy ExternalSummary v3 fields are retained only as compatibility projections
while scalar, memory, relation, and oracle checks migrate to typed projection
IDs and evidence paths.
