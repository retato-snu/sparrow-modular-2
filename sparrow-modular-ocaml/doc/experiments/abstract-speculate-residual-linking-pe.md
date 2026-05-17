# Abstract Speculate residual linking PE

## Claim

This experiment is the first-pass residual-linkability proof for Abstract Speculate:

```text
PE(I, m1), PE(I, m2), ... -> residual linking -> linked residual analyzer
```

The linked analyzer is produced after each module has already passed through the existing module-local Abstract Speculate PE path. It is not a whole-program pre-link PE and it does not use multi-file frontend merging as the proof path.

## Boundary

- Stage 1 remains module-local: `Real_sparrow_frontend.global_for_module` and `MetaSparse.run_stage1` produce each module's typed residual analyzer.
- Residual linking consumes typed stage-1 results, not only dumped JSON summaries.
- The linked bundle executor keeps source-hash/extern-root validation per module. Provider modules run with their validated module-local stage-2 input; importer modules that have matched residual-link obligations receive a linked stage-2 input derived from the provider semantic export environment.
- Stage 2 is solver-backed for the supported slice: staged residual cells are emitted as residual equations, the deterministic worklist solver propagates dynamic extern/link seeds, and reports distinguish `solver-backed-residual-fixpoint` from `component-overlay-legacy` evidence.
- The solver-backed claim requires actual solver-state reads: a residual equation receives `residual_state_view -> stage2_input`, dependent equations read prior cells through the state view, and review/audit gates reject zero state reads, zero seed reads, missing exact cell dependencies, or overlay-only evidence.
- Structural import/export obligations are derived from parsed CIL/global data retained in each stage-1 result. The linker does not accept manual import/export lists as the source of truth.
- The first semantic milestone was return-focused; the current handoff uses
  `ExternalSummary v2`, a prototype/internal typed effect summary over selected
  Sparrow-Itv witnesses.  Provider stage-2 rows are summarized into typed
  `return_effects`, `global_effects`, and `pointer_effects`, then consumed
  through `semantic_exports`, `linked_environment`, and linked stage-2 input
  derivations.  The embedded v1 projection is compatibility evidence only and
  is not authoritative.

## Full Sparrow-Itv semantic relation contract

The strengthened oracle-suite contract is now the full Sparrow-Itv relation for
the current residual-linking witness slice:

```text
link(PE(I, m1), ..., PE(I, mn)) ≈_full_itv premerge_itv_oracle(m1 ⊕ ... ⊕ mn)
```

The left side is the linked residual analyzer produced after each module has already passed through module-local PE. The right side is not an implementation path: it is the oracle/reference projection used to explain what Itv evidence must agree when the same modules are viewed as one linked program. The premerge linked observer remains oracle evidence only and must not be called from the residual linker.

The relation is still witness-bounded and prototype/non-public.  "Full
Sparrow-Itv" means the complete Itv evidence emitted by the accepted
residual-linking slice, not Oct, Taint, arbitrary-C semantic equivalence, or a
whole-program C theorem.  The theorem-ready implementation statement for the
supported fixtures is `solve(E_m, d) ⊒ I(m⊕d)`, where `E_m` is the emitted
module-local residual equation set and `d` is validated dynamic extern/link
input; equality is not claimed outside the checked witness relation.

### Review lock: residual equations over solver state

The accepted Option A review criterion is stronger than "stage-2 closures ran".
For this experiment, the following are mandatory claim gates:

- `residual_equation.apply` has the shape
  `residual_state_view -> stage2_input -> residual_component_result`;
- the residual solver invokes equations with its current state view, not only a
  dynamic input record;
- dependency propagation is evidenced by non-empty `exact_cell_dependencies`,
  positive `state_read_count`, positive `seed_input_read_count`, and
  `equation_apply_reads_solver_state=true`;
- the solver unit fixture represents the semantic chain `n -> x -> y -> ret`:
  only `x` reads the dynamic seed, while `y` and `ret` must read `x`/`y`
  through solver state; and
- the compatibility bridge that invokes existing component code inside an
  equation body remains implementation scaffolding, not a standalone PE proof.
- linked handoff evidence must prefer ExternalSummary v2 typed effects; v1
  compatibility projections cannot satisfy return/global/pointer summary gates.

Any future change that weakens one of these gates must downgrade the claim back
to instrumentation/prototype evidence until an equivalent state-reading
residual-equation contract is restored.

### ExternalSummary v2 typed-effect boundary

`ExternalSummary v2` is an internal prototype schema, not a public API.  Its
scope is `sparrow-itv-selected-witness` and its summary status is
`prototype-internal`.  Each summary records:

- `return_effects` for provider return cells used by linked importer extern
  calls;
- `global_effects` for selected global write/read evidence such as `shared_g`;
- `pointer_effects` for selected pointer/shared-memory evidence such as
  `(write_ptr,p)`;
- provider/source/phase/evidence-path provenance; and
- `external_summary_v1_compat` as a non-authoritative migration projection.

The checker rejects missing v2 summaries, v1-only summaries, schema/status
downgrades, missing or corrupted return effects, missing selected
global/pointer effects, corrupted selected global/pointer location, value, or
provenance, and linked stage-2 derivations that do not name the v2 return
`effect_id`.

### Domains

The relation ranges over prototype witness artifacts, not arbitrary C programs. Its domains are:

- residual artifacts emitted by module-local `PE(I, mi)` followed by residual linking;
- oracle/reference artifacts emitted by the premerge linked observer for the same witness group;
- full final input/output Itv table cells exposed by those artifacts;
- semantic exports, linked environments, typed ExternalSummary v2 effects,
  phase logs, linked stage-2 derivations, completion evidence, and provenance
  required to interpret those cells.

### Semantic universe manifest

Each witness report emits `full_itv_semantic_relation` with:

- `relation = "full-sparrow-itv-semantic-relation"`;
- `domain = "sparrow-itv"`;
- `semantic_universe` listing final tables, semantic exports, linked
  environment/call-effect evidence, completion/status evidence, provenance, and
  oracle identity;
- ExternalSummary v2 return/global/pointer effect evidence, with v1
  compatibility treated as non-authoritative;
- `semantic_universe_manifest` classifying facts as compared, missing,
  intentionally excluded, or expected-but-not-emitted;
- `failure_taxonomy` with bounded reasons:
  `missing_from_residual`, `missing_from_origin`, `value_mismatch`,
  `provenance_missing`, and `unclassified_universe_fact`;
- `canonicalization` for singleton intervals, finite ranges, Top, bottom/empty
  or unknown strings, and location-sensitive memory cells;
- `residual_to_origin` and `origin_to_residual` bidirectional checks;
- `oracle_identity` recording frozen-origin/premerge-observer lineage.

The oracle-suite witness status and suite status are gated on
`full_itv_semantic_relation.status`, not on the legacy selected-observation
relation.

## Selected-observation diagnostic relation

The older selected-observation relation remains as diagnostic/compatibility
evidence:

```text
link(PE(I, m1), ..., PE(I, mn)) ≈_selected_obs selected_obs(I(m1 ⊕ ... ⊕ mn))
```

It is no longer the authoritative oracle-suite pass gate.

### `selected_obs` projection

`selected_obs` keeps only observations required by the residual-linking obligations:

1. **Return values**: provider return summaries at locations such as `(linked_provider,__return__)`.
2. **Global read/write observations**: selected global locations such as `shared_g`, with interval-compatible normalization.
3. **Pointer alias/effect observations**: selected pointer-write locations such as `(write_ptr,p)`, compared by observable pointee/effect provenance.
4. **Provider/import bindings**: importer/provider/export/import identities, source hashes, declaration kinds, and linked return values.
5. **Phase ordering**: provider execution, semantic export derivation, linked environment binding, and importer linked execution order.
6. **Call effects**: linked importer extern-call effects, especially reason `linked-provider-return`, derived from provider stage-2 output.
7. **Provenance**: evidence paths into residual and oracle artifacts. Missing provenance is a relation failure.

### Normalization

Normalization is deliberately small:

- singleton integer strings and singleton intervals normalize to the same integer value;
- a residual interval-compatible global observation may match an oracle singleton when the singleton is contained in the residual interval;
- pointer observations compare selected alias/effect evidence, not the full C memory model;
- provider/import binding keys normalize over witness id, module ids, source hashes, import/export names, and declaration kinds;
- phase observations normalize to relative order constraints rather than exact absolute phase numbers.

### Failure cases

The relation fails when any selected obligation is absent or inconsistent, including:

- mismatched return/global/pointer values or effects;
- missing residual or oracle observation;
- missing row/effect provenance;
- ambiguous provider binding;
- wrong provider/export/environment/importer phase order;
- call effects not marked `linked-provider-return`;
- shortcut leakage through premerge/global merge implementation paths;
- mixed-role cycles, because cyclic fixpoint summary semantics are out of scope.

This diagnostic contract is `prototype-non-public`: it is a summary/checker
view for the current witness suite, not the full-Itv pass gate, final artifact
schema, full memory model, cyclic summary semantics, or whole-program C
equivalence proof.

### Primary-linkage check vs oracle-suite relation

There are two related checker outputs, and they intentionally make different claims:

- `primary_linkage_observation_check` is a residual-internal invariant check for the two-module PE smoke witness. It validates provider-derived importer inputs, selected return/call-effect rows, phase order, and provenance. It does **not** perform an oracle comparison and therefore must not report `residual_to_oracle`/`oracle_to_residual` directions.
- `full_itv_semantic_relation` is emitted by the oracle suite and is the authoritative pass gate. It compares the full Itv evidence universe exposed by each witness against origin/premerge oracle evidence.
- `selected_observation_relation` is emitted only under diagnostics/compatibility. It compares selected residual observations against selected premerge oracle/reference observations.

This naming split is part of the claim boundary: the primary PE check establishes residual-linkage evidence quality, while the oracle suite establishes witness-bounded full Sparrow-Itv relation coverage and keeps selected observations as legacy diagnostics.

## Witnesses

The first witness pair lives under:

- `test/fixtures/abstract_speculate_residual_linking_pe/importer.c`
- `test/fixtures/abstract_speculate_residual_linking_pe/provider.c`

Both files are real C inputs accepted independently by the current Abstract Speculate module-local frontend/preanalysis path. The importer declares `linked_provider`; the provider defines it. Both include a `main` because the current module-local AS path expects a main entry for independent PE.

## Validation oracle

The Dune alias `@abstract_speculate_residual_linking_pe` proves:

1. at least two independent module-local PE artifacts are emitted;
2. the residual linker consumes typed stage-1/analyzer values;
3. the linked bundle analyzer run is recorded as a derived evidence summary, not a bare assertion;
4. parsed CIL/global declarations produce `declared_imports`, `declared_exports`, `matched_obligations`, and `unresolved_obligations`;
5. shortcut guards reject pre-link `global_for_files`, `Mergecil.merge`, and premerge linked observer delegation.

`linked_residual_analyzer_ran` is true only when all five evidence predicates hold:

1. `linked_execute_returned` records the in-memory linked analyzer result before the artifact is written;
2. `all_modules_executed` requires the executed module count to match the input module count, every module log to mark `module_analyzer_executed`, and the `(module_id, source_hash)` identity set to match exactly across `input_modules`, `linked_stage2_input.keys`, and `module_logs`;
3. `residual_rows_observed` means the namespaced linked `final_input_table` plus `final_output_table` row count is greater than zero;
4. `obligations_closed` requires at least one matched structural obligation and zero unresolved obligations;
5. `no_shortcut_path` is recomputed by the checker from module artifacts, per-module dispatch keys, and source guards rather than trusted from a single emitted boolean.

The checker also runs false-case checks for each predicate. These checks are limited witness-level consistency checks over the generated artifact/log shape; they are not a whole-program semantic equivalence proof.

## Return-only semantic export linkage

The linked artifact now records the semantic dataflow used by the importer:

```text
provider stage2 output row
  -> semantic_exports
  -> linked_environment
  -> linked_stage2_input_derivation
  -> importer dynamic call result
```

For the current return witness, `semantic_exports` contains the provider-derived summary for `linked_provider`:

- provider module and source hash;
- return location `(linked_provider,__return__)`;
- abstract return value, currently the singleton interval row `([41, 41], bot, bot, bot, bot)`;
- concrete return value parsed from that provider stage-2 row;
- row/provenance evidence marked `provider-stage2-output`.

For global and pointer witnesses, the same ExternalSummary v2 object additionally
contains selected typed global or pointer effects.  These effects are checked
against the oracle-suite relation observations, but they remain bounded to the
selected Sparrow-Itv witness universe rather than a general C memory/effect
calculus.

`linked_environment` maps the importer's matched `linked_provider` obligation and extern root (currently `main-4`) to that provider summary. The importer's linked stage-2 extern effect uses reason `linked-provider-return`; the old module-local `unknown-extern-call` value is no longer accepted as linkage evidence, even when the numeric value happens to be `41`.

The checker recomputes this path from the linked artifact rather than trusting one summary field:

- re-reads the provider return row from the linked provider output;
- compares that row with `semantic_exports`;
- verifies the linked environment and input derivation provenance;
- verifies the importer dynamic extern-call row consumed the same value;
- verifies phase order: provider execution, semantic export derivation, linked environment binding, importer linked execution.

Ambiguous provider choices still fail with diagnostics. Deterministic multiple-import/provider and mixed-role-chain witnesses are handled by the oracle-suite topology scheduler described below.

## Non-goals

- No whole-program semantic equivalence proof beyond the witness-bounded full Sparrow-Itv evidence universe.
- No broad arbitrary-C coverage.
- No Oct or Taint semantics.
- No optimized or production-grade residual linker.
- No final API freeze for the residual module/linker contract.
- No final general memory/global/call-effect summary language; richer effect observations in the oracle suite remain witness-bounded prototype evidence.

## Contrast with real premerge linked observer

`real_sparrow_premerge_linked_observer` intentionally validates a different evidence track: it starts from a linked whole-program fixture and uses `global_for_files` / frontend merge before analysis. This experiment keeps that path as contrast only; Abstract Speculate residual linking must prove post-PE linkability from module-local residual analyzers.

## Semantic preservation proof-obligation oracle suite

The follow-up alias `@abstract_speculate_residual_linking_oracle_suite` extends the first return-only witness into an executable, witness-bounded proof-obligation suite.  The suite still preserves the core residual-linking boundary:

```text
PE(I, m1), PE(I, m2), ... -> residual linking -> linked residual analyzer
```

For each witness, the suite emits two artifact families:

1. residual-linked artifacts from module-local Abstract Speculate PE outputs; and
2. a premerge linked observer artifact used only as an oracle/reference.

The premerge observer is not called from `Abstract_speculate_residual_linker`; the suite checker scans the residual-linker source and residual-linking dump path for the forbidden premerge/global shortcuts and records the premerge artifacts only under oracle/reference fields.

### Witness categories

The oracle suite uses small C programs parsed by the current frontend:

- `global_write_read`: provider writes `shared_g`; importer reads/depends on the linked global observation.
- `pointer_memory_effect`: provider mutates through `int *p`; the suite records pointer alias/effect provenance and compares it with the oracle return/effect evidence.
- `multiple_providers_imports`: one importer links to two deterministic providers.
- `mixed_role_chain`: a middle module imports from one provider and exports a summary consumed by a downstream importer; this first oracle obligation proves scheduling/order and summary handoff, not upstream value-dependence through the middle summary.

Provider-side module-local fixtures keep small `main` shims because the module-local Abstract Speculate path currently expects each module to parse independently with a main entry.  The premerge oracle fixtures omit those provider `main` shims so the whole-program linked observer has a single program entry.

### Proof relation contract

The suite report is explicitly `prototype-non-public`.  Positive witnesses must include:

- residual linked artifact path;
- premerge observer artifact path;
- normalized residual and oracle observations;
- `full_itv_semantic_relation` with `semantic_universe_manifest`, bounded failure taxonomy, canonicalization, oracle identity, and bidirectional `residual_to_origin` / `origin_to_residual` checks;
- selected-observation relation only under diagnostics/compatibility;
- row/effect provenance into both artifact families;
- named obligations with pass/fail status.

The relation statement is witness-bounded and full Sparrow-Itv scoped.  The checker allows documented Itv coverage normalization for interval-compatible final-table cells and records equality as not claimed when residual cells are over-approximations; it does not claim Oct, Taint, arbitrary-C, or whole-program semantic equivalence.

### Named obligations

The suite-level report includes these obligations:

- `return_value_matches_oracle`;
- `global_write_read_matches_oracle`;
- `pointer_memory_effect_matches_oracle`;
- `provider_resolution_matches_oracle`;
- `mixed_role_chain_matches_oracle`;
- `no_premerge_implementation_shortcut`.

Negative-case coverage is represented in the report for mismatched values/effects, missing global/pointer observations, a non-selected Itv cell removal that fails the full relation while selected diagnostics still pass, ambiguous providers, invalid mixed-role propagation, shortcut leakage, missing oracle artifacts, witness identity mismatch, missing provenance, and mixed-role cycles.

## Topology support after the oracle-suite milestone

The residual linker now supports deterministic multiple import/provider bindings and mixed importer/provider role chains for function imports.  Binding ambiguity still fails if one importer import has more than one candidate provider.  Mixed roles are scheduled by a dependency DAG over function bindings; dependency cycles fail with a named diagnostic unless a future plan introduces explicit fixpoint semantics for cyclic linked summaries.  The current mixed-role proof obligation is deliberately scoped to scheduling/order and summary handoff evidence; a later richer summary-language milestone should make upstream value-dependence part of the mixed-role semantic relation.

Multiple extern roots are resolved by matching residual component provenance for the imported callee name, falling back to the singleton-root case for the original return-only witness.
