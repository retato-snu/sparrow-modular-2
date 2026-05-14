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
- Structural import/export obligations are derived from parsed CIL/global data retained in each stage-1 result. The linker does not accept manual import/export lists as the source of truth.
- The first semantic milestone is return-only: the provider's stage-2 output row for `(linked_provider,__return__)` is summarized into `semantic_exports`, then consumed through `linked_environment` to build the importer's dynamic extern-call input.

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

For the current witness, `semantic_exports` contains the provider-derived summary for `linked_provider`:

- provider module and source hash;
- return location `(linked_provider,__return__)`;
- abstract return value, currently the singleton interval row `([41, 41], bot, bot, bot, bot)`;
- concrete return value parsed from that provider stage-2 row;
- row/provenance evidence marked `provider-stage2-output`.

`linked_environment` maps the importer's matched `linked_provider` obligation and extern root (currently `main-4`) to that provider summary. The importer's linked stage-2 extern effect uses reason `linked-provider-return`; the old module-local `unknown-extern-call` value is no longer accepted as linkage evidence, even when the numeric value happens to be `41`.

The checker recomputes this path from the linked artifact rather than trusting one summary field:

- re-reads the provider return row from the linked provider output;
- compares that row with `semantic_exports`;
- verifies the linked environment and input derivation provenance;
- verifies the importer dynamic extern-call row consumed the same value;
- verifies phase order: provider execution, semantic export derivation, linked environment binding, importer linked execution.

Ambiguous provider choices still fail with diagnostics. Deterministic multiple-import/provider and mixed-role-chain witnesses are handled by the oracle-suite topology scheduler described below.

## Non-goals

- No full whole-program semantic equivalence proof.
- No broad arbitrary-C coverage.
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
- relation result for residual-to-oracle and oracle-to-residual selected observations;
- row/effect provenance into both artifact families;
- named obligations with pass/fail status.

The equivalence statement is witness-bounded and selected-observation-bounded.  The checker allows documented normalization for interval-compatible globals and pointer alias provenance; it does not claim arbitrary-C semantic equivalence.

### Named obligations

The suite-level report includes these obligations:

- `return_value_matches_oracle`;
- `global_write_read_matches_oracle`;
- `pointer_memory_effect_matches_oracle`;
- `provider_resolution_matches_oracle`;
- `mixed_role_chain_matches_oracle`;
- `no_premerge_implementation_shortcut`.

Negative-case coverage is represented in the report for mismatched values/effects, missing global/pointer observations, ambiguous providers, invalid mixed-role propagation, shortcut leakage, missing oracle artifacts, witness identity mismatch, missing provenance, and mixed-role cycles.

## Topology support after the oracle-suite milestone

The residual linker now supports deterministic multiple import/provider bindings and mixed importer/provider role chains for function imports.  Binding ambiguity still fails if one importer import has more than one candidate provider.  Mixed roles are scheduled by a dependency DAG over function bindings; dependency cycles fail with a named diagnostic unless a future plan introduces explicit fixpoint semantics for cyclic linked summaries.  The current mixed-role proof obligation is deliberately scoped to scheduling/order and summary handoff evidence; a later richer summary-language milestone should make upstream value-dependence part of the mixed-role semantic relation.

Multiple extern roots are resolved by matching residual component provenance for the imported callee name, falling back to the singleton-root case for the original return-only witness.
