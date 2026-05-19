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
- Cyclic linked function-import SCCs are supported for the checked witness slice by a shared SCC residual-equation layer: the linker discovers SCC topology, uses bootstrap/provider-derived runs only as scaffolding, lowers cyclic exports/imports/observable sink writes to shared solver cells, runs the existing residual worklist, and accepts cyclic values only when `source_shared_scc_cell_id` resolves through `shared_scc_final_cells`.
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


### Typed scalar-call protocol boundary

Scalar return/call evidence now passes through
`Abstract_speculate_residual_scalar_call` before it is encoded into reports.
The protocol is a typed envelope over the existing full-ITV residual-cell
semantics (`Abstract_speculate_itv_residual_cell`); JSON fields are an encoding
of that typed evidence, not the authority used to guess scalar calls.

The linker constructs one `abstract-speculate-residual-scalar-call/v1` provider
return value for each selected scalar return effect, then reuses that typed value
for:

- `external_summary_v1_compat.extern_scalar_value` and
  `function_return_summary` compatibility fields;
- `external_summary.return_effects` v2 return effects; and
- embedded `return_effect` objects in `linked_stage2_input_derivation`.

The additive metadata fields `scalar_protocol_schema`,
`scalar_call_protocol_id`, `scalar_value_kind`, `typed_scalar_metadata_valid`,
and `typed_scalar_metadata` record provider module/source hash, export name,
return node/location, effect id, provider phase, concrete singleton result, and
canonical full-ITV scalar representation.  The relation validates that linked
derivations, return effects, v1 compatibility payloads, and duplicated embedded
return effects agree on those fields.  A mismatch is reported as a
`typed_scalar_protocol_mismatch` call-effect failure.

This protocol deliberately excludes Oct and Taint and does not discover
providers, schedule calls, resolve imports, traverse module lists, or broaden
the existing call graph.  Those responsibilities remain in the existing
residual-linker matching and scheduling flow.

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
  `provenance_missing`, `typed_metadata_mismatch`,
  `typed_scalar_protocol_mismatch`, and `unclassified_universe_fact`;
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
- cyclic evidence corruption, including missing SCC topology, falsified cycle counts, missing round snapshots, accepted bootstrap-only bindings, non-stable final rounds, or overlay-only cycle claims.

This diagnostic contract is `prototype-non-public`: it is a summary/checker
view for the current witness suite, not the full-Itv pass gate, final artifact
schema, full memory model beyond the checked cycle witness slice, or whole-program C
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
- The scalar-call protocol does not broaden provider discovery, import resolution, or call-graph traversal; it only types and validates scalar evidence selected by the existing linker flow.

## Coverage gap against frozen Sparrow

This section records the concrete difference between the current
Abstract-Speculate PE/residual-linking implementation and the frozen `sparrow/`
analyzer.  It is a claim boundary, not a dismissal of the current milestone:
the current implementation proves a real post-PE residual-linking path for the
checked Sparrow-Itv witness universe, but it does not yet PE or relink all of
Sparrow.

### Domain and product-state coverage

Frozen Sparrow includes multiple analysis domains and instances: Itv, Taint,
Oct, OctImpact, product/memory domains, and the report-facing alarm layer.  The
current PE/linking relation is intentionally Itv-scoped:

- `full-sparrow-itv-semantic-relation` inventories Itv evidence exposed by the
  accepted residual-linking slice.
- Taint, Oct, OctImpact, and product-domain parity are intentionally excluded.
- Alarm/report PE is intentionally excluded; current evidence is table/effect
  relation evidence, not final user-facing Sparrow alarm classification.

Consequently, the current result should be read as:

```text
link(PE(I_itv-witness, m1), ..., PE(I_itv-witness, mn))
  agrees with the checked Sparrow-Itv oracle universe
```

not as:

```text
link(PE(I_full-sparrow, m1), ..., PE(I_full-sparrow, mn))
  equals full Sparrow over all domains and reports
```

### C command and transfer coverage

Frozen Sparrow's `ItvSem.run` handles the full command-level transfer path for
the Itv instance: assignments, externals, array/struct/string/function
allocations, assumes/pruning, undefined-library models, user calls, returns,
return-node propagation, skips, and asm fallbacks.

The current staged residual transfer only emits dynamic residual components for
the subset needed by the witness path:

- `Cexternal`;
- selected `Ccall` results, especially unknown/imported calls or calls whose
  arguments are already dynamic;
- `Cset` when the right-hand side reads a dynamic cell; and
- `Cif` / `Cassume` when the guard reads a dynamic cell.

`test/abstract_speculate_staged_transfer.ml` is the executable command-transfer
coverage gate for this subset.  It records command-form provenance by joining
each executed residual entry's node back to `bta_node_facts.command_kind`, and
it rejects metadata-only, ordinal-only, wrapper-only, and counter-only evidence.
The current executable matrix is:

| Command form | Fixture | Transfer evidence | Executed evidence | Mutation / negative gate |
| --- | --- | --- | --- | --- |
| `Ccall` unknown/imported | `test/fixtures/abstract_speculate_pe/extern_dependent_call.c` | `extern-call-result-created-D-during-transfer` | Executed typed residual for `call result`, value `41`, with `Ccall` node provenance | Malformed extern source hash/effects fail closed; changed extern value recomputes call result and dependent assignments |
| `Ccall` known dynamic | `test/fixtures/abstract_speculate_pe/known_dynamic_call.c` | `dynamic-call-result-created-D-during-transfer` | Executed typed residual for an identity helper call whose argument is dynamic, with `Ccall` node provenance | Changed extern value recomputes the dynamic call-result residual |
| `Cset` dynamic chain | `test/fixtures/abstract_speculate_pe/extern_dependent_call.c` | `dynamic-arithmetic-created-D-during-transfer` | Executed typed residuals for `x := tmp` and dependent `y := x+1`, with `Cset` node provenance | Changed extern value propagates through the chain (`7`, then `8`) |
| `Cassume` dynamic guard | `test/fixtures/abstract_speculate_metaocaml_sparse/dynamic_branch.c` | `dynamic-guard-created-D-during-transfer` | Executed typed guard residuals for both `guard x>0` and `guard !(x>0)`, with `Cassume` node provenance | Changed extern value flips both observed guard polarities |
| `Cif` dynamic guard | `test/fixtures/abstract_speculate_metaocaml_sparse/dynamic_branch.c` feasibility checkpoint | Frontend lowering inserts `Cassume` nodes and `remove_if_loop` removes executable `Cif` transfer nodes before staged sparse transfer | Nearest executable evidence is the paired `Cassume` guard matrix rows plus branch-shape control residuals | The test asserts the current frontend-lowered boundary so any future reachable `Cif` residual must be covered explicitly |
| `Cexternal` direct external read | `test/fixtures/abstract_speculate_pe/external_global_read.c` feasibility checkpoint | The current C frontend path does not produce executable `Cexternal` residual-transfer nodes for this focused source | Nearest executable external evidence remains unknown/imported `Ccall` extern effects | The test asserts `extern-read-created-D-during-transfer` is not fixture-reachable without a frontend/linker boundary change |

Other command forms may still be handled during the module-local static Sparrow
preanalysis/fixpoint, but they are not generally lowered into link-time
residual equations.  In particular, allocation, string allocation, function
allocation, complete user-call transfer, full return propagation, and
library-model side effects are not yet general residual-linking semantics.

### Residual value language coverage

Frozen Sparrow's Itv value carries more structure than an integer: interval
data, abstract locations, array blocks, struct blocks, and procedure sets all
participate in expression/lvalue evaluation.  Field/index resolution,
addressing, casts, `sizeof`, `StartOf`, pruning, and pointer-derived location
sets are part of the baseline Itv semantics.

The current typed residual component interface is narrower.  Link-time
residual code is primarily `stage2_input -> int`, and `ExternalSummary v2`
records selected typed effects rather than the full Itv value/memory language.
Fallbacks for unsupported residual expression shapes are intentionally
prototype-level.  This is sufficient for singleton return/global/pointer/cycle
witnesses, but not for arbitrary Itv values or full C memory semantics.

### Memory, alias, and effect-summary coverage

Frozen Sparrow's memory model includes location-sensitive abstract memory,
field/index resolution, array and struct blocks, allocation sites, and
instrumented read/write footprints.  Current residual linking summarizes only
selected witness effects:

- return effects for provider return cells;
- selected global write/read effects such as `shared_g`;
- selected pointer/shared-memory effects such as `(write_ptr,p)`; and
- provenance/call-effect evidence needed to connect provider stage-2 output to
  importer linked stage-2 input.

There is no final general memory/effect summary language yet.  General
alias-conditioned effects, arbitrary heap/struct/array updates, transitive
memory summaries, and cross-module memory effects outside the checked witness
locations remain future coverage.

### Interprocedural and callgraph coverage

Frozen Sparrow evaluates call targets, binds argument locals, records return
locations, and propagates callee return values back to caller lvalues through
the sparse fixpoint state.  Current residual linking resolves structural
function import/export obligations from parsed CIL/global data and rejects
ambiguous provider choices.  It supports deterministic multiple providers,
mixed-role chains, and checked function-import SCCs, but only through the
current semantic-export model.

Not yet covered as general PE/linking semantics:

- function pointers and multi-callee sets beyond the checked structural binding
  cases;
- full argument/return memory protocol across arbitrary calls;
- context-sensitive call summaries;
- recursion and callgraph cycles beyond the checked function-import SCC witness;
  and
- arbitrary provider/importer graphs whose effects are not expressible as the
  current return/global/pointer summaries.

### Library/API model coverage

Frozen Sparrow contains library/API semantics for many undefined or standard
library calls, including string, memory, input, and allocation-related models.
The current PE/linking path treats most imported/unknown calls primarily as
extern/link obligations with validated stage-2 input or provider-derived linked
effects.  Slice 1 of residual API model coverage is intentionally narrower than
the full Sparrow API surface: it covers exactly `memcpy`, `strcpy`, and
`strlen`, and leaves every other library/API model as an explicit non-claim.

The Slice 1 coverage matrix is:

| API | Baseline ItvSem / ApiSem anchor | Abstract effect covered | Residual equation / cell target | Semantically upstream dependencies | Stage-2 seed and provenance | Positive fixture / witness | Negative mutation |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `memcpy` | `sparrow/src/semantics/itvSem.ml:482-492`; `sparrow/src/apiSem.ml:70-75`; dispatch through `sparrow-modular-ocaml/src/abstract_speculate_meta_sparse.ml:673-690` | Source array memory is copied into destination locations; assigned return tracks the destination with source-backed copy provenance. | Destination memory cells plus assigned return cell emit `api-residual-model-memcpy-created-D-during-transfer` residual components / equations. | Source memory cells selected from the `src` argument; target/self-only dependencies are invalid. | Validated stage-2 extern/link seed for the source value, `api_semantic_provenance=residual-api-model-coverage/slice-1`, baseline-source field, and final residual-cell provenance. | `sparrow-modular-ocaml/test/fixtures/abstract_speculate_residual_linking_pe/importer.c` + `sparrow-modular-ocaml/test/fixtures/abstract_speculate_residual_linking_pe/provider.c` as the current residual-linking PE witness bundle. | Reject missing state reads, empty dependencies, target/self-only dependencies, metadata-only API rows, unsupported-API coverage flags, and corrupted `memcpy` provenance. |
| `strcpy` | `sparrow/src/semantics/itvSem.ml:510-522`; `sparrow/src/apiSem.ml:70-75`; dispatch through `sparrow-modular-ocaml/src/abstract_speculate_meta_sparse.ml:691-697` | Source string/null-position witness is copied into destination locations.  C-level return semantics are not claimed unless separate evidence proves them. | Destination string/null-position cells emit `api-residual-model-strcpy-created-D-during-transfer` residual components / equations. | Source string/null-position cells selected from the `src` argument; destination-only dependencies are invalid. | Validated stage-2 seed for the source null-position/value witness, `api_semantic_provenance=residual-api-model-coverage/slice-1`, baseline-source field, and final residual-cell provenance. | `sparrow-modular-ocaml/test/fixtures/abstract_speculate_residual_linking_pe/importer.c` + `sparrow-modular-ocaml/test/fixtures/abstract_speculate_residual_linking_pe/provider.c` as the current residual-linking PE witness bundle. | Reject missing source dependency, target/self-only dependencies, fabricated C-return claims, metadata-only API rows, unsupported-API coverage flags, and corrupted `strcpy` provenance. |
| `strlen` | `sparrow/src/semantics/itvSem.ml:392-398`; `sparrow/src/apiSem.ml:103-107`; dispatch through `sparrow-modular-ocaml/src/abstract_speculate_meta_sparse.ml:698-709` | Return interval/value witness is derived from the source string null-position. | Assigned return cell emits `api-residual-model-strlen-created-D-during-transfer` residual component / equation. | Source string/null-position cell selected from the sole argument; return-cell-only dependencies are invalid. | Validated stage-2 seed for the source null-position/value witness, `api_semantic_provenance=residual-api-model-coverage/slice-1`, baseline-source field, and final residual-cell provenance. | `sparrow-modular-ocaml/test/fixtures/abstract_speculate_residual_linking_pe/importer.c` + `sparrow-modular-ocaml/test/fixtures/abstract_speculate_residual_linking_pe/provider.c` as the current residual-linking PE witness bundle. | Reject missing seed reads, empty dependencies, return/self-only dependencies, metadata-only API rows, unsupported-API coverage flags, and corrupted `strlen` provenance. |

The required semantic chain for every row is:

```text
baseline ItvSem/ApiSem model
  -> stage-1 residual API component with api_baseline_source
  -> residual equation/cell target with exact upstream source dependencies
  -> validated stage-2 seed read and solver-state read
  -> final residual cell provenance
  -> PE checker field assertion and oracle-suite full-Itv witness relation
  -> negative mutation that fails closed
```

This slice deliberately does not claim coverage for `memmove`, `strncpy`,
`strcat`, `strdup` / `xstrdup`, input-buffer APIs, `fgets`, `scanf`, generic
allocation APIs, broad `ApiSem` entries, disabled `memset`, arbitrary-C API
semantics, non-Itv product domains, whole-program equivalence, or a stable
public API-summary schema.  Unsupported API rows must remain absent or marked
unsupported; setting `covered_api=true` for an API outside `memcpy` / `strcpy` /
`strlen` is a negative test case, not evidence.

### Solver and lattice coverage

The current residual solver is solver-backed and evidence-bearing, but it is
not a complete reimplementation of Sparrow's Itv sparse fixpoint lattice.  Its
residual final-table cells now pass through `Abstract_speculate_itv_residual_cell`,
a shared typed ITV boundary used by both the solver row adapter and the full-ITV
relation adapter.  The first-pass value set covers singleton integers, finite
ranges, explicit Top syntax, exact non-numeric legacy values such as `bot` /
`empty` / `unknown` / symbolic strings, and opaque exact-match strings.

The solver preserves legacy JSON row/cell fields while advertising additive
metadata (`typed-itv-residual-cell/v1`, `typed-itv-join/v1`, `typed-itv-leq/v1`)
in its existing lattice log fields.  Its row adapter joins and orders the target
cell through the typed module and preserves non-target row fields.  Conflicting
opaque/non-numeric evidence is not promoted to Top by default.

The module-local stage-1 path still uses Sparrow-derived sparse analysis and
staged lattice evidence, but the stage-2 residual solver does not generally
execute `ItvDom.Mem` lattice operations for all Sparrow cells.  Coverage must
expand by lowering more Sparrow semantic dependencies into typed residual
equations whose final cells are the authoritative source of linked results.

### Strategy and frontend coverage

The current PE path keeps the intended post-PE linking boundary: it does not use
multi-file frontend merge before PE.  The premerge linked observer is an oracle
only.  The current path also does not claim Partial Flow Sensitivity
staging/ranking parity, optimized residual-linker behavior, or final public
artifact/API stability.

### Cyclic coverage

The cyclic milestone is real but scoped.  Cyclic function-import SCCs are
lowered to shared SCC residual equations/cells, solved by the worklist, and
accepted only when `shared_scc_final_cells` source cyclic exports/imported
observables with exact singleton parity.  This does not yet cover arbitrary
cyclic C semantics such as global mutation cycles, pointer-alias cycles,
recursive call/memory cycles, or nested loop-fixpoint plus module-link-fixpoint
interactions outside the checked witness universe.

### Coverage expansion rule

Future coverage should be added only when the full semantic chain is present:

```text
baseline Sparrow dependency
  -> stage-1 residual cell/equation
  -> validated stage-2 extern/link seed
  -> solver state reads and exact cell dependencies
  -> final residual cell provenance
  -> oracle/negative-case evidence
```

Adding artifact fields or selected observations without this chain is
instrumentation, not expanded Sparrow PE coverage.

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

The relation statement is witness-bounded and full Sparrow-Itv scoped.  The checker allows documented Itv coverage normalization for interval-compatible final-table cells through the shared typed residual-cell adapter and records equality as not claimed when residual cells are over-approximations.  Additive typed cell metadata is diagnostic/compatibility evidence: required relation fields remain unchanged, and mismatched typed cell metadata is rejected by the full-ITV relation.  The relation does not claim Oct, Taint, arbitrary-C, or whole-program semantic equivalence.

### Named obligations

The suite-level report includes these obligations:

- `return_value_matches_oracle`;
- `global_write_read_matches_oracle`;
- `pointer_memory_effect_matches_oracle`;
- `provider_resolution_matches_oracle`;
- `mixed_role_chain_matches_oracle`;
- `no_premerge_implementation_shortcut`;
- `cyclic_residual_fixpoint_evidence`;
- `cyclic_imported_value_exact_singleton_parity`;
- `typed_scalar_call_protocol_matches`.

Negative-case coverage is represented in the report for mismatched values/effects, typed scalar protocol metadata/protocol-id mismatches, missing global/pointer observations, a non-selected Itv cell removal that fails the full relation while selected diagnostics still pass, ambiguous providers, invalid mixed-role propagation, shortcut leakage, missing oracle artifacts, witness identity mismatch, missing provenance, source-level removal of an imported cyclic observable sink write, and cycle-evidence falsification.

## Topology support after the oracle-suite milestone

The residual linker now supports deterministic multiple import/provider bindings, mixed importer/provider role chains, and checked cyclic function-import SCCs.  Binding ambiguity still fails if one importer import has more than one candidate provider.  Acyclic mixed roles are scheduled by a dependency DAG over function bindings; cyclic SCCs additionally emit a shared residual-equation/worklist run whose artifact evidence records topology, per-round export/environment snapshots, changed-binding counts, shared SCC equation/cell/dependency ids, state reads, worklist schedule, `shared_scc_final_cells`, accepted export provenance, and exact singleton imported-observable parity.  The current cyclic proof obligation is deliberately scoped to the oracle-suite witness relation and recomputable emitted-equation evidence; it is not an arbitrary-C or full product-domain theorem.

Multiple extern roots are resolved by matching residual component provenance for the imported callee name, falling back to the singleton-root case for the original return-only witness.

## Typed residual scalar-call protocol

Residual scalar call evidence is now produced through the internal
`Abstract_speculate_residual_scalar_call` protocol before it is encoded into
compatibility JSON. The protocol is a thin envelope over the existing full
Sparrow-Itv residual-cell canonicalization (`typed-itv-residual-cell/v1`); it
is not a provider resolver, scheduler, import resolver, module traversal pass,
or new scalar algebra.

The legacy report surfaces remain present for compatibility:

- `external_summary_v1_compat.extern_scalar_value`
- `external_summary_v1_compat.function_return_summary`
- `external_summary.return_effects`
- `linked_stage2_input_derivation.return_effect`
- `linked_stage2_input_derivation.external_summary`

Additive scalar metadata is emitted consistently on the v1 scalar payload,
function-return summary, v2 return effect, and linked derivation entries. The
metadata includes `scalar_protocol_schema`, `scalar_call_protocol_id`,
`scalar_value_model`, `scalar_value_kind`, provider identity/hash, export name,
return node/location, return effect id, and `typed_scalar_metadata_valid`.
Relation checks validate the shared metadata and surface
`typed_scalar_protocol_mismatch` when fixture-guessed or inconsistent scalar
call evidence is observed.

Scope remains intentionally scalar/full-Itv only: Oct and Taint semantics,
broader call-graph rewrites, and proof-system expansion are out of scope.
