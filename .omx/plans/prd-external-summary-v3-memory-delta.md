# PRD: ExternalSummary v3 memory delta provenance

## Requirements Summary

Source of truth: `.omx/specs/deep-interview-external-summary-v3-memory-delta.md`.

Design and implement an authoritative `ExternalSummary v3` memory-delta contract for Abstract Speculate residual linking. The new contract must make global and pointer read/write provenance deltas explicit enough to audit and reject wrong reader/writer roles, memory locations, value transitions, provenance/source hashes, and missing delta chains. V3 may replace or reshape the current v2 memory-effect surfaces, but it must stay inside the existing residual-linking producer/consumer pipeline and must not introduce a residual solver rewrite, proof-system expansion, Oct support, or Taint support.

## Brownfield Evidence

- Current external summary data is v2 and stores memory effects as loose JSON lists: `external_summary_v2.return_effects`, `global_effects`, `pointer_effects`, `provenance`, and `v1_compat` are declared in `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:70-76`; semantic exports carry that v2 summary at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:78-90`.
- V2 JSON serialization hard-codes schema `abstract-speculate-external-summary/v2`, status `prototype-internal`, scope `sparrow-itv-selected-witness`, and effect domains `return`, `global-write-read`, and `pointer-memory-effect` at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:171-186`.
- The linker currently derives global/pointer memory effects directly from `provider_row.memory`, skipping return cells and encoding each non-return memory cell as a typed effect at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:528-551`; it partitions them into `global_effects` and `pointer_effects` at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:553-560`.
- Linked stage-2 derivations embed the external summary and still label the schema as v2 at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:760-780`; linked derivation reports repeat v2 schema and full semantic export/external summary data at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:808-835`.
- The final linked artifact exposes `semantic_exports`, `external_summaries`, `linked_environment`, and `linked_stage2_input_derivation` in the report at `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml:1878-1881`.
- The relation module already has selected memory domains (`Global_value`, `Pointer_write`, `Memory_location`) at `sparrow-modular-ocaml/src/abstract_speculate_residual_relation.ml:13-22`, selected memory observation construction for global/pointer locations at `sparrow-modular-ocaml/src/abstract_speculate_residual_relation.ml:542-590`, and provenance failure handling in primary linkage at `sparrow-modular-ocaml/src/abstract_speculate_residual_relation.ml:396-409`.
- The PE checker currently validates v2 shape and v2 effect basics at `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_check.ml:145-158`, validates derivation use of v2 return effects at `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_check.ml:159-170`, and rejects v2 summary/schema/status/return mutations at `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_check.ml:383-423`.
- The oracle checker currently validates memory effects by comparing effect location/value/provenance against provider memory and summary provenance at `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml:120-157`, requires global/pointer effects at `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml:173-180`, and rejects missing or corrupted v2 global/pointer effects at `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml:663-743` and `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml:890-899`.
- Existing Dune gates include the residual scalar unit executable and residual-linking PE/oracle aliases at `sparrow-modular-ocaml/test/dune:25-30` and `sparrow-modular-ocaml/test/dune:371-445`.
- Documentation names ExternalSummary v2 as an internal prototype and lists current v2 memory-effect expectations at `sparrow-modular-ocaml/doc/experiments/abstract-speculate-residual-linking-pe.md:73-91`; it also states current global/pointer effects are bounded to selected Sparrow-Itv witnesses at `sparrow-modular-ocaml/doc/experiments/abstract-speculate-residual-linking-pe.md:272-276` and non-goals at `sparrow-modular-ocaml/doc/experiments/abstract-speculate-residual-linking-pe.md:290-297`.
- Verification docs already include focused residual-linking PE and oracle gates at `sparrow-modular-ocaml/doc/verification.md:12-22`, plus a typed scalar-call checklist that can be extended with v3 memory-delta gates at `sparrow-modular-ocaml/doc/verification.md:30-51`.

## RALPLAN-DR Summary

### Principles

1. **Provenance-first memory evidence.** Memory deltas are accepted only when role, location, value transition, source hash, and chain evidence agree; matching final values alone is insufficient.
2. **Typed boundary before JSON authority.** Linker JSON must be an encoding of shared typed v3 memory-delta constructors/validators, not hand-assembled stringly evidence.
3. **Authoritative v3, compatible reports.** V3 should replace/reshape v2 memory evidence where needed, while preserving essential report locations and existing scalar/return compatibility fields long enough for PE/oracle/proof gates to pass.
4. **Bounded domain expansion.** A general memory/alias delta abstraction is allowed only as an evidence envelope for selected Sparrow-Itv residual-linking witnesses; no solver rewrite, proof-framework expansion, Oct, or Taint.
5. **Executable rejection over documentation.** Each required wrong-evidence class must fail a unit or PE/oracle gate, not only be described in docs.

### Decision Drivers

1. **Reject plausible but wrong memory-flow evidence:** user requires rejection of wrong roles, locations, value transitions, hashes, and missing chains.
2. **Brownfield integration with existing gates:** PE, oracle-suite, proof, scalar-call protocol, and report field expectations must remain coherent.
3. **Avoid forbidden scope expansion:** the implementation must not become a new residual solver, proof system, Oct/Taint product domain, or broad call/memory scheduler rewrite.

### Viable Options

#### Option A — Shared v3 memory-delta protocol module (chosen)

Add a shared module such as `Abstract_speculate_residual_memory_delta` with typed records for memory actors, locations, value transitions, provenance hashes, chain entries, JSON encoders, and validators. The linker constructs v3 deltas from existing provider rows and linked binding context; relation/checkers validate through the same module. V3 becomes the authoritative memory contract while v2 fields are either removed from authoritative checks or retained as compatibility projections only.

Pros:
- Centralizes v3 invariants and prevents linker/relation/test drift.
- Mirrors the successful shared typed boundary pattern used by `Abstract_speculate_residual_scalar_call`.
- Directly supports role, location, value-transition, hash, and chain validation.
- Lets v3 reshape v2 without preserving weak memory-effect semantics as authority.

Cons:
- Adds a new module and requires coordinated linker/relation/test/doc edits.
- Requires careful report compatibility so downstream gates do not break for unrelated reasons.

#### Option B — Inline v3 JSON validation in linker/checkers

Keep all v3 evidence as JSON assembled in `abstract_speculate_residual_linker.ml` and duplicate validation logic in PE/oracle checkers.

Pros:
- Fewer files and lower immediate module plumbing.
- Faster if only one witness shape is considered.

Cons:
- Violates typed-boundary intent and invites producer/consumer drift.
- Makes wrong-evidence rejection depend on duplicated JSON path assumptions.
- Harder to evolve to general memory/alias delta evidence safely.

#### Option C — Strengthen v2 `global_effects`/`pointer_effects` in place

Keep schema v2 and add required role/transition/hash/chain fields to existing effect objects.

Pros:
- Smallest report-shape churn.
- Reuses existing PE/oracle mutation locations.

Cons:
- Conflicts with the clarified decision that v3 may replace/reshape v2.
- Preserves weak v2 naming even though the desired contract is a new authoritative memory-delta model.
- Makes it harder to distinguish compatibility projection from authoritative v3 evidence.

#### Option D — Broad residual memory/alias solver rewrite

Replace residual memory handling with a general memory/alias analysis or new solver-level model.

Pros:
- Could eventually cover richer memory semantics.

Cons:
- Rejected for this milestone: violates no solver rewrite, no proof expansion, no Oct/Taint, and no broad scheduling rewrite constraints.

### Chosen option

Choose Option A. Option C may be used only as a temporary compatibility projection: old `global_effects`/`pointer_effects` can remain in emitted reports if useful, but v3 `memory_deltas`/chain validation must be the authoritative evidence path. Option B is an implementation smell unless used only for very small adapter glue. Option D is out of scope.

### Pre-mortem (deliberate mode)

1. **Failure: v3 still proves only final-value equality.** A wrong writer/reader role or missing chain could pass if validation only checks effect location/value. Mitigation: typed validators must require actor roles, source/target locations, before/after values, source hash, chain id, and ordered chain entries; negative tests mutate each dimension.
2. **Failure: compatibility churn breaks scalar/proof/report gates.** Replacing v2 memory fields could accidentally disturb return effects, scalar-call metadata, linked derivation schema fields, or report locations. Mitigation: keep return/scalar compatibility fields stable, update only memory-authority assertions, and run PE/oracle/proof gates plus report field presence checks.
3. **Failure: general memory/alias abstraction expands into solver design.** A broad abstraction could leak into scheduling, residual equation semantics, or proof obligations. Mitigation: keep the new module pure and evidence-only; it consumes already-selected rows/bindings and never discovers providers, schedules calls, solves memory, or adds Oct/Taint/proof domains.

## Implementation Plan

### Step 1 — Add a shared v3 memory-delta protocol boundary

Files:
- Add `sparrow-modular-ocaml/src/abstract_speculate_residual_memory_delta.mli`.
- Add `sparrow-modular-ocaml/src/abstract_speculate_residual_memory_delta.ml`.
- Update `sparrow-modular-ocaml/src/dune` if the library stanza requires explicit module exposure.

Responsibilities:
- Define one canonical schema pair early and use it consistently in code, docs, and tests: external summary schema `abstract-speculate-external-summary/v3` plus nested memory delta schema `abstract-speculate-external-summary-memory-delta/v3`. Avoid multiple aliases for the same v3 contract.
- Define typed values for:
  - memory role: `Reader`, `Writer`, and explicit provider/importer/linker role labels;
  - memory location identity: raw location, normalized location, domain (`global-write-read` or `pointer-memory-effect`), symbol, and optional alias key for pointer evidence;
  - value transition: explicit `read_value` and `write_value` evidence. For this milestone, `read_value` means the bounded source value observed in the selected provider memory cell before summary encoding (or `unknown/not-observed` when the provider row has only a write witness), and `write_value` means the bounded destination value emitted by the provider final memory cell and carried into the v3 summary delta. Linked residual and oracle observations must reference the same transition by chain id rather than inventing a second transition. Pointer deltas use the same pair after alias/location normalization. Normalized/canonical value JSON should reuse existing full Sparrow-Itv residual-cell helpers where applicable;
  - provenance identity: provider module, source hash, artifact path, phase index, row/source evidence path, and chain hash/id;
  - delta chain: ordered entries linking provider memory cell evidence to summary memory delta to linked residual/oracle observation.
- Provide constructors from existing provider-row memory cells and the linker binding context that already exists in `make_external_summary` at `abstract_speculate_residual_linker.ml:485-579`.
- Provide JSON encoders/decoders and validators used by producer, relation, PE checker, oracle checker, and unit tests.

Constraints:
- The module must not traverse modules, discover providers, choose import/export matches, schedule residual work, invoke the solver, or model Oct/Taint.
- If a general memory/alias delta abstraction is introduced, it must remain a selected-witness evidence envelope with explicit unsupported-domain rejection.
- JSON shape must make missing chain data distinguishable from an empty but valid chain; missing chains reject.

### Step 2 — Reshape `ExternalSummary` to v3 and route linker memory evidence through typed constructors

File: `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml`.

Changes:
- Replace or extend `external_summary_v2` at `abstract_speculate_residual_linker.ml:70-76` with an `external_summary_v3` shape that contains authoritative v3 memory deltas and retains return/scalar compatibility as needed.
- Update `external_summary_to_json` at `abstract_speculate_residual_linker.ml:171-186` to emit schema `abstract-speculate-external-summary/v3`, a v3 memory-delta schema/version, and authoritative `memory_deltas`/`delta_chains` fields. Existing `return_effects` can remain for scalar-call compatibility; existing `global_effects`/`pointer_effects` should be retained only as non-authoritative compatibility projections or removed if all consumers are updated.
- In `make_external_summary` at `abstract_speculate_residual_linker.ml:485-579`, replace direct `typed_effect` construction for non-return memory cells at `:528-551` with calls to the v3 memory-delta constructors. The constructor call must bind each transition's source semantics: provider-row memory cell as the bounded read/source evidence, provider final memory cell/summary delta as the write/destination evidence, and linked/oracle observations as references to that same chain id.
- Preserve semantic export and linked report locations (`semantic_exports`, `external_summaries`, `linked_environment`, `linked_stage2_input_derivation`) at `abstract_speculate_residual_linker.ml:1878-1881` so existing report readers still know where to find evidence.
- Update linked stage-2 derivation metadata currently hard-coded to v2 at `abstract_speculate_residual_linker.ml:760-780` and `abstract_speculate_residual_linker.ml:808-835` so memory evidence points to v3 while scalar return evidence remains valid.

### Step 3 — Validate v3 memory deltas in relation code

File: `sparrow-modular-ocaml/src/abstract_speculate_residual_relation.ml`.

Changes:
- Import/use the shared v3 memory-delta validator beside existing scalar-call validation at `abstract_speculate_residual_relation.ml:10-12`.
- Add relation failure reasons for v3 memory mismatches, e.g. `memory_delta_role_mismatch`, `memory_delta_location_mismatch`, `memory_delta_value_transition_mismatch`, `memory_delta_provenance_mismatch`, and `memory_delta_chain_missing`. Prefer shared helper constants/assertions for these reason names so unit, PE, oracle, and relation tests do not drift.
- The relation layer must consume `external_summaries[*].memory_deltas` directly, not rely only on PE/oracle checker wrappers to inspect v3. `full_itv_semantic_relation_json` and selected-observation comparisons must fail with concrete v3 mismatch reasons when role, location, value-transition, provenance, or chain validation fails.
- Connect v3 deltas to selected memory observations currently produced by `global_observations`, `pointer_observations`, and `memory_location_observations` at `abstract_speculate_residual_relation.ml:542-590`.
- Keep witness-bounded relation boundaries: v3 may strengthen selected memory evidence but must not claim arbitrary-C whole-program memory equivalence.

### Step 4 — Add v3 unit tests and update Dune gates

Files:
- Add `sparrow-modular-ocaml/test/abstract_speculate_residual_memory_delta_unit.ml`.
- Update `sparrow-modular-ocaml/test/dune` executable declarations near `sparrow-modular-ocaml/test/dune:25-30` and active aliases near `sparrow-modular-ocaml/test/dune:371-445` as needed.

Test intent:
- Unit-test valid global write/read delta and pointer alias/effect delta construction.
- Unit-test rejection of wrong reader/writer role, wrong location, wrong before/after value transition, wrong provider/source hash, and missing/empty chain.
- Unit-test unsupported-domain rejection for Oct/Taint-like requests or any non-selected memory domain.
- Unit-test JSON round-trip compatibility and ensure validators fail when chain fields are omitted even if location/value still match.

### Step 5 — Update PE checker for v3 authoritative memory evidence

File: `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_check.ml`.

Changes:
- Replace v2 memory-authority checks in `external_summary_ok` at `abstract_speculate_residual_linking_pe_check.ml:145-158` with v3 summary and v3 memory-delta validation. Return-effect/scalar checks at `:125-170` should remain compatible unless the plan implementer intentionally renames only the surrounding summary schema.
- Extend false cases near `abstract_speculate_residual_linking_pe_check.ml:363-423` with v3 memory-specific mutations:
  - missing v3 summary / schema downgrade;
  - role swap (`reader` where `writer` is expected, or provider/importer role confusion);
  - wrong memory location / normalized location;
  - wrong before/read or after/write value transition;
  - wrong provider/source hash or chain hash;
  - missing chain or missing required chain entry.
- Update report fields currently saying `external_summary_v2_checked` at `abstract_speculate_residual_linking_pe_check.ml:611-612` and false-case labels at `abstract_speculate_residual_linking_pe_check.ml:615-637` to v3 names while preserving compatibility notes if v2 projections remain.

### Step 6 — Update oracle-suite checker and relation negative gates

File: `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml`.

Changes:
- Replace v2 memory-effect validation at `abstract_speculate_residual_linking_oracle_suite_check.ml:120-157` and `:173-180` with v3 delta validation through the shared module.
- Replace existing v2 global/pointer negative mutations at `abstract_speculate_residual_linking_oracle_suite_check.ml:663-743` and false-case labels at `:890-899` with v3 rejection cases for role, location, value transition, provenance hash, and missing chain.
- Ensure full relation failures expose the v3 memory mismatch reasons, not only generic `value_mismatch` or missing observation.
- Preserve other existing false cases such as scalar protocol mismatch, typed ITV cell metadata mismatch, ambiguous provider, mixed role chain, premerge shortcut, missing oracle, and cycle evidence checks at `abstract_speculate_residual_linking_oracle_suite_check.ml:875-905`.

### Step 7 — Update documentation and verification matrix

Files:
- Update `sparrow-modular-ocaml/doc/experiments/abstract-speculate-residual-linking-pe.md`.
- Update `sparrow-modular-ocaml/doc/verification.md`.

Documentation must state:
- ExternalSummary v3 is the authoritative memory-delta contract; any retained v2 memory fields are compatibility projections only.
- Required v3 memory delta fields: roles, location identity, before/after or read/write value transition, provenance/source hash, and chain entries.
- General memory/alias delta support is witness-bounded and does not claim arbitrary-C memory equivalence.
- Non-goals remain: no residual solver rewrite, no proof-system expansion, no Oct/Taint support, no broad call/link scheduling rewrite.
- Verification commands include the new memory-delta unit executable and existing PE/oracle/proof gates.

### Step 8 — Lock migration, compatibility, and failure-reason guardrails

Files:
- Update `sparrow-modular-ocaml/src/abstract_speculate_residual_memory_delta.{mli,ml}`.
- Update `sparrow-modular-ocaml/src/abstract_speculate_residual_linker.ml`.
- Update `sparrow-modular-ocaml/src/abstract_speculate_residual_relation.ml`.
- Update PE/oracle checker report assertions in `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_pe_check.ml` and `sparrow-modular-ocaml/test/abstract_speculate_residual_linking_oracle_suite_check.ml`.

Guardrails:
- Define a short migration adapter in the v3 module rather than open-coding v2-to-v3 compatibility in each checker. The adapter may expose compatibility projections for report continuity, but it must return an explicit `compatibility_projection_only` marker so tests cannot accidentally treat v2 memory fields as authoritative evidence.
- Keep failure-reason strings centralized with the v3 validator. PE/oracle/relation code should compare against shared reason constructors or constants for role, location, value transition, provenance, unsupported domain, and missing chain failures.
- Add a single report-level validation summary object for each summary, e.g. `memory_delta_validation`, containing v3 schema id, checked delta count, checked chain count, compatibility projection status, and ordered failure reasons when validation rejects. This gives reviewers an auditable place to confirm that v3 was checked without parsing every delta by hand.
- During migration, reject mixed-authority reports: if `memory_deltas` are absent but v2 `global_effects`/`pointer_effects` are present, the relation and checkers must fail with a v3 missing-evidence reason rather than silently falling back to v2.
- Keep generated identifiers deterministic. Chain ids/hashes should be computed from canonical role, normalized location, transition, provenance, and ordered chain-entry fields, or be deterministic strings derived from the same fields; never use timestamps, randomized ids, or filesystem order.
- Add a focused review check that the new module is evidence-only: it must not call provider discovery, relation solving, scheduling, proof construction, Oct/Taint modules, or filesystem traversal beyond data already supplied by callers.

## Worker-5 Implementation Hardening Addendum

This addendum narrows the handoff between plan ownership and implementation ownership so the v3 contract can be implemented without re-opening broad design choices.

### Shared validator API contract

The new memory-delta module should expose one producer path and one consumer path rather than letting each caller interpret JSON independently:

- `schema_id` and `memory_delta_schema_id` constants for `abstract-speculate-external-summary/v3` and `abstract-speculate-external-summary-memory-delta/v3`.
- `make_global_delta` and `make_pointer_delta` constructors that require role, normalized location, value transition, provenance identity, and ordered chain entries before JSON can be emitted.
- `validate_delta` and `validate_summary_memory` functions that return structured success metadata or a typed failure reason; callers may stringify the reason only at report boundaries.
- `compatibility_projection` helpers for old `global_effects`/`pointer_effects` fields that always mark those fields as `compatibility_projection_only` and never satisfy v3 authority by themselves.

### Deterministic chain and hash rules

Implementation should use deterministic identifiers from canonical fields only:

1. Canonicalize roles before locations, then transitions, provenance, and ordered chain entries.
2. Normalize pointer alias keys before computing chain ids so pointer evidence is stable across equivalent raw spellings.
3. Include both `read_value` and `write_value` in the chain hash input even when one side is `unknown/not-observed`; omitting a side must be distinguishable from explicitly unknown evidence.
4. Reject duplicate chain-entry ordinals and reject non-contiguous chains when the validator is asked to validate a complete summary.
5. Keep filesystem paths as supplied evidence strings; do not traverse the filesystem or read artifacts in the validator.

### File-level sequencing guardrail

A safe implementation order is:

1. Add the typed module and unit tests with no linker changes.
2. Convert linker construction to emit v3 while still retaining compatibility projections.
3. Switch relation/PE/oracle validation to v3 authority and make missing v3 evidence fail even when v2 projections remain.
4. Rename report/check labels from v2 to v3 only after the new validation path is active.
5. Update docs and verification matrix last, using command evidence from the new gates.

This order prevents a transient state where reports say v3 while the relation still accepts v2 memory effects as authoritative.

### Reviewer stop checks

Before accepting implementation, reviewers should inspect the diff for these specific failure modes:

- no hand-built authoritative v3 JSON in linker, PE, oracle, or relation code;
- no fallback where missing `memory_deltas` silently reads `global_effects` or `pointer_effects` as authority;
- no non-deterministic chain ids, timestamps, random ids, or filesystem-order-dependent hashes;
- no calls from the memory-delta module into provider discovery, scheduling, solver, proof, Oct, or Taint code;
- no deletion of existing scalar, mixed-role, cycle, missing-oracle, or proof guard tests to make v3 pass.

## Acceptance Criteria

1. `ExternalSummary` emits schema/versioned v3 memory-delta evidence for selected global and pointer memory effects.
2. V3 memory deltas are constructed through a shared typed boundary before JSON encoding; hand-built authoritative v3 JSON is not the producer path.
3. V3 deltas record reader/writer/provider/importer roles and reject role swaps or role omissions.
4. V3 deltas record raw and normalized memory location identity and reject wrong-location or wrong-alias evidence.
5. V3 deltas record value-transition evidence and reject wrong read/write, before/after, or canonical value transitions.
6. V3 deltas record provider/source/artifact/provenance identity and reject stale or wrong hashes.
7. V3 deltas require an ordered delta chain and reject missing chain fields even if old v2 effect location/value fields still look plausible.
8. Existing scalar return/call evidence remains valid and does not regress typed scalar-call validation.
9. PE checker rejects all required v3 negative mutations.
10. Oracle-suite/full relation rejects all required v3 negative mutations and reports concrete v3 memory mismatch reasons.
11. Documentation identifies v3 as authoritative, explains compatibility projections, and lists non-goals.
12. No implementation step rewrites residual solver scheduling, adds Oct/Taint support, or expands the proof framework.
13. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @check` passes.
14. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune exec test/abstract_speculate_residual_memory_delta_unit.bc` passes.
15. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe` passes.
16. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite` passes.
17. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_pe_proof` passes, or an environment-only blocker is documented with targeted gates passing.
18. `git diff --check` passes.

## Risks and Mitigations

- **Risk: v3/v2 ambiguity leaves both schemas partially authoritative.** Mitigation: docs and tests must mark v3 memory deltas authoritative; any v2 memory fields are compatibility projections only and are not sufficient for pass.
- **Risk: value-transition semantics are under-specified for pointer evidence.** Mitigation: encode a typed transition with explicit normalization and alias key; tests cover pointer alias/effect mutation separately from global location mutation.
- **Risk: chain hashes become brittle due to field ordering.** Mitigation: compute chain ids from deterministic canonical fields or use deterministic string ids; unit tests cover stable JSON encoding and mutation rejection.
- **Risk: checker updates hide genuine relation failures under schema churn.** Mitigation: keep existing non-memory false cases and add v3-specific failure reasons rather than deleting broad checks.
- **Risk: new module duplicates full ITV or scalar-call logic.** Mitigation: reuse `Abstract_speculate_itv_residual_cell` for value canonicalization and leave scalar return/call metadata in `Abstract_speculate_residual_scalar_call`.
- **Risk: proof gate cost or environment mismatch slows iteration.** Mitigation: run memory unit, PE, and oracle gates first; only document proof gate blockers when they are environment-only and targeted gates pass.

## Verification Steps

1. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @check`
2. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune exec test/abstract_speculate_residual_memory_delta_unit.bc`
3. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune exec test/abstract_speculate_residual_scalar_call_unit.bc`
4. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe`
5. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite`
6. `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_pe_proof`
7. Inspect generated PE/oracle reports for v3 `memory_deltas`/chain fields and preserved scalar/return compatibility fields.
8. `git diff --check`

## Expanded Test Plan (deliberate mode)

### Unit

- `abstract_speculate_residual_memory_delta_unit.ml` validates constructors, schema id, role encoding, memory location normalization, value-transition canonicalization, provenance/source hash binding, delta-chain creation, deterministic JSON encoding, and validator rejection reasons.
- Unit negatives cover wrong reader/writer role, wrong location, wrong before/after value, wrong provenance hash, missing chain, empty chain, unsupported domain, and JSON with plausible old v2 location/value but no v3 chain.

### Integration

- `abstract_speculate_residual_linking_pe_check.ml` validates the linker emits v3 memory deltas in `external_summaries` and rejects in-memory report mutations for all required wrong-evidence classes.
- `abstract_speculate_residual_linking_oracle_suite_check.ml` validates the oracle-suite witnesses consume v3 memory deltas for global and pointer cases and keep scalar/mixed-role/cycle false cases intact.

### E2E

- `@abstract_speculate_residual_linking_pe` regenerates the residual-linking PE report with v3 memory-delta evidence.
- `@abstract_speculate_residual_linking_oracle_suite` regenerates oracle-suite reports and exercises full relation gates over global and pointer memory witnesses.
- `@abstract_speculate_pe_proof` verifies the broader Abstract Speculate proof gate remains green after the v3 memory-delta replacement.

### Observability

- Generated reports expose enough v3 fields to inspect schema id, memory role path, raw/normalized location, value transition, provider/source/artifact identity, chain id/hash, ordered chain entries, and validation status.
- Failure reports include specific v3 memory mismatch reasons so a reviewer can distinguish role, location, value, provenance, and chain failures.
- Documentation and verification matrix list the new unit gate and explicitly identify v3 memory deltas as authoritative.

## ADR

### Decision

Adopt a shared `ExternalSummary v3` memory-delta protocol module and make v3 memory deltas the authoritative residual-linking memory evidence, with any v2 memory fields treated only as compatibility projections.

### Drivers

- User requires auditable rejection of wrong memory roles, locations, value transitions, provenance hashes, and missing chains.
- Current v2 global/pointer effect lists validate only a weaker selected effect shape and were explicitly allowed to be reshaped or replaced.
- Existing PE/oracle/proof gates and scalar-call protocol must continue to pass without solver/proof/domain expansion.

### Alternatives considered

- Inline v3 JSON validation in linker/checkers: rejected because it duplicates invariants and keeps JSON as the authority.
- Strengthen v2 in place: rejected because it blurs authoritative v3 evidence with compatibility fields and undercuts the clarified v3 replacement permission.
- Broad solver/general memory rewrite: rejected because it violates no solver rewrite, no proof expansion, and no Oct/Taint constraints.

### Why chosen

A shared typed v3 protocol gives the narrowest boundary that satisfies the new evidence semantics. It lets the linker, relation, PE checker, oracle checker, and unit tests agree on the same role/location/value/provenance/chain invariants without turning the task into a solver or proof-system rewrite.

### Consequences

- Adds one shared protocol module and a dedicated unit test executable.
- Requires coordinated updates to linker summary construction, relation validation, PE/oracle checks, docs, and verification notes.
- Existing v2 memory labels in tests/docs should be renamed or explicitly demoted to compatibility projections.
- Future memory-summary work should extend the v3 protocol rather than re-authorizing v2 memory effects.

### Follow-ups

- After implementation, consider a separate plan only if broader memory/alias deltas need more witnesses beyond selected global and pointer cases.
- If external consumers depend on v2 memory fields, document a temporary compatibility window before removing projections.
- If performance or report-size concerns appear, use `$performance-goal`; otherwise keep this as correctness/contract work.

## Available-Agent-Types Roster

- `explore` (`gpt-5.3-codex-spark`, low): fast repository mapping and exact file/line lookup.
- `architect` (`gpt-5.5`, high): boundary and schema review.
- `executor` (`gpt-5.5`, medium): implementation/refactoring lanes.
- `test-engineer` (`gpt-5.5`, medium): unit/integration/negative gate design.
- `debugger` (`gpt-5.5`, high): root-cause failing gate analysis.
- `verifier` (`gpt-5.5`, high): final evidence validation and claim audit.
- `writer` (`gpt-5.5`, high): docs/verification updates.
- `code-reviewer` (`gpt-5.5`, high): comprehensive pre-merge review.
- `critic` (`gpt-5.5`, high): plan/design challenge and risk review.

## Follow-up Staffing Guidance

### `$team` recommended path

Use Team for implementation because the work has separable protocol, linker/relation, tests, and docs lanes.

Suggested lanes:
1. **Protocol executor (medium):** own `abstract_speculate_residual_memory_delta.{mli,ml}` and unit test skeleton.
2. **Linker/relation executor (medium/high):** own `abstract_speculate_residual_linker.ml` and `abstract_speculate_residual_relation.ml`; coordinate with protocol lane on API shape.
3. **Test-engineer (medium):** own PE/oracle checker negative mutations and `test/dune` updates.
4. **Writer (high):** own docs and verification matrix updates.
5. **Verifier (high):** run gates, inspect reports, and validate no forbidden solver/proof/Oct/Taint expansion.

### `$ralph` fallback path

Use Ralph if Team is unavailable or after Team returns with integration failures. Ralph should run as a single-owner fix/verify loop over `.omx/plans/prd-external-summary-v3-memory-delta.md` and `.omx/plans/test-spec-external-summary-v3-memory-delta.md`, focusing on sequential integration and evidence collection.

## Goal-Mode Follow-up Suggestions

- `$ultragoal` is the default durable goal-mode follow-up if the user wants ledger-style progress tracking across implementation and verification.
- `$autoresearch-goal` is not primary here; this is brownfield implementation, not external research.
- `$performance-goal` should be used only if v3 report construction or validation becomes a measurable performance/size optimization task.
- For durable parallel delivery, use **Team + Ultragoal**: Team executes parallel lanes and returns checkpoint-ready evidence; Ultragoal remains leader-owned goal/ledger state.

## Team Launch Hints

```bash
$team .omx/plans/prd-external-summary-v3-memory-delta.md
```

Suggested direct shell form when using OMX CLI:

```bash
omx team --plan .omx/plans/prd-external-summary-v3-memory-delta.md --workers 5
```

Team instructions should include both plan artifacts:

```bash
.omx/plans/prd-external-summary-v3-memory-delta.md
.omx/plans/test-spec-external-summary-v3-memory-delta.md
```

## Team Verification Path

Before Team shutdown, the team should prove:
- unit memory-delta gate passes;
- PE and oracle-suite gates pass;
- `@check` and proof gate pass or documented environment-only blocker exists;
- generated reports contain v3 memory-delta fields and required scalar/return compatibility fields;
- negative tests reject role/location/value/provenance/chain mutations;
- no solver rewrite, proof-system expansion, Oct, or Taint paths were added.

After Team handoff, Ralph should verify only if needed:
- run the full command matrix again from a clean worktree;
- inspect diff for scope creep and compatibility projections;
- fix any remaining integration failures sequentially.

## Applied Review Changelog

- Initial planner draft created with deliberate RALPLAN-DR, ADR, staffing guidance, launch hints, and team verification path.
- Architect-required revision applied: clarified `read_value`/`write_value` source semantics and required linked/oracle observations to reference the same delta chain.
- Architect-required revision applied: required relation-level direct consumption of `external_summaries[*].memory_deltas` with concrete v3 mismatch reasons, not only checker-wrapper failures.
- Critic-approved tightening applied: pinned canonical v3 schema strings and requested shared mismatch-reason constants/assertions to prevent string drift.
