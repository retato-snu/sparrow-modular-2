# ADR-0007: ExternalSummary effect algebra authority boundary

Status: accepted for implementation boundary

## Decision

`ExternalSummary v3` witness fields are not the long-term authority for
residual-linking summaries.  The accepted redesign moves authority to typed
ExternalSummary effects and typed projections.  JSON summaries, relation witness
categories, return/global/pointer observations, taint evidence, and product-pair
evidence are serialization or review projections over those typed artifacts.

The redesign is intentionally local to the ExternalSummary/effect/adapter layer.
It must not become a verifier rewrite, whole-analysis semantics rewrite, proof
assistant migration, new dependency adoption, or compatibility effort for old v3
JSON consumers.

## Current implementation state

The current code still carries the pre-redesign v3 protocol:

- `abstract_speculate_residual_linker.ml` embeds `external_summary_v3` in
  `semantic_export` and serializes `return_effects`, `memory_deltas`,
  `delta_chains`, compatibility `global_effects` / `pointer_effects`, bounded
  taint components, and product-pair evidence.
- `Abstract_speculate_residual_memory_delta` and
  `Abstract_speculate_residual_scalar_call` provide typed envelopes for selected
  memory and scalar witnesses, but they are not yet the complete effect algebra
  authority described here.
- PE/oracle tests currently validate v3 schema, memory-delta chains, scalar
  derivations, and compatibility projection status.  Those tests must be
  migrated rather than relaxed when the effect algebra lands.

Until the new effect schema is emitted and consumed, the soundness claim remains
`prototype-internal`, witness-bounded, and v3-memory-delta based.

## Typed effect taxonomy

The effect algebra must cover the following typed domains before JSON
projection:

| Domain | Typed authority | Projection examples | Review notes |
| --- | --- | --- | --- |
| Return observation | Provider return effect with provider/export/return location and residual-cell value provenance | linked call derivation, scalar return projection | Old structural equality against v3 `return_effects` is not authority. |
| Memory transition | Read/write transition for selected global or pointer-backed cells | memory delta JSON, delta chain JSON | Chains bind provider, linker, reader/importer roles. |
| Alias evidence | Alias key/path evidence for pointer-backed observations | pointer projection, alias evidence path | Missing alias evidence must be an undefined operation, not best-effort JSON. |
| Heap location/allocation | Heap identity used by selected pointer/global projections | heap projection id, evidence path | No claim of complete heap modeling. |
| Struct field path | Field-sensitive selected path effect | field projection id | Unsupported or incompatible paths are typed undefined cases. |
| Array index/segment | Index or segment effect for selected array observations | array projection id | Lossy segment projection must be explicit. |
| Global observation | Projection derived from memory transition effects | global write/read relation evidence | Retained `global_effects` are compatibility projections only. |
| Pointer observation | Projection derived from memory/alias/heap effects | pointer-memory relation evidence | Retained `pointer_effects` are compatibility projections only. |
| Taint component | Bounded named taint source/sink component | taint component projection | Not full Taint-domain parity. |
| Product pair | Typed Itv+Taint relation over related effect/projection ids | product-pair evidence projection | Old v3 membership cannot be the source of truth. |

## Partial operations and laws

All composition APIs must return a typed result:

```ocaml
type 'a operation_result =
  | Defined of 'a
  | Undefined of undefined_reason
```

Required partial operations:

- `compose`: sequentially combines compatible effects in provenance/location
  order.
- `join`: joins compatible abstractions or explicitly typed summary/top effects.
- `restrict`: narrows an effect to a location, path, or observation scope.
- `observe`: derives return/global/pointer/taint/product projections from typed
  effects.
- `serialize`: serializes only `Defined` effects or projections.

Required law scope where operations are defined:

- `compose` identity and associativity.
- `join` idempotence and commutativity.
- `restrict` idempotence.
- Stable projection ids and evidence paths when observing the same compatible
  defined effect more than once.

The laws do not assert algebraic totality.  Undefined results are part of the
contract and must remain observable to tests and review.

## Undefined reason taxonomy

Undefined results must carry one exact reason from this taxonomy or a narrowly
reviewed extension:

- `Incompatible_domain`
- `Incompatible_provenance`
- `Incompatible_path`
- `Missing_alias_evidence`
- `Lossy_heap_projection`
- `Unsupported_observation`
- `Invalid_composition_order`
- `Taint_product_mismatch`

Undefined results must not serialize as valid effect or projection artifacts.
JSON validators may report the reason for audit, but they must not turn an
undefined operation into an accepted summary.

## Projection model

Typed effects are authoritative.  Projections are observations with stable ids,
provenance, and evidence paths:

- Return projections feed scalar-call derivations and importer linked stage-2
  input metadata.
- Global projections feed `global-write-read` obligations.
- Pointer projections feed `pointer-memory-effect` obligations and cite alias or
  heap evidence when needed.
- Taint/product projections cite related effect/projection ids and bounded
  witness ids.

Projection JSON may preserve familiar relation categories during migration, but
relation pass/fail logic must cite typed effect/projection ids rather than raw
v3 list membership.

## Guarantees

When implementation and tests satisfy the PRD/test spec, the layer guarantees:

- illegal summary states are rejected at typed construction or returned as
  typed undefined results;
- defined effects/projections include schema id, effect/projection id, domain,
  provenance, and evidence path;
- `external_summary_v1_compat`, retained `global_effects`, and retained
  `pointer_effects` are non-authoritative compatibility projections only;
- scalar-call derivations link through typed effect/projection ids;
- relation obligations may keep their category names initially, but their
  evidence source is typed projection authority;
- taint/product-pair evidence is either typed/projection-based or explicitly
  adapter-only and non-authoritative.

## Non-guarantees

The effect algebra does not claim:

- arbitrary-C whole-program semantic equivalence;
- a full verifier rewrite or complete analysis semantics rewrite;
- full heap precision, full alias completeness, or general Taint/product-domain
  parity;
- compatibility for old ExternalSummary v3 JSON consumers;
- public API stability beyond the prototype/internal residual-linking slice.

## Review gates

A change implementing this ADR must fail review if any of the following remain
true:

- `external_summary_v3` or old top-level v3 JSON fields are still the emitted
  authority for linked derivations;
- linked derivations compare raw `return_effect` JSON structurally to summary
  `return_effects` as the truth source;
- old v3 `taint_components` or `product_pair_evidence` membership determines
  pass/fail without typed related effect/projection ids;
- undefined operation results can serialize as accepted effects/projections;
- docs or reports upgrade the soundness claim beyond witness-bounded,
  prototype-internal evidence without new proof obligations.
