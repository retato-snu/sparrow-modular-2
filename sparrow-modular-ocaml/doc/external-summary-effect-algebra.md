# ExternalSummary typed effect algebra

This note defines the local soundness boundary for the ExternalSummary effect
algebra introduced for residual linking.  It is not a verifier rewrite and it is
not a whole-analysis semantic model; it is the authority boundary for external
summary observations that were previously carried directly by v3 JSON fields.

## Authority model

Typed effects are constructed first through `Abstract_speculate_effect_algebra`.
JSON is a projection over defined effects/projections.  Undefined operation
results are represented by typed `undefined_reason` values and must not be
serialized as valid effect artifacts.

Legacy v3-compatible fields may still appear as projection-only compatibility
metadata while migration is in progress.  They are not the construction
authority for linked scalar derivations when a typed return projection is
present.

## Taxonomy

Covered effect domains:

- return observation
- memory transition
- alias evidence
- heap location
- struct field path
- array index/segment
- global observation
- pointer observation
- taint component
- product-pair evidence

Projection kinds:

- return
- global
- pointer
- taint
- product-pair

## Partial operations

The algebra exposes four local operations:

- `compose`: sequentially combine compatible provenance/domain/path effects.
- `join`: merge effects only when domain, provenance, and location match.
- `restrict`: narrow an effect to a prefix path.
- `observe`: produce a typed projection only for supported domain/projection
  pairs.

Undefined reasons are intentionally enumerated: incompatible domain,
incompatible provenance, incompatible path, missing alias evidence, lossy heap
projection, unsupported observation, invalid composition order, and
taint/product mismatch.

## Guarantees

- Defined effect and projection JSON includes the typed schema ID, stable IDs,
  provenance, and evidence path information.
- Return derivation validation can rely on typed return projection IDs instead
  of structural equality with legacy `return_effects` lists.
- Taint/product evidence is represented as typed effects/projections when the
  bounded named product witness is present.

## Non-guarantees

- No claim is made that alias or heap precision is complete.
- No new Oct/Taint whole-domain parity is introduced.
- No full verifier rewrite or whole residual-analysis rewrite is included.
- Compatibility JSON names may remain during migration, but only as
  non-authoritative projections.
