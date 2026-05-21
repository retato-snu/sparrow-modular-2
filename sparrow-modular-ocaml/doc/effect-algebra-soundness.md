# External Summary Effect Algebra Soundness Taxonomy

The external summary boundary is now authority-inverted: typed effects and typed
projections are the construction authority; legacy v3-shaped JSON fields are
compatibility projections only.  A consumer may inspect `return_effects`,
`memory_deltas`, `global_effects`, `pointer_effects`, `taint_components`, or
`product_pair_evidence` for diagnostics, but those fields must not be the source
of derivation truth.

## Taxonomy

| Class | Typed authority | Guaranteed observation | Non-guarantee |
| --- | --- | --- | --- |
| Return | `Return` effect + return projection | stable effect/projection IDs, provider provenance, scalar value link | no arbitrary C return theorem |
| Memory | `Memory_transition` / global projection | selected witness read/write transition and evidence path | no complete heap semantics |
| Alias | `Alias_evidence` effect | pointer projection can cite alias evidence | no complete alias analysis |
| Heap | `Heap_location` effect | allocation/path evidence can support pointer observation | lossy heap projections are undefined |
| Struct | `Struct_field_path` effect | field path observation is explicit | no whole-struct shape proof |
| Array | `Array_segment` effect | index/segment path is explicit | no full array bounds proof |
| Global | `Global_observation` projection | existing global write/read obligation names remain stable | no whole-program global theorem |
| Pointer | `Pointer_observation` projection | existing pointer-memory obligation names remain stable | no complete pointer model |
| Taint | `Taint` effect | bounded named taint witness and provenance | no general product-domain parity |
| Product pair | `Product_pair` effect | Itv/Taint relation cites related effect IDs | no full verifier rewrite |

## Partial operations

`compose`, `join`, `restrict`, `observe`, and serialization are partial.  When an
operation is undefined it returns an explicit reason: incompatible domain,
incompatible provenance, incompatible path, missing alias evidence, lossy heap
projection, unsupported observation, invalid composition order, or taint/product
mismatch.  Undefined results are serialized only as negative diagnostics and are
not valid effect/projection artifacts.

## Authority and evidence rules

- Derivations link by typed effect/projection IDs and provenance.
- Legacy v3 schema names may appear only as compatibility labels or migration
  diagnostics.
- Relation obligation names/categories remain stable while evidence paths cite
  typed projection artifacts.
- This is not a verifier rewrite, whole-analysis rewrite, proof-assistant effort,
  or v3 JSON compatibility promise.
