# Real Sparrow Access-DUG — Artifact Schema and Experiment Plan

Status: **Phase 1 artifact contract**  
Schema version: `real-sparrow-access-dug/v1`  
Parent plan: `.omx/plans/plan-real-sparrow-sparse-access-dug.md`  
Phase 0 closure: `sparrow-modular-ocaml/doc/experiments/real-sparrow-access-dug-closure.md`

## Boundary claim

This experiment targets only:

```ocaml
Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make / Dug construction
```

The accepted evidence is normalized active-vs-frozen structural parity for:

- pre-DUG sparse-spec inputs (`premem`, `locset`, `locset_fs`, `Options.pfs` mode),
- access summaries,
- DUG nodes, directed edges, and abstract-location labels,
- static/dynamic/residual classification facts.

## Non-claims

No artifact may claim `Worklist.init`, `SparseAnalysis.perform`, Strong sparse fixpoint, widening, narrowing, convergence, PartialFlowSensitivity staging/ranking parity, whole-program merge equivalence, executable residual code/linking, or residual global-fixpoint behavior.

## Schema-wide normalization

Every module artifact includes:

- `schema_version`: `real-sparrow-access-dug/v1`
- `source`: fixture source path
- `scope`: `module-only`
- `boundary`: exact boundary above
- `lineage`: source anchors for AccessSem, AccessAnalysis, Dug, SsaDug, and sparse-spec derivation
- `projection`: normalized comparison payload
- `non_claims`: explicit excluded claims

Normalization rules:

1. Sort nodes, procedures, locations, maps, sets, edges, and labels by stable textual key.
2. Use fixture basenames for comparison where absolute paths differ.
3. Exclude timestamps, PIDs, memory addresses, hash order, raw graph order, and wall-clock timing.
4. Emit edge labels as sorted arrays.
5. Treat `residual` only as a classification label.

## Artifact classes

### 1. Pre-DUG sparse-spec inputs

Required fields:

```json
{
  "premem_summary": [{"location": "...", "value": "...", "bta": "static|dynamic"}],
  "locset": ["<loc>"],
  "locset_fs": ["<loc>"],
  "pfs": 100,
  "derivation_mode": "default-full-flow-sensitive-input-contract"
}
```

Comparison: active and frozen must be `structural-equiv` before access or DUG parity is accepted.

### 2. Access summaries

Required fields:

- per-node `use`, `def`, and `access` sets,
- per-procedure summary,
- per-procedure reachable summary,
- per-procedure local set,
- per-procedure reachable-without-local summary,
- program-local set,
- def-node and use-node indexes by abstract location,
- total abstract locations.

### 3. DUG structure

Required fields:

- sorted DUG nodes,
- sorted directed edges,
- sorted abstract-location labels per edge,
- edge kind: `intra` or `inter`.

### 4. BTA/static-residual classification

Facts classify module-local access/DUG facts as `static`; extern commands and unresolved module-only calls are `dynamic` or `residual`. Classification does not generate or execute residual code.

### 5. Comparison report

The comparison report emits module-level relation and category relations:

```json
{
  "schema_version": "real-sparrow-access-dug/v1",
  "claim": "module-only Access+DUG construction parity",
  "relation": "structural-equiv|structural-diverge",
  "modules": [
    {
      "source_basename": "fixture.c",
      "pre_dug_spec_inputs": "structural-equiv|structural-diverge",
      "access_summaries": "structural-equiv|structural-diverge",
      "dug_structure": "structural-equiv|structural-diverge",
      "bta": "accepted|rejected"
    }
  ]
}
```

## Fixture contract

Accepted module-only fixtures cover:

1. direct internal call,
2. local/global memory access,
3. branch/join opportunity,
4. interprocedural return/access case,
5. extern-dependent unresolved/dynamic classification,
6. extern-independent static completion.

## Continue decision

This schema is sufficient to begin source-lineage extraction. Any need for Worklist, sparse fixpoint, PFS ranking/staging, whole-program merge, or residual linker behavior is a scope breach and must return to planning.
