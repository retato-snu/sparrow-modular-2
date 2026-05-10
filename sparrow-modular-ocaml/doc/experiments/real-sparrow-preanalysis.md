# Real Sparrow PreAnalysis PE — Artifact Schema and Experiment Plan

Status: **Phase 1 artifact contract**  
Parent plan: `.omx/plans/plan-real-sparrow-preanalysis-pe.md`  
Phase 0 closure: `sparrow-modular-ocaml/doc/experiments/real-sparrow-preanalysis-closure.md`

## Boundary claim

This experiment targets only:

```ocaml
Global.init -> PreAnalysis.perform
```

The active artifact must show lineage through:

```ocaml
ItvSem.run AbsSem.Weak ItvSem.Spec.empty
```

No artifact produced by this milestone may claim Strong-mode staging, sparse/DUG parity, whole-program merge equivalence, executable residual code/linking, or link-time/global-fixpoint evidence.

## Schema-wide rules

Every emitted JSON artifact must include:

- `schema_version`: currently `real-sparrow-preanalysis/v1`
- `source`: fixture-relative source path
- `boundary`: exactly `Global.init -> PreAnalysis.perform`
- `lineage`: source files and source functions used for the projection
- `projection`: normalized comparison payload
- `scope`: `module-only`
- `bta`: static/dynamic/residual classifications where relevant

Normalization rules:

1. Sort procedure names, nodes, locations, call edges, map bindings, and set members by stable textual key.
2. Normalize source locations to fixture-relative paths plus line/byte information when available.
3. Do not include timestamps, process IDs, addresses, hash seeds, wall-clock durations, or iteration-order-dependent fields.
4. Use textual forms only when they are stable across active and frozen producers; otherwise map them through deterministic fixture-local IDs.
5. Treat `residual` only as a classification label. It is not executable residual code and not a linker/global-fixpoint claim.

## Artifact classes

### 1. Weak-step lineage

Purpose: prove the active path descends from real `PreAnalysis.onestep_transfer` and executes/stages Weak `ItvSem.run`.

Required fields:

```json
{
  "schema_version": "real-sparrow-preanalysis/v1",
  "artifact": "weak-step-lineage",
  "boundary": "Global.init -> PreAnalysis.perform",
  "lineage": {
    "preanalysis": "sparrow/src/core/preAnalysis.ml:19-23",
    "itv_sem_run": "sparrow/src/semantics/itvSem.ml:811-899",
    "mode": "AbsSem.Weak",
    "spec": "ItvSem.Spec.empty"
  },
  "steps": [
    {
      "procedure": "<stable-proc>",
      "node": "<stable-node>",
      "command_kind": "<stable-kind>",
      "transfer": "ItvSem.run",
      "mode": "Weak",
      "state_summary_id": "<stable-summary-id>",
      "bta": "static|dynamic|residual"
    }
  ]
}
```

Comparison: active and frozen must agree on step identity, mode, command kind, and classification for accepted fixtures.

### 2. Callgraph parity

Purpose: compare direct call edges and transitive callgraph shape after PreAnalysis.

Required fields:

```json
{
  "artifact": "callgraph-parity",
  "direct_edges": [["<caller>", "<callee>"]],
  "transitive_edges": [["<caller>", "<callee>"]],
  "unresolved_calls": [
    {"procedure": "<proc>", "node": "<node>", "reason": "extern-dependent", "bta": "dynamic|residual"}
  ]
}
```

Comparison: sorted edge lists must match. Extern-dependent unresolved calls must be classified, not guessed.

### 3. Pruning parity

Purpose: compare branch/pruning and unreachable-function effects.

Required fields:

```json
{
  "artifact": "pruning-parity",
  "prune_sites": [
    {"procedure": "<proc>", "node": "<node>", "condition": "<stable-condition>", "result_summary": "<stable-summary>"}
  ],
  "reachable_functions": ["<proc>"],
  "pruned_functions": ["<proc>"],
  "removed_nodes": ["<node>"]
}
```

Comparison: sorted reachable/pruned function sets and prune-site summaries must match.

### 4. Memory summary parity

Purpose: compare a stable projection of `global.mem` sufficient to justify call-target resolution and fixture claims.

Required fields:

```json
{
  "artifact": "memory-summary-parity",
  "locations": [
    {
      "location": "<stable-location>",
      "value": {
        "interval": "<normalized-itv>",
        "locations": ["<stable-location>"],
        "arrays": ["<stable-array-summary>"],
        "structs": ["<stable-struct-summary>"],
        "procedures": ["<stable-proc>"]
      },
      "bta": "static|dynamic|residual"
    }
  ]
}
```

Comparison: sorted location/value summaries must match for accepted fixture projections. If full internal memory is too noisy, the projection must be explicitly documented and sufficient for call-target and pruning evidence.

### 5. BTA/static-completion report

Purpose: show that calculations independent of externs were completed at staging time, while extern-dependent facts were not guessed.

Required fields:

```json
{
  "artifact": "bta-static-completion",
  "facts": [
    {
      "id": "<stable-fact-id>",
      "kind": "weak-step|call-target|prune|memory",
      "classification": "static|dynamic|residual",
      "reason": "<short stable reason>",
      "depends_on_extern": false
    }
  ]
}
```

Acceptance:

- extern-independent calculations needed by selected fixtures are `static`
- extern-dependent facts are `dynamic` or `residual`
- `residual` is never interpreted as generated code or linker/global-fixpoint work

## Fixture contract

Initial module-only fixtures should cover:

1. `direct_call.c` — internal direct call target and callgraph edge.
2. `unreachable_function.c` — function pruning after PreAnalysis callgraph closure.
3. `branch_pruning.c` — pruning evidence from a branch condition.
4. `memory_update.c` — simple local/global memory update visible in projection.
5. `extern_dynamic.c` — unresolved external input classified dynamic/residual.
6. `extern_independent.c` — computation independent of externs completed statically.

Fixture names may change during implementation, but the accepted fixture set must remain single-file/per-module and must not require whole-program merge equivalence.

## Comparison report

The final active-vs-frozen comparison report should include:

```json
{
  "schema_version": "real-sparrow-preanalysis/v1",
  "relation": "structural-equiv|structural-diverge",
  "claim": "module-only PreAnalysis boundary projection parity",
  "modules": [
    {
      "source": "<fixture-relative-path>",
      "weak_step_lineage": "structural-equiv|structural-diverge",
      "callgraph": "structural-equiv|structural-diverge",
      "pruning": "structural-equiv|structural-diverge",
      "memory_summary": "structural-equiv|structural-diverge",
      "bta": "accepted|rejected"
    }
  ],
  "non_claims": [
    "no Strong-mode staging",
    "no sparse/DUG parity",
    "no whole-program merge equivalence",
    "no executable residual linker/global-fixpoint evidence"
  ]
}
```

## Phase 1 continue decision

This schema is sufficient to begin Phase 2 source-lineage extraction. Implementation must preserve the schema contract or update this document before changing artifact shape.
