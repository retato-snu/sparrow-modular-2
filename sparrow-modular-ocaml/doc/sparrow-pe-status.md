# Sparrow PE implementation status

This note fixes the current research claim boundary for the active
`sparrow-modular-ocaml` implementation.  The baseline analyzer semantics live
in `../sparrow/` and remain the frozen oracle; the active implementation in this
directory is the staged/modular experiment.

## One-sentence claim

The current implementation is a MetaOCaml staged, module-local PE prototype for
a real-Sparrow ItvDom sparse-fixpoint slice, with executable `Trx.code`
residual components and witness-bounded residual-linking evidence.

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

Current evidence supports this only for selected ItvDom sparse-fixpoint
witnesses and selected linked observations, not arbitrary C programs or the full
Sparrow product analyzer.

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
| Stage-2 dynamic input validation | Implemented | `src/abstract_speculate_stage2_input.ml` validates source hash and extern roots. |
| BTA/fact provenance reports | Implemented | `abstract_speculate_metaocaml_sparse.*.json` reports. |
| Blind static-projection convergence guard | Implemented | Residual code growth is not used as the fixpoint convergence criterion. |
| First residual-linking prototype | Implemented for bounded witnesses | `src/abstract_speculate_residual_linker.ml`, `@abstract_speculate_residual_linking_pe`. |
| Residual-linking oracle suite | Implemented as prototype evidence | `@abstract_speculate_residual_linking_oracle_suite`. |

## Not implemented / not claimed

| Area | Missing capability | Why it matters |
| --- | --- | --- |
| Full Sparrow PE | Only selected real-Sparrow boundaries and the ItvDom sparse slice are staged. | Avoid claiming a complete PE of Sparrow. |
| Full product-domain staging | The accepted staged semantics are still Itv-focused; other product components are not generally residualized. | Product-domain fidelity is required before broad Sparrow claims. |
| Arbitrary-C semantic preservation | Fixtures and oracle-suite witnesses bound the evidence. | Current results are not a theorem for all C modules. |
| General residual summary language | Return/global/pointer observations are witness-bounded. | Broader linking needs a typed effect/summary algebra. |
| Cyclic residual linking | Mixed-role cycles are rejected. | Cycles require an explicit linked residual fixpoint semantics. |
| Whole-program residual global fixpoint | The residual linker does not rerun a global sparse fixpoint. | Needed for a stronger `link(PE(I,m),d)` equivalence claim. |
| Full dynamic control residualization | Loop/branch shape witnesses exist, but statement-level dynamic control coverage is limited. | Needed for larger program classes. |
| Mechanized proof | Verification is test/audit/oracle based, not proof-assistant mechanized. | Formal PL claims need a theorem statement and proof. |
| Stable public artifact schema | The oracle suite is prototype/non-public. | External users should not depend on the current JSON schema. |

## Residual-linking claim boundary

The residual-linking prototype composes independently produced module-local
residual analyzers.  It currently supports deterministic acyclic function
bindings, including multiple provider/import bindings and a mixed importer /
provider role chain.  It rejects ambiguous provider choices and cyclic mixed-role
topologies.

The oracle-suite relation is deliberately selected-observation-bounded:

```text
selected_obs(link(PE(I, m1), ..., PE(I, mn)))
  == selected_obs(premerge_oracle(m1 + ... + mn))
```

Covered positive witness categories:

- global write/read;
- pointer memory effect;
- multiple providers/imports;
- mixed-role scheduling and summary handoff.

Covered negative cases include mismatched returns/effects, missing global or
pointer observations, ambiguous provider acceptance, invalid mixed-role phase
ordering, forbidden premerge shortcut leakage, missing oracle artifacts, witness
identity mismatch, missing provenance, and mixed-role dependency cycles.

## Verification commands

Use the MetaOCaml BER switch for active verification:

```sh
cd sparrow-modular-ocaml
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_metaocaml_sparse_pe
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_pe
opam exec --switch MetaOCaml-full -- dune build @abstract_speculate_residual_linking_oracle_suite
opam exec --switch MetaOCaml-full -- dune build @runtest
```

Baseline cleanliness remains a separate root-level check:

```sh
git diff --exit-code -- sparrow
```

## Recommended next milestone

Next work should not broaden the public claim first.  It should make the current
prototype easier to evaluate:

1. stabilize and commit the residual-linking oracle-suite artifacts;
2. specify the selected-observation relation in the experiment document;
3. introduce a small typed summary/effect language for returns, globals, pointer
   writes, and provenance;
4. factor residual-linking scheduling into an explicit acyclic dependency graph;
5. keep cycles rejected until a linked residual fixpoint semantics is designed.

Only after that should the implementation broaden beyond the current ItvDom
sparse-fixpoint slice.
