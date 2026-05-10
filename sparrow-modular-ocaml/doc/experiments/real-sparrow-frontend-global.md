# Experiment Log — Real Sparrow Frontend/Global

Date: 2026-05-11

## Goal

Build a first active frontend/global milestone from real Sparrow source lineage
and run it module-by-module on C inputs.

## Dependency observation

The current switch is `MetaOCaml-full` with OCaml 5.3.0 and Goblint-CIL 2.0.7.
The frozen Sparrow build was verified with:

```sh
cd sparrow && dune build @all
```

The build succeeds on the MetaOCaml-full switch.  Earlier compatibility debt was
resolved by using the switch whose Goblint-CIL API matches frozen Sparrow and by
mechanically adapting the active source-lineage fork where needed.

## Implemented active evidence

- `Real_sparrow_frontend.parse_one_file` uses `Frontc.parse` and
  `RmUnused.removeUnused`, matching Sparrow's `parseOneFile` shape.
- `Real_sparrow_frontend.make_cfg_info` uses MakeCFG's
  `calls_end_basic_blocks`, `globally_unique_vids`, `prepareCFG`, and
  `computeCFGInfo`.
- `Real_sparrow_global.init` calls the extracted Sparrow `Global.init`: extracted `InterCfg.init`, extracted `IntraCfg.init`, `_G_` construction, empty callgraph, support-only bottoms, and unreachable-node pruning.
- The dedicated Dune alias `@real_sparrow_frontend_global` runs only this real
  frontend/global acceptance path.

## Frozen observer evidence

A non-semantic observer was added under `sparrow/test/real_frontend_global_observer.ml`.
It links frozen `sparrow_lib`, executes `Frontend.parse -> Frontend.makeCFGinfo ->
Global.init` on each single module input, and writes JSON comparable with active
artifacts.  The active-vs-frozen report is `_build/real-sparrow/frontend-global/frozen-parity.json`
and has relation `structural-equiv` when regenerated.

## Not implemented

Pre-analysis PE, DUG PE, interval main-analysis PE, executable MetaOCaml `D code`,
residual linking, and residual global fixpoint are not implemented by this
milestone.
