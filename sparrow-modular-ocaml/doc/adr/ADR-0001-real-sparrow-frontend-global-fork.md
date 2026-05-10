# ADR-0001 — Real Sparrow Frontend/Global Source-Lineage Fork

Date: 2026-05-11
Status: accepted for first milestone implementation
Primary reference: `Doc/FOUNDATIONS.md`

## Decision

Implement the first active modular milestone as a source-lineage extraction of
real Sparrow frontend/global components:

`parseOneFile -> makeCFGinfo -> Global.init`

The active implementation lives under `sparrow-modular-ocaml/src/real_sparrow_*`.
The frozen `sparrow/src` analyzer semantics remain unchanged.

## Top drivers

1. Preserve C-1: the program-input `I` is frozen real Sparrow, not a toy analyzer.
2. Preserve O-4: routine implementation belongs in the active modular tree.
3. Keep the first milestone finite at frontend/global artifacts.

## Why chosen

This is the only route that starts from real Sparrow code, keeps baseline
semantics fixed, and gives later MetaOCaml PE work an owned active component
boundary.  Directly editing baseline Sparrow would destroy the fixed reference
for `I`; trace-only reimplementation would not justify a PE-of-Sparrow claim.

## Consequences

- The active fork must carry explicit source lineage and license headers.
- Parser/CFG artifacts are produced by Goblint-CIL through Sparrow's frontend
  transformation shape, not by source-shape matching.
- A non-semantic frozen Sparrow observer lives under `sparrow/test/` and links
  `sparrow_lib`; it emits `Global.init` boundary JSON for active-vs-frozen
  structural parity checks without editing analyzer semantics.
- Analysis/domain fields in `Global.t` are support-only markers in this first
  milestone.

## Rejected alternatives

- Modify `sparrow/src`: rejected by O-4 and the user's no-baseline-semantic-edits
  constraint.
- Continue finite/toy pipeline acceptance: rejected because it is not PE of real
  Sparrow.
- Use only `sparrow_lib` as the active implementation: rejected as the main path
  because it hides the component boundary needed for staged PE; it is used only
  by the frozen observer.

## Follow-up

Keep the frozen observer non-semantic and outside `sparrow/src`; rerun it when
frontend/global extraction changes before claiming strict C-2 frontend/global
baseline parity.
