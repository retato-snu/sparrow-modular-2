# Verification notes

Fresh evidence collected during Ralph implementation:

- Active MetaOCaml build/test: `cd sparrow-modular-ocaml && opam exec --switch MetaOCaml-full -- dune build @runtest` passed.
- Baseline regression: `cd sparrow && opam exec --switch sparrow -- dune runtest --force` passed.
- Baseline no-edit audit: `git diff --exit-code -- sparrow` passed.
- No-flat-D source audit: `grep -R "FlatD\|SuspensionMarker\|TODO_D_MARKER" sparrow-modular-ocaml/src sparrow-modular-ocaml/test && exit 1 || true` passed.
- Stage 1 now reads `--module-a` / `--module-b`: module A source determines the supported loop/if residual shape, and module B source supplies the exported `n` value. A mismatched `--case` is rejected.
- Summary load validates residual `source` against `artifact_source_checksum` and the executable generator before reconstructing `Trx.code`; the round-trip test also verifies a deliberately corrupted source/checksum is rejected.
- The actual executed MetaOCaml quotes in `src/residual.ml` contain the residual loop iteration (`let rec iterate ... if next = header ...`) and dynamic branch selection (`if then_path ... else if ...`) instead of delegating the whole residual to flat helper calls.

One environment note: running the frozen baseline under the current `MetaOCaml-full` switch fails due a `goblint-cil` constructor-shape mismatch. The baseline passes under the existing `sparrow` switch, so no baseline source change was made.
