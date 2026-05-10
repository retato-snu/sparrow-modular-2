# sparrow-modular-ocaml

Active modular implementation for the MetaOCaml staged Sparrow slice.

Rules:
- `../sparrow/` is the frozen baseline/oracle. Do not edit its analyzer semantics from this tree.
- Changes here may implement staged modular analysis, summaries, residual execution, tests, and docs.
- Any intentional divergence from baseline Sparrow must be documented with the comparison relation and cause.
- Dynamic values in this slice must carry executable MetaOCaml `D code`; flat dynamic markers are forbidden.
