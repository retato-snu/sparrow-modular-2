# sparrow-modular-ocaml

Active implementation of the first MetaOCaml staged interval slice for Sparrow.

This directory is intentionally separate from `../sparrow/`, which remains the frozen baseline/oracle. The slice demonstrates the first milestone from `.omx/plans/plan-metaocaml-staged-sparrow-interval.md`:

For the current PE claim boundary, implemented-vs-missing status, residual-linking scope, and next milestone, see [`doc/sparrow-pe-status.md`](doc/sparrow-pe-status.md).

- Stage 1 emits per-module summaries.
- Dynamic cells carry executable MetaOCaml `D code` (`Trx.code`) rather than flat markers.
- Stage 2 links module summaries and executes `D code` chains directly.
- Summary round-trip serialization preserves residual artifact metadata and executable shape.

## Build

Use a BER MetaOCaml switch. This workspace already has `MetaOCaml-full` with `ocaml-variants.5.3.0+BER`.

```sh
opam exec --switch MetaOCaml-full -- dune build @runtest
```

## Baseline oracle

Run the frozen Sparrow baseline with its own switch:

```sh
cd ../sparrow
opam exec --switch sparrow -- dune runtest --force
```

The active slice must not modify `../sparrow/`:

```sh
git diff --exit-code -- sparrow
```

## Scope of the first slice

The slice is deliberately narrow. It exercises arithmetic interval cells for:

- a two-module extern-dependent loop;
- a dynamic-condition if statement.

The product representation still preserves the baseline component names (`itv`, `powloc`, `array_blk`, `struct_blk`, `powproc`). Components outside the first slice are explicit unsupported values, never silently dropped projections.

## Residual serialization boundary

The durable summary contains stable metadata, product-state cells, and generated residual source artifact references/checksums. On load, the source checksum is validated and the source must match the executable generator for its loop/if shape before runtime `Trx.code` is reconstructed. The durable format never degrades dynamic cells into flat markers such as `FlatD` or `SuspensionMarker`.
