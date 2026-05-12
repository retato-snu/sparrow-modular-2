# ADR-0006: Abstract Speculate MetaOCaml Sparse Proof Gate

Status: accepted for implementation evidence

## Decision

Abstract Speculate proof evidence is valid only when the implementation directly runs the module-local Sparrow sparse boundary at stage 1, splits module-local static rows from extern/link-dependent obligations, constructs typed MetaOCaml residual analyzer values, and executes those generated code values online with `Runcode.run`.

## Constraints

- Pre-link analysis uses `Real_sparrow_frontend.global_for_module` / `parse_one_file` only.
- Residual obligations are represented as `D row_code`-style `Trx.code` values and spliced into the generated analyzer; report JSON is only execution evidence.
- Dynamic loop/branch evidence is serialized from typed witnesses carrying residual obligations, not residual source text scans.
- Stage-1 residual growth is gated by static-projection blind equality so residual code structure cannot drive convergence.
- Generated OCaml source strings, metadata-only reports, old wrapper delegation, linked pre-link facts, generic `Top` substitution, and toy-only fixtures are rejected.
- The frozen `sparrow/src` baseline remains the semantic oracle.

## Consequences

- The old proof alias is compatibility-only and depends on `abstract_speculate_metaocaml_sparse_pe`.
- The proof gate records every spliced row/control obligation in `execution_log.executed_residuals` after online `Runcode.run` of the analyzer.
- The proof gate compares stage-2 final sparse tables against frozen Sparrow and audits module boundary, fact provenance, BTA, forbidden shortcuts, typed shape witnesses, and blind equality.
