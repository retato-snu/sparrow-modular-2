---
name: MetaOCaml constraints 
description: Load-bearing MetaOCaml facts
audience: contributor / agent
status: active
last-reviewed: 2026-05-06
---

# MetaOCaml constraints 

This document records MetaOCaml-specific facts.

General MetaOCaml engineering reference (performance strategies,
benchmarking methodology, full API surface) is maintained in
`METAOCAML_REFERENCE.md` in this directory.

---

## 1. Constructor restriction

**Constraint:** User-defined type constructors and record labels
cannot appear inside MetaOCaml brackets (`.< >.`) unless the type is
defined in a **separately compiled module**.

This means `Val.t`, `Loc.t`, `Mem.t`, `AbstractValue.t`, and any
other types that appear in generated code must live in `.ml`/`.mli`
files that are compiled before the staging code that brackets them.
Defining a type and bracketing its constructors in the same compilation
unit causes a hard compile error.

**Diagnostic:** The error message is not self-explanatory. If you see
an error about constructors in brackets that you cannot resolve, this
restriction is almost certainly the cause.

---

## 2. `genlet` and `with_locus` — let-insertion for code generation

**Constraint:** MetaOCaml provides `genlet` (from the code library)
to insert `let`-bindings into generated code at a controlled scope.
`genlet e` inserts `let t = e in ...` at the nearest enclosing locus
and returns `t`. `with_locus` sets the upper bound for where
`genlet`-inserted bindings can land.

**Scope extrusion:** BER MetaOCaml detects scope extrusion (a variable
escaping its binding scope) at code generation time and raises an
exception. This is a safety feature, not a bug. If you see a scope
extrusion error, the generated code is attempting to use a variable
outside the scope where it was bound — typically from incorrect
`genlet` locus placement.

---

## 3. Online vs. offline code generation

**Constraint:** MetaOCaml supports two modes of using generated code:

- **Online:** `Runcode.run` compiles and executes generated code
  immediately within the current process.
- **Offline:** `print_code` / `format_code` / `print_closed_code`
  serializes generated code as OCaml source text for later compilation
  by a separate build.

**B-0 local note:** In the current dune layout, bytecode execution via
`Runcode.run_bytecode` works smoothly for **closed code** that does not
reference project-local modules. Generated code that persists
project-local identifiers by name (for example, constructors from a
support module such as `SmokeAst`) is accepted by the MetaOCaml
compiler, but running it online may require explicit compiler search-path
setup or a native runner. B-0 therefore uses `run_bytecode` for the
closed arithmetic smoke case and compile/print validation for the
imported-module constructor case.

---

## 4. Cross-stage persistence (CSP)

**Constraint:** Values from the current stage can appear inside
brackets via cross-stage persistence. There are two cases:

- **Imported identifiers** (module-level names from separately
  compiled modules, standard library functions): persist by name
  reference. No special handling needed.
- **Locally bound values** (local `let` bindings, function
  parameters): persist by value capture. For non-trivial types, this
  may require a `lift` function that serializes the value into a code
  representation.

---

## 5. Hygiene and lexical scope

**Constraint:** MetaOCaml maintains lexical scoping and hygiene across
stages. Variable names in generated code are alpha-renamed to prevent
capture. This is unlike Lisp-style quasiquotation where accidental
capture is possible.

---

## 6. Bracket/splice syntax

Current BER MetaOCaml syntax:

```ocaml
(* Bracket: create code *)
let code_expr = .<expr>.

(* Splice: insert code into a bracket *)
let combined = .<1 + .~code_expr>.

(* Run: compile and execute (online mode) *)
let result = Runcode.run combined

(* Print: serialize as source (offline mode) *)
let () = print_code Format.std_formatter combined
```

Extension-node alternative (equivalent):

```ocaml
let code_expr = [%metaocaml.bracket expr]
let combined = [%metaocaml.bracket 1 + [%metaocaml.escape code_expr]]
```

Both forms are accepted. The `.< >.` / `.~` notation is more common
in the literature.

---

## Principles touched

- **Decision 3** (BTA strategy): BTA classification must produce
  output compatible with MetaOCaml's bracket/splice model — static
  values are computed at staging time, dynamic values become spliced
  code fragments.
- **Decision 4** (loop strategy): `genlet`/`with_locus` (§2) is the
  implementation mechanism for hoisting static-projection widening
  out of residualized loops.
- **B-8** (serialization): online vs. offline (§3) and CSP/lifters
  (§4) directly constrain the serialization strategy.
