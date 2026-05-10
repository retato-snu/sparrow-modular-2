---
name: MetaOCaml engineering reference
description: External MetaOCaml knowledge. Not a project principle — reference material for staged code engineering.
audience: contributor / agent
status: active
last-reviewed: 2026-05-06
---

# MetaOCaml engineering reference

> **This is external knowledge reference, not a project principle.**
> It captures MetaOCaml engineering practices, see
> `METAOCAML_CONSTRAINTS.md` in this directory.

Significant performance gains in MetaOCaml typically come not from "hoping the compiler does a good job," but from designing the generator to emit **superior code shapes** from the start. Particularly effective are **Partial Evaluation and Specialization**, **genlet-based sharing/let-insertion to eliminate redundancy and unnecessary computation**, and **lowering hot loops into allocation-free imperative loops with appropriate data layouts.**

## Key Conclusions and Application Priorities

| Priority | Technique | Most Effective Scenarios | Expected Effect | Primary Risk |
|---|---|---|---|---|
| Critical | Partial Eval / Const Prop / Specialization | When some arguments are static | Elimination of branch/recursion/interpretation overhead | Code explosion, specialization cost |
| Critical | genlet / with_locus based sharing | Multiple uses of the same expr or loop invariants | Redundancy elimination, LICM, evaluation order control | Scope issues if locus is misconfigured |
| High | Explicit Inlining | Modules/Functors/HOFs in hot paths | Removal of indirect calls, more backend optimization | Binary size increase, cache locality |
| High | Loop Transformations | Numeric kernels, parser/stream inner loops | Reduced branch/index/call overhead | Register pressure, I-cache pressure |
| High | Data Layout Optimization | Float/Int column-major, C/Fortran interop | Reduced boxing, better cache locality | Conversion cost, API penetration |
| High | Allocation Minimization / GC Impact | Allocation-sensitive loops, closure-heavy code | Reduced minor GC pressure | Code becomes more imperative/low-level |
| Medium | CPS / ANF Normalization | Complex generators with difficult sharing control | Simplified let-insertion and control-flow generation | Reduced readability, maintenance difficulty |
| Medium | Flambda / ocamlopt Fine-tuning | Release builds, cross-module boundaries | Residual inlining, specialization, closure unboxing | Version/flag dependency, compile time |
| Medium | JIT-style Runtime Specialization | Frequently reused shape/constant combinations | Improved steady-state performance | Gen/Compile/Link cost, requires cache policy |
| Low | -unsafe / FFI Attribute Optimization | Bounds check / C-calls are bottlenecks | Additional constant factor reduction | Reduced safety, debugging difficulty |

## Strategy Details by Technique

**Partial Evaluation and Constant Propagation:** Eliminate branches and recursion at generation time for static arguments. Requires clearly identifiable static inputs. Constraints: As specialization depth increases, code cache and build times grow.

**Explicit Inlining and Module Specialization:** Pass operations as `code` templates rather than values and splice them to eliminate indirect calls across functor boundaries. Combine with `[@@inline always]`, `[@inlined always]`, and `[@specialised always]` attributes. Indiscriminate inlining can degrade code size and locality.

**Let-insertion (`letl` vs `genlet`):** Two mechanisms for intermediate-value sharing. `Codelib.letl e (fun t -> body)` inserts a `let t = e in body` at the current position — use for immediate sharing of effectful or expensive expressions. `genlet e` floats the binding to the nearest enclosing `with_locus` boundary — use for LICM (loop-invariant code motion) and cross-scope sharing. The distinction matters: `letl` is local, `genlet` hoists.

**`val code` type — duplication-safe subtype:** BER MetaOCaml provides `'a val code` as a subtype of `'a code`. Only literals and simple variable references have `val code` type. If an API parameter accepts `'a val code`, the caller must let-bind complex expressions before passing them — the type system enforces this. Use `'a val code` for parameters that will be spliced multiple times; use `'a code` for single-use parameters.

**Binding-time improvement (eta-expansion):** A dynamic function `(int -> int) code` prevents inlining — the generator sees an opaque code value. Eta-expanding to `int code -> int code` makes the function static: the generator can inline its body. Pattern: replace `(a -> b) code` with `a code -> b code` wherever the function shape is known at generation time. This is the practical mechanism underlying BTA classification.

**Partially-static data:** Represent values as `S of 'a` (statically known) or `D of 'a code` (dynamic). Implement algebraic simplification in the generator: `S 0 + x = x`, `S 1 * x = x`, `S 0 * x = S 0`, `S a + S b = S (a+b)`. This eliminates trivial operations from generated code without runtime cost. Directly applicable to abstract domain staging: a lattice value with a known static component can be simplified at staging time. Used in FFTW-style codelet generation to eliminate twiddle-factor multiplications by 0, ±1, ±i.

**Tagless-final DSL pattern:** Abstract DSL operations as a module signature with `type 'a repr`. Each backend (evaluation, code generation, optimized generation) implements the signature. Key property: `lam f = .<fun x -> .~(f .<x>.)>.` — `f` is a generator-stage function, not a generated closure. The generated code contains no closures or ADT dispatch. Combining tagless-final with partially-static data gives an optimization backend that performs constant folding during generation.

**Stream fusion:** Represent streams as push functions: `type 'a stream = { fold : ('a code -> unit code) -> unit code }`. Combinators like `map`, `filter`, `fold` are static higher-order functions that compose at generation time. The result is a single fused loop with no intermediate arrays, closures, or iterator objects. The Strymonas library demonstrates this pattern producing hand-loop-level performance.

**Offshoring (C code generation):** For compute kernels requiring SIMD, OpenMP, or zero GC overhead, MetaOCaml can output C code via `OffshoringIR`. The generator uses full OCaml (recursion, pattern matching, modules); only the generated code is restricted to a C-compatible subset (`for`/`while` loops, float/int arrays, arithmetic). The generated C can then be compiled with `gcc -O2 -march=native` for auto-vectorization.

**Loop Transformations:** It is critical to target `for`/`while` loops with index variables and local scalars in the final output. Since Flambda recursion unrolling is limited, manually determine unroll factors through measurement. For selective unrolling (sparse matrix, Shonan Challenge pattern): unroll rows below a threshold, emit runtime loops for dense rows.

**CPS/ANF Normalization:** Exposing "all non-trivial computations as named lets" in hot-path generators makes sharing points easier to identify. Prefer wrapping external APIs in tagless-final or module interfaces.

**Data Layout:** Use `Float.Array.t` for unboxed raw doubles and `Bigarray` for C/Fortran compatibility + out-of-heap storage. Numeric kernels should prefer SoA (Structure of Arrays) or flat buffers. Due to BER MetaOCaml constructor restrictions, layout types should reside in separately compiled modules.

**GC Impact Minimization:** Avoid closure creation, intermediate list/tuple values, and polymorphic containers in hot loops. Favor preallocated mutable buffers and scalar locals. Use `-unbox-closures` but verify with benchmarks.

**Code Size Management:** Generation, compilation, and linking carry costs; these are usually recovered after ~1000+ reuses. Requires cache policies and break-even analysis.

## MetaOCaml Implementation Details

**Syntax:** Brackets `.< e >.` for code generation, splice `.~e` for code insertion. Extension node alternatives: `[%metaocaml.bracket ...]`, `[%metaocaml.escape ...]`.

**Execution:** `Runcode.run` (online, immediate JIT), `print_code` / `format_code` (offline, source text output).

**Safety:** Maintains hygiene and lexical scope. Scope extrusion is detected as an exception at generation time. Incorrect `genlet` locus configuration is a common cause of scope extrusion.

**Module Patterns:** Abstracting operations with `(t -> t -> t) code` signatures allows maintaining module boundaries while producing code free of indirect calls. Separate core types into distinct `mli/ml` files due to constructor restrictions.

**CSP (Cross-Stage Persistence):** Imported identifiers are kept as name references. Local static values require lifters or explicit serialization, especially critical in offline mode.

**Multi-stage Types:** Interaction between polymorphism/value restriction and CSP can be subtle. Using name references for stdlib symbols and lifters for local values is the most stable approach.

## Compiler Flags

- Release: `ocamlopt` + Flambda by default. Preserve `.cmx` for cross-module optimization.
- `-O2`/`-O3`: Enhanced inlining/specialization search. Verify decisions with `-inlining-report`.
- `-unbox-closures`, `[@@inline always]`, `[@inlined always]`, `[@specialised always]`, `[@unrolled n]`, `[@tailcall]`: Control backend optimization of generated code.
- `-S` (assembly), `-save-ir-after scheduling`, `-verbose`: Inspectability tools.
- Note: Flambda does not remove bounds checks. Use `-unsafe` only as a separate experimental item.

## Engineering Checklist

- Measure four versions: generic baseline, staged without Flambda, staged + Flambda O3, staged + extra options.
- Separate generation cost from steady-state execution cost: `T_total = T_generate + T_compile + N * T_run`.
- Microbenchmarking: `Core_bench`. Hardware counters: `perf stat`. Sampling: `perf record`.
- Allocation tracking: `memtrace` (link-only usage). Experiment with `Gc` / `OCAMLRUNPARAM` sensitivity.
- Manual code inspection: Use `print_code`, `ocamlopt -S`, and `-inlining-report`.
- CI Regression: `current-bench`, `Sandmark`. Commit generated code diffs alongside benchmark diffs.
- Testing: Cover both semantic boundaries (n=0/1, empty, tile remainder) and performance boundaries (warm/cold, small/large, AoS vs SoA).

## Diagnostic Checklist

| Problem Symptom | Cause | Technique |
|---|---|---|
| Same expression appears multiple times in generated code | Escape copies code | `letl` / let-insertion |
| if-else remains in generated code | Condition treated as dynamic | Lift condition to static parameter / eta-expansion |
| Loop counter comparison/increment visible | Loop not unrolled | Loop unrolling (static bound) |
| `0 * x`, `1 * x`, `0 + x` remain in code | No algebraic simplification | Partially-static data |
| Indirect function calls in generated code | Function represented as dynamic | Eta-expansion / tagless-final |
| Intermediate arrays between map/filter | Stream materialized | Stream fusion (push-stream) |
| OCaml GC pauses, boxing overhead | Pure OCaml runtime | Offshoring to C |
| Combinatorial code explosion from aspect combinations | Manual code duplication | Monad/Functor parameterization + tagless-final |
| Generated code references wrong variable | Scope extrusion | Check `genlet` locus; BER MetaOCaml detects at generation time |

**Recommended workflow:** (1) Write correct unstaged version → (2) Decide static/dynamic split → (3) Add staging annotations → (4) Inspect generated code (scope extrusion, unnecessary copies) → (5) Add let-insertion for sharing → (6) Domain optimizations (partially-static, fusion) → (7) Offshore if needed.

## Code Examples

### Partial Evaluation

```ocaml
let rec spower n x =
  if n = 0 then .<1>.
  else if n land 1 = 0 then .<square .~(spower (n lsr 1) x)>.
  else .<.~x * .~(spower (n - 1) x)>.

let power7_code = .<fun x -> .~(spower 7 .<x>.)>.
(* Generated: fun x -> x * (x * (x * (x * (x * 1)))) *)
```

### letl vs genlet

```ocaml
(* letl: immediate let-insertion at current position *)
let safe_sqr (e : int code) : int code =
  Codelib.letl e (fun t -> .<t * t>.)
(* safe_sqr .<f ()>.  →  let t = f () in t * t *)

(* genlet: floating let-insertion to nearest with_locus *)
let sum_up =
  let body arr =
    with_locus @@ fun locus ->
    let arr = genlet arr in
    let sum = genlet ~name:"sum" ~locus .<ref 0>. in
    .<for i = 0 to Array.length .~arr - 1 do
         .~sum := ! .~sum + (.~arr).(i)
       done;
       ! .~sum>.
  in
  .<fun x -> .~(body .<Array.map succ x>.)>.
```

### val code — duplication safety

```ocaml
(* 'a val code ⊆ 'a code — only literals and variables *)
let sqr_safe (x : int val code) : int code =
  .<.~x * .~x>.     (* safe: x is duplicable *)

(* sqr_safe .<f ()>.   ← COMPILE ERROR: f() is not val code *)

(* Force let-binding before passing: *)
let use_sqr (e : int code) : int code =
  .<let v = .~e in .~(sqr_safe .<v>.)>.
```

### Eta-expansion — binding-time improvement

```ocaml
(* BAD: (int -> int) code — function is dynamic, no inlining *)
let apply (f : (int -> int) code) (x : int code) : int code =
  .<.~f .~x>.    (* generated: f x  ← indirect call *)

(* GOOD: int code -> int code — function is static, inlined *)
let eta (f : int code -> int code) : (int -> int) code =
  .<fun x -> .~(f .<x>.)>.    (* generator inlines f *)

(* Static branching via CPS: *)
type activation = ReLU | Sigmoid | Linear

let gen_activation (act : activation) (x : float code) : float code =
  match act with
  | ReLU    -> .<let v = .~x in if v > 0.0 then v else 0.0>.
  | Sigmoid -> .<1.0 /. (1.0 +. exp (-. .~x))>.
  | Linear  -> x   (* branch itself disappears *)
```

### Partially-static data

```ocaml
type 'a ps =
  | S of 'a          (* Static: known at generation time *)
  | D of 'a code     (* Dynamic: runtime code *)

let lift (x : 'a ps) : 'a code = match x with
  | S v -> Codelib.genconst v
  | D c -> c

let ps_add (a : int ps) (b : int ps) : int ps =
  match a, b with
  | S 0, x  | x, S 0  -> x              (* 0+x = x *)
  | S a, S b           -> S (a + b)     (* constant fold *)
  | S a, D b           -> D .<a + .~b>.
  | D a, S b           -> D .<.~a + b>.
  | D a, D b           -> D .<.~a + .~b>.

let ps_mul (a : int ps) (b : int ps) : int ps =
  match a, b with
  | S 0, _ | _, S 0  -> S 0             (* 0*x = 0 *)
  | S 1, x | x, S 1  -> x              (* 1*x = x *)
  | S a, S b          -> S (a * b)
  | S a, D b          -> D .<a * .~b>.
  | D a, S b          -> D .<.~a * b>.
  | D a, D b          -> D .<.~a * .~b>.
```

### Tagless-final DSL

```ocaml
module type Arith = sig
  type 'a repr
  val int  : int -> int repr
  val add  : int repr -> int repr -> int repr
  val mul  : int repr -> int repr -> int repr
  val lam  : ('a repr -> 'b repr) -> ('a -> 'b) repr
  val app  : ('a -> 'b) repr -> 'a repr -> 'b repr
end

(* Backend 1: direct evaluation *)
module Eval : Arith with type 'a repr = 'a = struct
  type 'a repr = 'a
  let int n = n
  let add a b = a + b
  let mul a b = a * b
  let lam f = f
  let app f x = f x
end

(* Backend 2: MetaOCaml code generation *)
module MetaGen : Arith with type 'a repr = 'a code = struct
  type 'a repr = 'a code
  let int n = .<n>.
  let add a b = .<.~a + .~b>.
  let mul a b = .<.~a * .~b>.
  let lam f = .<fun x -> .~(f .<x>.)>.
  let app f x = .<.~f .~x>.
end

(* Same program, both backends: *)
let prog (module A : Arith) =
  A.(lam (fun x -> add (mul x x) (int 1)))
```

### Module Specialization

```ocaml
module type ARITHC = sig
  val add  : (int -> int -> int) code
  val mul2 : (int -> int) code
end

module Inline_kernel (A : ARITHC) = struct
  let axpy2 =
    .<fun a x y ->
        let t = (.~A.mul2) x in
        (.~A.add) ((.~A.mul2) a) ((.~A.add) t y)>.
end
```

### Loop Unrolling Combinator

```ocaml
let unroll (n : int) (body : int -> unit code) : unit code =
  List.fold_right
    (fun i acc -> .<.~(body i); .~acc>.)
    (List.init n Fun.id)
    .<()>.

(* 4-element vector add: *)
let gen_vec4_add (a : float array code) (b : float array code)
    (c : float array code) : unit code =
  unroll 4 (fun i ->
    .<(.~c).(i) <- (.~a).(i) +. (.~b).(i)>.)
(* Generated: c.(0)<-a.(0)+.b.(0); c.(1)<-...; c.(2)<-...; c.(3)<-... *)
```

### Stream Fusion (Push-stream)

```ocaml
type 'a stream = {
  fold : 'w. ('a code -> 'w code) -> 'w code
}

let of_arr (arr : 'a array code) (n : int) : 'a stream =
  { fold = fun k -> .<for i = 0 to n-1 do .~(k .<(.~arr).(i)>.) done>. }

let map (f : 'a code -> 'b code) (s : 'a stream) : 'b stream =
  { fold = fun k -> s.fold (fun x -> k (f x)) }

let filter (pred : 'a code -> bool code) (s : 'a stream) : 'a stream =
  { fold = fun k -> s.fold (fun x -> .<if .~(pred x) then .~(k x)>.) }

let sum (s : int stream) : int code =
  .<let acc = ref 0 in
    .~(s.fold (fun x -> .<acc := !acc + .~x>.));
    !acc>.

(* Pipeline: map → filter → sum → single fused loop *)
```

### Data Layout (SoA)

```ocaml
(* layout.ml — separately compiled *)
type point_soa = { xs : Float.Array.t; ys : Float.Array.t }

(* kernel.ml *)
let gen_norm2_soa n =
  .<fun (p : Layout.point_soa) ->
      let acc = ref 0.0 in
      for i = 0 to n - 1 do
        let x = Float.Array.get p.xs i in
        let y = Float.Array.get p.ys i in
        acc := !acc +. (x *. x +. y *. y)
      done;
      !acc>.
```

### Offshoring (C Code Generation)

```ocaml
open Offshoring

let gen_saxpy (n : int) : (float -> float array -> float array -> unit) code =
  .<fun alpha x y ->
    for i = 0 to n-1 do
      y.(i) <- alpha *. x.(i) +. y.(i)
    done>.

let () = Offshoring.print_c "saxpy" (gen_saxpy 256)
(* Output:
   void saxpy(double alpha, double *x, double *y) {
     for (int i = 0; i <= 255; i++) {
       y[i] = alpha * x[i] + y[i];
     }
   }
*)
```

## Primary References

1. MetaOCaml Official Overview + Tutorials (okmij.org)
2. MetaML/MetaOCaml bibliography (github.com/metaocaml)
3. MetaOCaml: ten years later — system description (N153)
4. The Design and Implementation of BER MetaOCaml
5. let (rec) insertion without Effects — `genlet`/`mkgenlet` semantics
6. Stream Fusion, to Completeness + Highest-performance Stream Processing (strymonas)
7. flap: Deterministic Parser with Fused Lexing
8. Multi-stage programming with functors and monads
9. OCaml Official Docs: ocamlopt, Flambda, GC, Bigarray, Float.Array
10. Tooling: memtrace, Core_bench, current-bench, Sandmark
