# Generalized Foundations — Staged Abstract Interpreter via Partial Evaluation

This document extracts and generalizes **C-1 through C-3** (conceptual) and
**O-1 through O-4** (operational) from a project-specific foundations document.
All project names, file paths, and implementation identifiers have been replaced
with abstract placeholders so the principles apply to any staged abstract
interpreter built via partial evaluation.


---



## Conceptual foundations

### C-1 · PE model

**Rule:** A partial evaluator operates on two inputs — a *program* `I` and a
*static input* — producing a residual program specialized on the static input;
the residual, when executed with the *dynamic input*, behaves the same as the
original program applied to (static ⊕ dynamic).

Three roles must be distinguished:

- **Program-input `I`** — the semantics of the analyzer being staged. A
  canonical, frozen implementation of `I` serves as the baseline definition.
- **Static input** — the module or component being analyzed at stage-1 time;
  the information that is known locally.
- **Dynamic input** — facts from external modules or the environment that are
  unknown at stage-1 time and are deferred to stage-2 (link time).

The staged implementation is a mechanization of `λm. PE(I, m)`:

```
run(StagedAnalyzer, m) ≃ PE(I, m)
```

`StagedAnalyzer` is *not* the program-input; it is the mechanization that,
given a module, produces the residual for that module.

**Why:** Conflating `I` with its implementation blurs the research claim. The
contribution is "we have a staged implementation of `λm. PE(I, m)`" — not "our
analyzer is partially evaluated." Treating the staged evaluator as `I` loses the
connection to the baseline that defines what `I` actually is.

**Guard:** Do not apply this model to tools that do not implement
`λm. PE(I, m)`. Frontend/parser harnesses that produce the static-input
representation are not part of the PE machinery itself.

**Example:**
- `I` = the analyzer semantics, as defined by the canonical baseline implementation.
- Static input: a module-local representation for `foo.c`.
- Dynamic input: abstract state of `extern` symbols declared in `foo.c` but
  defined in `bar.c`.
- Running `StagedAnalyzer` on `foo.c` produces a module summary containing
  resolved values for everything `foo` settles locally, plus explicit unresolved
  dependencies and restart obligations for the rest. The linker resumes this
  residual with dynamic input.

**Compliance check:** Any doc section or code comment describing "partial
evaluation" in this project must either (a) name program-input, static input,
and dynamic input in the three-role framing, or (b) cite C-1 explicitly.

---

### C-2 · Soundness criterion

**Rule:** For any module `m` and dynamic input `d`, the stage-2 linked result of
the staged stage-1 residual must agree with the baseline analyzer applied to the
combined input. The strict parity goal is:

```
link(PE(I, m), d) ≃ I(m ⊕ d)
```

When the implementation cannot yet deliver strict parity, a weaker
over-approximation is acceptable:

```
link(PE(I, m), d) ⊒ I(m ⊕ d)
```

where `⊒` is the soundness preorder on abstract states: `a ⊒ b` means `a`
over-approximates `b` in the abstract-domain order. `⊒` is **a fallback
justified by current implementation limits, not the soundness target**.

**Why:** Distinguishes meaningful modular analysis from silent
over-approximation creep. If "sound" collapses to "any over-approximation is
acceptable," the staged analyzer drifts toward trivial summaries (everything
`⊤`) while still claiming correctness. Stating `≃` as the goal and `⊒` as a
bounded fallback keeps the research claim honest.

**Guard:** `⊒` replaces `≃` only in cases forced by current implementation
limits (features not yet modeled, external information that cannot be restarted).
These gaps should shrink over time, not be canonized. Any specific claim of
`⊒`-only behavior must cite a specific implementation limit or open issue.

**Example:** Two-module scenario with `foo` (loop bounded by `extern n`) and
`bar` (defines `n = 3`).
- Stage 1 on `foo` produces a module summary with a suspended loop-fixpoint
  obligation and a residual reference to `n`.
- Stage 1 on `bar` produces a summary exporting `n = [3, 3]`.
- Stage 2 resolves `n` in the `foo` residual and restarts the abstract fixpoint,
  refining the loop exit.
- Target (`≃`): the linked result matches what the baseline produces when given
  both modules together.

**Compliance check:** Any claim of "sound under missing information" must
(a) state whether `≃` or `⊒` is being claimed, and (b) either name a runnable
comparison or cite a specific implementation limit justifying `⊒`.

### Current Option A residual-equation semantics

The active Abstract Speculate implementation follows Option A for the supported
Itv staged-cell slice: stage 1 emits module-local residual equations `E_m` whose
transfer bodies are executable MetaOCaml `D code`; stage 2 validates dynamic
extern/link input `d`, initializes residual state from static rows plus dynamic
seeds, runs a deterministic bounded worklist solver, and materializes final
input/output rows only after the worklist drains.

Reports must distinguish this primary path from compatibility-only component
overlay evidence.  Solver-backed evidence uses `residual_solver_run=true`,
`solver_backed_residual_fixpoint=true`, `worklist_drained=true`, and
`overlay_only=false`; overlay-only evidence cannot support the Option A claim.
For the current fixtures the theorem-ready shape is:

```text
solve(E_m, d) ⊒ I(m⊕d)
```

The relation is intentionally scoped to the emitted equation language and
witness universe.  It is not a full arbitrary-C or full product-domain Sparrow
PE theorem, and cyclic linked residual solving remains future work.

*Note on soundness decomposition.* The end-to-end claim decomposes into three
independently verifiable layers:

| Layer | Claim | Verification |
|---|---|---|
| Stage-1 local | `PE(I,m)` is sound w.r.t. `I(m⊕d)` for any `d` | `Approx` values `⊒` actual; `Known` = exact |
| Stage-2 merge | composition preserves per-module invariants | node identity, monotone memory merge, key-stable residual merge |
| Stage-2 restart | restart recovers precision up to `⊒` | footprint reset to `⊥` + re-fixpoint convergence |

---

### C-3 · Abstract-interpreter structure of the program-input

**Rule:** The legitimacy of convergence-based stopping rests on a specific
structural fact about the program-input `I`: `I` is an abstract interpreter — a
fixpoint computation over a monotone lattice. This is not incidental background;
it is the feature that turns "unroll/inline the program until something
stabilizes" from a heuristic into a grounded PE methodology.

A generic partial evaluator given an arbitrary program cannot in general decide
when to stop unrolling — the residual can grow without bound. But because `I` is
an abstract interpreter, its semantics is structured as:

```
I(p) = lfp(F_p)     where F_p : L → L is monotone over lattice L
```

with widening/narrowing as convergence aids. `F_p` is a system of per-node
transfer functions over the program's control-flow graph (CFG), solved by
worklist / SCC propagation with widening at loop headers.

When PE specializes `I` on a static input `m` (leaving dynamic input symbolic):
- The locally visible abstract state evolves by monotone iteration over `L`;
  widening forces ascending chains to reach a post-fixpoint in finite time.
- Post-fixpoint reachability in `L` is a well-defined, checkable termination
  signal, independent of residual code's syntactic growth.
- The PE's stopping rule can be "stop when static `L`-values have stabilized,"
  ignoring both the shape of the residual AST and the dynamic values that remain
  symbolic.

**Operational consequence:** PE termination is argued by `L`-stability, not by
residual-syntax comparison. The detailed stopping algorithm and equality rule
are specified in O-2 and O-3.

**Why:** Without this principle, the project's methodology looks like a
collection of ad-hoc heuristics. Naming the abstract-interpreter structure of
`I` explicitly makes those choices consequences of a single fact. It also
isolates the conditions under which the methodology transfers: the approach
works for program-inputs that are abstract interpreters over lattices with
computable widening; it does not automatically generalize to arbitrary analyzers.

**Guard:** Do not apply this principle to claim termination for computations on
`I` that are not part of its fixpoint semantics. Book-keeping side effects,
allocator-ID generation, and residual-syntax construction are not lattice-valued
and do not participate in the `L`-convergence argument.

*Note on generic staging frameworks.* The abstract-interpreter structure of `I`
is the reason generic staging frameworks cannot straightforwardly stage `I`. In
a regular interpreter, program structure is static and values are dynamic —
`Rep[T]` handles this via types. In an abstract interpreter, fixpoint
convergence is **data-dependent**: if the abstract state is staged
(`Rep[AbstractState]`), then the convergence test `state_equals(s, s')` produces
`Rep[Boolean]` — a value whose truth is unknown at staging time. The framework
cannot decide whether the loop terminates, so it residualizes the entire fixpoint
loop. The PE of `I` is therefore hand-written by construction.

**Example:**
- `I` = abstract interpreter. Abstract domain `L` = product of interval, points-to,
  array-block, and function-target components.
- Widening: interval domain collapses unbounded growth to `−∞`/`+∞` endpoints
  in finite steps.
- Static input `m` = one module including `while (i < 3) { i++; }`.
- PE specialization: per-node transfer functions iterate until the loop-header
  entry store stabilizes; termination is signaled by `L`-map stability at each
  CFG node, not by syntactic similarity of any emitted residual nodes.

**Compliance check:** Architecture docs and normative comments must either cite
C-3 or explicitly name the `L`-valued state whose stabilization is being used.
Audit = grep for `terminat|converge|fixpoint|widening` across `Doc/` and source;
each hit in a normative context must either cite C-3 or name an `L`-valued
quantity and its stabilization.

---

## Operational principles

### O-1 · Maximal partial execution

**Rule:** At stage 1, the staged analyzer must execute every operation that is
determined by static input alone, to the strongest sound summary the local module
can produce. Operations involving dynamic input are residualized at the boundary,
but surrounding static work is not abandoned as a consequence.

O-1 has two subclauses:

- **(a) Static execution must not be trivialized by dynamic boundaries.** A
  dynamic value at one point does not license abandoning local computation at
  unrelated points downstream.
- **(b) Product-domain fidelity.** A module's analysis carries the full
  product-domain state into the exported summary; projecting a value to a single
  abstract-domain channel happens only at a specific consumer's request.

**Why:** The research claim (C-1) is that one module's analysis is PE of `I` on
that module. If the staged analyzer residualizes as soon as it encounters any
unknown and stops, Phase 1 computes nothing locally and the linker does all the
work — that is not partial evaluation; it is deferring `I` entirely to link time.
O-1(a) rules out this trivialization. The dual failure is silently weakening
local precision because single-channel residual output is easier to emit. O-1(b)
rules that out: lossless product representation is the default, and
single-channel projections are consumer-driven views.

**Guard:** O-1 applies to operations reachable by stage-1's static execution
trace. Code unreachable under the local module's static input (branches proved
dead) is not required to be executed. For loops, O-1 requires static execution to
continue; **O-2 decides convergence** and is the sole source of loop stopping
rules.

**Example:**
- (a) `x = [1,1] + [2,2]` fuses to `x = [3,3]` at stage 1; not preserved as
  a residual binary-operation node.
- (a) `y = extern_call(extern_x)` becomes a residual unknown-call node; the next
  statement `z = 1 + 2` still resolves locally to `z = [3,3]`, because the
  dynamic boundary on `y` does not contaminate unrelated static work.
- (b) A local variable carrying both interval and points-to information is
  represented in the exported residual with both components — not narrowed to
  either channel alone.

**Compliance check:** For each residualization path added (new residual-node
emission site), verify that (a) surrounding static statements still fold and
(b) the exported cell carries the full product value. Test coverage must exercise
both static-folding (a) and product-preservation (b) scenarios.

---

### O-2 · Convergence-based stopping

**Rule:** Stage 1 unrolls the abstract interpreter's fixpoint iteration
`⊔ᵢ F̂ⁱ(⊥)` (per C-3) step by step. Each abstract memory location carries a
**partially-static** classification: either `S v` (value determined by static
input, computable at stage 1) or `D code` (value depends on dynamic input;
recorded as executable residual code for stage-2 resolution). Operations
propagate this classification automatically — `S op S → S`, any `D` operand
produces `D`. Stage 1 halts when the **static projection** of the `L`-valued
state has converged (O-3, blind equality). When dynamic input becomes available
at stage 2, residual code is executed to resolve `D` values.

#### Partially-static value classification

The abstract memory is a map from locations to `AbstractValue ps`, where:

```
type 'a ps =
  | S of 'a        (* Static: computed exactly at stage 1 *)
  | D of 'a code   (* Dynamic: residual code executed at stage 2 *)
```

Operations between partially-static values propagate the classification:

```
ps_op(S a, S b) = S (op a b)          (* constant-fold *)
ps_op(S a, D b) = D .<op a .~b>.      (* one operand dynamic → residual *)
ps_op(D a, D b) = D .<op .~a .~b>.    (* both dynamic → residual *)
```

The abstract domain's `L`-value for a `D` location is its sound
over-approximation (e.g., the widened interval); this is what O-3's convergence
check compares, never the residual code structure itself.

#### Dynamic control flow: loops

When the loop condition is `D cond_code` (dynamic):

- The PE cannot determine whether to enter the loop body.
- One step of abstract transfer is applied: for each location the body may
  modify, the result is the join of the pre-loop value and the abstract effect
  of one body execution. Locations whose abstract effect depends on dynamic
  values transition from `S pre` to `D loop_residual`.
- Blind equality (O-3) on `L`-values detects stabilization after this
  transition: once a location is `D`, its abstract over-approximation does not
  change further.
- Stage 1 declares convergence and continues downstream analysis with the
  partially-static post-loop state.

**Critical invariant — loop structure must be preserved in the residual.** The
`D code` for a dynamically-suspended loop must be a **loop-shaped residual** —
an executable representation of the entire loop computation — not merely a flat
marker saying "this value depends on a dynamic input." A flat marker loses the
information that re-execution requires a fixpoint, not a single substitution.

```
(* Wrong: flat marker — structure lost *)
x = D(some_value_depending_on_n)

(* Correct: loop structure preserved — executable at link time *)
x = D(.<while (n > 0) { abstract_transfer_of_body } >.)
```

Partially-static propagation automatically preserves downstream dependencies:
if `x` is `D loop_residual`, then `y = x + 1` becomes
`D .<.~loop_residual_code + 1>.`, carrying the loop dependency forward without
manual tracking.

Residual output for each source-code loop falls into one of two categories:

- **Static completion** — all values in the loop are `S`; no residual loop
  emitted; final state only.
- **Dynamic suspension** — one or more locations are `D`; the suspension entry
  records the loop-shaped residual code, the set of `D` locations, and the
  post-loop abstract state.

#### Dynamic control flow: if statements

The same principle applies to conditional branches with dynamic conditions.
When the condition is `D cond_code`:

- **Current (unsound-for-PE) approach:** join both branch abstract states →
  branch-dependent values collapse to their join (e.g., `[0,0] ⊔ [1,1] = [0,1]`),
  losing the precise branch-dependent structure.
- **PE-correct approach:** residualize the if expression:

  ```
  (* condition is D → preserve if structure *)
  x = D .<if .~cond_code then then_val else else_val>.
  ```

  Values identical in both branches remain `S`; only branch-differing values
  become `D`. This is strictly more precise than joining.

*Note on code explosion.* Each if statement with a dynamic condition may
introduce a new `D` expression per affected location. With many such
conditionals, the number of distinct residual expressions grows. Common
Subexpression Elimination (CSE) on residual code is important to control this
growth; the loop case is less affected because the loop structure itself is one
`D` unit.

#### Link-time resolution strategies

When dynamic input arrives at stage 2, there are two approaches to resolving
`D` values. Their trade-offs are load-bearing for the overall architecture.

**Strategy A — Worklist fixpoint restart (simpler to implement):**
```
n arrives as abstract [3,3]
→ worklist resumes from the loop's suspend entry
→ same abstract fixpoint machinery (with widening) runs with n=[3,3]
→ downstream nodes updated through worklist propagation
```
Reuses the existing abstract fixpoint infrastructure. The abstract result is
identical to running the original analyzer on the combined input: both apply
widening and narrowing, producing the same abstract value.

**Strategy B — Residual code execution (different structure, not necessarily
more precise):**
```
n arrives as abstract [3,3]
→ dependency resolved: n_code ← [3,3]
→ residual loop code executed: runs the embedded abstract transfer function
   with n=[3,3] — the same widening behavior as Strategy A applies
→ downstream D values resolved by residual code chain
→ worklist finds values already stable
```
The residual code execution precedes the worklist and allows downstream `D`
chains to resolve without full worklist re-propagation. However, if the
residual code embeds the same abstract fixpoint machinery as Strategy A
(with widening), the final abstract result is the same.

*The distinction between the two convergences:* Stage-1 blind equality declares
convergence of abstract `L`-values — this is the PE stopping condition, not a
claim that the residual computation has converged. The residual computation
converges at stage 2, either through worklist fixpoint (Strategy A) or through
direct residual code execution (Strategy B).

#### Precision analysis: do the strategies differ?

#### Precision analysis: do the strategies differ?

**Both strategies produce the same abstract result.** At link time, the dynamic
input `d` arrives as an abstract value (e.g., n=[3,3]). Both strategies execute
the same abstract semantics of the loop with that abstract n. The abstract Kleene
chain for a loop whose bound is concrete (n=[3,3]) is **mathematically finite**
(bounded by the value's upper limit), so widening is irrelevant — the chain
converges in finite steps regardless of which strategy executes it.

```
n=[3,3] arrives → abstract Kleene chain:
  i₀ = [0,0]
  i₁ = [0,0] ⊔ [1,1] = [0,1]
  i₂ = [0,1] ⊔ [1,2] = [0,2]
  i₃ = [0,2] ⊔ [1,3] = [0,3]
  i₄ = [0,3] ⊔ [1,3] = [0,3]  ← stable, no widening needed
post-loop: i ≥ 3 AND i ∈ [0,3] → i = [3,3]
```

Both Strategy A (worklist) and Strategy B (residual code execution) compute this
same chain. The difference is structural, not semantic:

| | Strategy A | Strategy B |
|---|---|---|
| Abstract result | `i=[3,3]` | `i=[3,3]` |
| How | worklist re-traverses full CFG | D code chain executed directly |
| PE faithfulness | automatic | automatic |
| Advantage | reuses existing infrastructure | resolves only affected D chains |

**PE faithfulness holds automatically for both strategies** — both execute the
abstract semantics of `I` with the same dynamic input. No definition of `I` needs
to be revised, and no deviation needs to be documented.

**Implementation difficulty of Strategy B.** Strategy A reuses existing
infrastructure; Strategy B requires:

1. **Residual code representation** — a `'a code` type that is executable at
   stage 2, not just a metadata record. Options: a staged language (e.g.,
   MetaOCaml `.<>.` brackets), a manual AST with an interpreter, or closures.
2. **PS-aware transfer functions** — every abstract domain operation must be
   lifted to the `ps` type, propagating `S`/`D` classification through all
   operands. This is a pervasive change to existing transfer function code.
3. **Residual code carries abstract semantics** — the loop residual encodes the
   abstract transfer function applied iteratively, not the source loop. Abstract
   domain objects must be embeddable in residual code (cross-stage persistence).
4. **Link-time residual executor** — a new engine that, given dynamic input,
   substitutes values into `D code`, executes residual loops to convergence,
   and propagates resolved values through downstream `D` chains.
5. **Scope and variable hygiene** — residual code captures values from stage-1
   context; variable binding must be managed to prevent stale captures or scope
   extrusion.

Strategy A is the simpler first-pass choice: it reuses existing worklist
infrastructure with no new machinery. Strategy B reduces link-time overhead by
executing only the affected `D` chains rather than re-traversing the full CFG,
at the cost of building the residual executor. The choice is one of engineering
cost vs. link-time efficiency, not of abstract precision.



**Why:** C-3 establishes that `L`-stability is a valid termination signal
abstractly. O-2 turns it into an operational rule by pinning down (a) *whose*
convergence counts — the abstract `L`-values at stage 1, not the residual
computation at stage 2 — and (b) how the post-fixpoint shape of a source-code
loop and conditional determines residual emission. The partially-static
framing makes precise what "residualization" means: not a marker, but an
executable code object whose structure must match the control-flow construct
being residualized (loop → loop-shaped residual; if → if-shaped residual).

**Guard:**
- O-2 covers CFG-loop and conditional stopping and residual-emission policy
  only. The equality test used to detect `L`-stability is O-3 (blind equality).
- Termination for non-`L` bookkeeping uses structural termination, not
  `L`-stability (see C-3 Guard).
- The choice between Strategy A and Strategy B is an implementation decision;
  both are consistent with O-2. The principle specifies the residual contract
  (loop-shaped, if-shaped), not the link-time execution mechanism.

**Example:**
- Static completion: `int i = 0; while (i < 3) { i++; }` — condition `i < 3`
  is `S`; all values are `S`; fixpoint converges to `i = S [3,3]`; no residual
  emitted.
- Dynamic suspension (loop): `extern int n; int i = 0; while (i < n) { i++; }` —
  condition `i < n` is `D`; `i` transitions to `D (loop_residual)`; abstract
  over-approximation `[0, +∞]` is used for downstream `L`-value convergence.
  Residual = loop-shaped code `.<while (n > 0) { i++ }>`.
  - Strategy A at link time (n=3): worklist iterates abstract fixpoint → `i=[3,3]`.
  - Strategy B at link time (n=3): execute `while (3>0){i++}` directly → `i=3`.
- Dynamic suspension (if): `if (n > 0) { x = 1; } else { x = 0; }` — condition
  is `D`; PE-correct residual = `D .<if (.~n_code > 0) then 1 else 0>.`;
  at link time, execute residual → `x = 1` (for n=3). Current implementations
  that join both branches instead produce `x = [0,1]` — sound (`⊒`) but not
  PE-faithful.

**Compliance check:** Verify the "no stage-1 lowered while-loop" invariant and
that dynamic-suspension residuals are loop-shaped (not flat markers). For each
residualization site added, verify that: (a) the residual code structure matches
the control-flow construct (loop residual for loops, if residual for branches),
and (b) downstream `D` chains correctly carry dependencies through PS propagation.
Grep for any direct lowered-while-loop construction in stage-1 code; count must
stay at zero.

---

### O-3 · Blind equality

**Rule:** The equality test that drives stage 1's convergence/stability check
(per O-2) must be evaluated on the **abstract-domain values alone**, blind to the
structure of any residual code subtrees held inside those values. Two `L`-states
are equal iff their product-domain components compare equal under the domain's own
equality, regardless of how any embedded residual-code fragments are shaped.

Equivalently, stage-1 equality is equality after projecting the state to its
`L`-valued abstract-domain content. Residual node structure is not part of that
projection. Conversely, if the projected abstract-domain values differ, the
states are unequal even when the residual trees are structurally identical.

**Why — primary: stopping infinite residual code growth.** At each step of the
abstract fixpoint iteration, the PE generates residual code. For a dynamic loop,
each iteration produces more residual code:

```
Iteration 1: residual_code₁
Iteration 2: residual_code₁ ++ residual_code₂   (code grows)
Iteration 3: residual_code₁ ++ residual_code₂ ++ residual_code₃  (grows more)
...
```

If the convergence test compares residual code structure, the fixpoint can never
declare stable — the code is always changing — and stage 1 never terminates.
This is the **code explosion** problem in online partial evaluation. Blind
equality is the mechanism that stops code generation: when the abstract `L`-values
stabilize (widening has done its job), the equality test returns true and code
generation is frozen, regardless of how the residual code has been growing.

**Why — secondary: correctness of the convergence criterion.** Even setting code
explosion aside, structural equality on residuals is the wrong equality notion
for a different reason: two abstract states that are semantically equivalent
(same `L`-values) might produce residuals with different internal shapes (e.g.,
different context annotations on a call node). Using structural equality would
declare them unequal and continue iterating past the true fixpoint. Blind
equality is the only equality consistent with the abstract domain's semantics.

**Relation to offline vs. online PE.** The need for blind equality is
characteristic of **online PE**, where the static/dynamic classification of
values is decided during execution:

- **Offline PE (BTA-based):** binding-time analysis classifies values as
  static/dynamic *before* execution. Code generation proceeds deterministically
  from this classification; no convergence test is needed during execution — the
  BTA itself ensures termination.
- **Online PE (current approach):** static/dynamic classification is decided
  at each step based on what is known so far. The fixpoint iteration decides
  convergence as it runs. Blind equality is the online PE's equivalent of BTA's
  pre-classification: it provides a termination criterion without requiring a
  full pre-analysis pass.

In the abstract-interpreter setting, BTA is hard to perform completely (because
dynamic control flow creates data-flow dependencies that are difficult to classify
statically). Blind equality allows online PE to make progress and terminate
without a complete BTA, at the cost of potentially generating residuals that a
BTA-guided approach might produce more compactly.

**Guard:**
- O-3 applies to stage-1's convergence/stability test only. It does **not**
  apply to serialization round-trip tests (which validate structural fidelity),
  residual-tree CSE/deduplication (a separate optimization), or stage-2 restart
  logic (where residual shape may be load-bearing).
- O-3 specifies the *semantics* of equality, not the algorithm. A correct
  implementation may exploit structural shortcuts (e.g., object-identity fast
  paths) so long as the result agrees with domain-level equality.

**Example:**
- Two successive iterations both producing `x = ⊥` must compare equal. A
  correct domain-level equality returns `true` for any two `⊥` values regardless
  of representation.
- Iteration n's cell for `y` has abstract component `[0,0]`; iteration n+1's
  cell for `y` also has `[0,0]` but carries a different (larger) residual
  fragment generated in that iteration. Under O-3, these `L`-states are equal;
  the residual-fragment difference is not consulted. Code generation stops here.
- Historical bug: two semantically-equal `⊥` states not comparing equal due to
  different internal representations, causing the fixpoint to keep iterating
  past the true stable point — a structural equality failure in the container,
  not the domain.

**Compliance check:** Any call site driving stage-1 convergence must delegate to
abstract-domain equality. Cells that hold both domain values and residual nodes
must route equality through an explicit `L`-projection, not through container
structural `==`.

---

### O-4 · Modification scope

**Rule:** Routine staged-analyzer implementation work happens in the *active
modular implementation*. The *canonical baseline* is a **frozen reference** — it
defines what program-input `I` (per C-1) means, and is not edited to change
analyzer behavior.

Concretely:
- Implementation changes land in the active modular implementation, with
  corresponding tests/docs. This covers modular pre-analysis, abstract
  interpretation, module summaries, residual obligations, loop handling, and
  stage-2 linking/restart.
- Legacy prototype changes are limited to maintenance, regression preservation,
  documentation, and migration reference unless this principle is explicitly
  revised.
- If the canonical baseline is imprecise on some idiom, the mitigation (if any)
  lives in the active modular implementation. **This does not redefine `I`: it is
  documented as an intentional deviation and cannot be cited as `≃` parity; per
  C-2 it is an `⊒`-form claim or a new analyzer-semantics claim distinct from
  `I`.**
- If the canonical baseline has a *genuine soundness bug*, a baseline patch is
  permitted only as an explicit "baseline version update" with justification —
  not as routine implementation work. Otherwise the fixed reference of C-1/C-2
  is lost.

**Why:** The research claim of C-1/C-2 is parity between `link(PE(I, m), d)` and
`I(m ⊕ d)`. For this to be meaningful, `I` needs a stable reference definition
*independent* of the implementation that partially evaluates it. If both sides
are edited simultaneously, the claim dissolves into "two analyzers that agree
with each other," which is strictly weaker than "the staged analyzer correctly
partially evaluates `I`."

**Guard:**
- The frozen surface is **analyzer semantics**, not the baseline directory's
  entire contents. Build-system, metadata, documentation, and test-harness
  changes that do not alter analyzer behavior are permitted.
- O-4 does not prohibit reading or running the baseline. Consulting (and
  executing) it is the correct way to establish ground truth for `I(m ⊕ d)`
  when validating C-2 parity.
- A baseline-soundness patch is the one permitted analyzer-behavior change in
  the baseline, and only with explicit "baseline version update" framing.

**Example:**
- New modular-summary operator: lands in the active modular implementation;
  baseline untouched.
- Staged analyzer produces a sharper result than baseline on some idiom: do
  *not* patch the baseline to match. Either (a) accept the divergence and
  document it as an `⊒`-form claim per C-2, or (b) narrow the staged analyzer
  to match baseline precision.
- Baseline has a demonstrable soundness bug: fixable in the baseline, but only
  with a justification entry labeled "baseline version update" stating what
  changed in `I` and why the fix is sound.

**Compliance check:**
- Any source-level change to the canonical baseline should be flagged for
  review. A flagged change is not blocked; it is allowed on override with a
  required justification — either a "baseline version update" label, or an
  explicit "non-semantic edit" note (build, docs, tests).
- Any statement of the form "analyzer behavior now X" or "precision improved on Y"
  must be traceable to a diff in the active modular implementation unless
  explicitly framed as a baseline version update.
- Any deviation between staged and baseline behavior on a C-2 test case must
  cite a specific `⊒`-form justification per C-2's Guard.
