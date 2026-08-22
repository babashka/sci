# ADR 0021: primitive-interface closures and fused loop drivers

Status: proposed (2026-08-22). No code and no measurements yet; this records
the design analysis and the measurement gates that decide whether the work
starts.

## Context

The idea, as submitted: bb's image already contains the compiled primitive
function interfaces (`clojure.lang.IFn$LL`, `$LLL`, `$DD`, ...). SCI's
analyzer could emit closures implementing those for type-hinted fns and loop
bodies, so `(fn [^long i] ...)` in a hot loop runs unboxed through a
prebaked interface instead of boxing through `IFn.invoke`. Add fused loop
drivers — `dotimes`/`reduce` nodes compiled as tight Java loops that invoke
the body through the primitive interface — and both real taxes are attacked
at once: boxing and per-iteration dispatch. Claimed ceiling: 2-5x on numeric
kernels. The appeal is that it follows the pattern SCI already walks
(call-node specialization, ADR 0009; fused binding nodes, ADR 0006):
enumerate the hot shapes at build time, map user code onto them at runtime.

The target platform is native bb, where there is no C2 and no
`clojure.core/eval`: the JVM already interprets a tight loop at ~2.9
ns/iteration and has ADR 0018's eval tier for the rest; native bb runs the
same loop at ~19 ns/iteration and neither tier applies (ADR 0016's Crema
emitter is the other contender there).

## Analysis

### The boundary is not where the boxing is

Three source facts kill the idea in its submitted form:

1. Fn invocation stores every argument into an `object-array` invocation
   array (`fns.cljc`, `gen-fn`: `(object-array invoc-size)` + one `aset`
   per param). A closure implementing `IFn$LL` would accept an unboxed
   `long` and box it on the very next instruction to store it in the
   register file. One box per *call* is removed; the body's interior — every
   node, every register read and write, every iteration — stays boxed,
   because the node protocol is `Object eval(ctx, bindings)` over `Object[]`.

2. `loop*` analyzes to `let*` plus an immediately-invoked fn
   (`analyzer.cljc`, `analyze-loop*`). The fn is entered **once**;
   iterations run inside the recur machinery in `fns.cljc`, rewriting the
   same `Object[]` registers per iteration. A primitive interface on the
   loop fn therefore removes at most one box per *loop*, not per iteration.
   The per-iteration tax — the claimed target — is untouched.

3. Nobody calls `invokePrim`. A host caller only emits `.invokePrim` when
   it was itself compiled against the hint; bb's built-ins (`reduce`,
   `map`, ...) are compiled generically and call `.invoke`. Sci-side call
   sites are under our control, but then we own both sides of the call and
   the interface is irrelevant — the win reduces to the interior
   representation either way.

So "hinted fns through prebaked `IFn$LL`" as stated buys approximately one
box per call and nothing per iteration. Not worth an analyzer branch.

### Fused loop drivers

- `dotimes` is a macro expanding to `loop*`/`recur`, so it already goes
  through the fused-binding floor (ADR 0006). There is no separate
  `dotimes` node to write.
- `reduce` is not in `copy-vars/inlined-vars` (no `:inline` meta in core),
  so per ADR 0009's hard constraint it reaches call sites as a sci var and
  honors redefinition; it cannot be specialized at analysis time. A
  guard-based driver (`identical?` root check per call, interpreter
  fallback) is possible — but host `reduce` over a chunked or indexed coll
  is *already* a tight compiled loop calling `f.invoke(acc, x)`. A sci
  driver would replicate that loop without removing the boxed boundary,
  unless the callee body runs primitive inside, which is the interior
  problem again. Dead as an independent line of work.

### The salvageable core: primitive interior for a closed shape family

Restated to where it can win: at analysis time, run the fixed-point
inference ADR 0018 built for its prim-register emission — mark loop
registers whose every recur argument is statically `long` (literals, other
candidate registers, calls into the long-returning arithmetic subset ADR
0009 already enumerates). Where the inference closes over the *whole* loop
body, build a specialized node family with primitive registers (`long[]`
alongside nothing else) and `long`-returning eval methods: a
mini-interpreter over enumerated numeric shapes, precompiled into the
image. Entry coerces from the boxed world behind the same
`(instance? Long b0)` guard ADR 0018 uses; exit boxes once.

This is the native-bb counterpart of ADR 0018's primitive loop registers:
same inference, a node tree instead of emitted Clojure forms, and it works
where no compiler exists. It attacks per-iteration boxing directly and
per-iteration dispatch partially (prim nodes still dispatch virtually, but
the calls are monomorphic per site and AOT-compiled).

Costs and risks:

- Node surface roughly doubles for every covered op (an Object path and a
  long path; doubles would triple it). The set must stay closed — the ADR
  0009 arithmetic subset over registers and literals — or this becomes the
  rewrite-sized primitive-everywhere interpreter (Truffle without the
  partial evaluator) that the incremental framing is trying to avoid.
- Overflow semantics are preserved by the same argument as ADR 0018:
  the guard proves the coercion exact and `Numbers.inc(long)` overflows
  exactly like the boxed one.
- Compiled loops must still honor `:interrupt-fn` / max-iterations checks
  that `gen-fn` emits (ADR 0018 limitation 3 applies here too).
- On the JVM the expected win is near zero (monomorphic node trees, C2) —
  every measurement must be native bb.

## Measurement gates, in order

Gate 1 — frequency (cheapest, run first). Counter behind an env var at the
point where the inference would fire; run bb lib tests *and* a set of
numeric-kernel scripts (AoC-style — the user-visible complaint this serves,
lib code is not the audience). ADR 0020's corollary says high frequency is
not evidence; zero frequency is still a kill.

Gate 2 — ceiling. Hand-write the specialized node for ONE shape (the
ADR 0018 sum-loop) and A/B in native bb against master and against the
Crema spike's 1.6-1.7x. If the hand-written ceiling on the flagship kernel
is under ~2x, stop: the remaining tax is dispatch, and that territory is
ADR 0016's.

Gate 3 — bb image size delta for the node family, measured on the
one-shape prototype and extrapolated before writing the rest.

## Relation to other ADRs

- ADR 0006: the fused floor this builds on; tight loops are already flat
  there in the boxed world.
- ADR 0009: the arithmetic subset and the `inlined-vars` hard constraint
  (which kills the `reduce` driver).
- ADR 0016 (Crema): same territory — native bb numeric perf. This proposal
  is the no-new-runtime-dependency option; it wins if Crema stalls, loses
  if Crema ships, and Gate 2 quantifies the gap.
- ADR 0018: source of the register inference and the coercion-guard
  pattern; covers the JVM so this proposal doesn't have to.
- ADR 0020: the frequency corollary applied in Gate 1.

## Decision

None yet. The submitted form (primitive interfaces at fn boundaries, fused
`reduce`/`dotimes` drivers) is rejected on the source analysis above. The
restated form (primitive-register loop nodes for a closed numeric shape
family, native bb target) is proposed, gated on the three measurements —
none of which have been run.
