# ADR 0018: Experimental JVM codegen tier for SCI

Status (2026-07-27): demo spike, code on branch `jvm-jit-demo` (pushed, not
merged; this ADR is also on master, the code is not). Off by
default (`sci.impl.types/jit-enabled` volatile, `SCI_JVM_JIT=true` turns it
on for a whole run, `jit/enable!` / `disable!` for tests and benchmarks).
Not proposed for master as-is: the escape hatch and the call-site stack
tables are missing (see Known limitations).

## Context

ADR 0014 added a JS codegen tier for SCI on CLJS: the analyzer attaches a
walkable mini-AST at supported sites and `sci.impl.jit/compile-template`
compiles each analyzed fn body once, via `js/Function`. The JVM has a
compiler in the runtime already — `clojure.core/eval` — so the same
architecture ports directly, with Clojure forms in place of JS source.

The earlier JVM attempt (ADR 0016) emitted BYTECODE with `clojure.asm`,
because its target was native-image + Crema, where no Clojure compiler
exists. On a plain JVM that machinery buys nothing: `eval` produces the
same bytecode with none of the emitter. This ADR is the cheap path, and it
also answers the question ADR 0016 left open — what primitive loop
registers are worth — because the host compiler does the inference once we
hand it the right form.

The baseline is not the same as on CLJS. On the JVM the interpreter is
already fast: a tight `(loop [i 0] (if (< i 1e7) (recur (inc i)) i))` runs
at ~2.9ns/iteration, because the node tree at a hot site is monomorphic and
C2 inlines through it. The 190ms/1e7 figure from native bb (ADR 0016) is a
different world — no C2 there. So the CLJS tier's 35-68x headline numbers
are not available here; the ceiling is compiled Clojure, and the question
is how much of the remaining 20x gap the tier closes.

## Architecture

Everything above the emitter is shared with ADR 0014 — same analyzer sites,
same AST shapes, same laziness (a `:jit-template` delay per fn body, forced
at closure creation), same fallback discipline.

- **AST carrier.** CLJS nodes are `NodeR` records with an `ast` field. `:clj`
  nodes are `reify`s with no field to spare, so the ast lives in a weak,
  identity-keyed side table in `sci.impl.types` (`attach-ast!` / `node-ast`,
  a `WeakHashMap`; reify identity semantics make it identity-keyed, and weak
  keys mean an entry lives exactly as long as its node). Both `->Node`'s
  optional ast argument and `attach-ast` route through it, gated on
  `jit-enabled`, so a disabled run allocates nothing and the interpreter's
  node types are untouched.
- **Emission is Clojure forms, not source text.** `recur` maps to Clojure's
  own `recur`, so the loop scaffold, tail-position checking and the recur
  sentinel all disappear. `let` maps to `let*` — each iden already owns a
  distinct invocation slot, so slot `n` becomes local `bn` with no aliasing.
- **Inlined core fns emit their SYMBOL.** A `:call-direct` callee that is
  identical to a `clojure.core` root value emits as `clojure.core/inc`, not
  as a consts entry: the var's `:inline` expansion is by definition the fn
  body the interpreter would have invoked, so this is the same call with the
  host compiler's own inlining applied. Same argument as the CLJS operator
  table, but the table is free here — `copy-vars/inlined-vars` keyed by
  identity of the resolved root.
- **`:call-var` emits `((deref v) args)`**, deref per call, so redefinition
  through a compiled call site is honored. `SciVar.invoke` is `(@this args)`,
  so this is exactly the interpreter's call with one hop removed. No epoch
  cache: ADR 0009's measurements found no deref tax to remove on the JVM.
- **Primitive loop registers.** The one place the port has to think. Params
  arrive as Object, so `(loop [b0 b0] ...)` keeps the register boxed and a
  1e7 loop allocates 1e7 Longs. A tiny fixed-point inference marks the
  registers whose every recur argument is statically long (long literals,
  other candidate registers, and calls to the long-returning arithmetic
  subset), and the template emits both loops behind a guard:
  `(if (instance? Long b0) (loop [b0 (long b0)] ...) (loop [b0 b0] ...))`.
  Semantics are preserved because the guard proves the coercion exact and
  primitive `Numbers.inc(long)` overflows exactly like the boxed one. When
  the inference is wrong the compiler says so — a recur arg that isn't long
  is a compile error, not a wrong answer — so the fallback is to compile the
  boxed form instead. This is what ADR 0016 named as its top TODO; it is the
  difference between ~4x and compiled-Clojure parity.
- **Consts** are read once per template instance into locals (`c0`, `c1`,
  ...) in an outer `let`. Enclosed values are unpacked per call, NOT hoisted:
  the self-reference slot is patched after the closure is created.
- Emission must be EAGER. A lazy seq in the emitter interns its consts after
  the template's const bindings have been built, producing a form that
  references unbound `c0` — which then fails to compile and silently falls
  back to the interpreter. Cost an hour; `mapv` everywhere.

## Measurements (JVM, best of 3 interleaved isolated runs, ms)

| workload | interp | jit | compiled Clojure | speedup |
|---|---|---|---|---|
| sum-loop 1e7 | 50.7 | 2.51 | 2.39 | 20.2x |
| fib 27 | 9.36 | 3.38 | 1.46 | 2.8x |
| tight-loop 1e7 | 28.4 | 0.030 | 0.028 | (folded) |

- The sum-loop is the honest 1e7 number: 20x, within 5% of the same loop
  compiled by Clojure, and linear at 1e8 (25.6ms) so nothing is being folded
  away.
- The tight loop (no accumulator) is a useless benchmark on the JVM: C2
  deletes the loop entirely, in the jitted case and in the compiled-Clojure
  case alike. It is reported only because a 950x number would otherwise look
  like a result.
- fib stops at 2.8x because the call itself is the work: each call is a
  var deref plus an `IFn.invoke` on a closure the tier did not create.
  Compiled Clojure calls a static method. Closing that gap means compiling
  the callee and the call site together, which this tier does not do.
- Compile cost: one `clojure.core/eval` per analyzed fn body at first
  closure creation. Warm (repeatedly evaluating the same one-fn program,
  isolated JVMs) it is ~20µs per body on top of analysis — the same order as
  `new Function` on CLJS (~18µs). COLD it is ~30x that: 20 loop-shaped defns
  analyzed and first-called in a fresh JVM go 21.2ms -> 33ms, i.e. ~0.6ms per
  body, because the first evals warm the Clojure compiler itself. Nothing
  here is fatal, but it is why a JVM tier wants a tiering heuristic rather
  than the CLJS tier's compile-everything policy: a script that loads 500
  interpreted fns and calls each once would pay ~300ms to compile bodies it
  runs once.

## Correctness

- 21-program differential smoke set (closures, nested loops, self-reference,
  redefinition through a compiled call site, non-numeric and double
  registers, overflow, escapes, varargs, multi-arity) run with the jit off
  and on: 0 mismatches, `strict-compile?` on, 13 of the 21 compile at least
  one template.
- Full JVM suite green with the tier off: 388 tests, 1487 assertions.
- Forced on: 5 failures, 0 errors — all of them error-location or
  stacktrace fidelity, from the missing call-site stack tables (below).
- One interpreter fix was needed, and it is not jit-specific: the
  `sci.impl.` prefix strip in `utils/rewrite-ex-msg` is aimed at
  `sci.impl.fns`, so any other sci-internal fn class fails the name
  comparison and an arity error prints the generated class name instead of
  the sci var name.

## Known limitations

1. **No interpreter escape.** The CLJS tier's `H.ev` — compile an
   unsupported subtree to an interpreter call sharing the invocation array —
   is the feature that makes it a mixed-mode tier. This spike has none, so a
   single unsupported node drops the whole body to the interpreter, and
   there is no array register mode either. That is why `case`, `throw` and
   `try` bodies show up as whole-fn fallbacks in the smoke set.
2. **No call-site stack tables.** Errors thrown from compiled code carry the
   enclosing frame but not the per-call frames the interpreter records, which
   is the entire content of the 5 suite failures. The CLJS solution (an `s`
   register per call site, one try/catch per template, a const stacks table)
   ports directly and was skipped only for time.
3. **No interrupt-fn.** Compiled loops don't emit the check `gen-fn` does, so
   a ctx with an `:interrupt-fn` skips the tier entirely rather than
   silently becoming uninterruptible.
4. Varargs, multi-arity, `this-as` and macro fns fall back, as on CLJS.

## Next steps, ranked

1. The escape hatch (`H.ev` equivalent) plus array register mode. Without it
   the tier only fires on kernel-shaped code, which is why the differential
   set has a clean split between 20x loops and untouched glue.
2. Call-site stack tables, for error parity — a precondition for anything
   on by default.
3. A tiering heuristic. Cold `clojure.core/eval` is ~0.6ms per fn body, so
   compile-everything is not obviously right on the JVM; invocation counting
   at the per-arity stub the CLJS tier already has would be the natural
   place.
4. Only then: is this worth it for babashka? bb is native, where there is no
   `clojure.core/eval` — the answer there is still ADR 0016's bytecode
   emitter plus Crema/Ristretto. This tier helps sci on the JVM (and any
   embedder that runs sci in a JVM process), not bb.
