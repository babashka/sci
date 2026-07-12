# ADR 0014: Experimental JS codegen tier for SCI on CLJS (prototype)

Status: prototype on branch `worktree-js-eval` (2026-07-12). Not wired to any
public API yet; disabled by default.

## Context

On CLJS, SCI evaluates a tree of `NodeR` closures: every node eval is a
closure call, every fn invocation allocates an invocation `object-array`, and
loops re-eval the body node per iteration. Call-node specialization (ADR
0009) already removed most dispatch for inlined core fns, but the closure
tree itself remains the floor.

Cherry demonstrates the compilation ceiling: a tight numeric loop runs ~64x
faster compiled than interpreted. Idea: JIT-compile SCI fn bodies to JS at
runtime via `new Function`, while keeping the interpreter as a full fallback
— both for unsupported nodes and for environments where CSP forbids eval
(browser extensions injecting scittle into arbitrary pages).

Why not compile with cherry directly? Cherry has its own analyzer and
macroexpansion: sci vars, sci macros and sci-defined fns are invisible to it,
its output does unrestricted interop (bypassing sci's sandbox), and embedding
it costs ~330KB+. It remains interesting as a separate opt-in package, not as
a transparent tier inside sci.

## Architecture

Codegen is driven by SCI's own analysis, so semantics come for free:

- Supported analyzer sites additionally attach a small walkable AST
  (`:sci.impl/ast` on the `NodeR` record's extmap, CLJS only): `:if`, `:do`,
  `:let`, `:recur`, `:call-direct` (inlined core fn), `:call-var` (sci var,
  deref per call — redefinition honored), `:call-bind` (binding in call
  position).
- `loop*` already expands to `let*` + immediately-invoked `fn*`, so fn-body
  compilation covers loops; `recur` becomes assignments + `continue`.
- One template per analyzed fn body, compiled once at analysis time
  (`sci.impl.jit/compile-template`); closure creation just instantiates the
  template with (ctx, enclosed-array). Nested fns like `fib` never recompile.
- Universal escape hatch: any node without an AST compiles to
  `H.ev(node, CTX, B)` — an interpreter call sharing the invocation array.
  Tail-position escapes check for the recur sentinel and `continue`. Mixed
  mode from day one; coverage can grow incrementally.
- Two register modes: escape-free bodies map binding indices to real JS
  locals (params double as slots, enclosed values unpacked from `E`);
  bodies with escapes keep the `B` array so interpreted subtrees see
  bindings.
- Call conventions: inlined core fns are called directly, preferring the
  `.cljs$core$IFn$_invoke$arity$N` impl to skip variadic dispatch (V8 then
  inlines e.g. `+` arity-2). Unknown callees use `f.call(null, ...)` — same
  as the CLJS compiler, works for keywords/MetaFn/fns.
- Generated source references only `Function` parameters (consts array,
  helpers object with string keys, CTX/E/INT/B) — no textual cljs.core
  names, so it survives advanced compilation without externs.
- Interrupt check (`:interrupt-fn`) emitted at loop heads, matching `gen-fn`.
- CSP probe: `enable!` try/catches a `new Function` construction; when
  blocked everything stays interpreted.

## Measurements (node 22, `-O simple` harness in scratch/, min of 7)

| workload | interp | jit | speedup | vs compiled CLJS | vs cherry |
|---|---|---|---|---|---|
| tight-loop (1e6) | 46.9ms | 1.32ms | 35.7x | 3.7x slower | 4.5x slower |
| fib 20 | 2.03ms | 0.097ms | 20.9x | 1.7x slower | 1.7x slower |
| eq-rem-loop | 11.5ms | 1.49ms | 7.7x | 2.3x faster | 3.9x slower |
| data-access | 19.3ms | 8.3ms | 2.3x | 1.3x slower | 2.5x slower |
| assoc-loop | 2.2ms | 1.22ms | 1.8x | parity | 1.8x slower |
| seq-walk | 1.49ms | 0.49ms | 3.1x | 2.1x slower | 4.9x slower |
| destructure | 6.0ms | 3.04ms | 2.0x | 1.9x slower | 1.5x slower |
| higher-order | 3.46ms | 0.77ms | 4.5x | parity | 1.4x slower |

- Correctness: full CLJS test suite with jit force-enabled, `:none` and
  `:advanced`: 1338 assertions, 5 failures, 0 errors — all 5 are
  stacktrace/error-location tests (see limitations). 16-case smoke set
  (redef through jitted call sites, keyword-as-fn via binding, recur through
  interpreted `case`, dynamic binding, variadic/multi-arity fallback, fn
  meta) matches the interpreter exactly.
- Bundle: +7.4KB raw / +2.3KB gzip (~1.1%) on the shadow release build.
- Analysis-time cost when enabled: +5.7% on a 50-defn source (~18µs per fn
  body including `new Function`).
- Allocation-heavy shapes (assoc-loop) barely move — interpreter overhead is
  not the bottleneck there. Gains concentrate in loops/calls/arithmetic,
  matching the JVM specialization findings.

## Known limitations (prototype)

- Stacktrace fidelity: compiled frames push no per-node stack info, so
  `sci/stacktrace` loses intra-fn frames and error locations coarsen to the
  fn. Candidate fix: per-call try/catch emitting rethrow-with-location (V8
  try is ~free when not throwing); needs measurement.
- Fallback (interpreter, correct but uncompiled): varargs bodies,
  `this-as`, and any form without an AST — `or`/`and`, `case`, `try`,
  collection literals, interop, fn-creation nodes (the created fn's own
  body still jits).
- Global enable flag; production wants a ctx-scoped option
  (e.g. `(sci/init {:experimental-jit true})`) so a sandboxed ctx and a
  jitted ctx can coexist. AST attachment currently unconditional
  (analysis allocation even when disabled) — should be gated.
- Escape-heavy bodies still compile (template shell + escapes ≈ interpreter
  speed, wasted compile). Needs a coverage heuristic or invocation-count
  tiering.

## CLJS gotchas hit (worth remembering)

- `(identical? :kw x)` fails under `:none`/`:simple`: keyword literals are
  constructed per usage site. Cost a silent 8x regression (every direct call
  fell into the `.call` branch). Use `keyword-identical?`/`case`/flags.
- `#js {...}` literal + `.-prop` access broke under `:advanced` renaming
  (state fields disagreed between writer and reader → `B is not defined`).
  Emitter state is now a deftype; helpers object uses `js-obj` with string
  keys because generated code references them textually.
- `cljs.main` caches compiled deps in the `-d`/default `out/` dir; a stale
  cache silently reran old emitter code. `rm -rf out` when in doubt.

## Decisions after review (2026-07-12)

- Eager compilation accepted: +5.7% analysis is fine against 20x+ runtime
  wins. Tiering and coverage heuristics dropped from the plan.
- Stacktrace fidelity deferred: acceptable to document for an opt-in
  experimental flag; the per-call try/catch idea needs its own measurement
  before committing to it.

## Next steps, ranked by payoff/effort

1. **Operator inlining for the numeric spec set** (in progress). Verified
   against cljs.core 1.11.132 source: the compiled fn bodies of `+ - * < >
   <= >= ==` (arity 2), `inc dec zero? pos? neg? nil? not` (arity 1) and the
   `unchecked-*` aliases are bare JS operators (`-` is `x - y`, `zero?` is
   `x === 0`, `==` is `x === y`, `not` is cljs falsy check). The interpreter
   already calls those fn bodies, so emitting the operator inline is
   semantically identical — no type guards ((inc "a") stays "a1" either
   way). NOT eligible: `rem` (quot-based composite body, not JS `%`), `=`
   (deep equality), `get`. Expected: tight-loop from 1.32ms toward compiled
   CLJS (~0.4ms), 35x -> ~100x.
2. **Collection literal nodes** (vector/map/set). Any literal currently
   escapes, forcing array mode for the whole body. Supporting them keeps
   real-world bodies escape-free (locals mode). Helps destructuring and
   data-access shapes broadly.
3. **`or`/`and`**: trivial emit (truthiness temps, short circuit),
   ubiquitous in real code, same escape-free benefit.
4. **ctx-scoped opt-in** (`{:experimental-jit true}`-style): ship
   requirement, not perf. Replaces the global `enable!` volatile, gates the
   AST attach so disabled ctxs pay zero analysis cost.
5. Browser validation: scittle demo under a CSP page (probe fallback), real
   DOM workloads; nbb/joyride integration.

ROI caveat: items 1-3 widen the gap on loop/arithmetic code and keep more
bodies in locals mode. Allocation-heavy code stays ~2x regardless — that
floor is cljs.core itself, not the interpreter.
