# ADR 0014: Experimental JS codegen tier for SCI on CLJS

Status (2026-07-15): complete implementation under review on branch `jit`
(one commit on master; `worktree-js-eval` carries the full history). ON BY
DEFAULT, no user-facing flags: an eager probe at load checks `js/Function`
availability and falls back to the interpreter permanently and silently when
eval is blocked (CSP). Opt-outs: `jit/enable!`/`disable!` (internal, for
tests), the public compile-time `goog-define sci.core/disable-jit` (also
used by the jit-off CI leg), and the public runtime
`js/globalThis.SCI_DISABLE_JIT`. Validated: 5635 assertions x 3 CI legs (jit-on `:none`,
jit-on `:advanced`, jit-off `:none`), ~450k differential fuzz programs (see
Fuzzing below), full nbb ci:test, scittle CSP field test.

Opt-outs (both public): `js/globalThis.SCI_DISABLE_JIT = true` set BEFORE
loading sci is checked before the probe, so a CSP page stays silent (a
refused Function construction logs a console violation even inside
try/catch) and consumers of compiled artifacts (scittle) get a runtime
switch. `:closure-defines {sci.core/disable-jit true}` is the compile-time
switch; `sci.core` vresets the `sci.impl.types/jit-enabled` volatile off at
load. The define lives in the PUBLIC `sci.core`, not the impl namespace
that holds the volatile. The CI jit-off leg uses this same define.

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

- Supported analyzer sites additionally attach a small walkable AST (the
  `ast` FIELD on the `NodeR` record; a macro discards it at expansion on
  clj/cljd, and construction is gated on the jit-enabled flag so disabled
  environments pay nothing): `:if`, `:do`, `:let`, `:recur`,
  `:call-direct` (inlined core fn), `:call-var` (sci var, deref per call —
  redefinition honored), `:call-bind` (binding in call position), `:or`,
  `:and`, `:case`, and under ctx `:unrestricted` the interop kinds
  `:iget`, `:imeth`, `:jsstatic`, `:jsctor`.
- `loop*` already expands to `let*` + immediately-invoked `fn*`, so fn-body
  compilation covers loops; `recur` becomes assignments + `continue`.
- One template per analyzed fn body (`sci.impl.jit/compile-template`),
  compiled LAZILY: analysis attaches a delay, closure creation returns a
  per-arity stub, and the first invocation forces the compile (see the
  analysis-cost bullet under measurements). Closure creation instantiates
  the template with (ctx, enclosed-array); nested fns like `fib` never
  recompile.
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
- CSP probe: `sci.impl.types` try/catches a `new Function` construction at
  load; when blocked everything stays interpreted (field-tested: a scittle
  page with a CSP meta tag without unsafe-eval runs interpreted with
  identical results, no page errors).
- Var-deref caching with epoch invalidation (BOTH modes: jitted call
  sites and, on CLJS, the interpreter's var-call wrap closures — interp
  var-call 146.5 -> 139.2ms/1e6): every var mutation primitive
  (bindRoot, unbind, root set!, TBox set, thread-binding push/pop — defn
  redefinition and alter-var-root route through bindRoot) bumps a global
  epoch; jitted :call-var sites keep a per-site [value epoch] cache, so
  the hot path is two array reads + compare instead of a deref. binding
  correctness falls out: push/pop bump, sites re-deref inside and after
  the scope. defn fib 30 warm: 38 -> 17.5ms (let-bound 14.7, interp 112).
  A missed bump means a stale callee: any NEW var mutation path must bump
  (see the var-epoch docstring). Both modes share the cache, so
  interp/jit agreement cannot catch a missed bump — the mutation
  visibility tests assert LITERAL values. GC note: only CALL-position
  derefs cache (fn values); vars read in value position ((count x))
  re-deref every time, so plain data in vars is never retained. Worst
  case is one generation of a redefined fn pinned per cold call site.
- Stacktraces via call-site tables (the line-number-table trick): one
  try/catch per template, a plain `s` register written before each call
  site, and a const table mapping site index -> the call node's existing
  stack map. The catch rethrows through the same
  rethrow-with-location-of-node machinery as the interpreter, so frames and
  locations are identical (verified: the previously-failing 5 stacktrace
  tests pass; nbb output byte-identical to interpreted). Operator-inlined
  sites can't throw and get no table entry. Measured cost: zero (all bench
  numbers within noise).

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

- Correctness: full CLJS test suite green on all three CI legs (jit-on
  `:none`, jit-on `:advanced`, jit-off `:none` via the goog-define):
  5635 assertions, 0 failures, 0 errors. The jit test namespace adds
  differential cases (run under both a sandboxed and an `:unrestricted`
  ctx so interop emission is exercised in-suite), a 4k-assertion operator
  parity matrix, and var-mutation visibility tests asserting literal
  values. 16-case smoke set
  (redef through jitted call sites, keyword-as-fn via binding, recur through
  interpreted `case`, dynamic binding, variadic/multi-arity fallback, fn
  meta) matches the interpreter exactly.
- Bundle: +7.4KB raw / +2.3KB gzip (~1.1%) on the shadow release build.
- Analysis-time cost when enabled: originally +5.7% on a 50-defn source
  (~18µs per fn body including `new Function`), and +8.1% on loading
  honeysql through nbb (93.0 -> 100.5ms). Templates are now compiled
  LAZILY (a delay forced at first closure creation), so fn bodies — loops
  included — inside never-executed code don't compile at load: honeysql
  load overhead drops to ~3%, and with per-arity first-invocation stubs
  (compile deferred past closure creation to first call; array-slot impl
  cache, inlined nil-check hot path — var-call unchanged at ~29ms/1e6) to
  ~2%; the 50-defn microbench is noise or negative. Caveat
  for harnesses that flip enable!/disable! globally: a body's delay seals
  in the mode active at its first closure creation.
- Allocation-heavy shapes (assoc-loop) barely move — interpreter overhead is
  not the bottleneck there. Gains concentrate in loops/calls/arithmetic,
  matching the JVM specialization findings.
- Real programs (nbb, jit build vs same build forced off): editscript diff
  on a 300-key nested map 93.0 -> 68.9ms (1.35x), honeysql formatting 500
  complex queries 133.7 -> 118.3ms (1.13x), a 20k-line log-parsing script
  20.3 -> 18.2ms (1.11x). Real library code is dominated by compiled
  cljs.core plus escapes (collection literals, destructuring, or/and,
  multimethods) that also force array mode — that's the gap items 2-3 in
  the ranked next steps attack. Kernel-style user loops see the 20-60x;
  glue code sees 1.1-1.4x today.

## Known limitations

- Fallback (interpreter, correct but uncompiled): varargs bodies,
  `this-as`, and any form without an AST (see remaining escape sources).
- Escape-heavy bodies still compile a template shell at first invocation
  (shell + escapes runs at ~interpreter speed, so the compile is wasted).
  A coverage heuristic could skip those; not worth it until profiles show
  it.

## Remaining escape sources, by estimated real-world impact

Every escape also drags its enclosing fn body from locals mode into array
mode, so eliminating one pays twice.

1. **fn-creation nodes** (fn/closure in let-init or argument position):
   the created fn's own body jits, but the creating site escapes. Hits any
   code using local helper fns or higher-order style — editscript's current
   ceiling. Tricky: closure capture reads the creating fn's bindings array,
   so locals mode needs the capture set spilled or the enclosed-array
   construction emitted inline. Related conservatism: ANY escape forces the
   whole body into array mode, even when the escaped node provably never
   touches the environment — a ZERO-capture (fn ...) in a let-init drags
   its enclosing body out of locals mode for nothing. The capture set is
   known at analysis, so a cheap intermediate fix is an env-free flag on
   such escapes (pass null for B, keep locals mode); the full fix
   (compiling fn-creation) subsumes it for the capturing case. Do both
   together in the follow-up PR.
2. **case**: DONE, hybrid — dispatch stays the interpreter's structural
   map lookup (JS switch semantics are ===, and keyword literals aren't
   interned under :none/:simple, so switching on values would be wrong);
   the helper returns a branch index and a JS switch runs compiled arms.
   Arms are true tail positions (recur = continue through the switch).
   Real-program deltas modest (honeysql 1.19x, editscript 1.41x, crunch
   1.17x vs interp): remaining escape mass is fn-creation sites and try.
3. **try/catch/finally**: JS has the same construct; main work is binding
   the catch local and matching sci's exception-class dispatch.
4. **Instance interop** (method calls, field access): DONE for the
   unrestricted case, together with js/ static calls and constructors —
   the analyzer's own unconditional-allow gate (`:allow :all` deliberately
   routes to the config-aware node so `:closed` can win). Emission
   replicates invoke-instance-method exactly: obj, then method lookup with
   the interpreter's "Could not find instance method" error BEFORE args,
   then Reflect.apply (nbb#118). Restricted ctxs keep the checked escape:
   no permission logic in generated code. Getting error-location parity
   required the ambient-site discipline (below).

   The gate is `(:unrestricted ctx)`: the ctx-scoped flag shipped to
   master as #1065 (which also removed the process-global `*unrestricted*`
   var and made `enable-unrestricted-access!` throw), fixing the
   pre-existing nested-sandbox leak — a nested restricted ctx inside
   bb/nbb now really sandboxes interop, and the jit arms sit behind the
   same conditional. Still open: a wider gate for ctxs configured with
   `{:classes {:allow :all}}` and no member-level configs, which today
   stay on the config-aware escape.
5. **Site discipline** (learned via fuzzing the interop emission): the s
   register must mirror the interpreter's try nesting — only call nodes
   own sites, a call's site covers callee+args and closes with its
   statement, s=-1 means transparent rethrow. The emitter tracks s
   statically, re-asserts the ambient site at statement boundaries and
   invalidates across control-flow merges. Interop/escape/operator nodes
   carry no own site; they defer to the ambient one, exactly like
   interpreter nodes without a catch.
6. **Multimethods / protocol dispatch**: currently var-calls (already
   jitted as calls); the dispatch itself lives in compiled cljs.core, so
   little headroom beyond what call sites already get.
7. **Varargs and this-as bodies**: whole-fn fallback; varargs needs rest
   arg building in the prologue, mechanical. this-as is browser-interop
   territory, low value.
8. **Literals over the call-arity cap** (>8 elements, non-constant):
   escape via the arity check; could emit chunked builders if it ever shows
   up in a profile.
9. **Direct method call for static/instance interop**: `:jsstatic` emits
   `Reflect.apply(C[method], C[class], [args])` and `:imeth` an analogous
   indirect call. Measured (node): a jitted `(fn [] (Math/sin 3))` runs
   139ms/1e7 vs 40ms native (3.5x), while a jitted non-interop body is
   near-native (arith `(+ 1 2)` 55ms, 1.37x) — so the gap is the interop
   emission, not fn-call overhead. Micro-bench isolates the cause: it is
   NOT the args-array allocation (`Reflect.apply` 706ms ~= `m.call(cls,x)`
   685ms for 5e7), it is the indirect call through a function-object
   reference, which V8 cannot fold into the monomorphic `Math.sin(3)`. A
   DIRECT `C[class][name](args)` runs 189ms, 3.7x faster. Emitting the
   direct form (name known at analysis, interpolated via JSON.stringify;
   re-looking-up `class[name]` on a stable class is semantically identical
   and binds `this` correctly) should pull jitted interop toward the
   fn-call floor. Applies to `:jsstatic` and, where the method reference is
   safe, `:imeth` (mind nbb#118: instance dispatch used Reflect.apply
   deliberately, so verify).
10. **Skip the loop scaffold for non-recurring bodies**: every template
   emits `r: for(;;){ ... }`, the recur-sentinel handling and the
   loop-head interrupt check, even for straight-line bodies (the common
   case). A `recur` targeting THIS frame is detectable at compile time
   (loop* makes its own frame, so only self-tail-recursion counts); when
   absent, emit the body directly with no loop label, no `for(;;)`, no
   sentinel checks. Payoff is mostly generated-code size (helps every
   non-recursive fn) plus a simpler tail-escape path; runtime gain is
   small (V8 handles a one-iteration loop well). Cheap to add.

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
- cljs `==`: the MACRO emits `===` but the FN's arity-2 body is
  `(-equiv x y)` — structural. Worse: sci-CLJS `==` is path-dependent on
  master TODAY — the fused specialization (ADR 0009) compiles the macro,
  the general path calls the fn. Caught by the fuzzer (fn side) and the
  operator-parity matrix test (fused side); the jit mirrors the
  path-dependence exactly. Upstream fix worth considering. Every
  operator-table entry is now pinned by a 4k-assertion parity matrix in
  test/sci/jit_test.cljs instead of hand verification.

## Decisions after review (2026-07-12)

- Eager compilation accepted: +5.7% analysis is fine against 20x+ runtime
  wins. Tiering and coverage heuristics dropped from the plan.
- Non-opt-in: JIT is on by default, no flags. First compilation lazily
  probes eval availability once; CSP-blocked environments fall back to the
  interpreter permanently and silently. enable!/disable! remain as internal
  overrides for tests/benchmarks. Site-table cost on call-dense workloads
  (1e6 calls): <=2.5%, same order as run noise, so error fidelity is not a
  reason to keep a flag.
- Stacktrace fidelity: DONE via call-site tables (see architecture).
  Considered and rejected: shadow stacks (happy-path cost violates
  pay-on-throw), parsing named-fn JS stacks (engine-specific, fn-level
  granularity only). The site-table design was chosen because it keeps all
  cost in the throw path except one register write per call site.

## Next steps, ranked by payoff/effort

1. **Operator inlining for the numeric spec set**: DONE. Verified
   against cljs.core 1.11.132 source: the compiled fn bodies of `+ - * < >
   <= >= ==` (arity 2), `inc dec zero? pos? neg? nil? not` (arity 1) and the
   `unchecked-*` aliases are bare JS operators (`-` is `x - y`, `zero?` is
   `x === 0`, `==` is `x === y`, `not` is cljs falsy check). The interpreter
   already calls those fn bodies, so emitting the operator inline is
   semantically identical — no type guards ((inc "a") stays "a1" either
   way). NOT eligible: `rem` (quot-based composite body, not JS `%`), `=`
   (deep equality), `get`. Measured: tight loop 35.7x -> 68.5x, fib -> 28x,
   data-access beats `-O simple` compiled CLJS. Every table entry pinned by
   the parity matrix test.
2. **Collection literal nodes** (vector/map/set). DONE: non-constant
   literals were already return-call nodes with a builder fn (vector,
   map-fn, checked set builder), so they reuse the :call-direct machinery —
   an ast attach at two analyzer sites, no new emitter vocabulary. Literals
   over the call-arity cap (8) still escape.
3. **`or`/`and`**: DONE — short-circuit chains; last child is a true tail
   position (recur-safe), non-last children early-return in tail mode.
   With 2+3: editscript diff 1.35x -> 1.37x, honeysql 1.13x -> 1.24x,
   log-crunch 1.11x -> 1.20x vs interpreter; suite green both levels;
   30k fuzz seeds clean on the new emission.
4. Browser validation: DONE for scittle — CSP field test on a real page
   (plain page 2M loop 8.5ms jitted, CSP meta-tag page 45-50ms clean
   interpreter fallback, identical results and redef semantics). nbb runs
   the full ci:test suite green with the jit build. Joyride integration
   still pending. (The ctx-scoped opt-in idea that used to sit here was
   superseded by the non-opt-in decision below.)
5. **Migrate nbb to `sci.async`**: SCI now has its own async/await
   (`^:async` fn metadata + `await`, `sci.impl.async-macro`), so nbb's
   bespoke await machinery can be retired. Interacts cleanly with the jit:
   the async transform runs before analysis, turning bodies into plain
   promise-helper call chains (`sci.impl.async-await/then|catch-for-try|...`
   inlined vars), which compile like any other calls. Error UX today:
   errors thrown by SCI code inside async fns keep location + callstack
   (wrapped at the throw node, so the rejection carries the :sci/error
   ex-info); `try/catch` around `await` unwraps to the original exception
   (ex-data intact). Gap: host rejections whose value is not an
   Error/ex-info (e.g. `(js/Promise.reject 42)`) surface with no message,
   location, or stack — nothing ever passes a SCI throw site, so nothing
   wraps it. A migration should make the embedder's top-level rejection
   handler render non-Error rejection values explicitly. Worst current case
   (old and new nbb alike): an error thrown in a non-promise host callback
   (`js/setTimeout`, event handlers) never reaches nbb's error printer —
   Node's default handler dumps the wrapped ex-info as raw object guts
   (minified frames + the SCI callstack as persistent-vector internals).
   The migration should install `process.on("uncaughtException")` /
   `"unhandledRejection"` handlers routing through the SCI error formatter.
6. **Compile `^:async` fns to native async/await** (follow-up to 5): the
   promise-chain transform is jit-hostile — every `await` point becomes a
   closure (fn-creation = escape source #1), so async bodies drop to array
   mode or the interpreter. Instead keep `await` as a mini-AST node and emit
   real `await` inside an `async function` template (`AsyncFunction`
   constructor, same eval gate as `Function`). JS locals persist across
   `await`, so locals mode and the `s` site register survive suspension
   points unchanged; `loop`+`await` is the existing `for(;;)` with an await
   inside; `try` around `await` is native JS try once try-compilation lands
   (no `catch-for-try` unwrapping); V8 async stack traces + site tables
   should beat the chain's error output. Constraint: `H.ev` is synchronous,
   so an `await` inside an unsupported construct cannot partially escape —
   the whole fn falls back to the promise-chain transform, which means
   analyzing async fns twice (untransformed for the template, transformed
   as interpreter fallback). Acceptable: `^:async` fns are rare.

ROI caveat: items 1-3 widen the gap on loop/arithmetic code and keep more
bodies in locals mode. Allocation-heavy code stays ~2x regardless — that
floor is cljs.core itself, not the interpreter.

## Fuzzing (run this after ANY emitter or attach-site change)

The differential fuzzer lives in `test/jit_fuzz/` (`jit-fuzz.gen` is a
seeded LCG program generator, `jit-fuzz.main` evaluates each program
through interpreter and jit and compares values, error messages AND error
locations). It runs under an `:unrestricted` ctx so interop emission is
exercised. The suffix-less ns names keep it out of the cljs-test-runner's
test discovery.

    clojure -M:test -m cljs.main -t nodejs -O simple -o fuzz.js -c jit-fuzz.main
    node fuzz.js 0 20000        # start-seed count; ~35s for 20k seeds
    FUZZ_DUMP=1 node fuzz.js 42 3   # print the generated programs first

Exit code 1 on any mismatch, with seed + program + both results printed.
Seeds are deterministic: a mismatch reproduces with `node fuzz.js <seed> 1`.
History: ~450k seeds clean at review time; earlier rounds caught the
operator-site location bug, the ambient-stack discipline gaps (944 interop
location mismatches), and the cljs `==` fn-vs-macro divergence. Practice:
20k seeds after a localized change, 100k+ after touching emit-call, the
stack discipline, or `escape-free?`.

The generator must GROW with the emitter: a new compiled node kind gets
zero fuzz coverage until `jit-fuzz.gen` produces programs containing it
(case, interop and var-mutation shapes were each added when their emission
landed). When adding an emitter capability, add a generator shape in the
same change and re-baseline with 100k seeds.

Strict compile (test-only): `compile-template` wraps emission in a catch
that returns nil on any exception, so a compiler BUG silently falls back to
the interpreter — and the differential checks can't see it, because both
sides then run the interpreter and agree. `sci.impl.jit/strict-compile?`
(a volatile, set true by both `jit_test` and the fuzzer) makes that catch
rethrow, so an emitter exception fails loudly instead of hiding. It stays
false in production, where a compile failure must never break evaluation.
Legitimate non-compilation (varargs, this-as, escapes) does not reach the
catch — it returns nil through guards or emits an `H.ev` escape — so strict
mode has no false positives (verified: full suite and 20k seeds green with
it on).

## Idea parked: lazy body analysis in the interpreter itself

The first-invocation stubs mean loaded-but-never-called fns skip CODEGEN.
The same idea applied one level down — deferring the interpreter's own
ANALYSIS of fn bodies to first invocation — is independent of the jit and
would attack the biggest lib-load cost (analysis is ~38% of bb lib
loading). It is not free, unlike the stub trick: analysis is where
unresolved symbols, bad arities and syntax errors surface, so deferring it
moves those errors from load time to first call, an observable semantics
change (Clojure itself analyzes eagerly). Would need to be opt-in or
limited to shapes where the errors can't differ. A partial split may dodge
the semantics problem: keep resolution/validation (macroexpansion, symbol
resolution, arity checks — where the errors live) eager, defer only NODE
CONSTRUCTION (closure allocation) to first call. Needs a profile first:
if resolution dominates analysis time, the deferred half is not worth the
machinery. Applies to the JVM/bb equally. Not part of this work; recorded
because the stub design makes the follow-on question obvious.
