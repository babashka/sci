# ADR 0009: Extended Call-Node Specialization

| Status | Date | Related |
|--------|------|---------|
| Proposed | 2026-07-07 | Builds on ADR 0006 (fused binding nodes); commit `ba95a759` on `perf` branch |

## Context

The analyzer's `return-call` (built by `gen-return-call` in `analyzer.cljc`)
specializes calls to identity-known core functions: instead of a node that does
`(f (t/eval arg0 ...) ...)` through `IFn.invoke`, it emits a node calling the
static host equivalent directly (`clojure.lang.Numbers/inc`,
`clojure.lang.RT/first`, ...). Before this change the mechanism covered:

- 9 fns at arity 1 and 12 at arity 2 (arithmetic, comparisons, `rem`, `nil?`, `not`)
- only when every argument was a local binding (`BindingNode`) or, at arity 2,
  a binding/constant mix
- dispatch via `condp identical?` chains inline in `return-call`

Everything else — `=`, `get`, `count`, any arity-3 call, any call whose
argument is itself a call — went through `IFn.invoke` on the fn object.

## What was measured

Criterium `quick-bench` means, JVM (M-series Mac, direct linking), interpreted
workloads defined via `sci/eval-string*` and invoked as fns. Baseline = master
`0a816c7e`. Run-to-run noise is ±5–7%; combined-suite numbers additionally
drift from JIT profile pollution (same workload measured 0.88 ms in-suite vs
0.55 ms isolated), so regressions were re-verified in isolated runs.

| workload | code shape | baseline |
|---|---|---|
| loop-1m | `(loop [v 0 c 1000000] (if (pos? c) (recur (inc v) (dec c)) v))` | 6.86 ms |
| fib-25 | recursive fn, `(+ (fib (- n 1)) (fib (- n 2)))` | 4.55 ms |
| eq-rem-loop | `(= i 100000)` exit + `(= 0 (rem i 3))`, 100k iters | 3.73 ms |
| seq-first-next | `(loop [s (seq xs)] ... (next s) (first s))`, 1k elems | 33 µs |
| count-get-loop | `(+ acc (count v) (get m :a))`, 100k iters | 6.74 ms |
| destructure-call | fn with `{:keys [a b]}` param, 10k calls | 0.67 ms |
| assoc-loop | `(assoc m (rem i 100) i)`, 10k iters | 0.78 ms |
| analyze-defn | analysis only, medium `defn` form | 21.0 µs |

## The items, biggest win first

### 1. Arity-3 specialization: `get`, `+`, `-`, `*`

Arity-3 calls previously always went through `IFn.invoke`. `get` has an exact
static equivalent (`RT/get`); `+`/`-`/`*` fold left, so `(+ a b c)` compiles to
two nested `Numbers/add` calls with identical semantics.

- count-get-loop: **−53% cumulative (2.1×)**; this item alone took it from
  −18% to −53%, i.e. roughly **−43% on top of item 2**
- assoc-loop: ~−5%
- Confirmed on GraalVM native (babashka): count-get shape 95.2 ms → 60.9 ms,
  **−36% (1.56×)**

Three-argument `+` with a fused sub-call is a very common shape; this was the
single biggest jump in the whole exercise.

### 2. Extended arity-1/2 specialization tables

New entries, same mechanism as the existing 21: `=` (`Util/equiv`),
`identical?`, `quot`, `min`, `max`, `bit-and/or/xor`, `bit-shift-left/right`,
`get` at arity 2; `count`, `boolean` at arity 1.

- eq-rem-loop: **−13%** (`=` on bindings and on constant+call args)
- count-get-loop: **−18%** (before item 1; `count`/`get` fused)
- destructure-call: **−12%** (map destructuring expands to `get` calls)

The candidate set is bounded by `copy-vars/inlined-vars` — see Safety below.
Entries outside that set (`first`, `next`, `rest`, `seq`, `cons`, `contains?`,
arity-3 `assoc`) were tried, turned out to be dead code (those fns reach call
sites as sci vars, never as raw fn objects), and were removed again; the
pre-existing dead `not` entry was removed along the way. Removal measurably
cost nothing (seq-first-next 26 µs isolated, at the low end of all runs).

Semantics notes: `Util/equiv` *is* `=`; `RT/booleanCast` matches
`(boolean 0)` → `true`; user redefinitions still win because dispatch is on fn
identity resolved at analysis time (verified: `(defn = ...)` in script hits the
user fn).

### 3. Specializing the general path (nested-call arguments)

Previously the static-call dispatch only fired when args were
bindings/constants; `(+ (fib (- n 1)) (fib (- n 2)))` paid `IFn.invoke` on `+`.
Now the general path (args evaluated via `t/eval`) also dispatches to the
static call at arities 1–3.

- fib-25: **−6–8%**
- also contributes to eq-rem-loop (the `(= 0 (rem i 3))` node)

### 4. Factory-map dispatch (enabler, not a win by itself)

Growing the `condp identical?` chains to ~40 fns × 3 arg-shape paths regressed
**analysis +43%** (21 → 30 µs): `return-call` is one generated method, and the
chains both grew it toward the JIT's 8000-bytecode `DontCompileHugeMethods`
limit and made every call-node creation walk them linearly. Replaced with
identity-keyed maps def'd once (`{fn-object (fn [idxs/args stack] node)}` —
fn objects hash by identity, so `get` is O(1); each factory compiles to its own
small method; `return-call` lands at 6191 bytes, still JIT-compiled).

- analyze-defn: **flat vs baseline** (19.9–21.0 µs) with all tables in place
- items 1–3 at this table size are not shippable without this

## Safety: redefinition semantics unchanged

Dispatch is `identical?` on the fn object the analyzer already resolved, so
specialization can only fire where analysis had committed to a concrete host
fn anyway. The gate is `copy-vars/inlined-vars`: exactly the clojure.core vars
that carry `:inline` metadata (`+`, `=`, `get`, `count`, `zero?`, bit ops, ...).
Only those get the raw fn baked into call nodes via `:sci.impl/inlined` meta in
`analyze-call`; every other core fn (`assoc`, `first`, `seq`, `contains?`, ...)
is invoked through its sci var and sees redefinitions. This mirrors Clojure
itself, where `:inline` ops compile inline and ignore var mutation at call
sites while everything else respects it.

Verified per case, master and branch behaving identically:

- Default SCI: built-in core vars are read-only; `with-redefs` and
  `alter-var-root` on them throw. Nothing to interact with.
- Under `sci.impl.unrestrict/*unrestricted*` (babashka): `alter-var-root` on
  `=` (inlined set) is invisible at call sites on master too — analysis-time
  baking, not this change. `alter-var-root` on `assoc`/`first`/`not` (outside
  the set) is honored on both. Value position (`(reduce + ...)`) goes through
  the var and sees redefs on both.
- Script-level `(defn = ...)` or `:namespaces {'clojure.core {'+ my-plus}}`
  resolve to the user fn, the identity check misses, general invoke runs the
  user fn (tested).
- `with-redefs` on user vars: call sites invoke through the sci Var (deref per
  call), f is never a raw host fn there, so never specialized.

Extending `inlined-vars` itself (e.g. baking `first`/`assoc` for more speed)
would change observable redef semantics under `*unrestricted*` and diverge from
Clojure's `:inline` line — deliberately not done here.

## Bundle size (CLJS)

shadow-cljs release build (`npx shadow-cljs release sci`, advanced
optimizations): 884,331 → 893,064 bytes raw (**+8,733, +1.0%**), 197,695 →
198,438 gzipped (**+743, +0.4%**). The build report attributes all growth to
`analyzer.cljc` (63,380 → 72,096 optimized bytes): each factory entry is a
real closure in the bundle. Trimming the tables to `inlined-vars` (see Safety)
already removed the dead weight; further table growth should be weighed
against this.

## Non-effects

- loop-1m: flat — `inc`/`dec`/`pos?`/`zero?` on plain bindings were already
  fused (ADR 0006 era tables). A tight `(inc i)`/`(dec j)` loop shows nothing,
  by design.
- Analysis speed: flat (item 4).
- All 371 JVM tests pass on Clojure 1.10.3/1.11.1/1.12; clj-kondo clean vs
  master.

## Broader measurement: library loading in babashka (JVM mode)

Profiled real lib workloads through `babashka.main/main` in-process (sci
submodule on this branch, lib-tests classpath, fresh env per iteration by
resetting `babashka.main/env`, clj-async-profiler at 1ms):

- Loading 8 libs (honeysql, honeysql helpers, loom.graph, loom.alg, medley,
  aero, camel-snake-kebab, version-clj) into a fresh env: **~52 ms median**.
  App-sample breakdown: **analysis 38%, sci infra (resolve/vars/load) 20%,
  parse (edamame) 18%, bb infra 16%, eval 9%**. `analyze-call` is on-stack in
  58% of load samples. Within bb infra, ~8% of all samples are `stat` syscalls
  (`File.exists` classpath probing in the load-fn) — a babashka-side caching
  candidate, not sci.
- Re-running those libs' test suites: **eval 69%** of app samples.
- A/B this branch vs master on those workloads: **flat** — load 52 vs 50 ms
  median, test run 15 vs 15 ms. Expected: load time is analysis+parse-bound
  (this change keeps analysis flat by design), and these suites are
  string/collection-churn, not the arithmetic/`get`/`count` shapes the fused
  paths accelerate.

Implication: the fused-call work pays off on compute-style interpreted code
(loops, arithmetic, data access), not on lib load. The next frontier for load
time is the analyzer itself — hot frames there are `PersistentArrayMap`/
`PersistentHashMap` ops from per-node stack maps
(`(assoc m :ns @current-ns :file @current-file :sci.impl/f-meta ...)`) and
megamorphic dispatch (itable stubs) — plus edamame and the bb classpath stat
churn.

## If introduced to master per item

Item 4 must land first (or be bundled): items 1–3 without it regress analysis.
Suggested order by value: 4 → 2 → 1 → 3, or 4 then the rest as one change —
they touch the same ~150 lines of `gen-return-call` and separate cleanly only
as table entries, not as mechanism.

CLJS parity was kept (tables map to `cljs.core` fns; direct arity-invoke
instead of `f.call`); node tests pass at both `:none` and `:advanced`
(364 tests, 1260 assertions, 0 failures).
