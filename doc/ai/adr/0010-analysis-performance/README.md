# ADR 0010: Analysis/Load Performance

| Status | Date | Related |
|--------|------|---------|
| In progress | 2026-07-07 | ADR 0009 (broader measurement section); commit `fe58699c` on `perf` branch |

## Context

Loading a library in babashka is parse+analysis-bound, not eval-bound
(ADR 0009: analysis 38%, sci infra 20%, parse 18%, eval 9%), and bb is all
about startup time. Benchmark: `sci/eval-string*` of honeysql (sql.cljc +
helpers.cljc, ~2.5k lines) in a fresh ctx, JVM, criterium + clj-async-profiler
CPU and alloc profiles (`{:event :alloc}` ranks allocation sites directly).

Baseline: **8.0 ms** per load. CPU: analysis 35%, parse 31%, sci-infra 15%,
eval 3%.

## Done (commit `fe58699c`)

1. **Ctx record fields** for `recur-target`, `params`, `parents`,
   `closure-bindings`, `fn-expr` (opts.cljc). These were extmap keys; assoc
   with a non-field key copies the record AND rebuilds the extmap.
   `with/without-recur-target` fires several times per analyzed form
   (`analyze-children-tail`). Call sites unchanged — declaring the fields is
   the whole fix. ~2.5% wall clock; `Ctx.assoc` `Object[]` extmap churn gone
   from the alloc profile.
2. **parse-opts cache in `parse-next`** (parser.cljc). Every top-level form
   rebuilt the edamame options: 6-key assoc onto the Options record plus three
   fresh closures — the single biggest allocation site during loading (~38% of
   sci alloc samples). Now a single-entry cache keyed on identity of ctx,
   current-ns aliases map, readers, and `*read-eval*`; any `require`/`in-ns`
   changes the aliases map identity and invalidates. Wall-clock flat on JVM
   (TLAB allocs are cheap under C2) but the allocation is gone — matters more
   on native image (serial GC, no JIT).

Result: 8.0 → 7.8 ms JVM; total allocation during load down materially
(top alloc site eliminated). All test suites green.

## Measured but not done (ranked candidates)

1. **edamame bump** (~7% of load): sci pins 1.6.41; edamame HEAD has the
   flattened reader (2.26× parse in isolation) and Options-extmap fix
   (`f724fe8`). A/B with `:local/root` showed parse dropping 2.7 → 2.1 ms per
   load. Needs an edamame release, then a one-line dep bump.
2. **De-lazify hot analyzer helpers** (~3-5% est.): `analyze-let*`,
   `expand-fn-args+body`, `destructure` use `take-while`/`map`/lazy chains.
   On Clojure 1.12 every realized LazySeq also allocates a
   `ReentrantLock$NonfairSync` (visible in the alloc profile). Replace with
   eager loops/mapv.
3. **Lazy stack maps** (~3-5% est.): every call node eagerly builds
   `(assoc m :ns @current-ns :file @current-file :sci.impl/f-meta f-meta)`
   (multi-key assoc → ArraySeq + PAM copies). The `->Node` macro inlines its
   stack argument into the `stack` method body, so capturing the pieces and
   assembling the map inside the method would move all of it to the error
   path. Touches `gen-return-call` and every `->Node` stack site — moderate
   blast radius.
4. **`update-parents`/closure-bindings machinery**: nested `update-in`/vswap
   per binding identity; visible but diffuse.
5. **babashka-side, not sci**: classpath `File.exists` stat churn is ~8% of
   total load samples in bb (one stat per classpath entry per require).
   Caching directory listings in bb's load-fn is likely the single biggest
   *startup* lever outside sci.

## Non-goals

- Eval-side specialization (ADR 0009) does not move load time.
- Serializing analyzed ASTs (an "analysis cache" across processes) is not
  feasible: nodes are closures/reify instances.

## Measurement notes

- Fresh-ctx loads of real libs need `:features #{:clj :bb}` in `sci/init`
  opts — without features no reader-conditional branch matches and `(catch
  #?(:clj Throwable ...) ...)` parses into garbage ("Unable to resolve
  classname: t").
- honey.sql needs `java.util.Locale` + `Throwable`/`Exception` in `:classes`;
  medley needs host classes endlessly (Boolean, PersistentQueue, ...) — use
  honeysql + its helpers ns instead.
