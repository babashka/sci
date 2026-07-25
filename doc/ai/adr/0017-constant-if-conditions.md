# ADR 0017: fold constant `if` conditions, and why the other fusions were rejected

Status: accepted (2026-07-25).

## Context

`return-call` has, since ADR 0006, inspected its analyzed children at analysis
time: a `BindingNode` child is emitted as a direct `(aget bindings idx)` and a
constant is inlined, so the child's `t/eval` call disappears from the parent's
body. ADR 0009 extended this to specialized call shapes. The optimization
exists only in `gen-return-call`, and only at arities 1 and 2.

A comparison with [jolt](https://github.com/jolt-lang/jolt), which fuses
uniformly because it emits source rather than closures, raised the question of
generalizing the same trick to the other consumers of analyzed children:
`return-if`'s condition, `return-recur`'s arguments, keyword calls, `let`
binding values, `return-do` children.

Three of those were implemented and measured. One survived.

## Decision

Fold a constant `if` condition at analysis time. Reject the rest.

## The bug the fold fixes

`return-if` already had constant-condition handling:

```clojure
(cond (not condition) nil
      (constant? condition) then
      :else <if node>)
```

Both tests are applied to the *analyzed* child, and `utils/constant?` answers
for raw values (`nil?`, `number?`, `string?`, `keyword?`, `boolean?`,
`Pattern`). On CLJS and cljd `->constant` is the identity, so an analyzed
constant is still a raw value and both tests work. On the JVM `->constant`
wraps in `ConstantNode`, which is neither `nil` nor `constant?` — so **neither
test ever matched on the JVM** and the fold silently did not happen there.

```
(if true :y :n)   master, JVM:  analyzer$return_if$reify__4570
                  master, CLJS: folded
```

The replacement asks the question in a platform-correct way:

```clojure
(defn const-node? [x]
  #?(:cljd (not (t/eval-node? x))
     :clj (or (instance? sci.impl.types.ConstantNode x)
              (not (instance? sci.impl.types.Eval x)))
     :cljs (not (t/eval-node? x))))
```

The `not (instance? Eval)` arm on the JVM is not redundant with the
`ConstantNode` arm. `analyze` returns some values unwrapped (a resolved class,
a symbol, a constant collection), and those are evaluable only because
`sci.impl.evaluator` extends `Eval` to `Object` and `nil`. They are constants
too, so the fold now also covers `(if 'x ...)`, `(if String ...)` and
`(if [] ...)` — on the JVM and, as a small widening, on CLJS.

Falsey-first ordering is preserved: `(when (const-node-val condition) then)`
for the 2-arity case, `(if (const-node-val condition) then else)` for 3.

## Why it matters beyond literal `(if true ...)`

`cond` expands to nested `if`, and its final clause becomes `(if :else expr
nil)`. Every `cond` with an `:else` was evaluating a constant truthiness test
at runtime on the JVM. That whole node now disappears at analysis.

## Measurements

JVM, best of 3, each run in its own JVM, master and branch interleaved,
10k-iteration loops, analysis excluded (a `(fn [] ...)` is analyzed once via
`eval-string` and the returned fn is benched):

| workload | master | folded | |
|---|---|---|---|
| `(if true (inc acc) acc)` in a loop | 0.121 ms | 0.074 ms | −39% |
| `cond` with `:else` in a loop | 0.422 ms | 0.374 ms | −11% |
| no constant condition (control) | 0.158 ms | 0.164 ms | flat |

Frequency in real code, counted by instrumenting the analyzer and running 10
library test namespaces under babashka (medley, aero, camel-snake-kebab,
version-clj, clj-yaml, better-cond, data.csv, honeysql, expound — 144 tests,
641 assertions):

```
if nodes analyzed: 1768   constant-folded: 68 (4%)   condition is a local: 798 (45%)
keyword calls:      308   target is a local: 262 (85%)
```

Bundle size (shadow-cljs `release`, `:node-library`): 212547 B gzip against
master's 212915, i.e. 368 B smaller — the three-branch `cond` in `return-if`
became a two-branch `if`.

## Rejected: `recur` argument fusion

Each `recur` argument is `t/eval`'d into a temporary before the `aset`. The
experiment captured a per-argument boolean at analysis time and emitted
`(if bnd (aget bindings idx) (t/eval node ctx bindings))`.

It is a net loss. Fusable arguments save a dispatch, but every non-fusable
argument pays an added flag test, and non-fusable is the common case
(`(recur (inc i) (conj acc x))`).

| workload | master | with recur fusion |
|---|---|---|
| control, no fusable recur arg | 0.160 ms | 0.169 ms (+5.8%) |
| loop with 4 pass-through binding args | 0.119 ms | 0.116 ms (−2.5%) |

Removing the recur hunk returned the control to +1.4%, i.e. noise, which
confirmed the regression was entirely its own. A guarded variant (emit the
flagged code only when some argument is a binding) would double the generated
code of `gen-return-recur`, which is arity-unrolled 1–19, to buy 2.5% in the
good case. Not taken.

## Rejected: keyword call fusion

`(:k m)` and `(:k m default)` analyze to their own node outside `return-call`
and always `t/eval` the target. Fusing a `BindingNode` target fires on 85% of
keyword calls in real library code and is worth about 2% on a
keyword-dominated loop — the map lookup itself dominates, not the dispatch.
Fifteen lines of duplicated node construction for that was judged not worth
it.

## Why the fusions underperformed

`t/eval` is a protocol call, and the ADR 0010 framing of it as a megamorphic
cost centre is only true where a call site actually sees many node types. In a
hot loop the `if` condition slot or the recur argument slot usually sees one
node type, C2 devirtualizes it, and replacing it with an array read saves
close to nothing. The constant fold wins because it removes the node
altogether rather than making its dispatch cheaper.

The frequency data above sharpens the same point. 45% of `if` nodes in real
library code test a local and would fuse, yet fusing them moved a full
babashka lib-test run by 0.2% (4675 ms → 4664 ms, best of 5) and a hot loop
over real library code (camel-snake-kebab, medley, honeysql, version-clj,
data.csv) by 1.3% (473 ms → 467 ms). Real library code spends its time in seq
and map operations, string building, multimethod dispatch and host interop;
the interpreter's own dispatch is a thin slice of it.

Note also the noise floor: the micro workloads drift ±10% between sessions
even at best-of-3 with isolated JVMs. `if-local` measured both faster and
slower than master across rounds in which the two trees ran provably identical
code. Single-digit percentages on these benchmarks are not evidence.

## Consequences

- Constant `if` conditions now fold on all platforms, and on the JVM they fold
  for the first time.
- `const-node?` and `const-node-val` are available for other consumers that
  want an analysis-time constant, and are the platform-correct way to ask.
- The fused-child trick stays confined to `return-call`. Extending it to more
  consumers is not, on this evidence, a productive direction; the remaining
  interpreter wins are in not interpreting (ADR 0014 for CLJS, ADR 0016 for
  the JVM), not in shaving dispatch off the interpreter.

## Verification

- `script/test/jvm`: Clojure 1.10.3 and 1.11.1, 373 tests, 1390 assertions, 0
  failures.
- `script/test/node`: `:none`, `:advanced`, and jit forced off, 402 tests,
  5689 assertions, 0 failures each.
- babashka lib tests for 10 namespaces against the branch: 144 tests, 641
  assertions, 0 failures.
- Constant conditions of every truthiness (`0`, `""`, `:k`, `nil`, `false`,
  `[]`, `{}`, a class, a quoted symbol), 2-arity and 3-arity, side effects in
  the untaken branch: identical results to master.
- clj-kondo over `src`: findings identical to master.
