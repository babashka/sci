# ADR 0006: Fused Binding Nodes for Inlined Function Calls

| Status | Date | Related |
|--------|------|---------|
| Accepted | 2026-03-06 | Performance optimization for tight loops |

## Context

SCI evaluates code by walking a tree of nodes, each implementing the `Eval`
protocol. For a call like `(inc i)` where `inc` is an inlined core function and
`i` is a local binding, two nodes are created during analysis:

1. A **call node** whose eval method does `(clojure.core/inc (t/eval arg0 ctx bindings))`
2. A **binding node** (anonymous `reify`) whose eval method does `(aget bindings idx)`

At runtime, evaluating `(inc i)` requires two protocol dispatches: one for the
call node, then one for the binding node. On HotSpot, monomorphic dispatch sites
get devirtualized by the JIT, so the overhead is modest (~22%). On GraalVM
native-image (babashka), there is no JIT — every dispatch is a real vtable
lookup, making the overhead much larger (~34%).

In a tight loop like:

```clojure
(loop [i 0 j 10000000]
  (if (zero? j) i (recur (inc i) (dec j))))
```

There are 8 protocol dispatches per iteration: `if` node, `(zero? j)` call,
`j` arg, `recur` node, `(inc i)` call, `i` arg, `(dec j)` call, `j` arg.
Three of these are binding lookups that can be eliminated.

## Decision

Two changes that work together:

### 1. `BindingNode` deftype (types.cljc)

Replace anonymous `reify` classes for local binding lookups with a concrete
named `BindingNode` deftype (JVM only):

```clojure
(deftype BindingNode [^int idx _meta]
  Eval (eval [_ _ bindings] (aget ^objects bindings idx))
  ...)
```

Benefits:
- The `idx` field is inspectable at analysis time via `(.idx node)`
- A single concrete class helps the JIT optimize dispatch vs many anonymous classes
- Supports metadata (IObj/IMeta) for type tag propagation

### 2. Fused call nodes (analyzer.cljc)

In `gen-return-call` and `gen-return-binding-call`, at analysis time, check if
all arguments are `BindingNode` instances. If so, generate a fused node that
reads directly from the bindings array:

```clojure
;; Before (two dispatches per arg):
(f (t/eval arg0 ctx bindings))        ;; dispatch on call node
;;   └─ (aget bindings idx)           ;; dispatch on binding node

;; After (one dispatch total):
(f (aget ^objects bindings idx0))      ;; dispatch on call node only
```

The `instance? BindingNode` check happens once at analysis time. The resulting
node has zero extra runtime overhead — it either does direct `aget` (fused path)
or `t/eval` (general path), decided at analysis time.

For the non-wrap case in each arity:

```clojure
(if (and (instance? BindingNode arg0)
         (instance? BindingNode arg1) ...)
  ;; fused: all args are direct aget
  (let [bidx0 (.idx ^BindingNode arg0) ...]
    (->Node (try (f (aget ^objects bindings bidx0) ...) (catch ...)) stack))
  ;; general: go through t/eval
  (->Node (try (f (t/eval arg0 ctx bindings) ...) (catch ...)) stack))
```

Growth is linear per arity (one extra branch), not exponential. Mixed cases
(some bindings, some expressions) fall back to the general path — acceptable
since the common hot-loop patterns (`(inc i)`, `(dec j)`, `(zero? j)`,
`(+ x y)`) have all-binding arguments.

## Alternatives Considered

### Per-arg optimization (rejected)

Check each argument independently and generate 2^n combinations for n args.

Rejected because of exponential code growth in the generated macro. The
all-or-nothing check covers the common patterns and keeps the macro simple.

### Runtime instance check in eval (rejected)

Add `(if (instance? BindingNode expr) (aget ...) (t/eval ...))` at every eval
call site at runtime.

Rejected because it adds a branch on every eval, even for non-binding nodes.
The analysis-time approach has zero runtime cost for the decision.

### Primitive type specialization (not pursued)

Track that loop bindings are numeric, use `long[]` arrays, operate on primitives
directly. Would eliminate boxing overhead (the remaining ~95% of the gap to
native Clojure).

Not pursued due to high complexity — requires type tracking through the
analyzer, parallel primitive arrays, and specialized recur handling. Could be
a future optimization.

## Benchmarks

`(loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))`

| Runtime | Before | After | Improvement |
|---------|--------|-------|-------------|
| bb native (GraalVM) | ~338ms | ~222ms | **34% faster** |
| JVM (HotSpot, warmed) | ~124ms | ~97ms | **22% faster** |
| Python 3.13 (reference) | ~520ms | — | bb is 2.3x faster |

The native-image improvement is larger because there is no JIT to compensate
for protocol dispatch overhead — every eliminated dispatch is a real vtable
call saved.

## Files Changed

- `src/sci/impl/types.cljc` — Added `BindingNode` deftype (JVM only)
- `src/sci/impl/resolve.cljc` — Use `BindingNode` for local binding lookups
- `src/sci/impl/analyzer.cljc` — Fused path in `gen-return-call` and `gen-return-binding-call`
