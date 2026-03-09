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

### Phase 1: Fused binding nodes (BindingNode + direct aget)

| Runtime | Before | After | Improvement |
|---------|--------|-------|-------------|
| bb native (GraalVM) | ~338ms | ~222ms | **34% faster** |
| JVM (HotSpot, warmed) | ~124ms | ~97ms | **22% faster** |
| Python 3.13 (reference) | ~520ms | — | bb is 2.3x faster |

### Phase 2: Specialized inlined calls + constant arg fusing + resolved fallback

Adds three optimizations on top of Phase 1:

1. **Specialized calls**: For known functions (`inc`, `dec`, `+`, `-`, etc.),
   call the underlying static method directly (e.g. `Numbers/inc`) instead of
   going through `IFn.invoke`.
2. **Constant arg fusing**: The "resolved" tier handles mixed binding+constant
   args (e.g. `(+ x 1)`) without `t/eval` dispatch.
3. **Resolved fallback**: Even for non-specialized functions, the resolved tier
   avoids `t/eval` when all args are bindings or constants.

`(loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))`

| Runtime | Fused only | + Phase 2 | Improvement |
|---------|-----------|-----------|-------------|
| bb native (GraalVM) | ~222ms | ~191ms | **14% faster** |

`(loop [i 0 j 10000000] (if (pos? j) (recur (+ i 10) (- j 1)) i))`

| Runtime | Fused only | + Phase 2 | Improvement |
|---------|-----------|-----------|-------------|
| bb native (GraalVM) | ~381ms | ~285ms | **25% faster** |

### Cumulative improvement

`(loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))`

| Runtime | Original | Final | Total improvement |
|---------|----------|-------|-------------------|
| bb native (GraalVM) | ~338ms | ~191ms | **43% faster** |
| Python 3.13 (reference) | ~520ms | — | bb is 2.7x faster |

The native-image improvement is larger because there is no JIT to compensate
for protocol dispatch overhead — every eliminated dispatch is a real vtable
call saved.

## Files Changed

- `src/sci/impl/types.cljc` — Added cross-platform `BindingNode` deftype and `eval-node?` helper
- `src/sci/impl/resolve.cljc` — Use `BindingNode` for local binding lookups
- `src/sci/impl/analyzer.cljc` — Fused path in `gen-return-call` and `gen-return-binding-call`; specialized `condp` for known functions
- `src/sci/impl/evaluator.cljc` — Use `eval-node?` for node detection

### `eval-node?` helper

Introducing `BindingNode` as a new node type means any code that checks
"does this value need evaluation?" must know about it. Previously on CLJS this
was `(instance? NodeR x)`, and on CLJ `(instance? sci.impl.types.Eval x)`.
Rather than adding `or` checks everywhere, a single `eval-node?` function in
`types.cljc` centralizes this:

```clojure
(defn eval-node? [x]
  #?(:clj (instance? sci.impl.types.Eval x)
     :cljs (or (instance? NodeR x)
               (instance? BindingNode x))))
```

Note: on CLJ, the dotted class form `sci.impl.types.Eval` must be used, not the
protocol var `Eval` — the var holds the protocol map, not the Java interface.

### Specialized inlined calls

For known core functions, the fused path can go further: instead of calling
through `IFn.invoke` (e.g. `(f (aget bindings idx))`), emit the underlying
static method directly (e.g. `(Numbers/inc (aget bindings idx))`).

The function mappings are defined once as data:

```clojure
spec-fns-1  ;; 1-arg: {clojure.core/inc -> Numbers/inc, clojure.core/dec -> Numbers/dec, ...}
spec-fns-2  ;; 2-arg: {clojure.core/+ -> Numbers/add, clojure.core/- -> Numbers/minus, ...}
```

A `gen-specs` helper takes a mapping, an arity, and an arg-accessor function,
and generates `condp identical?` entries. The `condp identical?` runs at
analysis time — comparing `f` (the actual function object from
`:sci.impl/inlined`) against known functions. Zero runtime cost; it just
selects which `->Node` reify to create.

Specialization is applied in the **fused** and **resolved** tiers only (where
args are already resolved to direct array access or constants). For the general
tier (expression args), the overhead of `t/eval` dispatch dominates, so
specializing `f` gives negligible benefit — the code uses plain `(f ...)` instead.

### Constant arg fusing (the "resolved" path)

Consider `(+ x 1)` where `x` is a loop binding. After analysis:
- `arg0` = a `BindingNode` (for `x`)
- `arg1` = a `ConstantNode` (for `1`) — on CLJ; raw `1` on CLJS

**Without constant fusing**, this doesn't pass `all-bindings?` (because `arg1`
is not a BindingNode), so it falls to the general path:

```clojure
;; At eval time (every iteration):
(Numbers/add (t/eval arg0 ctx bindings)    ;; protocol dispatch → aget
             (t/eval arg1 ctx bindings))   ;; protocol dispatch → returns 1
```

Two `t/eval` dispatches per call, just to get `x` and `1`.

**With constant fusing**, there is a middle tier called "resolved". An arg is
"resolved" if it is either a `BindingNode` or a constant — meaning its value
can be determined at analysis time without `t/eval` at runtime.

At **analysis time** (runs once, when SCI analyzes user code), the resolved
path extracts what it needs from each arg:

```clojure
(let [bnd0 (instance? BindingNode arg0)   ;; true  (x is a binding)
      rv0  (if bnd0
             (.idx arg0)                   ;; → 0 (the binding's array index)
             (.x arg0))                    ;; not taken
      bnd1 (instance? BindingNode arg1)   ;; false (1 is a constant)
      rv1  (if bnd1
             (.idx arg1)                   ;; not taken
             (.x arg1))]                   ;; → 1 (the constant value)
  ;; bnd0=true, rv0=0, bnd1=false, rv1=1 are captured in the node's closure.
  (->Node
    (Numbers/add
      (if bnd0 (aget bindings rv0) rv0)   ;; bnd0=true → (aget bindings 0)
      (if bnd1 (aget bindings rv1) rv1))  ;; bnd1=false → 1
    stack))
```

At **eval time** (every iteration), the node runs:

```clojure
(Numbers/add
  (if true (aget bindings 0) 0)    ;; → (aget bindings 0), gets x
  (if false (aget bindings 1) 1))  ;; → 1
```

The `if` on a captured boolean is essentially free — the branch predictor
always gets it right. No `t/eval` dispatches at all.

### Three tiers of arg resolution

The decision tree runs at **analysis time** (once per call site):

```
Analyzing (+ x 1):
├── All args are BindingNode?  (e.g. (+ x y))
│   └── Fused path: (Numbers/add (aget bindings idx0) (aget bindings idx1))
│       Zero dispatches. Direct array access.
│
├── All args are "resolved"?  (each is BindingNode OR constant, e.g. (+ x 1))
│   └── Resolved path: (Numbers/add (if bnd0 (aget ..) rv0) (if bnd1 (aget ..) rv1))
│       Zero dispatches. One cheap boolean branch per arg.
│
└── Some arg is an expression?  (e.g. (+ x (inc y)))
    └── General path: (f (t/eval arg0 ..) (t/eval arg1 ..))
        One protocol dispatch per arg. No function specialization.
```

| Tier | When | Runtime cost per arg | Example |
|------|------|---------------------|---------|
| Fused | All args are bindings | `aget` (direct) | `(+ x y)` |
| Resolved | All args are bindings or constants (arity 2+ only) | `if bool` + `aget` or constant | `(+ x 1)` |
| General | Some arg is an expression | `t/eval` dispatch | `(+ x (inc y))` |

The resolved tier with function specialization (`condp`) is only generated for
arity 2+. For arity 1, constant arguments get folded at analysis time (e.g.
`(inc 1)` becomes `2`), so the resolved path would be dead code.

In the fused and resolved tiers, when `f` is not a known specialized function
(not in `spec-fns`), the node still benefits from direct `aget` / constant
access — it just calls through `(f ...)` instead of the static method.

## Future Work

Potential further fusing optimizations to explore:

- **Interop call args**: `(.method obj binding1 binding2)` — interop calls
  evaluate their args via `t/eval` just like regular calls. Fusing BindingNode
  args to direct aget could help, especially for interop-heavy code (e.g.
  instaparse). See `analyze-dot` and `invoke-instance-method` in `analyzer.cljc`.

- **Recur args**: `(recur (inc i) (dec j))` — the recur node evaluates each arg
  via `t/eval`. When a recur arg is a simple binding (e.g. `(recur j i)` for
  swap), fusing the aget would save dispatches. Note: most recur args are
  expressions like `(inc i)`, not bare bindings.

- **Var deref nodes**: `(inc i)` where `inc` is a core var — the var is derefed
  at analysis time via the inlining mechanism, but non-inlined var calls still
  go through `(deref v)` at runtime. A `VarNode` deftype could help the JIT.

- **Primitive type specialization for loop bindings**: Track that loop bindings
  are numeric, use `long[]` arrays, operate on primitives directly. Would
  eliminate boxing overhead (the remaining ~95% of the gap to native Clojure).
  High complexity.

- **Bytecode VM for loop bodies (rejected)**: A stack-based bytecode VM was
  prototyped to replace the node tree for loop bodies with a flat opcode array,
  using `case` dispatch (tableswitch) instead of protocol dispatches. However,
  the stack machine overhead (aget/aset for every push/pop, plus separate case
  dispatches for opcode type and function ID) exceeded the savings from replacing
  vtable lookups with tableswitch. The fused node approach already eliminates the
  most expensive dispatches (binding arg lookups), leaving only ~5 dispatches per
  loop iteration — each doing minimal work. The bytecode VM replaced those 5
  with 13+ case dispatches plus stack operations, resulting in ~28% slower
  execution. A register-based VM or closure compilation might fare better but
  would be significantly more complex.
