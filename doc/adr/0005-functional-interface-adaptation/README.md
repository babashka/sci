# ADR 0005: Functional Interface Adaptation for Instance Targets

| Status | Date | Related |
|--------|------|---------|
| Accepted | 2026-02-16 | Clojure 1.12 FI support, cloverage babashka compatibility |

## Context

Clojure 1.12 supports functional interface (FI) adaptation: passing a Clojure function where a Java functional interface is expected. SCI already handled this for **method arguments** via `box-arg` in `reflector.cljc` (e.g., `(.sort list even?)` where `even?` is adapted to a `Comparator`).

However, FI adaptation was missing for the **instance target** case:

```clojure
(let [^java.util.function.Predicate p even?]
  (.test p 42))
```

Here `p` is the method target/receiver, not an argument. The existing `box-arg` path in `invoke-matching-method` only adapts arguments, not the target.

## How Clojure Handles This

Decompiling the bytecode (`clj-java-decompiler`) reveals Clojure adapts at the **let binding** site (`Compiler.java` line 7072):

```java
// Compiler.java, LetExpr.emit():
Class bindingClass = HostExpr.maybeClass(bi.binding.tag, true);
if(!FISupport.maybeEmitFIAdapter(objx, gen, bi.init, bindingClass))
    bi.init.emit(C.EXPRESSION, objx, gen);
```

Emitted bytecode:
```java
if (o instanceof IFn && !(o instanceof Predicate)) {
    rawRoot = FnInvokers::invokeOO;  // invokedynamic adapter
}
```

Key points:
- FI detection (`maybeFIMethod`) happens at compile time
- The `instanceof` guard runs at the let binding (once per bind)
- Subsequent method calls on the binding hit a real `Predicate` — no per-call overhead
- This is specifically a **let binding** feature; fn argument type hints do NOT trigger FI adaptation in Clojure

## Options Considered

### Option A: Runtime adaptation in `invoke-instance-method` (rejected)

Add an `isInstance` + `box-arg` check in `invoke-instance-method` before method dispatch.

```clojure
(let [obj (if (and (not (.isInstance target-class obj))
                   (instance? clojure.lang.IFn obj))
            (reflector/box-arg target-class obj)
            obj)
      ...]
```

**Pros:**
- Simple, localized change
- Handles all cases (let, fn args, inline hints)

**Rejected because:**
- Adds an `isInstance` check on **every** instance method call
- Benchmarked ~40% slower than bind-time adaptation with multiple calls per binding (140ms vs 100ms for 5 `.test` calls, 100k iterations)
- Diverges from Clojure's approach

### Option B: Analysis-time adaptation at let binding (rejected)

In `analyze-let*`, resolve the binding's tag, check `maybe-fi-method`, and wrap the binding value node with FI adaptation.

```clojure
;; In analyze-let*, after analyzing binding value:
fi-class (when binding-tag
           (let [c (interop/resolve-class ctx binding-tag)]
             (when (and c (reflector/maybe-fi-method c))
               c)))
v (if fi-class
    (->Node (let [val (eval v ctx bindings)]
              (if (and (instance? IFn val) (not (instance? fi-class val)))
                (box-arg fi-class val) val)) nil)
    v)
```

**Pros:**
- Matches Clojure's approach (adapt at bind time)
- Adapts once per binding, not per method call

**Rejected because:**
- Calls `resolve-class` + `maybe-fi-method` on **every tagged let binding** at analysis time, even non-FI tags like `^String`
- Benchmarked ~15% slower analysis for tagged bindings (53ms vs 50ms baseline, 10k evals)
- Redundant class resolution: `resolve-class` called at the binding and again at `resolve-tag-class` in method call analysis

### Option C: Analysis-time adaptation at method call site (chosen)

In `analyze-dot` / `expand-dot*`, after `resolve-tag-class` resolves the instance expression's tag to a class, check if it's a FI and wrap with adaptation.

```clojure
(defn- maybe-wrap-fi-adapter [instance-expr]
  (let [tag-class (-> instance-expr meta :tag-class)]
    (if (and (instance? Class tag-class) (reflector/maybe-fi-method tag-class))
      (let [orig instance-expr]
        (with-meta
          (->Node (let [val (eval orig ctx bindings)]
                    (if (and (instance? IFn val) (not (instance? tag-class val)))
                      (box-arg tag-class val) val)) nil)
          (meta instance-expr)))
      instance-expr)))
```

**Pros:**
- Zero overhead in `analyze-let*` — no `resolve-class` or `maybe-fi-method` for any binding
- Piggybacks on `resolve-tag-class` which already resolves the class at the method call site
- `maybe-fi-method` check happens at analysis time (once per call site), only cheap `instanceof` at eval time
- Benchmarked identical to baseline for non-FI tagged bindings (~50ms vs ~50ms, 10k evals)
- Bonus: also works for FI-hinted **function arguments**, which Clojure doesn't support

**Cons:**
- Per-call adaptation at eval time (vs once at bind time in Option B)
- Acceptable trade-off since FI-as-target is extremely rare

## Additional Changes

### Tag inference for let bindings

When a binding's value is another binding with a tag, the tag is propagated:

```clojure
(let [^Predicate p even?, q p] (.test q 42))
```

In `analyze-let*`, after analyzing the binding value, if the binding name has no tag but the analyzed value does (via `resolve/lookup`), the tag is propagated to the binding name. This matches Clojure's behavior (verified via decompilation: `((String)q).length()` is emitted without reflection).

### `maybe-fi-method` and `box-arg` made public in `reflector.cljc`

Changed from `defn-` to `defn` to allow use from `analyzer.cljc`.

## Benchmarks

Analysis time (10,000 repeated `eval` of `(let [^String s "foo", q s] (.length q))`), JVM, warmed:

| Approach | Time |
|----------|------|
| Baseline (no FI support) | ~50ms |
| **Usage-site (chosen)** | ~51ms |
| Let-binding (Option B) | ~53ms |

Runtime (100k iterations of `(let [^Predicate p even?] (.test p 42) (.test p 43) ... x5)`):

| Approach | Time |
|----------|------|
| Let-binding (Option B) | ~100ms |
| Runtime (Option A) | ~140ms |

## Decision

Option C — FI adaptation at the method call analysis site. Zero overhead for the common case, clean separation of concerns, and as a bonus enables FI adaptation for function arguments.
