# ADR 0001: Stack Trace at Point of Execution

| Status | Date | Related |
|--------|------|---------|
| Rejected | 2026-02-02 | [Issue #774](https://github.com/babashka/sci/issues/774) |

## Context

Users requested the ability to get a stack trace at the point of execution for debugging purposes, without needing to throw an exception. This would complement the existing `sci/stacktrace` function which only works with caught exceptions.

## Problem

Users want to get the current call stack at the point of execution for debugging purposes, without needing to throw an exception. When catching exceptions with `^:sci/error`, only the immediate error frame is captured, not the full call chain.

## Why Runtime Tracking is Necessary

Function bodies in SCI are analyzed once at definition time, not re-analyzed at each call site. This means:
- At analysis time, we don't know what functions will call a given function
- The actual call chain is only known at runtime
- Exception stacks work by unwinding during propagation, but `current-stacktrace` needs the stack *before* any exception

## Solution: Runtime Stack Tracking with Minimal Overhead

### Design Goals
1. Zero overhead when tracking is disabled (default)
2. Modest overhead (~3-4%) when tracking is enabled
3. Reuse existing stack frame infrastructure from exception handling

### Implementation

**1. Dynamic var for call stack** (`src/sci/impl/utils.cljc`):
```clojure
(def ^:dynamic *call-stack* nil)

(defn push-call-stack! [frame]
  (when *call-stack*
    (vswap! *call-stack* conj frame)))

(defn pop-call-stack! []
  (when *call-stack*
    (vswap! *call-stack* pop)))

(defn get-call-stack []
  (when *call-stack*
    @*call-stack*))
```

**2. Push/pop in function calls** (`src/sci/impl/analyzer.cljc` `gen-return-call` macro):
```clojure
;; Wrap function call with push/pop:
(sci.impl.types/->Node
  (do
    (utils/push-call-stack! ~'stack)
    (try
      (~'f ~@args)
      (catch ... e#
        (rethrow-with-location-of-node ...))
      (finally
        (utils/pop-call-stack!))))
  ~'stack)
```

**3. API function** (`src/sci/core.cljc`):
```clojure
(defn current-stacktrace
  "Returns the current call stack at the point of invocation.
   Returns a list of stack frame maps with keys :ns, :name, :file, :line, :column.
   Only works when *call-stack* is bound (via binding)."
  []
  (when-let [stack (utils/get-call-stack)]
    (cs/stacktrace (volatile! stack))))
```

### Usage

Users must:
1. Expose `sci.core/current-stacktrace` to their SCI context
2. Bind `utils/*call-stack*` to `(volatile! [])` to enable tracking

```clojure
(require '[sci.core :as sci]
         '[sci.impl.utils :as utils])

(let [ctx (sci/init {:namespaces {'my.debug {'stacktrace sci/current-stacktrace}}})]
  (binding [utils/*call-stack* (volatile! [])]
    (sci/eval-string* ctx "(defn foo [] (my.debug/stacktrace)) (foo)")))
```

### Performance

#### Initial benchmarks (eval-string overhead dominates)

Benchmarked with `(defn outer [] (defn inner [] :done) (inner)) (outer)` × 100k via `sci/eval-string*`:

| Mode | Time | Overhead |
|------|------|----------|
| Master baseline | ~6564ms | - |
| With nil check (disabled) | ~6538ms | ~0% |
| With tracking enabled | ~6803ms | ~3.6% |

#### Tight loop benchmarks (isolates actual overhead)

Function call test (10M iterations):
```bash
# push/pop commented out (noop):
$ clj -M:babashka/dev -e '(defn foo [] 1) (time (dotimes [i 10000000] (foo)))'
"Elapsed time: 174.169625 msecs"

# with push/pop implementation:
$ clj -M:babashka/dev -e '(defn foo [] 1) (time (dotimes [i 10000000] (foo)))'
"Elapsed time: 2083.561333 msecs"
```
**Result: ~12x slower**

Loop test (10M iterations):
```bash
# push/pop commented out (noop):
$ clj -M:babashka/dev -e '(time (let [x 0 j 10000000] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))))'
"Elapsed time: 180.404709 msecs"

# with push/pop implementation:
$ clj -M:babashka/dev -e '(time (let [x 0 j 10000000] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))))'
"Elapsed time: 3040.98 msecs"
```
**Result: ~17x slower**

#### ThreadLocal + ArrayList approach (10M iterations)

Replaced dynamic var + volatile/vector with ThreadLocal + mutable ArrayList:

```bash
# ThreadLocal/ArrayList with push/pop nooped (just ThreadLocal.get overhead):
$ clj -M:babashka/dev -e '(time (let [x 0 j 10000000] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))))'
"Elapsed time: 654.413333 msecs"
```
**Result: ~3.6x slower than baseline** (just from ThreadLocal.get() calls, without any actual push/pop)

#### Lazy binding + closure chain approach (10M iterations)

Use `binding` with a chain of lazy functions - stack only built when requested:

```clojure
(def ^:dynamic *stack-fn* (fn [] []))

;; At runtime:
(binding [*stack-fn* (let [parent *stack-fn*]
                       (fn [] (conj (parent) frame)))]
  ...)
```

```bash
$ clj -M:babashka/dev -e '(time (let [x 0 j 10000000] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))))'
"Elapsed time: 3059.710041 msecs"
```
**Result: ~17x slower** - closure allocation on every call negates any laziness benefit

#### Pre-created wrapper + binding approach (10M iterations)

Pre-create the wrapper function at analysis time to avoid runtime closure allocation:

```clojure
;; Analysis time:
(let [wrap-fn (fn [parent-fn] (fn [] (conj (parent-fn) frame)))]
  ;; Runtime:
  (binding [*stack-fn* (wrap-fn *stack-fn*)] ...))
```

Still creates inner closure at runtime when `wrap-fn` is called.

#### Linked list approach (10M iterations)

Use a linked structure `[frame, parent]` instead of functions:

```clojure
(def ^:dynamic *stack* nil)

;; Runtime:
(binding [*stack* [frame *stack*]] ...)

;; Only walk when needed:
(defn get-call-stack []
  (loop [node *stack*, result []]
    (if node
      (recur (second node) (conj result (first node)))
      result)))
```

```bash
$ clj -M:babashka/dev -e '(time (let [x 0 j 10000000] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j))))))'
"Elapsed time: ~3000 msecs"
```
**Result: ~17x slower** - still allocating a 2-element vector per call

### Summary of approaches

| Approach | Overhead | Notes |
|----------|----------|-------|
| Baseline (no tracking) | ~180ms | - |
| Dynamic var + volatile/vector | ~3040ms (17x) | push/pop on every call |
| ThreadLocal + ArrayList (noop) | ~654ms (3.6x) | just ThreadLocal.get() overhead |
| Binding + lazy closures | ~3060ms (17x) | closure allocation per call |
| Linked list with binding | ~3000ms (17x) | vector allocation per call |

### Why JVM stack walking won't work

The alternative of walking the JVM stack when `current-stacktrace` is called (zero runtime overhead) was considered but is not feasible:

1. **No way to map JVM frames → SCI frames**: All `->Node` instances use the same `reify` class, so we can't distinguish them by class name in the JVM stack trace.

2. **Stack frame info is in node instances**: The `{:ns :name :line :column}` metadata is stored in node instances, which aren't visible from JVM stack frames.

3. **Would require bytecode generation**: To make this work, we'd need to generate unique classes per call site at analysis time, encoding location info in class names. This is complex and probably slow.

4. **Not portable**: JVM-specific solution wouldn't work for ClojureScript.

### Conclusion

**Runtime call stack tracking is not feasible without significant performance overhead.**

The fundamental problem: any per-function-call overhead (even just a ThreadLocal.get()) accumulates to unacceptable slowdowns in tight loops. The exception-based stack building works because it only pays the cost during error unwinding, not during normal execution.

## Status

**Feature not implementable with acceptable performance.**

Possible compromises (not implemented):
1. **Accept the overhead** - Users who need this feature can opt-in knowing the ~17x slowdown in tight loops
2. **Lexical stack only** - Provide analysis-time stack (function definition nesting) instead of runtime call stack
3. **Sampling profiler** - Periodically sample the JVM stack for debugging, not exact stacks
