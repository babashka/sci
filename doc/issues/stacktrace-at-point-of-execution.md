# Plan: Stack Trace at Point of Execution

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

Benchmarked with `(defn outer [] (defn inner [] :done) (inner)) (outer)` × 100k:

| Mode | Time | Overhead |
|------|------|----------|
| Master baseline | ~6564ms | - |
| With nil check (disabled) | ~6538ms | ~0% |
| With tracking enabled | ~6803ms | ~3.6% |

The nil check adds no measurable overhead. When tracking is enabled, overhead is ~3.6%.

### Files Modified

| File | Changes |
|------|---------|
| `src/sci/impl/utils.cljc` | Add `*call-stack*`, `push-call-stack!`, `pop-call-stack!`, `get-call-stack` |
| `src/sci/impl/analyzer.cljc` | Wrap function calls in `gen-return-call` with push/pop |
| `src/sci/core.cljc` | Add `current-stacktrace` function |
| `test/sci/error_test.cljc` | Add `current-stacktrace-test` |

## Status

POC implemented and tested. Performance validated.
