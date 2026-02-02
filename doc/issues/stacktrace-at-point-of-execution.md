As a follow up to [#774](https://github.com/babashka/sci/issues/774), it has
come up that users want to request a stack trace at the point of execution, just
for debugging / printing. To facilitate this we should pre-process the stack
instead of constructing during an exception such that you have it readily
available.

You can see the beginnings of it in this case: `(try (/ 1 0) (catch ^:sci/error
Exception e))`where the error has one stack element about `/` but none of the
preceding ones, which we can add during analysis time.

---

# Plan: Stack Trace at Point of Execution

## Problem

When catching exceptions with `^:sci/error`, you only get the immediate error frame, not the full call chain:

```clojure
(defn baz []
  (try
    (assoc :foo :bar :baz)
    (catch ^:sci/error Exception e
      (-> e ex-data :sci.impl/callstack deref))))

(defn bar [] (baz))
(defn foo [] (bar))
(foo)
;; Stack has only 1 frame (assoc), missing baz, bar, foo
```

This is because the stack is built during exception **propagation**. When you catch early with `^:sci/error`, propagation stops and you only get the immediate frame.

## Why Analysis-Time Only Doesn't Work

Each node gets its stack frame embedded at analysis time. However, **function bodies are analyzed once at definition time**, not re-analyzed at each call site.

Example:
```clojure
(defn inner [] (current-stacktrace))  ;; analyzed here - don't know who will call inner
(defn outer [] (inner))               ;; creates call node, but inner's body already analyzed
(outer)
```

When `(current-stacktrace)` inside `inner` is analyzed, `outer` doesn't exist yet. The runtime call chain (`outer` -> `inner`) can only be known at runtime.

## Solution: Runtime Tracking with Minimal Overhead

Each node already has its stack frame embedded at analysis time. We add push/pop at runtime, but with **near-zero overhead when not enabled**.

### How It Works

1. **`*call-stack*` dynamic var** (in utils.cljc): `nil` by default
2. **Push/pop functions** only do work when `*call-stack*` is non-nil:
   ```clojure
   (defn push-call-stack! [frame]
     (when *call-stack*           ;; nil check = near-zero overhead
       (vswap! *call-stack* conj frame)))
   ```
3. **To enable tracking**: bind `*call-stack*` to `(volatile! [])`
4. **`current-stacktrace`**: reads from the volatile

### Implementation

**1. Modify `gen-return-call`** (analyzer.cljc) - add push/pop around calls:

```clojure
;; Inside ->Node body (runs at runtime):
(do
  (utils/push-call-stack! stack)  ;; stack is the node's frame, set at analysis time
  (try
    (f arg0 arg1 ...)
    (catch Throwable e
      (rethrow-with-location-of-node ctx bindings e this))
    (finally
      (utils/pop-call-stack!))))
```

**2. `current-stacktrace`** (core.cljc):

```clojure
(defn current-stacktrace []
  (utils/get-call-stack))
```

### Files to modify

| File | Changes |
|------|---------|
| `src/sci/impl/analyzer.cljc` | Modify `gen-return-call` to add push/pop around calls |
| `src/sci/impl/utils.cljc` | ✅ Already done: `*call-stack*`, `push-call-stack!`, `pop-call-stack!`, `get-call-stack` |
| `src/sci/core.cljc` | Update `current-stacktrace` to call `utils/get-call-stack` |

### Performance

- **When not tracking** (`*call-stack*` is nil): just a nil check per function call
- **When tracking**: push/pop on volatile per function call

### API Usage

```clojure
;; Host enables tracking by binding *call-stack*:
(binding [sci.impl.utils/*call-stack* (volatile! [])]
  (sci/eval-string* ctx "(outer)"))

;; Or expose via context for SCI code to use:
(def ctx (sci/init {:namespaces
                    {'debug {'stacktrace sci/current-stacktrace}}}))
```

## Verification

Test in `test/sci/error_test.cljc`:

```clojure
(deftest current-stacktrace-test
  (testing "returns call chain at point of execution"
    (let [ctx (sci/init {:namespaces {'sci.core {'current-stacktrace sci/current-stacktrace}}})
          stacktrace (sci/binding [sci/file "test.clj"]
                       (sci/eval-string* ctx "
(defn inner [] (sci.core/current-stacktrace))
(defn outer [] (inner))
(outer)"))]
      (is (= [{:ns 'user, :name 'inner, :file "test.clj", :line 2, :column 16}
              {:ns 'user, :name 'outer, :file "test.clj", :line 3, :column 15}]
             (mapv #(select-keys % [:ns :name :file :line :column]) stacktrace))))))
```

Run: `script/test/jvm`
