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

## Solution

Maintain a runtime call stack that gets pushed/popped as functions are entered/exited. The **infrastructure** (push/pop code) is generated at analysis time, but executes at runtime - exactly like the existing exception handling try/catch wrappers.

## How Exception Stack Works (for reference)

Currently, when an exception occurs:
1. Each function call is wrapped in try/catch (set up at analysis time)
2. When exception is thrown, each wrapper catches it, adds its frame via `rethrow-with-location-of-node`, and rethrows
3. Stack is built up as exception propagates

Example output from `bb`:
```
user/inner - /tmp/test.bb:1:16
user/inner - /tmp/test.bb:1:1
user/outer - /tmp/test.bb:2:16
user/outer - /tmp/test.bb:2:1
user       - /tmp/test.bb:3:1
```

## Proposed Approach

Similar to exception handling, but track the stack actively:

1. **At analysis time**: Modify `gen-return-call` to wrap function calls with push/pop logic
2. **At runtime**: Each call pushes its frame to a thread-local stack, executes, then pops
3. **`current-stacktrace`**: Simply reads the current stack

### Implementation

In `gen-return-call` (analyzer.cljc), wrap calls like:

```clojure
;; Pseudocode for generated node
(let [stack-frame {...}]  ;; computed at analysis time
  (push-frame! stack-frame)
  (try
    (f arg0 arg1 ...)
    (finally
      (pop-frame!))))
```

The stack would be stored in a dynamic var or thread-local, similar to how `*in-try*` works.

### API

```clojure
;; Host-level API
(sci.core/current-stacktrace)
;; => [{:ns user :name outer :file "script.clj" :line 2 :column 16}
;;     {:ns user :name foo :file "script.clj" :line 3 :column 1}]

;; Host exposes to users as they wish:
(def ctx (sci/init {:namespaces
                    {'debug {'stacktrace sci.core/current-stacktrace}}}))
```

### Files to modify

| File | Changes |
|------|---------|
| `src/sci/impl/analyzer.cljc` | Modify `gen-return-call` to add push/pop around calls |
| `src/sci/impl/utils.cljc` | Add dynamic var for current call stack, push/pop functions |
| `src/sci/core.cljc` | Add `current-stacktrace` public function |

### Performance consideration

This adds overhead to every function call. Options:
1. Always enabled (simpler, some overhead)
2. Opt-in via context flag `{:track-stacktrace true}`
3. Only enable when `current-stacktrace` is used in the code (analyzer detects usage)

## Verification

```clojure
(deftest current-stacktrace-test
  (let [result (sci/eval-string "
(defn inner [] (sci.core/current-stacktrace))
(defn outer [] (inner))
(outer)")]
    ;; Should show outer as caller, not inner (inner is current fn)
    (is (= 'outer (:name (first result))))))
```

Run: `script/test/jvm`
