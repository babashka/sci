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

## Summary

Add `sci.core/current-stacktrace` to return the lexical stack (enclosing forms/functions) at any point during execution. Stack info is built during analysis and inlined as a constant, so zero runtime overhead.

## API

```clojure
;; sci.core/current-stacktrace is a special var recognized by the analyzer.
;; When called from SCI code, returns the lexical stack at that point:
;; => [{:ns user :name inner :file "script.clj" :line 1 :column 14}
;;     {:ns user :name outer :file "script.clj" :line 2 :column 14}]

;; Host exposes to users as they wish:
(def ctx (sci/init {:namespaces
                    {'debug {'stacktrace sci.core/current-stacktrace}}}))

;; SCI user calls it:
(defn my-fn []
  (debug/stacktrace))  ;; analyzer inlines the lexical stack here
```

## Implementation

### Approach
Treat `current-stacktrace` as a **special form** recognized by the analyzer. When the analyzer encounters a call to the function, it replaces it with a constant containing the current lexical stack. Zero runtime cost.

### Step 1: Track lexical stack during analysis

In `src/sci/impl/analyzer.cljc`:
- Add `:lexical-stack` key to `ctx` (initially `[]`)
- Push stack frame when entering `defn`/`fn`/`let`/`loop` (line ~355, ~540, ~767)
- Use `make-stack` from utils to create frames

```clojure
;; In analyze-fn*, after creating fn-name:
(let [stack-frame (utils/make-stack (meta fn-expr))
      stack-frame (assoc stack-frame :name fn-name)
      ctx (update ctx :lexical-stack (fnil conj []) stack-frame)]
  ...)
```

### Step 2: Handle current-stacktrace calls

In `src/sci/impl/analyzer.cljc`, in `analyze-call`:
- Check if resolved symbol is `current-stacktrace`
- If so, return a constant node with `(:lexical-stack ctx)`

```clojure
;; In analyze-call, after resolving f:
(if (= 'sci.core/current-stacktrace (some-> f meta :sci/built-in))
  (->constant (:lexical-stack ctx))
  ;; ... normal call handling
  )
```

### Step 3: Register the function

In `src/sci/impl/namespaces.cljc`:
- Add `current-stacktrace` to `sci.core` namespace
- Mark it with metadata so analyzer can recognize it

In `src/sci/core.cljc`:
- Add public `current-stacktrace` function (for documentation/API)

### Files to modify

| File | Changes |
|------|---------|
| `src/sci/impl/analyzer.cljc` | Track `:lexical-stack` in ctx, push on fn/let/loop entry, handle `current-stacktrace` calls |
| `src/sci/impl/namespaces.cljc` | Add `current-stacktrace` to `sci.core` namespace with marker metadata |
| `src/sci/core.cljc` | Add `current-stacktrace` for public API/docs |

## Note on aliasing

When host exposes `current-stacktrace` under a different name:
```clojure
(sci/init {:namespaces {'debug {'st sci.core/current-stacktrace}}})
```
The analyzer resolves `debug/st` to the same var, recognizes it by identity (via metadata marker), and inlines the stack. The var carries the marker, not the symbol.

## Verification

1. Add test in `test/sci/stacktrace_test.cljc`:
```clojure
(deftest current-stacktrace-test
  (let [ctx (sci/init {:namespaces
                       {'debug {'stacktrace sci.core/current-stacktrace}}})
        result (sci/eval-string* ctx
                 "(defn inner [] (debug/stacktrace))
                  (defn outer [] (inner))
                  (outer)")]
    (is (= 'inner (:name (first result))))
    (is (= 'outer (:name (second result))))))
```

2. Run: `script/test/jvm`

---

## POC Results

Minimal POC implemented in `analyzer.cljc` only (2 changes). Results:

### With plain `fn` (line/column preserved):
```clojure
(sci/eval-string "((fn outer [] ((fn inner [] (sci.impl/current-stacktrace)))))")
;; => [{:line 1, :column 2, :ns user, :file nil, :name outer}
;;     {:line 1, :column 16, :ns user, :file nil, :name inner}]
```

### With `defn` (line/column lost due to macro expansion):
```clojure
(sci/eval-string "(defn outer [] (defn inner [] (sci.impl/current-stacktrace)) (inner)) (outer)")
;; => [{:ns user, :file nil, :name outer}
;;     {:ns user, :file nil, :name inner}]
```

### Known limitation
`defn` macro expansion creates a new `fn*` form without preserving line/column from original expression. This can be fixed by passing metadata through the macro expansion.
