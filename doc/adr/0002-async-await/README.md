# ADR 0002: Async/Await for ClojureScript

| Status | Date | Related |
|--------|------|---------|
| Implemented | 2026-02-06 | Inspired by core.async ioc_macros |

## Context

Users want async/await syntax for SCI in ClojureScript to simplify working with Promises.

## Problem

Writing Promise-based code with nested `.then` chains is verbose. JavaScript and other languages provide async/await syntax that allows writing asynchronous code in a more sequential style.

## Inspiration: core.async

The `clojure.core.async` library transforms `go` blocks into state machines via SSA (Static Single Assignment) form. See `~/dev/core.async/src/main/clojure/clojure/core/async/impl/go.clj`. Key insights:

- Walks AST and transforms special forms (`let`, `if`, `do`, `loop`, etc.)
- Uses `item-to-ssa` multimethod dispatching on `:op`
- Custom terminators for channel operations (`<!`, `>!`)

For our implementation, we use a **simpler approach**: direct code transformation without full SSA, targeting Promise `.then` chains.

## Solution: Macro-based Code Transformation

Transform async function bodies at analysis time into Promise `.then` chains.

### Syntax

```clojure
(defn ^:async foo []
  (let [x (await (js/Promise.resolve 1))]
    (inc x)))

;; Transforms to:
(defn foo []
  (.then (js/Promise.resolve 1) (fn [x] (inc x))))
```

### Implementation Scope

**Supported features:**
- `^:async` metadata on `defn` and `fn` forms
- `await` recognized syntactically (just the symbol, no var/require needed)
- Single and multiple arities
- Destructuring in let bindings (via fn parameter destructuring)
- Nested async functions

**Supported special forms:**
- `let*` (and `let` via macro expansion)
- `do`
- `if`
- `loop*/recur`
- `try/catch/finally`
- `case*` (and `case` via macro expansion)

**Macros:**
- All macros are expanded before await detection
- This includes user-defined macros that expand to `await` calls
- Standard macros like `->`, `->>`, `when`, `cond`, `doseq`, etc. work automatically

## Implementation

### Key Insight: Promise Form Detection

The core insight is using `promise-form?` to detect which subexpressions produce promises:

```clojure
(defn- promise-form?
  "Check if form is already a promise-producing expression"
  [form]
  (and (seq? form)
       (or (= '.then (first form))
           (= '.catch (first form))
           (= '.finally (first form))
           (= 'js/Promise.resolve (first form)))))
```

When transforming expressions:
1. Recursively transform all subforms first (this expands macros)
2. Check if any transformed subform is a `promise-form?`
3. If so, chain with `.then` to sequence the promise

This avoids needing a separate "contains await" check and handles macro expansion naturally.

### Transformation Algorithm

**For `let*`:**
```clojure
(let* [x 1
       y (await p1)
       z (await p2)]
  (+ x y z))

;; Transforms to:
(let* [x 1]
  (.then (js/Promise.resolve p1)
    (fn [y]
      (.then (js/Promise.resolve p2)
        (fn [z]
          (+ x y z))))))
```

**For `do`:**
```clojure
(do
  (await p1)
  (await p2)
  result)

;; Transforms to:
(.then (js/Promise.resolve p1)
  (fn [_]
    (.then (js/Promise.resolve p2)
      (fn [_]
        result))))
```

**For `if`:**
```clojure
(if (await p)
  (await then-p)
  (await else-p))

;; Transforms to:
(.then (js/Promise.resolve p)
  (fn [test__123]
    (if test__123
      (js/Promise.resolve then-p)
      (js/Promise.resolve else-p))))
```

**For `loop*/recur`:**
Loops with await are transformed into recursive promise-returning functions:

```clojure
(loop [x 0]
  (if (< x 3)
    (do (await (js/Promise.resolve x))
        (recur (inc x)))
    x))

;; Transforms to:
(js/Promise.resolve
  ((fn loop_fn__123 [x]
     (if (< x 3)
       (.then (js/Promise.resolve x)
         (fn [_] (loop_fn__123 (inc x))))
       (js/Promise.resolve x)))
   0))
```

Key points:
- `recur` calls are replaced with recursive function calls
- The loop function always returns a promise
- Nested `fn`/`fn*`/`loop*` bodies are not descended into (recur targets different loop)

**For `try/catch/finally`:**
```clojure
(try
  (await p)
  (catch js/Error e
    (handle e))
  (finally
    (cleanup)))

;; Transforms to:
(.finally
  (.catch (js/Promise.resolve p)
    (fn [e] (handle e)))
  (fn [] (cleanup)))
```

**For `case*`:**
Important: Match constants are NOT transformed, only test expression and result expressions:

```clojure
(case (await p)
  1 :one
  2 :two
  :default)

;; Transforms to:
(.then (js/Promise.resolve p)
  (fn [case_test__123]
    (case* case_test__123
      1 :one
      2 :two
      :default)))
```

### Analyzer Integration

In `src/sci/impl/analyzer.cljc`, the `analyze-fn*` function detects `:async` metadata and calls the transformer:

```clojure
;; Check for :async metadata
async? (or (:async fn-expr-m) (:async bodies-m))

;; If async, transform the body before analysis
body (if async?
       (async-macro/transform-async-fn-body ctx locals body)
       body)
```

### Files

| File | Description |
|------|-------------|
| `src/sci/impl/async_macro.cljc` | Transformation functions (~350 lines) |
| `src/sci/impl/analyzer.cljc` | ~10 lines added for async detection |
| `test/sci/async_await_test.cljs` | Comprehensive test suite |

## Testing

Run with: `script/test/node`

Test cases cover:
- Basic await in let binding
- Multiple sequential awaits
- Await in expressions (not just bindings)
- Threading macros with await
- if/when/cond with await
- do with await
- try/catch/finally with await
- loop/recur with await
- case with await (in test expr, results, default)
- Destructuring with await
- doseq with await
- letfn with await
- throw with await
- Nested async functions
- Returning non-promise values (auto-wrapped)
- User-defined macros expanding to await

## Design Decisions

### Why `promise-form?` detection instead of `contains-await?`

Initially we tried tracking whether expressions contain `await` calls. This had issues:
1. Macros might hide await calls
2. Required two passes (detect then transform)

Using `promise-form?` on transformed results:
1. Macros are expanded first, revealing awaits
2. Single pass transformation
3. More robust: if something produces a promise, chain it

### Why wrap await args in `js/Promise.resolve`?

This ensures non-promise values work correctly:
```clojure
(await 42)  ;; Works - 42 is wrapped in Promise.resolve
```

### Why recursive functions for loop/recur?

Direct translation to recursive calls is simpler than state machines and works well with promises. The browser's event loop handles "stack overflow" naturally since each `.then` callback runs in a fresh stack frame.

## Limitations

- **No implicit parallelism**: Sequential awaits run sequentially. Use `js/Promise.all` for parallel execution.
- **No top-level await**: Must be inside `^:async fn` or `^:async defn`.
- **CLJS only**: This feature targets ClojureScript's Promise interop.

## Future Extensions

- Explicit parallel await syntax: `(await-all [p1 p2 p3])`
- Integration with promesa library
- Error stack traces pointing to original source
