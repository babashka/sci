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

### Performance: Helper Functions

For performance, the transformation emits calls to helper functions in `sci.impl.async-await` rather than raw JS interop. These helpers:
1. Are native CLJS functions that compile to efficient JS
2. Serve as reliable markers for `promise-form?` detection
3. Avoid SCI's interpreted interop overhead

```clojure
;; In src/sci/impl/namespaces.cljc (CLJS only)
(defn promise-resolve [v] (js/Promise.resolve v))
(defn promise-then [p f] (.then p f))
(defn promise-catch [p f] (.catch p f))
(defn promise-finally [p f] (.finally p f))
```

The namespace is registered as `sci.impl.async-await` (internal, not for user consumption).

### Key Insight: Promise Form Detection

The core insight is using `promise-form?` to detect which subexpressions produce promises:

```clojure
(defn- promise-form?
  "Check if form is already a promise-producing expression.
   Detects metadata marker or calls to sci.impl.async-await helpers."
  [form]
  (or (:sci.impl/promise (meta form))
      (and (seq? form)
           (let [op (first form)]
             (or (= 'sci.impl.async-await/then op)
                 (= 'sci.impl.async-await/catch op)
                 (= 'sci.impl.async-await/finally op)
                 (= 'sci.impl.async-await/resolve op))))))
```

Forms that produce promises but aren't direct helper calls (like `if` with promise branches) are marked with metadata via `(vary-meta form assoc :sci.impl/promise true)`. This avoids needing a noop wrapper macro.

When transforming expressions:
1. Recursively transform all subforms first (this expands macros)
2. Check if any transformed subform is a `promise-form?`
3. If so, chain with helper functions to sequence the promise

This avoids needing a separate "contains await" check and handles macro expansion naturally.

### Transformation Algorithm

All examples below use `sip` as an alias for `sci.impl.async-await` for brevity.

**For `let*`:**
```clojure
(let* [x 1
       y (await p1)
       z (await p2)]
  (+ x y z))

;; Transforms to:
(let* [x 1]
  (sip/then (sip/resolve p1)
    (fn [y]
      (sip/then (sip/resolve p2)
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
(sip/then (sip/resolve p1)
  (fn [_]
    (sip/then (sip/resolve p2)
      (fn [_]
        result))))
```

**For `if`:**
```clojure
(if (await p)
  (await then-p)
  (await else-p))

;; Transforms to:
(sip/then (sip/resolve p)
  (fn [test__123]
    (if test__123
      (sip/resolve then-p)
      (sip/resolve else-p))))
```

Key points:
- When one branch has await and the other doesn't, both are normalized to return promises (prevents "p.then is not a function" errors)
- Missing else with await in then is handled correctly (implicit nil wrapped in promise)

**For `loop*/recur`:**
Loops with await are transformed into recursive promise-returning functions, wrapped in `let*` to preserve sequential scoping:

```clojure
(loop [x 0]
  (if (< x 3)
    (do (await (js/Promise.resolve x))
        (recur (inc x)))
    x))

;; Transforms to:
(let* [x 0]
  (sip/resolve
    ((fn loop_fn__123 [x]
       (if (< x 3)
         (sip/then (sip/resolve x)
           (fn [_] (loop_fn__123 (inc x))))
         (sip/resolve x)))
     x)))
```

Key points:
- Init values are wrapped in `let*` so each sees previous bindings (important when a binding shadows a macro like `->`)
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
(sip/finally
  (sip/catch
    (sip/then (sip/resolve nil) (fn [_] (sip/resolve p)))  ;; Body wrapped in .then
    (fn [e] (handle e)))
  (fn [] (cleanup)))
```

Key points:
- Body is wrapped in `.then` callback so synchronous throws are caught by `.catch`
- If no await in try/catch/finally, returns original expression unchanged (no promise overhead)

**For `case*`:**
Important: Match constants are NOT transformed, only test expression and result expressions:

```clojure
(case (await p)
  1 :one
  2 :two
  :default)

;; Transforms to:
(sip/then (sip/resolve p)
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
| `src/sci/impl/async_macro.cljc` | Transformation functions (~370 lines) |
| `src/sci/impl/namespaces.cljc` | Helper functions + `sci.impl.async-await` namespace registration |
| `src/sci/impl/analyzer.cljc` | ~10 lines added for async detection |
| `test/sci/async_await_test.cljs` | Comprehensive test suite |

## Testing

Run with: `script/test/node`

Test cases cover:
- Basic await in let binding
- Multiple sequential awaits
- Await in expressions (not just bindings)
- Threading macros with await
- if/when/cond with await (including mixed promise/non-promise branches)
- do with await
- try/catch/finally with await (including sync throw)
- loop/recur with await
- Loop bindings that shadow macros (e.g., `[-> fn x (-> 1)]`)
- case with await (in test expr, results, default)
- Destructuring with await
- doseq with await
- letfn with await
- throw with await
- Nested async functions
- Returning non-promise values (auto-wrapped)
- User-defined macros expanding to await
- Collection literals (vectors, sets, maps) with await

## Design Decisions

### Why helper functions instead of raw interop?

The transformation emits `(sci.impl.async-await/then ...)` instead of `(.then ...)` for two reasons:

1. **Performance**: Helper functions are native CLJS that compile to direct JS interop, avoiding SCI's interpreted method dispatch overhead. This matters for tight loops with many awaits.

2. **Reliable detection**: Using our own symbols as markers for `promise-form?` is more robust than checking for generic `.then` syntax. Users might write `.then` directly in their code, which shouldn't be mistaken for transformed await calls.

### Why `promise-form?` detection instead of `contains-await?`

Initially we tried tracking whether expressions contain `await` calls. This had issues:
1. Macros might hide await calls
2. Required two passes (detect then transform)

Using `promise-form?` on transformed results:
1. Macros are expanded first, revealing awaits
2. Single pass transformation
3. More robust: if something produces a promise, chain it

### Why wrap await args in `sci.impl.async-await/resolve`?

This ensures non-promise values work correctly:
```clojure
(await 42)  ;; Works - 42 is wrapped in Promise.resolve
```

### Why recursive functions for loop/recur?

Direct translation to recursive calls is simpler than state machines and works well with promises. The browser's event loop handles "stack overflow" naturally since each `.then` callback runs in a fresh stack frame.

### Why metadata markers instead of a wrapper macro?

Forms like `(if test then-with-await else)` produce promises but aren't direct helper calls. We need to mark them as promise-producing for containing expressions. Using metadata `^{:sci.impl/promise true}` is cleaner than a noop wrapper macro because:
1. No macro expansion needed later
2. Form structure stays unchanged
3. More idiomatic Clojure

### Why wrap loop inits in let*?

Loop bindings are sequential - each init can see previous bindings. When a binding shadows a macro (like `(loop [-> inc x (-> 1)] ...)`), the second init `(-> 1)` should call the function, not expand as the threading macro. Wrapping in `let*` ensures proper scoping during analysis.

## Limitations

- **No implicit parallelism**: Sequential awaits run sequentially. Use `js/Promise.all` for parallel execution.
- **No top-level await**: Must be inside `^:async fn` or `^:async defn`.
- **CLJS only**: This feature targets ClojureScript's Promise interop.

## Future Extensions

- Explicit parallel await syntax: `(await-all [p1 p2 p3])`
- Integration with promesa library
- Error stack traces pointing to original source
