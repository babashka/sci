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
- `quote` (passed through unchanged)
- `fn*` (not descended into — nested fns are handled by the analyzer separately)

**Macros:**
- All macros are expanded before await detection
- This includes user-defined macros that expand to `await` or `recur` calls
- Standard macros like `->`, `->>`, `when`, `cond`, `doseq`, etc. work automatically
- Locals are tracked and passed to `ctx :bindings` so macros see them in `&env` (e.g., a macro checking if `->` is locally bound)

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
(defn promise-catch-for-try [p f]
  (.catch p (fn [e]
              (f (if (= :sci/error (:type (ex-data e)))
                   (or (ex-cause e) e)
                   e)))))
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
             (and (symbol? op)
                  (= "sci.impl.async-await" (namespace op)))))))
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
  ((fn loop_fn__123 [x]
     (if (< x 3)
       (sip/then (sip/resolve x)
         (fn [_] (loop_fn__123 (inc x))))
       (sip/resolve x)))
   x))
```

Key points:
- If no await in loop, returns original `(loop* ...)` unchanged (no promise overhead)
- Init values are wrapped in `let*` so each sees previous bindings (important when a binding shadows a macro like `->`)
- `recur` is handled post-macro-expansion in `transform-async-body` via `:recur-target` in ctx — this means macros that expand to `recur` work correctly
- Recur args may themselves contain await, so they are processed via `transform-expr-with-await` to chain promises
- The loop function always returns a promise (body wrapped if needed)
- Loop call is marked as promise-producing via metadata (no extra resolve wrapper)

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
  (sip/catch-for-try
    (sip/then (sip/resolve nil) (fn [_] (sip/resolve p)))  ;; Body wrapped in .then
    (fn [e] (handle e)))  ;; catch-for-try unwraps SCI error wrapping
  (fn [] (cleanup)))
```

Key points:
- Body is wrapped in `.then` callback so synchronous throws are caught by `.catch`
- Uses `catch-for-try` instead of plain `catch` — this unwraps SCI's error wrapping (`rethrow-with-location-of-node` adds `{:type :sci/error}` ex-data and wraps the original as `ex-cause`) so that user code sees the original exception with its ex-data preserved
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
| `src/sci/impl/async_macro.cljc` | Transformation functions (~420 lines) |
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
- User-defined macros expanding to recur
- Quoted forms inside async bodies (not descended into)
- Macros that expand to quoted forms (e.g., reify-like patterns)
- ex-data preservation through async try/catch
- Collection literals (vectors, sets, maps) with await
- Transformation performance benchmark

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

### Why handle recur in transform-async-body instead of a pre-pass?

Initially `recur` was replaced with recursive function calls in a separate `replace-recur` pass before transformation. This had two problems:

1. **Macros expanding to recur**: A macro like `(defmacro my-recur [& args] (cons 'recur args))` wouldn't be expanded yet during the pre-pass, so the recur would be missed.
2. **Quoted recur**: The pre-pass had to be careful not to descend into `(quote ...)` forms.

By handling `recur` inline in `transform-async-body` (after macro expansion), both issues are solved naturally. The loop passes `:recur-target` through `ctx`, and when `transform-async-body` encounters `recur`, it replaces it with a call to the loop function. Since `recur` args may contain `await`, they are processed via `transform-expr-with-await`.

### Why `catch-for-try` instead of plain `catch`?

SCI's `rethrow-with-location-of-node` wraps exceptions with `{:type :sci/error}` ex-data and stores the original as `ex-cause`. In synchronous try/catch, the dynamic var `*in-try*` prevents this wrapping. But dynamic bindings don't persist across `.then` chains, so async throws get wrapped.

`catch-for-try` is a runtime function that unwraps this: if the caught exception has `{:type :sci/error}` ex-data, it passes `(ex-cause e)` to the handler instead, preserving the original exception and its ex-data.

An alternative approach of using `binding [*in-try* true]` in the `.then` callback was tried but failed because throws happen in inner `.then` callbacks (from awaits), not the outer one where the binding is set.

### Why wrap loop inits in let*?

Loop bindings are sequential - each init can see previous bindings. When a binding shadows a macro (like `(loop [-> inc x (-> 1)] ...)`), the second init `(-> 1)` should call the function, not expand as the threading macro. Wrapping in `let*` ensures proper scoping during analysis.

### Why `chain-promises` as a shared helper?

Both `transform-expr-with-await` (for function calls like `(+ (await p1) (await p2))`) and `transform-coll-with-await` (for `[(await p1) (await p2)]`) need the same chaining logic: walk transformed elements, and when one is a promise, bind it via `.then` and continue with the resolved value. `chain-promises` extracts this shared pattern, taking a `rebuild-fn` that assembles the final form from resolved elements.

### Why does `normalize-branches` return `[normalized? branches]`?

When `if` or `case*` has mixed promise/non-promise branches, all branches must return promises so the containing expression can chain `.then` on the result. `normalize-branches` wraps non-promise branches in `resolve`. It returns a boolean alongside the result because checking "did normalization happen?" by comparing before/after is unreliable — if only one branch is a promise, the non-promise branch changes but the promise branch doesn't, making equality checks confusing. A single reduce pass that tracks `has-promise?` during traversal is both simpler and correct.

## Limitations

- **No implicit parallelism**: Sequential awaits run sequentially. Use `js/Promise.all` for parallel execution.
- **No top-level await**: Must be inside `^:async fn` or `^:async defn`.
- **CLJS only**: This feature targets ClojureScript's Promise interop.

## Future Extensions

- Explicit parallel await syntax: `(await-all [p1 p2 p3])`
- Integration with promesa library
- Error stack traces pointing to original source
