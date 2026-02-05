# ADR 0002: Async/Await for ClojureScript

| Status | Date | Related |
|--------|------|---------|
| Proposed | 2026-02-05 | Inspired by core.async ioc_macros |

## Context

Users want async/await syntax for SCI in ClojureScript to simplify working with Promises.

## Problem

Writing Promise-based code with nested `.then` chains is verbose. JavaScript and other languages provide async/await syntax that allows writing asynchronous code in a more sequential style.

## Inspiration: core.async

The `clojure.core.async` library transforms `go` blocks into state machines via SSA (Static Single Assignment) form. See `~/dev/core.async/src/main/clojure/clojure/core/async/impl/go.clj`. Key insights:

- Walks AST and transforms special forms (`let`, `if`, `do`, `loop`, etc.)
- Uses `item-to-ssa` multimethod dispatching on `:op`
- Custom terminators for channel operations (`<!`, `>!`)

For our POC, we use a **simpler approach**: direct code transformation without full SSA, targeting Promise `.then` chains.

## Proposed Solution: Macro-based Code Transformation

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

### POC Scope

- Single arity functions only
- No macro expansion inside async fns
- Only support `let*` initially
- `await` is recognized syntactically (just the symbol, no var/require needed)

## Implementation

### 1. New namespace: `src/sci/impl/async_macro.cljc`

Contains transformation functions that rewrite async function bodies:

```clojure
(ns sci.impl.async-macro)

(defn await-call?
  "Check if form is (await ...)"
  [form]
  (and (seq? form) (= 'await (first form))))

(defn transform-let*
  "Transform let* with await calls into .then chains.

  Algorithm:
  1. Walk binding pairs [name init]
  2. When init contains await, emit (.then <promise> (fn [name] <rest>))
  3. Non-await bindings accumulate into regular let*
  4. Recursively process remaining bindings and body"
  [bindings body]
  (loop [pairs (partition 2 bindings)
         acc-bindings []]
    (if-let [[name init] (first pairs)]
      (if (await-call? init)
        ;; Emit .then, wrap remaining in continuation
        (let [promise (second init)
              rest-pairs (rest pairs)
              rest-body (if (seq rest-pairs)
                          (transform-let* (vec (mapcat identity rest-pairs)) body)
                          (if (= 1 (count body))
                            (first body)
                            (cons 'do body)))]
          (if (seq acc-bindings)
            ;; Wrap accumulated non-await bindings
            (list 'let* (vec acc-bindings)
                  (list '.then promise
                        (list 'fn [name] rest-body)))
            (list '.then promise
                  (list 'fn [name] rest-body))))
        ;; Accumulate non-await binding
        (recur (rest pairs) (conj acc-bindings name init)))
      ;; No more pairs, emit remaining bindings + body
      (if (seq acc-bindings)
        (list* 'let* (vec acc-bindings) body)
        (if (= 1 (count body))
          (first body)
          (cons 'do body))))))

(defn transform-async-body
  "Walk body and transform await calls in let* forms"
  [body]
  (cond
    (and (seq? body) (= 'let* (first body)))
    (let [[_ bindings & exprs] body]
      (transform-let* bindings exprs))

    :else body))
```

### 2. Transformation Examples

**Sequential awaits:**
```clojure
(let* [x (await p1)
       y (await p2)]
  (+ x y))

;; Transforms to:
(.then p1
  (fn [x]
    (.then p2
      (fn [y]
        (+ x y)))))
```

**Mixed bindings:**
```clojure
(let* [x 1
       y (await p1)
       z 2]
  (+ x y z))

;; Transforms to:
(let* [x 1]
  (.then p1
    (fn [y]
      (let* [z 2]
        (+ x y z)))))
```

### 3. Hook into analyzer (minimal change)

In `src/sci/impl/analyzer.cljc`, in `analyze-fn*` around line 341:

```clojure
;; Check for :async metadata
async? (:async fn-expr-m)

;; If async, transform each body before analysis
bodies (if async?
         (map #(update % 1 async-macro/transform-async-body) bodies)
         bodies)
```

### Files to Create/Modify

| File | Change |
|------|--------|
| `src/sci/impl/async_macro.cljc` | **NEW** - transformation functions |
| `src/sci/impl/analyzer.cljc` | ~5 lines - detect `:async`, call transformer |

## Verification

Create test file `test/sci/async_macro_test.cljs`:

```clojure
(deftest async-fn-test
  (async done
    (p/let [ctx (sci/init {})
            result (-> (sci/eval-string ctx
                         "(defn ^:async foo []
                            (let [x (await (js/Promise.resolve 1))]
                              (inc x)))
                          (foo)"))]
      (is (= 2 result))
      (done))))
```

Run with: `script/test/node`

## Future Extensions

Once POC works, consider supporting:
- Multiple arities
- Nested let* forms
- Other special forms (if, do, loop/recur)
- Error handling (try/catch → .catch)
- Full SSA approach like core.async for complex control flow
