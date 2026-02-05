(ns sci.impl.async-macro
  "Transforms async function bodies with await into Promise .then chains.

  Supports await in:
  - let bindings: (let [x (await p)] ...)
  - do forms: (do (await p) ...)
  - arbitrary expressions: (inc (await p))
  - macros like -> are expanded first

  Example:
    (defn ^:async foo []
      (let [x (await (js/Promise.resolve 1))]
        (inc x)))

  Transforms to:
    (defn foo []
      (.then (js/Promise.resolve 1) (fn [x] (inc x))))"
  (:require [clojure.walk]
            [sci.impl.macroexpand :as macroexpand]))

(defn await-call?
  "Check if form is (await ...)"
  [form]
  (and (seq? form) (= 'await (first form))))

(defn contains-await?
  "Check if form contains any (await ...) calls"
  [form]
  (cond
    (await-call? form) true
    (seq? form) (some contains-await? form)
    (vector? form) (some contains-await? form)
    (map? form) (some contains-await? (concat (keys form) (vals form)))
    :else false))

(defn wrap-promise
  "Wrap value in js/Promise.resolve to handle non-Promise values"
  [expr]
  (list 'js/Promise.resolve expr))

(declare transform-async-body)

(defn transform-let*
  "Transform let* with await calls into .then chains."
  [ctx bindings body]
  (loop [pairs (partition 2 bindings)
         acc-bindings []]
    (if-let [[binding-name init] (first pairs)]
      (if (or (await-call? init) (contains-await? init))
        ;; Emit .then, wrap remaining in continuation
        (let [;; Transform the init expression and use it as the promise
              transformed-init (if (await-call? init)
                                 (wrap-promise (second init))
                                 (transform-async-body ctx init))
              rest-pairs (rest pairs)
              ;; Recursively transform the rest body
              rest-body (if (seq rest-pairs)
                          (transform-let* ctx (vec (mapcat identity rest-pairs)) body)
                          (let [transformed-body (map #(transform-async-body ctx %) body)]
                            (if (= 1 (count transformed-body))
                              (first transformed-body)
                              (cons 'do transformed-body))))]
          (if (seq acc-bindings)
            ;; Wrap accumulated non-await bindings
            (list 'let (vec acc-bindings)
                  (list '.then transformed-init
                        (list 'fn [binding-name] rest-body)))
            (list '.then transformed-init
                  (list 'fn [binding-name] rest-body))))
        ;; Accumulate non-await binding
        (recur (rest pairs) (conj acc-bindings binding-name init)))
      ;; No more pairs, emit remaining bindings + body (recursively transformed)
      (let [transformed-body (map #(transform-async-body ctx %) body)]
        (if (seq acc-bindings)
          (list* 'let (vec acc-bindings) transformed-body)
          (if (= 1 (count transformed-body))
            (first transformed-body)
            (cons 'do transformed-body)))))))

(defn transform-do
  "Transform do with await calls into .then chains."
  [ctx exprs]
  (loop [exprs exprs
         acc []]
    (if (seq exprs)
      (let [expr (first exprs)
            rest-exprs (rest exprs)]
        (if (await-call? expr)
          ;; Found await - wrap rest in .then
          (let [promise (wrap-promise (second expr))
                rest-body (if (seq rest-exprs)
                            (transform-do ctx rest-exprs)
                            nil)]
            (if (seq acc)
              ;; Have accumulated expressions before await
              (let [then-expr (if rest-body
                                (list '.then promise (list 'fn ['_] rest-body))
                                promise)]
                (list* 'do (conj acc then-expr)))
              ;; No accumulated expressions
              (if rest-body
                (list '.then promise (list 'fn ['_] rest-body))
                promise)))
          ;; Not an await, accumulate and continue
          (recur rest-exprs (conj acc (transform-async-body ctx expr)))))
      ;; No more exprs
      (if (= 1 (count acc))
        (first acc)
        (list* 'do acc)))))

(defn transform-expr-with-await
  "Transform an expression containing await by extracting the await
   and wrapping in .then."
  [ctx expr]
  (if (await-call? expr)
    ;; Direct await - just return the wrapped promise
    (wrap-promise (second expr))
    ;; Find and extract the first await, replace with gensym, wrap in .then
    (let [await-sym (gensym "await__")
          await-expr (atom nil)
          ;; Replace first await with gensym
          replaced (clojure.walk/prewalk
                     (fn [form]
                       (if (and (nil? @await-expr) (await-call? form))
                         (do (reset! await-expr (second form))
                             await-sym)
                         form))
                     expr)]
      (if @await-expr
        ;; Found an await - check if the replaced expr still has awaits
        (let [transformed-replaced (if (contains-await? replaced)
                                     (transform-async-body ctx replaced)
                                     replaced)]
          (list '.then (wrap-promise @await-expr)
                (list 'fn [await-sym] transformed-replaced)))
        ;; No await found (shouldn't happen if contains-await? was true)
        expr))))

(defn transform-async-body
  "Walk body and transform await calls. Expands macros first if needed."
  [ctx body]
  (if-not (contains-await? body)
    ;; No await, return as-is
    body
    ;; Has await - check if we need to expand macros first
    (let [;; Try to expand macros if it's a seq starting with a symbol
          expanded (if (and (seq? body)
                           (symbol? (first body))
                           (not (await-call? body))
                           (not (#{'let 'let* 'do 'fn 'fn* 'if 'quote} (first body))))
                     (macroexpand/macroexpand ctx body)
                     body)
          ;; If expansion changed the form, recursively transform
          body (if (not= expanded body)
                 (transform-async-body ctx expanded)
                 body)]
      (cond
        (and (seq? body) (#{'let 'let*} (first body)))
        (let [[_ bindings & exprs] body]
          (transform-let* ctx bindings exprs))

        (and (seq? body) (= 'do (first body)))
        (transform-do ctx (rest body))

        ;; Handle any other expression containing await
        (contains-await? body)
        (transform-expr-with-await ctx body)

        :else body))))
