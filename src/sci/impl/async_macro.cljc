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
  [ctx locals bindings body]
  (loop [pairs (partition 2 bindings)
         acc-bindings []
         current-locals locals]
    (if-let [[binding-name init] (first pairs)]
      (if (or (await-call? init) (contains-await? init))
        ;; Emit .then, wrap remaining in continuation
        (let [;; Transform the init expression and use it as the promise
              transformed-init (if (await-call? init)
                                 (wrap-promise (second init))
                                 (transform-async-body ctx current-locals init))
              rest-pairs (rest pairs)
              ;; Add current binding to locals for rest of body
              new-locals (conj current-locals binding-name)
              ;; Recursively transform the rest body
              rest-body (if (seq rest-pairs)
                          (transform-let* ctx new-locals (vec (mapcat identity rest-pairs)) body)
                          (let [transformed-body (map #(transform-async-body ctx new-locals %) body)]
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
        ;; Accumulate non-await binding, add to locals for subsequent bindings
        (recur (rest pairs)
               (conj acc-bindings binding-name init)
               (conj current-locals binding-name)))
      ;; No more pairs, emit remaining bindings + body (recursively transformed)
      (let [transformed-body (map #(transform-async-body ctx current-locals %) body)]
        (if (seq acc-bindings)
          (list* 'let (vec acc-bindings) transformed-body)
          (if (= 1 (count transformed-body))
            (first transformed-body)
            (cons 'do transformed-body)))))))

(defn transform-do
  "Transform do with await calls into .then chains."
  [ctx locals exprs]
  (loop [exprs exprs
         acc []]
    (if (seq exprs)
      (let [expr (first exprs)
            rest-exprs (rest exprs)]
        (if (await-call? expr)
          ;; Found await - wrap rest in .then
          (let [promise (wrap-promise (second expr))
                rest-body (if (seq rest-exprs)
                            (transform-do ctx locals rest-exprs)
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
          (recur rest-exprs (conj acc (transform-async-body ctx locals expr)))))
      ;; No more exprs
      (if (= 1 (count acc))
        (first acc)
        (list* 'do acc)))))

(defn transform-try
  "Transform try/catch/finally with await into Promise .catch/.finally chains.
   (try (await p) (catch :default e handler) (finally cleanup))
   ->
   (-> (js/Promise.resolve p) (.catch (fn [e] handler)) (.finally (fn [] cleanup)))"
  [ctx locals exprs]
  (let [;; Separate body from catch/finally clauses
        catch-finally? (fn [form]
                         (and (seq? form)
                              (#{'catch 'finally} (first form))))
        body-exprs (take-while (complement catch-finally?) exprs)
        clauses (drop-while (complement catch-finally?) exprs)
        catch-clauses (filter #(and (seq? %) (= 'catch (first %))) clauses)
        finally-clause (first (filter #(and (seq? %) (= 'finally (first %))) clauses))
        ;; Transform body as a do block
        transformed-body (if (= 1 (count body-exprs))
                           (transform-async-body ctx locals (first body-exprs))
                           (transform-do ctx locals body-exprs))
        ;; Wrap in Promise.resolve if not already a promise chain
        promise-chain (if (and (seq? transformed-body)
                               (or (= '.then (first transformed-body))
                                   (= 'js/Promise.resolve (first transformed-body))))
                        transformed-body
                        (wrap-promise transformed-body))]
    ;; Add .catch clauses
    (let [with-catch (reduce
                       (fn [chain catch-clause]
                         ;; (catch Type e handler-body...)
                         (let [[_ _type binding & handler-body] catch-clause
                               ;; Add catch binding to locals for handler
                               handler-locals (conj locals binding)
                               transformed-handler (if (= 1 (count handler-body))
                                                     (transform-async-body ctx handler-locals (first handler-body))
                                                     (transform-do ctx handler-locals handler-body))]
                           (list '.catch chain (list 'fn [binding] transformed-handler))))
                       promise-chain
                       catch-clauses)
          ;; Add .finally if present
          with-finally (if finally-clause
                         (let [[_ & finally-body] finally-clause
                               transformed-finally (if (= 1 (count finally-body))
                                                     (transform-async-body ctx locals (first finally-body))
                                                     (transform-do ctx locals finally-body))]
                           (list '.finally with-catch (list 'fn [] transformed-finally)))
                         with-catch)]
      with-finally)))

(defn transform-expr-with-await
  "Transform an expression containing await by extracting the await
   and wrapping in .then."
  [ctx locals expr]
  (if (await-call? expr)
    ;; Direct await - transform the argument if it contains nested awaits
    (let [await-arg (second expr)
          transformed-arg (if (contains-await? await-arg)
                            (transform-async-body ctx locals await-arg)
                            await-arg)]
      (wrap-promise transformed-arg))
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
        ;; Found an await - transform both the await arg and the replaced expr
        (let [;; Transform the await argument if it contains nested awaits
              transformed-await-arg (if (contains-await? @await-expr)
                                      (transform-async-body ctx locals @await-expr)
                                      @await-expr)
              ;; Add the await-sym to locals for the continuation
              new-locals (conj locals await-sym)
              ;; Transform the replaced expr if it still has awaits
              transformed-replaced (if (contains-await? replaced)
                                     (transform-async-body ctx new-locals replaced)
                                     replaced)]
          (list '.then (wrap-promise transformed-await-arg)
                (list 'fn [await-sym] transformed-replaced)))
        ;; No await found (shouldn't happen if contains-await? was true)
        expr))))

(defn transform-async-body
  "Walk body and transform await calls. Expands macros first if needed.
   locals is a set of locally bound symbols that should not be macro-expanded."
  ([ctx body]
   (transform-async-body ctx #{} body))
  ([ctx locals body]
   (if-not (contains-await? body)
     ;; No await, return as-is
     body
     ;; Has await - check if we need to expand macros first
     (let [;; Try to expand macros if it's a seq starting with a symbol
           ;; BUT not if the symbol is locally bound
           op (when (seq? body) (first body))
           expanded (if (and (seq? body)
                             (symbol? op)
                             (not (contains? locals op))  ;; Don't expand if locally bound
                             (not (await-call? body))
                             (not (#{'let 'let* 'do 'fn 'fn* 'if 'quote 'try} op)))
                      (macroexpand/macroexpand-1 ctx body)
                      body)
           ;; If expansion changed the form, recursively transform
           body (if (not= expanded body)
                  (transform-async-body ctx locals expanded)
                  body)]
       (cond
         (and (seq? body) (#{'let 'let*} (first body)))
         (let [[_ bindings & exprs] body]
           (transform-let* ctx locals bindings exprs))

         (and (seq? body) (= 'do (first body)))
         (transform-do ctx locals (rest body))

         (and (seq? body) (= 'try (first body)))
         (transform-try ctx locals (rest body))

         (and (seq? body) (= 'if (first body)))
         (let [[_ test then else] body
               transformed-test (if (contains-await? test)
                                  (transform-async-body ctx locals test)
                                  test)
               ;; Transform branches independently - awaits stay in their branches
               transformed-then (if (contains-await? then)
                                  (transform-async-body ctx locals then)
                                  then)
               transformed-else (when else
                                  (if (contains-await? else)
                                    (transform-async-body ctx locals else)
                                    else))
               ;; Check if test was transformed to a promise
               test-is-promise? (and (seq? transformed-test)
                                     (or (= '.then (first transformed-test))
                                         (= 'js/Promise.resolve (first transformed-test))))]
           (if test-is-promise?
             ;; Test has await - chain the if after the promise
             (let [test-binding (gensym "test__")]
               (list '.then transformed-test
                     (list 'fn [test-binding]
                           (if transformed-else
                             (list 'if test-binding transformed-then transformed-else)
                             (list 'if test-binding transformed-then)))))
             ;; Test has no await, just rebuild if with transformed branches
             (if transformed-else
               (list 'if transformed-test transformed-then transformed-else)
               (list 'if transformed-test transformed-then))))

         ;; Handle any other expression containing await
         (contains-await? body)
         (transform-expr-with-await ctx locals body)

         :else body)))))
