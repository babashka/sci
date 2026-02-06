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
  (:require [sci.impl.macroexpand :as macroexpand]))

(defn await-call?
  "Check if form is (await ...)"
  [form]
  (and (seq? form) (= 'await (first form))))

(defn contains-await?
  "Check if form contains any (await ...) calls at current scope.
   Skips all fn/fn* bodies - awaits there belong to the inner function's scope."
  [form]
  (cond
    (await-call? form) true
    (and (seq? form) (#{'fn 'fn*} (first form))) false
    (seq? form) (some contains-await? form)
    (vector? form) (some contains-await? form)
    (map? form) (some contains-await? (concat (keys form) (vals form)))
    :else false))

(defn wrap-promise
  "Wrap value in js/Promise.resolve to handle non-Promise values"
  [expr]
  (list 'js/Promise.resolve expr))

(defn- promise-form?
  "Check if form is already a promise-producing expression"
  [form]
  (and (seq? form)
       (or (= '.then (first form))
           (= '.catch (first form))
           (= '.finally (first form))
           (= 'js/Promise.resolve (first form)))))

(defn- ensure-promise-result
  "Ensure async function body returns a promise.
   Wraps the last expression in js/Promise.resolve if not already a promise."
  [body-exprs]
  (if (empty? body-exprs)
    (list (wrap-promise nil))
    (let [exprs (vec body-exprs)
          last-idx (dec (count exprs))
          last-expr (nth exprs last-idx)]
      (if (promise-form? last-expr)
        body-exprs
        (assoc exprs last-idx (wrap-promise last-expr))))))

(declare transform-async-body)

(defn transform-let*
  "Transform let* with await calls into .then chains."
  [ctx locals bindings body]
  (loop [pairs (partition 2 bindings)
         acc-bindings []
         current-locals locals]
    (if-let [[binding-name init] (first pairs)]
      (let [has-await? (or (await-call? init) (contains-await? init))]
        (if has-await?
          ;; Has await - emit .then, wrap remaining in continuation
          (let [transformed-init (if (await-call? init)
                                   (wrap-promise (second init))
                                   (transform-async-body ctx current-locals init))
                rest-pairs (rest pairs)
                new-locals (conj current-locals binding-name)
                rest-body (if (seq rest-pairs)
                            (transform-let* ctx new-locals (vec (mapcat identity rest-pairs)) body)
                            (let [transformed-body (map #(transform-async-body ctx new-locals %) body)]
                              (if (= 1 (count transformed-body))
                                (first transformed-body)
                                (cons 'do transformed-body))))]
            (if (seq acc-bindings)
              (list 'let (vec acc-bindings)
                    (list '.then transformed-init
                          (list 'fn [binding-name] rest-body)))
              (list '.then transformed-init
                    (list 'fn [binding-name] rest-body))))
          ;; No await - accumulate binding as-is
          (recur (rest pairs)
                 (conj acc-bindings binding-name init)
                 (conj current-locals binding-name))))
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

(defn- replace-first-await
  "Replace the first (await ...) in form with replacement.
   Sets await-atom to the await argument when found.
   Skips fn/fn* bodies - awaits there belong to inner function."
  [form await-atom replacement]
  (cond
    ;; Already found an await
    @await-atom
    form

    ;; Found await - capture and replace
    (await-call? form)
    (do (reset! await-atom (second form))
        replacement)

    ;; Skip fn/fn* bodies entirely
    (and (seq? form) (#{'fn 'fn*} (first form)))
    form

    ;; Recurse into sequences
    (seq? form)
    (let [result (reduce (fn [acc item]
                           (conj acc (replace-first-await item await-atom replacement)))
                         []
                         form)]
      (apply list result))

    ;; Recurse into vectors
    (vector? form)
    (reduce (fn [acc item]
              (conj acc (replace-first-await item await-atom replacement)))
            []
            form)

    ;; Recurse into maps
    (map? form)
    (reduce (fn [acc [k v]]
              (assoc acc
                     (replace-first-await k await-atom replacement)
                     (replace-first-await v await-atom replacement)))
            {}
            form)

    ;; Other values pass through
    :else
    form))

(defn transform-expr-with-await
  "Transform an expression containing await by extracting the await
   and wrapping in .then."
  [ctx locals expr]
  (if (await-call? expr)
    ;; Direct await - transform the argument if it needs async transformation
    (let [await-arg (second expr)
          transformed-arg (if (contains-await? await-arg)
                            (transform-async-body ctx locals await-arg)
                            await-arg)]
      (wrap-promise transformed-arg))
    ;; Find and extract the first await, replace with gensym, wrap in .then
    (let [await-sym (gensym "await__")
          await-expr (atom nil)
          replaced (replace-first-await expr await-expr await-sym)]
      (if @await-expr
        ;; Found an await - transform both the await arg and the replaced expr
        (let [;; Transform the await argument if it needs async transformation
              transformed-await-arg (if (contains-await? @await-expr)
                                      (transform-async-body ctx locals @await-expr)
                                      @await-expr)
              ;; Add the await-sym to locals for the continuation
              new-locals (conj locals await-sym)
              ;; Transform the replaced expr if it still needs transformation
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
   (let [op (when (seq? body) (first body))
         ;; Try to expand macros first to see awaits hidden inside macro calls
         expanded (if (and (seq? body)
                           (symbol? op)
                           (not (contains? locals op))  ;; Don't expand if locally bound
                           (not (#{'await 'let 'let* 'do 'fn* 'if 'quote 'try} op)))
                    (macroexpand/macroexpand-1 ctx body)
                    body)]
     (if (not= expanded body)
       ;; Macro expanded, recurse with expanded form
       (transform-async-body ctx locals expanded)
       ;; No expansion, check for await
       (cond
         ;; No await in this form, return as-is
         (not (contains-await? body))
         body

         ;; Has await - transform based on form type
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
               transformed-then (if (contains-await? then)
                                  (transform-async-body ctx locals then)
                                  then)
               transformed-else (when else
                                  (if (contains-await? else)
                                    (transform-async-body ctx locals else)
                                    else))
               test-is-promise? (and (seq? transformed-test)
                                     (or (= '.then (first transformed-test))
                                         (= 'js/Promise.resolve (first transformed-test))))]
           (if test-is-promise?
             (let [test-binding (gensym "test__")]
               (list '.then transformed-test
                     (list 'fn [test-binding]
                           (if transformed-else
                             (list 'if test-binding transformed-then transformed-else)
                             (list 'if test-binding transformed-then)))))
             (if transformed-else
               (list 'if transformed-test transformed-then transformed-else)
               (list 'if transformed-test transformed-then))))

         ;; General case - extract await and wrap in .then
         :else
         (transform-expr-with-await ctx locals body))))))

(defn transform-async-fn-body
  "Transform async function body expressions and ensure result is a promise.
   This is the main entry point for async function transformation."
  [ctx locals body-exprs]
  (->> body-exprs
       (map #(transform-async-body ctx locals %))
       ensure-promise-result))
