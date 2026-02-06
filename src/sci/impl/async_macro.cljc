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
  [ctx locals found-await? bindings body]
  (loop [pairs (partition 2 bindings)
         acc-bindings []
         current-locals locals]
    (if-let [[binding-name init] (first pairs)]
      ;; Transform the init first to expand macros and find awaits
      (let [await-before? @found-await?
            _ (reset! found-await? false)
            transformed-init (transform-async-body ctx current-locals found-await? init)
            has-await? @found-await?]
        ;; Restore found-await? state (we found await if we did before OR now)
        (reset! found-await? (or await-before? has-await?))
        (if has-await?
          ;; Has await - emit .then, wrap remaining in continuation
          (let [;; If it's a direct await call that was transformed, use it directly
                ;; Otherwise wrap in promise
                promise-expr (if (promise-form? transformed-init)
                               transformed-init
                               (wrap-promise transformed-init))
                rest-pairs (rest pairs)
                new-locals (conj current-locals binding-name)
                rest-body (if (seq rest-pairs)
                            (transform-let* ctx new-locals found-await? (vec (mapcat identity rest-pairs)) body)
                            (let [transformed-body (map #(transform-async-body ctx new-locals found-await? %) body)]
                              (if (= 1 (count transformed-body))
                                (first transformed-body)
                                (cons 'do transformed-body))))]
            (if (seq acc-bindings)
              (list 'let (vec acc-bindings)
                    (list '.then promise-expr
                          (list 'fn [binding-name] rest-body)))
              (list '.then promise-expr
                    (list 'fn [binding-name] rest-body))))
          ;; No await in this binding - accumulate and continue
          (recur (rest pairs)
                 (conj acc-bindings binding-name transformed-init)
                 (conj current-locals binding-name))))
      ;; No more pairs, emit remaining bindings + body (recursively transformed)
      (let [transformed-body (map #(transform-async-body ctx current-locals found-await? %) body)]
        (if (seq acc-bindings)
          (list* 'let (vec acc-bindings) transformed-body)
          (if (= 1 (count transformed-body))
            (first transformed-body)
            (cons 'do transformed-body)))))))

(defn transform-do
  "Transform do with await calls into .then chains."
  [ctx locals found-await? exprs]
  (loop [exprs exprs
         acc []]
    (if (seq exprs)
      (let [expr (first exprs)
            rest-exprs (rest exprs)
            ;; Check for direct await call before transformation
            direct-await? (await-call? expr)]
        (if direct-await?
          ;; Direct await - wrap rest in .then
          (do
            (reset! found-await? true)
            (let [promise (wrap-promise (second expr))
                  rest-body (if (seq rest-exprs)
                              (transform-do ctx locals found-await? rest-exprs)
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
                  promise))))
          ;; Not a direct await, transform and check if it produced a promise
          (let [await-before? @found-await?
                _ (reset! found-await? false)
                transformed (transform-async-body ctx locals found-await? expr)
                has-await? @found-await?]
            (reset! found-await? (or await-before? has-await?))
            (if has-await?
              ;; Transformation found an await - need to chain
              (let [rest-body (if (seq rest-exprs)
                                (transform-do ctx locals found-await? rest-exprs)
                                nil)]
                (if (seq acc)
                  (let [then-expr (if rest-body
                                    (list '.then transformed (list 'fn ['_] rest-body))
                                    transformed)]
                    (list* 'do (conj acc then-expr)))
                  (if rest-body
                    (list '.then transformed (list 'fn ['_] rest-body))
                    transformed)))
              ;; No await, accumulate
              (recur rest-exprs (conj acc transformed))))))
      ;; No more exprs
      (if (= 1 (count acc))
        (first acc)
        (list* 'do acc)))))

(defn transform-try
  "Transform try/catch/finally with await into Promise .catch/.finally chains."
  [ctx locals found-await? exprs]
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
                           (transform-async-body ctx locals found-await? (first body-exprs))
                           (transform-do ctx locals found-await? body-exprs))
        ;; Wrap in Promise.resolve if not already a promise chain
        promise-chain (if (promise-form? transformed-body)
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
                                                     (transform-async-body ctx handler-locals found-await? (first handler-body))
                                                     (transform-do ctx handler-locals found-await? handler-body))]
                           (list '.catch chain (list 'fn [binding] transformed-handler))))
                       promise-chain
                       catch-clauses)
          ;; Add .finally if present
          with-finally (if finally-clause
                         (let [[_ & finally-body] finally-clause
                               transformed-finally (if (= 1 (count finally-body))
                                                     (transform-async-body ctx locals found-await? (first finally-body))
                                                     (transform-do ctx locals found-await? finally-body))]
                           (list '.finally with-catch (list 'fn [] transformed-finally)))
                         with-catch)]
      with-finally)))

(defn transform-expr-with-await
  "Transform an expression containing await by extracting the await
   and wrapping in .then. The found-await? atom tracks if await was found."
  [ctx locals found-await? expr]
  (if (await-call? expr)
    ;; Direct await - transform the argument and wrap in promise
    (do
      (reset! found-await? true)
      (let [await-arg (second expr)
            transformed-arg (transform-async-body ctx locals found-await? await-arg)]
        (wrap-promise transformed-arg)))
    ;; General expression - transform all subforms, track if any had await
    (if (seq? expr)
      (let [op (first expr)
            args (rest expr)
            ;; Transform all arguments, collecting results and tracking await
            await-before? @found-await?
            _ (reset! found-await? false)
            transformed-args (doall (map #(transform-async-body ctx locals found-await? %) args))
            any-await? @found-await?]
        (reset! found-await? (or await-before? any-await?))
        (if any-await?
          ;; Some argument had an await - need to sequence them
          ;; Find the first argument that's a promise and chain the rest
          (loop [args-before []
                 remaining-args (map vector args transformed-args)
                 needs-chain? false]
            (if (seq remaining-args)
              (let [[orig-arg transformed-arg] (first remaining-args)]
                (if (and (not needs-chain?) (promise-form? transformed-arg))
                  ;; Found first promise - chain from here
                  (let [await-sym (gensym "await__")
                        rest-args (rest remaining-args)
                        rest-expr (if (seq rest-args)
                                    ;; Build expression with remaining args
                                    (let [rebuilt (apply list op
                                                         (concat args-before
                                                                 [await-sym]
                                                                 (map second rest-args)))]
                                      ;; Recursively transform in case there are more awaits
                                      (transform-async-body ctx (conj locals await-sym) found-await? rebuilt))
                                    ;; No more args, just use the await-sym
                                    (apply list op (concat args-before [await-sym])))]
                    (list '.then transformed-arg
                          (list 'fn [await-sym] rest-expr)))
                  ;; Not a promise yet, accumulate
                  (recur (conj args-before transformed-arg)
                         (rest remaining-args)
                         (or needs-chain? (promise-form? transformed-arg)))))
              ;; All args processed, no promise found (shouldn't happen if any-await? is true)
              (apply list op transformed-args)))
          ;; No await found in args, return rebuilt expression
          (apply list op transformed-args)))
      ;; Not a seq, return as-is
      expr)))

(defn transform-async-body
  "Walk body and transform await calls. Expands macros first if needed.
   locals is a set of locally bound symbols that should not be macro-expanded.
   found-await? is an atom that gets set to true when await is encountered."
  [ctx locals found-await? body]
  (let [op (when (seq? body) (first body))
        ;; Try to expand macros first to see awaits hidden inside macro calls
        expanded (if (and (seq? body)
                          (symbol? op)
                          (not (contains? locals op))  ;; Don't expand if locally bound
                          (not (#{'await 'let 'let* 'do 'fn 'fn* 'if 'quote 'try} op)))
                   (macroexpand/macroexpand-1 ctx body)
                   body)]
    (if (not= expanded body)
      ;; Macro expanded, recurse with expanded form
      (transform-async-body ctx locals found-await? expanded)
      ;; No expansion, handle based on form type
      (cond
        ;; Direct await call
        (await-call? body)
        (do
          (reset! found-await? true)
          (let [await-arg (second body)
                transformed-arg (transform-async-body ctx locals found-await? await-arg)]
            (wrap-promise transformed-arg)))

        ;; Let form
        (and (seq? body) (#{'let 'let*} (first body)))
        (let [[_ bindings & exprs] body]
          (transform-let* ctx locals found-await? bindings exprs))

        ;; Do form
        (and (seq? body) (= 'do (first body)))
        (transform-do ctx locals found-await? (rest body))

        ;; Try form
        (and (seq? body) (= 'try (first body)))
        (transform-try ctx locals found-await? (rest body))

        ;; If form
        (and (seq? body) (= 'if (first body)))
        (let [[_ test then else] body
              transformed-test (transform-async-body ctx locals found-await? test)
              transformed-then (transform-async-body ctx locals found-await? then)
              transformed-else (when else
                                 (transform-async-body ctx locals found-await? else))
              test-is-promise? (promise-form? transformed-test)]
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

        ;; fn/fn* - don't recurse into (handled by analyzer)
        (and (seq? body) (#{'fn 'fn*} (first body)))
        body

        ;; General expression - transform subforms
        (seq? body)
        (transform-expr-with-await ctx locals found-await? body)

        ;; Not a seq, return as-is
        :else
        body))))

(defn transform-async-fn-body
  "Transform async function body expressions and ensure result is a promise.
   This is the main entry point for async function transformation."
  [ctx locals body-exprs]
  (let [found-await? (atom false)]
    (->> body-exprs
         (map #(transform-async-body ctx locals found-await? %))
         ensure-promise-result)))
