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

(defn wrap-promise
  "Wrap value in promise resolve to handle non-Promise values"
  [expr]
  (list 'sci.impl.async-await/resolve expr))

(defn promise-then
  "Create a .then chain"
  [promise-expr callback]
  (list 'sci.impl.async-await/then promise-expr callback))

(defn promise-catch
  "Create a .catch chain"
  [promise-expr callback]
  (list 'sci.impl.async-await/catch promise-expr callback))

(defn promise-catch-for-try
  "Create a .catch chain that unwraps SCI error wrapping before calling handler"
  [promise-expr callback]
  (list 'sci.impl.async-await/catch-for-try promise-expr callback))

(defn promise-finally
  "Create a .finally chain"
  [promise-expr callback]
  (list 'sci.impl.async-await/finally promise-expr callback))

(defn mark-promise
  "Mark a form as promise-producing using metadata."
  [form]
  (vary-meta form assoc :sci.impl/promise true))

(defn- promise-form?
  "Check if form is already a promise-producing expression.
   Detects metadata marker or calls to sci.impl.async-await helpers."
  [form]
  (or (:sci.impl/promise (meta form))
      (and (seq? form)
           (let [op (first form)]
             (and (symbol? op)
                  (= "sci.impl.async-await" (namespace op)))))))

(defn- normalize-branches
  "If any branch is a promise, wrap all non-promise branches in resolve
   so all branches consistently return promises. Returns [normalized? branches]."
  [branches]
  (let [[has-promise? result]
        (reduce (fn [[has-promise? acc] branch]
                  (if (promise-form? branch)
                    [true (conj acc branch)]
                    [has-promise? (conj acc branch)]))
                [false []]
                branches)]
    (if has-promise?
      [true (mapv #(if (promise-form? %) % (wrap-promise %)) result)]
      [false branches])))

(declare transform-async-body)

(defn transform-do
  "Transform do with await calls into .then chains."
  [ctx locals exprs]
  (loop [exprs exprs
         acc []]
    (if (seq exprs)
      (let [expr (first exprs)
            rest-exprs (rest exprs)
            transformed (transform-async-body ctx locals expr)]
        (if (promise-form? transformed)
          ;; Found promise - wrap rest in .then
          (let [rest-body (when (seq rest-exprs) (transform-do ctx locals rest-exprs))]
            (if (seq acc)
              (mark-promise (list* 'do (conj acc (if rest-body
                                                   (promise-then transformed (list 'fn* ['_] rest-body))
                                                   transformed))))
              (if rest-body
                (promise-then transformed (list 'fn* ['_] rest-body))
                transformed)))
          (recur rest-exprs (conj acc transformed))))
      ;; No more exprs
      (if (= 1 (count acc))
        (first acc)
        (list* 'do acc)))))

(defn transform-let*
  "Transform let* with await calls into .then chains."
  [ctx locals bindings body]
  (loop [pairs (partition 2 bindings)
         acc-bindings []
         current-locals locals]
    (if-let [[binding-name init] (first pairs)]
      (let [transformed-init (transform-async-body ctx current-locals init)]
        (if (promise-form? transformed-init)
          ;; Has await - emit .then, wrap remaining in continuation
          (let [new-locals (conj current-locals binding-name)
                rest-pairs (rest pairs)
                rest-body (if (seq rest-pairs)
                            (transform-let* ctx new-locals (vec (mapcat identity rest-pairs)) body)
                            (transform-do ctx new-locals body))]
            (if (seq acc-bindings)
              (mark-promise (list 'let* (vec acc-bindings)
                                  (promise-then transformed-init (list 'fn* [binding-name] rest-body))))
              (promise-then transformed-init (list 'fn* [binding-name] rest-body))))
          ;; No await - accumulate and continue
          (recur (rest pairs)
                 (conj acc-bindings binding-name transformed-init)
                 (conj current-locals binding-name))))
      ;; No more pairs
      (let [transformed-body (transform-do ctx current-locals body)]
        (if (seq acc-bindings)
          (if (promise-form? transformed-body)
            (mark-promise (list 'let* (vec acc-bindings) transformed-body))
            (list 'let* (vec acc-bindings) transformed-body))
          transformed-body)))))

(defn transform-loop*
  "Transform loop* with await into a recursive promise-returning function.
   Wraps init values in let* to preserve sequential scoping (each init sees previous bindings).
   Returns original loop unchanged if there are no promises.

   (loop* [x 0] (if (< x 3) (do (await p) (recur (inc x))) x))
   =>
   (let* [x 0]
     ((fn loop-fn [x] (if (< x 3) (.then p (fn [_] (loop-fn (inc x)))) (js/Promise.resolve x))) x))"
  [ctx locals bindings body]
  (let [loop-fn-name (gensym "loop_fn__")
        pairs (partition 2 bindings)
        param-names (mapv first pairs)
        init-values (mapv second pairs)
        body-locals (into locals param-names)
        ;; Pass recur-target through ctx so transform-async-body replaces recur after macro expansion
        loop-ctx (assoc ctx :recur-target loop-fn-name)
        transformed-body (transform-do loop-ctx body-locals body)
        ;; Transform inits via let* to check for promises (also handles sequential scoping)
        let-bindings (vec (interleave param-names init-values))
        transformed-let-check (transform-let* ctx locals let-bindings (list transformed-body))
        ;; Check if body or inits have any promises
        has-promise? (or (promise-form? transformed-body)
                         (promise-form? transformed-let-check))]
    (if has-promise?
      ;; Has promises - build async loop
      (let [promised-body (if (promise-form? transformed-body)
                            transformed-body
                            (wrap-promise transformed-body))
            loop-fn (list 'fn* loop-fn-name param-names promised-body)
            ;; Mark as promise (not wrap) since loop-fn already returns a promise
            loop-call (mark-promise (apply list loop-fn param-names))]
        (transform-let* ctx locals let-bindings (list loop-call)))
      ;; No promises - return original loop unchanged
      (list* 'loop* bindings body))))

(defn transform-try
  "Transform try/catch/finally with await into Promise .catch/.finally chains.
   Only uses promise chains if there's actually an await somewhere."
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
        ;; Transform catch handlers
        transformed-catches (mapv (fn [catch-clause]
                                    (let [[_catch-sym type binding & handler-body] catch-clause
                                          handler-locals (conj locals binding)
                                          transformed-handler (if (= 1 (count handler-body))
                                                                (transform-async-body ctx handler-locals (first handler-body))
                                                                (transform-do ctx handler-locals handler-body))]
                                      {:clause catch-clause
                                       :type type
                                       :binding binding
                                       :transformed transformed-handler}))
                                  catch-clauses)
        ;; Transform finally if present
        transformed-finally (when finally-clause
                              (let [[_ & finally-body] finally-clause]
                                (if (= 1 (count finally-body))
                                  (transform-async-body ctx locals (first finally-body))
                                  (transform-do ctx locals finally-body))))
        ;; Check if any part has promises
        has-promise? (or (promise-form? transformed-body)
                         (some #(promise-form? (:transformed %)) transformed-catches)
                         (promise-form? transformed-finally))]
    (if has-promise?
      ;; Use promise chains - wrap body in .then so sync throws are caught
      ;; catch-for-try unwraps SCI error wrapping to preserve original ex-data
      (let [promise-chain (promise-then (wrap-promise nil)
                                        (list 'fn* ['_] transformed-body))
            with-catch (reduce
                        (fn [chain {:keys [binding transformed]}]
                          (promise-catch-for-try chain (list 'fn* [binding] transformed)))
                        promise-chain
                        transformed-catches)
            with-finally (if transformed-finally
                           (promise-finally with-catch (list 'fn* [] transformed-finally))
                           with-catch)]
        with-finally)
      ;; No promises - return original try expression unchanged
      (list* 'try exprs))))

(defn- chain-promises
  "Chain a sequence of already-transformed elements, replacing promise-producing ones
   with gensym bindings via .then chains. Calls rebuild-fn with all resolved elements."
  [transformed rebuild-fn]
  (letfn [(chain [elems-before remaining]
            (if (seq remaining)
              (let [elem (first remaining)]
                (if (promise-form? elem)
                  (let [await-sym (gensym "await__")]
                    (promise-then elem (list 'fn* [await-sym]
                                             (chain (conj elems-before await-sym)
                                                    (rest remaining)))))
                  (chain (conj elems-before elem) (rest remaining))))
              (rebuild-fn elems-before)))]
    (chain [] transformed)))

(defn- transform-coll-with-await
  "Transform a collection (vector or map entries), chaining any promise-producing elements.
   rebuild-fn takes the transformed elements and rebuilds the collection."
  [ctx locals elements rebuild-fn original]
  (let [transformed (doall (map #(transform-async-body ctx locals %) elements))]
    (if (some promise-form? transformed)
      (chain-promises transformed rebuild-fn)
      ;; No promises - return original if unchanged
      (if (= transformed (seq elements)) original (rebuild-fn transformed)))))

(defn transform-expr-with-await
  "Transform a general expression, chaining any promise-producing subforms."
  [ctx locals expr]
  (if (seq? expr)
    (let [op (first expr)
          args (rest expr)
          transformed-args (doall (map #(transform-async-body ctx locals %) args))]
      (if (some promise-form? transformed-args)
        (chain-promises transformed-args #(apply list op %))
        ;; No promises - return original if unchanged
        (if (= transformed-args (seq args)) expr (apply list op transformed-args))))
    expr))

(defn transform-async-body
  "Walk body and transform await calls. Expands macros first if needed.
   locals is a set of locally bound symbols that should not be macro-expanded."
  [ctx locals body]
  (let [op (when (seq? body) (first body))
        ;; Try to expand macros first to see awaits hidden inside macro calls
        ;; Don't expand our own promise marker - it should only be expanded by the regular analyzer
        ;; Pass locals to ctx :bindings so macros see them in &env
        ctx-with-locals (if (seq locals)
                          (update ctx :bindings merge (zipmap locals locals))
                          ctx)
        expanded (if (and (seq? body)
                          (symbol? op)
                          (not (contains? locals op))  ;; Don't expand if locally bound
                          (not (#{'recur 'await 'let* 'loop* 'do 'fn* 'if 'quote 'try 'case*} op)))
                   (macroexpand/macroexpand-1 ctx-with-locals body)
                   body)]
    (if (not= expanded body)
      ;; Macro expanded, recurse with expanded form
      (transform-async-body ctx locals expanded)
      ;; No expansion, handle based on form type
      (if (seq? body)
        (case op
          ;; Recur - replace with loop function call (after macro expansion)
          ;; Delegate to transform-expr-with-await to handle promise args
          recur
          (let [recur-target (:recur-target ctx)]
            (if recur-target
              (transform-expr-with-await ctx locals (apply list recur-target (rest body)))
              body))

          ;; Direct await call
          await
          (let [await-arg (second body)
                transformed-arg (transform-async-body ctx locals await-arg)]
            (wrap-promise transformed-arg))

          ;; Let* form (let expands to let* first)
          let*
          (let [[_ bindings & exprs] body]
            (transform-let* ctx locals bindings exprs))

          ;; Loop* form - convert to recursive function
          loop*
          (let [[_ bindings & exprs] body]
            (transform-loop* ctx locals bindings exprs))

          ;; Case* form (JVM format): (case* ge shift mask default imap switch-type check-type skip-check)
          ;; imap: {key [test-constant result-expr], ...}
          case*
          (let [[_ ge shift mask default imap switch-type check-type skip-check] body
                transformed-ge (transform-async-body ctx locals ge)
                transformed-default (transform-async-body ctx locals default)
                transformed-imap (reduce-kv
                                  (fn [m k [test-const result]]
                                    (assoc m k [test-const (transform-async-body ctx locals result)]))
                                  {} imap)
                result-exprs (mapv (fn [[_ [_ result]]] result) transformed-imap)
                all-results (conj result-exprs transformed-default)
                [any-result-promise? normalized-results] (normalize-branches all-results)
                ge-is-promise? (promise-form? transformed-ge)
                ;; Rebuild imap and default from normalized results
                normalized-default (last normalized-results)
                normalized-imap (let [keys-in-order (keys transformed-imap)]
                                  (zipmap keys-in-order
                                          (map (fn [k result]
                                                 (let [[test-const _] (get transformed-imap k)]
                                                   [test-const result]))
                                               keys-in-order normalized-results)))
                rebuild (fn [ge default imap]
                          (list 'case* ge shift mask default imap switch-type check-type skip-check))]
            (if (or ge-is-promise? any-result-promise?)
              (if ge-is-promise?
                (let [ge-binding (gensym "case_test__")]
                  (promise-then transformed-ge
                                (list 'fn* [ge-binding]
                                      (rebuild ge-binding normalized-default normalized-imap))))
                (mark-promise (rebuild transformed-ge normalized-default normalized-imap)))
              ;; No promises - return original if unchanged
              (if (and (= transformed-ge ge) (= transformed-default default)
                       (= transformed-imap imap))
                body
                (rebuild transformed-ge transformed-default transformed-imap))))

          ;; Do form
          do
          (transform-do ctx locals (rest body))

          ;; Try form
          try
          (transform-try ctx locals (rest body))

          ;; If form
          if
          (let [[_ test then else] body
                transformed-test (transform-async-body ctx locals test)
                transformed-then (transform-async-body ctx locals then)
                transformed-else (when else (transform-async-body ctx locals else))
                test-is-promise? (promise-form? transformed-test)
                [any-branch-promise? [final-then final-else]] (normalize-branches [transformed-then transformed-else])]
            (if (or test-is-promise? any-branch-promise?)
              ;; Has promises - chain
              (if test-is-promise?
                (let [test-binding (gensym "test__")]
                  (promise-then transformed-test
                                (list 'fn* [test-binding]
                                      (if final-else
                                        (list 'if test-binding final-then final-else)
                                        (list 'if test-binding final-then)))))
                (mark-promise (if final-else
                                (list 'if transformed-test final-then final-else)
                                (list 'if transformed-test final-then))))
              ;; No promises - return original if unchanged
              (if (and (= transformed-test test) (= transformed-then then)
                       (= transformed-else else))
                body
                (if transformed-else
                  (list 'if transformed-test transformed-then transformed-else)
                  (list 'if transformed-test transformed-then)))))

          ;; fn* - don't recurse into (handled by analyzer separately)
          fn* body
          ;; quote: just return expression as is
          quote body

          ;; General expression - transform subforms
          (transform-expr-with-await ctx locals body))
        ;; Not a seq - check for vector, set, or map
        (cond
          (vector? body)
          (transform-coll-with-await ctx locals body vec body)

          (set? body)
          (transform-coll-with-await ctx locals body set body)

          (map? body)
          (transform-coll-with-await ctx locals (mapcat identity body) #(apply hash-map %) body)

          :else body)))))

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

(defn transform-async-fn-body
  "Transform async function body expressions and ensure result is a promise.
   This is the main entry point for async function transformation."
  [ctx locals body-exprs]
  ;; Debug prints - keep for development, do not remove
  #_(prn :in body-exprs)
  (let [ret (->> body-exprs
                 (map #(transform-async-body ctx locals %))
                 ensure-promise-result)]
    #_(prn :ret ret)
    ret))
