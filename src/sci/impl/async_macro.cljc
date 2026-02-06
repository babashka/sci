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

(defn promise-finally
  "Create a .finally chain"
  [promise-expr callback]
  (list 'sci.impl.async-await/finally promise-expr callback))

(defn mark-promise
  "Mark a form as promise-producing. This marker is detected by promise-form?
   and stripped away by the analyzer."
  [form]
  (list 'sci.impl.async-await/promise form))

(defn- promise-form?
  "Check if form is already a promise-producing expression.
   Detects calls to sci.impl.async-await helpers and the promise marker."
  [form]
  (and (seq? form)
       (let [op (first form)]
         (or (= 'sci.impl.async-await/then op)
             (= 'sci.impl.async-await/catch op)
             (= 'sci.impl.async-await/finally op)
             (= 'sci.impl.async-await/resolve op)
             (= 'sci.impl.async-await/promise op)))))

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
          (let [rest-body (if (seq rest-exprs)
                            (transform-do ctx locals rest-exprs)
                            nil)]
            (if (seq acc)
              ;; Have accumulated expressions before promise - mark the do as promise-producing
              (let [then-expr (if rest-body
                                (promise-then transformed (list 'fn* ['_] rest-body))
                                transformed)]
                (mark-promise (list* 'do (conj acc then-expr))))
              ;; No accumulated expressions
              (if rest-body
                (promise-then transformed (list 'fn* ['_] rest-body))
                transformed)))
          ;; No promise, accumulate
          (recur rest-exprs (conj acc transformed))))
      ;; No more exprs
      (if (= 1 (count acc))
        (first acc)
        (list* 'do acc)))))

(defn replace-recur
  "Replace (recur ...) with (loop-fn-name ...) in form.
   Skips fn/fn* bodies since recur there refers to a different target."
  [form loop-fn-name]
  (cond
    (and (seq? form) (= 'recur (first form)))
    (cons loop-fn-name (rest form))

    (and (seq? form) (#{'fn 'fn* 'loop*} (first form)))
    form  ;; Don't descend into nested fn or loop

    (seq? form)
    (apply list (map #(replace-recur % loop-fn-name) form))

    (vector? form)
    (mapv #(replace-recur % loop-fn-name) form)

    (map? form)
    (into {} (map (fn [[k v]] [(replace-recur k loop-fn-name)
                               (replace-recur v loop-fn-name)]) form))

    :else form))

(defn transform-loop*
  "Transform loop* with await into a recursive promise-returning function.
   (loop* [x 0] (if (< x 3) (do (await p) (recur (inc x))) x))
   =>
   ((fn loop-fn [x] (if (< x 3) (.then p (fn [_] (loop-fn (inc x)))) (js/Promise.resolve x))) 0)"
  [ctx locals bindings body]
  (let [loop-fn-name (gensym "loop_fn__")
        pairs (partition 2 bindings)
        param-names (mapv first pairs)
        init-values (map second pairs)
        ;; Add params to locals
        body-locals (into locals param-names)
        ;; Replace recur with loop function call
        body-with-replaced-recur (map #(replace-recur % loop-fn-name) body)
        ;; Transform the body using transform-do to properly chain promises
        transformed-body (transform-do ctx body-locals body-with-replaced-recur)
        ;; Ensure promise result
        promised-body (if (promise-form? transformed-body)
                        transformed-body
                        (wrap-promise transformed-body))
        ;; Build the loop function
        loop-fn (list 'fn* loop-fn-name param-names promised-body)
        ;; Immediately invoke with initial values
        loop-call (apply list loop-fn init-values)]
    ;; Wrap in js/Promise.resolve so it's recognized as promise-producing
    (wrap-promise loop-call)))

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
          (let [rest-pairs (rest pairs)
                new-locals (conj current-locals binding-name)
                rest-body (if (seq rest-pairs)
                            (transform-let* ctx new-locals (vec (mapcat identity rest-pairs)) body)
                            ;; Use transform-do to properly chain promises in body
                            (transform-do ctx new-locals body))]
            (if (seq acc-bindings)
              ;; Wrap let* containing promise in mark-promise so it's detected
              (mark-promise (list 'let* (vec acc-bindings)
                                  (promise-then transformed-init
                                                (list 'fn* [binding-name] rest-body))))
              (promise-then transformed-init
                            (list 'fn* [binding-name] rest-body))))
          ;; No await in this binding - accumulate and continue
          (recur (rest pairs)
                 (conj acc-bindings binding-name transformed-init)
                 (conj current-locals binding-name))))
      ;; No more pairs, emit remaining bindings + body (recursively transformed)
      (let [transformed-body (transform-do ctx current-locals body)]
        (if (seq acc-bindings)
          ;; If body is promise-form, mark the whole let* as promise-producing
          (let [result (list 'let* (vec acc-bindings) transformed-body)]
            (if (promise-form? transformed-body)
              (mark-promise result)
              result))
          transformed-body)))))

(defn transform-try
  "Transform try/catch/finally with await into Promise .catch/.finally chains."
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
        promise-chain (if (promise-form? transformed-body)
                        transformed-body
                        (wrap-promise transformed-body))
        ;; Add .catch clauses
        with-catch (reduce
                    (fn [chain catch-clause]
                      ;; (catch Type e handler-body...)
                      (let [[_ _type binding & handler-body] catch-clause
                            ;; Add catch binding to locals for handler
                            handler-locals (conj locals binding)
                            transformed-handler (if (= 1 (count handler-body))
                                                  (transform-async-body ctx handler-locals (first handler-body))
                                                  (transform-do ctx handler-locals handler-body))]
                        (promise-catch chain (list 'fn* [binding] transformed-handler))))
                    promise-chain
                    catch-clauses)
        ;; Add .finally if present
        with-finally (if finally-clause
                       (let [[_ & finally-body] finally-clause
                             transformed-finally (if (= 1 (count finally-body))
                                                   (transform-async-body ctx locals (first finally-body))
                                                   (transform-do ctx locals finally-body))]
                         (promise-finally with-catch (list 'fn* [] transformed-finally)))
                       with-catch)]
    with-finally))

(defn- transform-coll-with-await
  "Transform a collection (vector or map entries), chaining any promise-producing elements.
   rebuild-fn takes the transformed elements and rebuilds the collection."
  [ctx locals elements rebuild-fn]
  (let [transformed (doall (map #(transform-async-body ctx locals %) elements))]
    (if (some promise-form? transformed)
      ;; Chain promises sequentially - don't re-transform already transformed elements
      (letfn [(chain-remaining [elems-before remaining]
                (if (seq remaining)
                  (let [elem (first remaining)]
                    (if (promise-form? elem)
                      ;; Found promise - chain from here
                      (let [await-sym (gensym "await__")
                            rest-transformed (rest remaining)
                            ;; Recursively chain the rest WITHOUT re-transforming
                            rest-expr (chain-remaining (conj elems-before await-sym)
                                                       rest-transformed)]
                        (promise-then elem (list 'fn* [await-sym] rest-expr)))
                      ;; Not a promise, accumulate
                      (chain-remaining (conj elems-before elem)
                                       (rest remaining))))
                  ;; No more elements - rebuild collection
                  (rebuild-fn elems-before)))]
        (chain-remaining [] transformed))
      ;; No promises
      (rebuild-fn transformed))))

(defn transform-expr-with-await
  "Transform a general expression, chaining any promise-producing subforms."
  [ctx locals expr]
  (if (seq? expr)
    (let [op (first expr)
          args (rest expr)
          transformed-args (doall (map #(transform-async-body ctx locals %) args))]
      ;; Check if any argument is a promise - need to chain
      (if (some promise-form? transformed-args)
        ;; Find the first promise and chain the rest
        (loop [args-before []
               remaining-args transformed-args]
          (if (seq remaining-args)
            (let [arg (first remaining-args)]
              (if (promise-form? arg)
                ;; Found first promise - chain from here
                (let [await-sym (gensym "await__")
                      rest-args (rest remaining-args)
                      rebuilt (apply list op (concat args-before [await-sym] rest-args))
                      ;; Recursively transform in case there are more promises
                      rest-expr (transform-async-body ctx (conj locals await-sym) rebuilt)]
                  (promise-then arg (list 'fn* [await-sym] rest-expr)))
                ;; Not a promise, accumulate
                (recur (conj args-before arg)
                       (rest remaining-args))))
            ;; Shouldn't reach here if some promise-form? was true
            (apply list op transformed-args)))
        ;; No promises, return rebuilt expression
        (apply list op transformed-args)))
    ;; Not a seq, return as-is
    expr))

(defn transform-async-body
  "Walk body and transform await calls. Expands macros first if needed.
   locals is a set of locally bound symbols that should not be macro-expanded."
  [ctx locals body]
  (let [op (when (seq? body) (first body))
        ;; Try to expand macros first to see awaits hidden inside macro calls
        ;; Don't expand our own promise marker - it should only be expanded by the regular analyzer
        expanded (if (and (seq? body)
                          (symbol? op)
                          (not (contains? locals op))  ;; Don't expand if locally bound
                          (not (#{'await 'let* 'loop* 'do 'fn* 'if 'quote 'try 'case*
                                  'sci.impl.async-await/promise} op)))
                   (macroexpand/macroexpand-1 ctx body)
                   body)]
    (if (not= expanded body)
      ;; Macro expanded, recurse with expanded form
      (transform-async-body ctx locals expanded)
      ;; No expansion, handle based on form type
      (if (seq? body)
        (case op
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

          ;; Case* form - transform test expr and result exprs, but NOT match constants
          ;; Structure: (case* test match1 result1 match2 result2 ... [default])
          case*
          (let [[case*-sym test-expr & clauses] body
                transformed-test (transform-async-body ctx locals test-expr)
                ;; Transform result expressions (every second element), keep match constants
                ;; If odd number of clauses, last one is default
                clause-pairs (partition 2 clauses)
                has-default? (odd? (count clauses))
                default-expr (when has-default? (last clauses))
                ;; Transform results in pairs
                transformed-pairs (mapcat (fn [[match-const result-expr]]
                                            [match-const (transform-async-body ctx locals result-expr)])
                                          (if has-default?
                                            (butlast clause-pairs)
                                            clause-pairs))
                transformed-default (when has-default?
                                      (transform-async-body ctx locals default-expr))
                ;; Check if any result is a promise-form
                result-exprs (concat (map second (partition 2 transformed-pairs))
                                     (when has-default? [transformed-default]))
                any-result-is-promise? (some promise-form? result-exprs)
                ;; If any result is promise, wrap non-promise results so all branches return promises
                normalized-pairs (if any-result-is-promise?
                                   (mapcat (fn [[match-const result]]
                                             [match-const (if (promise-form? result)
                                                            result
                                                            (wrap-promise result))])
                                           (partition 2 transformed-pairs))
                                   transformed-pairs)
                normalized-default (when has-default?
                                     (if (and any-result-is-promise?
                                              (not (promise-form? transformed-default)))
                                       (wrap-promise transformed-default)
                                       transformed-default))
                all-normalized (if has-default?
                                 (concat normalized-pairs [normalized-default])
                                 normalized-pairs)
                rebuilt-case (apply list case*-sym transformed-test all-normalized)]
            (if (promise-form? transformed-test)
              (let [test-binding (gensym "case_test__")]
                (promise-then transformed-test
                              (list 'fn* [test-binding]
                                    (apply list case*-sym test-binding all-normalized))))
              ;; Mark as promise-producing if any branch has promise
              (if any-result-is-promise?
                (mark-promise rebuilt-case)
                rebuilt-case)))

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
                transformed-else (when else
                                   (transform-async-body ctx locals else))
                then-is-promise? (promise-form? transformed-then)
                else-is-promise? (promise-form? transformed-else)
                ;; If either branch is a promise-form, ensure BOTH branches return promises
                ;; so we can safely call .then on the if result
                ;; Also handle missing else when then is promise (implicit nil else)
                final-then (if (and else-is-promise? (not then-is-promise?))
                             (wrap-promise transformed-then)
                             transformed-then)
                final-else (cond
                             ;; else is promise but then isn't - wrap then (handled above), keep else
                             else-is-promise? transformed-else
                             ;; then is promise but else isn't (including nil) - wrap else
                             then-is-promise? (wrap-promise transformed-else)
                             ;; neither is promise - keep as-is
                             :else transformed-else)
                branches-have-promise? (or then-is-promise? else-is-promise?)]
            (if (promise-form? transformed-test)
              (let [test-binding (gensym "test__")]
                (promise-then transformed-test
                              (list 'fn* [test-binding]
                                    (if final-else
                                      (list 'if test-binding final-then final-else)
                                      (list 'if test-binding final-then)))))
              ;; If branches have promises, mark the whole if as promise-producing
              (let [result (if final-else
                             (list 'if transformed-test final-then final-else)
                             (list 'if transformed-test final-then))]
                (if branches-have-promise?
                  (mark-promise result)
                  result))))

          ;; fn* - don't recurse into (handled by analyzer)
          fn* body

          ;; General expression - transform subforms
          (transform-expr-with-await ctx locals body))
        ;; Not a seq - check for vector, set, or map
        (cond
          ;; Vector literal - transform elements
          (vector? body)
          (transform-coll-with-await ctx locals body vec)

          ;; Set literal - transform elements
          (set? body)
          (transform-coll-with-await ctx locals body set)

          ;; Map literal - transform keys and values as pairs
          (map? body)
          (transform-coll-with-await ctx locals
                                     (mapcat identity body)  ;; flatten to [k1 v1 k2 v2 ...]
                                     #(apply hash-map %))

          ;; Other non-seq (symbol, keyword, number, etc.) - return as-is
          :else
          body)))))

(defn transform-async-fn-body
  "Transform async function body expressions and ensure result is a promise.
   This is the main entry point for async function transformation."
  [ctx locals body-exprs]
  #_(prn :in body-exprs)
  (let [ret (->> body-exprs
                 (map #(transform-async-body ctx locals %))
                 ensure-promise-result)]
    #_(prn :ret ret)
    ret))
