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

(defn async-fn?
  "Check if form is ^:async fn or ^:async fn*"
  [form]
  (and (seq? form)
       (#{'fn 'fn*} (first form))
       (:async (meta (first form)))))

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

(defn needs-async-transform?
  "Check if form needs async transformation - either contains await or ^:async fn.
   Skips non-async fn/fn* bodies."
  [form]
  (cond
    (await-call? form) true
    (async-fn? form) true  ;; ^:async fn needs transformation
    (and (seq? form) (#{'fn 'fn*} (first form))) false
    (seq? form) (some needs-async-transform? form)
    (vector? form) (some needs-async-transform? form)
    (map? form) (some needs-async-transform? (concat (keys form) (vals form)))
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
      (let [has-await? (or (await-call? init) (contains-await? init))
            needs-transform? (needs-async-transform? init)]
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
          ;; No await - but might have ^:async fn that needs transformation
          (if needs-transform?
            ;; Transform init but don't wrap in .then (it's not a promise)
            (let [transformed-init (transform-async-body ctx current-locals init)]
              (recur (rest pairs)
                     (conj acc-bindings binding-name transformed-init)
                     (conj current-locals binding-name)))
            ;; Plain binding, accumulate as-is
            (recur (rest pairs)
                   (conj acc-bindings binding-name init)
                   (conj current-locals binding-name)))))
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
   ;; First check for ^:async fn - handle before expansion so we don't lose metadata
   (let [op (when (seq? body) (first body))
         result
         (cond
           ;; Handle ^:async fn before expansion (metadata would be lost)
           (and (seq? body)
                (#{'fn 'fn*} op)
                (:async (meta op)))
           (let [expanded (if (= 'fn op)
                            (macroexpand/macroexpand-1 ctx body)
                            body)
                 [fn*-sym & arities] expanded]
             (cons fn*-sym
                   (map (fn [arity]
                          (let [[args & fn-body] arity
                                fn-locals (into locals (filter symbol? (flatten args)))]
                            (cons args (map #(transform-async-body ctx fn-locals %) fn-body))))
                        arities)))

           ;; Try to expand macros (before checking needs-async-transform?)
           ;; This ensures we see awaits that are hidden inside macro calls
           :else
           (let [expanded (if (and (seq? body)
                                   (symbol? op)
                                   (not (contains? locals op))  ;; Don't expand if locally bound
                                   (not (await-call? body))
                                   (not (#{'let 'let* 'do 'fn* 'if 'quote 'try} op)))
                            (macroexpand/macroexpand-1 ctx body)
                            body)]
             (if (not= expanded body)
               ;; Macro expanded, recurse with expanded form
               (transform-async-body ctx locals expanded)
               ;; No expansion, now check for await (safe to skip fn/fn* since macros expanded)
               (cond
                 ;; No await or async fn in this form, return as-is
                 (not (needs-async-transform? body))
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
                 :else
                 (transform-expr-with-await ctx locals body)))))]
     (when (not= body result)
       #?(:clj (binding [*out* *err*]
                 (println "async transform:")
                 (println "  in: " (pr-str body))
                 (println "  out:" (pr-str result)))
          :cljs (do
                  (js/console.error "async transform:")
                  (js/console.error "  in: " (pr-str body))
                  (js/console.error "  out:" (pr-str result)))))
     result)))

(defn transform-async-fn-body
  "Transform async function body expressions and ensure result is a promise.
   This is the main entry point for async function transformation."
  [ctx locals body-exprs]
  (->> body-exprs
       (map #(transform-async-body ctx locals %))
       ensure-promise-result))

;; TODO:


;; cljs.user=> (.then (sci/eval-string "((^:async fn [] (await 1)))" {:classes {'js js/globalThis :allow :all}}) prn)
;; Execution error (ExceptionInfo) at (<cljs repl>:1).
;; Unable to resolve symbol: await
