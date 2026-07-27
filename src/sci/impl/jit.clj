(ns sci.impl.jit
  "Experimental JVM codegen for analyzed fn bodies (JVM only, off by default).

  The JVM counterpart of the CLJS codegen tier (ADR 0014): the analyzer
  attaches the same walkable mini-AST, and compile-template turns an
  analyzed fn body into a Clojure form that clojure.core/eval compiles to
  bytecode, once per analyzed body. Closure creation instantiates the
  compiled factory with (ctx, enclosed-array).

  Where the CLJS tier emits JS source for js/Function, this emits Clojure
  forms for the host compiler, so `recur` maps to Clojure's own `recur`
  and calls to inlined core fns map to the core symbol - whose :inline
  expansion is by definition the fn body the interpreter would have
  called. Loop registers whose recur arguments are statically long are
  compiled a second time as primitive longs, guarded by an instance?
  check on entry; if that form does not compile, the boxed one is used.

  Demo scope (this worktree only): a body compiles only when the whole
  tree is supported, so there is no interpreter escape, no array register
  mode, no call-site stack tables and no interrupt-fn check. Everything
  else falls back to the interpreter."
  {:no-doc true}
  (:require [sci.impl.copy-vars :as copy-vars]
            [sci.impl.types :as t]))

(set! *warn-on-reflection* true)

(def strict-compile?
  "When true, compile-template rethrows an emitter exception instead of
  falling back to the interpreter."
  (volatile! false))

(def collect-forms?
  "Debug: when true, compiled template forms accumulate in last-forms."
  (volatile! false))

(def last-forms (volatile! []))

(defn enable! [] (vreset! t/jit-enabled true))
(defn disable! [] (vreset! t/jit-enabled false))
(defn enabled? [] @t/jit-enabled)

;; --- AST ---

(defn ->ast
  "Resolve a child (node or constant) to a walkable AST vector."
  [x]
  (cond
    (instance? sci.impl.types.ConstantNode x) [:const (.x ^sci.impl.types.ConstantNode x)]
    (instance? sci.impl.types.BindingNode x) [:binding (.idx ^sci.impl.types.BindingNode x)]
    (t/eval-node? x) (or (t/node-ast x) [:escape x])
    :else [:const x]))

(defn compilable?
  "True when the whole subtree is supported: this tier has no interpreter
  escape, so one unsupported node disqualifies the body."
  [x]
  (let [[op x1 x2 x3] (->ast x)]
    (case op
      (:const :binding) true
      :if (and (compilable? x1) (compilable? x2) (compilable? x3))
      :do (every? compilable? x1)
      :let (and (every? compilable? x2) (compilable? x3))
      :recur (every? compilable? x1)
      (:call-direct :call-var) (every? compilable? x2)
      false)))

;; --- emitter ---

(def ^:private inline-ops
  "Core fns that emit as their own symbol: the interpreter invokes exactly
  these fn objects (they reach call nodes as :sci.impl/inlined values), so
  emitting the symbol compiles the same call - with the compiler's :inline
  expansion applied, which is what the fn body does anyway."
  (into {}
        (keep (fn [sym]
                (let [fqsym (symbol "clojure.core" (name sym))]
                  (when-let [v (resolve fqsym)]
                    [@v fqsym]))))
        copy-vars/inlined-vars))

(def ^:private long-ops
  "Inline ops that return a primitive long when all their arguments are
  primitive longs."
  '#{clojure.core/+ clojure.core/- clojure.core/* clojure.core/inc clojure.core/dec
     clojure.core/quot clojure.core/rem clojure.core/long
     clojure.core/unchecked-add clojure.core/unchecked-subtract
     clojure.core/unchecked-multiply clojure.core/unchecked-inc clojure.core/unchecked-dec
     clojure.core/bit-and clojure.core/bit-or clojure.core/bit-xor
     clojure.core/bit-shift-left clojure.core/bit-shift-right})

(defn- reg-sym [idx] (symbol (str "b" idx)))

(defn- const!
  "Intern a value in the consts array and return the symbol bound to it."
  [consts v]
  (let [idx (count @consts)]
    (vswap! consts conj v)
    (symbol (str "c" idx))))

(defn- literal?
  "Values the compiler can see through: emitting them inline instead of as
  a consts entry is what lets loop bounds stay primitive."
  [v]
  (or (nil? v) (number? v) (string? v) (keyword? v) (boolean? v) (char? v)))

(declare emit)

;; emission is eager throughout: a lazy seq would intern its consts after
;; the template's const bindings have been built
(defn- emit-call [consts f args]
  (let [op (get inline-ops f)
        callee (or op (const! consts f))]
    (cons callee (mapv #(emit consts %) args))))

(defn- emit [consts x]
  (let [[op x1 x2 x3] (->ast x)]
    (case op
      :const (if (literal? x1) x1 (const! consts x1))
      :binding (reg-sym x1)
      :if (list 'if (emit consts x1) (emit consts x2) (emit consts x3))
      :do (cons 'do (mapv #(emit consts %) x1))
      :let (list 'let*
                 (vec (mapcat (fn [idx init] [(reg-sym idx) (emit consts init)]) x1 x2))
                 (emit consts x3))
      :recur (cons 'recur (mapv #(emit consts %) x1))
      :call-direct (emit-call consts x1 x2)
      ;; deref per call, like the interpreter: redefinition is honored
      :call-var (let [callee (list 'clojure.core/deref (const! consts x1))]
                  (cons callee (mapv #(emit consts %) x2))))))

;; --- primitive loop registers ---

(defn- long-expr?
  "True when the emitted form for x is a primitive long, assuming the
  registers in longs are primitive longs."
  [longs x]
  (let [[op x1 x2 x3] (->ast x)]
    (case op
      :const (instance? Long x1)
      :binding (contains? longs x1)
      :if (and (long-expr? longs x2) (long-expr? longs x3))
      :do (long-expr? longs (last x1))
      :let (long-expr? longs x3)
      :call-direct (and (contains? long-ops (get inline-ops x1))
                        (every? #(long-expr? longs %) x2))
      false)))

(defn- recur-nodes [acc x]
  (let [[op x1 x2 x3] (->ast x)]
    (case op
      :if (-> acc (recur-nodes x1) (recur-nodes x2) (recur-nodes x3))
      :do (reduce recur-nodes acc x1)
      :let (-> (reduce recur-nodes acc x2) (recur-nodes x3))
      :recur (conj acc x1)
      (:call-direct :call-var) (reduce recur-nodes acc x2)
      acc)))

(defn- long-registers
  "The loop registers that can be primitive longs: every recur argument
  for the register is a long expression, assuming the other candidates
  are too. Shrinks to a fixed point."
  [arity body]
  (let [recurs (recur-nodes [] body)]
    (loop [cands (set (range arity))]
      (if (empty? cands)
        cands
        (let [shrunk (reduce (fn [cands args]
                               (reduce (fn [cands idx]
                                         (if (long-expr? cands (nth args idx))
                                           cands
                                           (disj cands idx)))
                                       cands
                                       cands))
                             cands
                             recurs)]
          (if (= shrunk cands) cands (recur shrunk)))))))

;; --- template ---

(defn- template-form
  "(fn [CTX C E] (let [c0 (aget C 0) ...] (fn [b0 ...] ...))): C is the
  consts array, E the enclosed array. Consts are read once per template
  instance; enclosed values are unpacked per call, not hoisted, because
  the self-reference slot is patched after the closure is created.
  Returns [form consts-array]."
  [arity e2i-idxs body longs]
  (let [consts (volatile! [])
        params (mapv reg-sym (range arity))
        unpack (into [] (mapcat (fn [^objects pair]
                                  [(reg-sym (aget pair 1))
                                   (list 'clojure.core/aget 'E (aget pair 0))]))
                     e2i-idxs)
        emit-loop (fn [longs]
                    (list 'loop*
                          (vec (mapcat (fn [idx]
                                         [(reg-sym idx)
                                          (if (contains? longs idx)
                                            (list 'clojure.core/long (reg-sym idx))
                                            (reg-sym idx))])
                                       (range arity)))
                          (emit consts body)))
        loop-form (if (seq longs)
                    (list 'if
                          (cons 'clojure.core/and
                                (mapv (fn [idx]
                                        (list 'clojure.core/instance? 'java.lang.Long (reg-sym idx)))
                                      (sort longs)))
                          (emit-loop longs)
                          (emit-loop #{}))
                    (emit-loop #{}))
        const-binds (vec (mapcat (fn [idx]
                                   [(symbol (str "c" idx))
                                    (list 'clojure.core/aget 'C idx)])
                                 (range (count @consts))))]
    [(list 'fn* [(with-meta 'CTX nil)
                 (with-meta 'C {:tag 'objects})
                 (with-meta 'E {:tag 'objects})]
           (list 'let* const-binds
                 (list 'fn* params
                       (if (seq unpack)
                         (list 'let* unpack loop-form)
                         loop-form))))
     (object-array @consts)]))

(defn- compile-form [form]
  (binding [*ns* (find-ns 'sci.impl.jit)
            *warn-on-reflection* false
            *unchecked-math* false]
    (eval form)))

(defn compile-template
  "Compile a fn body to a template (fn [ctx enclosed-array] -> IFn), or
  nil when the body can't be compiled."
  [fn-body]
  (when (and (enabled?)
             (nil? (:vararg-idx fn-body))
             (compilable? (:body fn-body)))
    (try
      (let [arity (:fixed-arity fn-body)
            body (:body fn-body)
            e2i-idxs (:enclosed->invocation-idxs fn-body)
            longs (long-registers arity body)
            [form consts] (template-form arity e2i-idxs body longs)
            _ (when @collect-forms? (vswap! last-forms conj form))
            [factory consts]
            (if (seq longs)
              ;; a register the inference thinks is long can still fail to
              ;; compile (recur type mismatch); the boxed form always works
              (try [(compile-form form) consts]
                   (catch Exception _
                     (let [[form consts] (template-form arity e2i-idxs body #{})]
                       [(compile-form form) consts])))
              [(compile-form form) consts])]
        (fn [ctx enclosed-array]
          (factory ctx consts enclosed-array)))
      (catch Exception e
        (when @strict-compile? (throw e))
        nil))))

(defn make-fn
  "The closure for fn-body: a compiled template instance, or the
  interpreter fallback."
  [fn-body ctx enclosed-array fallback]
  (let [d (:jit-template fn-body)]
    (if (or (nil? d)
            ;; compiled loops don't emit the interrupt check gen-fn does
            (some? (:interrupt-fn ctx)))
      (fallback)
      (if-let [tpl @d]
        (tpl ctx enclosed-array)
        (fallback)))))
