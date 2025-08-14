(ns sci.impl.analyzer
  {:no-doc true
   :clj-kondo/config '{:linters {:unresolved-symbol {:exclude [ctx this bindings]}}}}
  (:refer-clojure :exclude [destructure macroexpand macroexpand-all macroexpand-1])
  (:require
   #?(:clj [sci.impl.types :as t :refer [#?(:cljs ->Node) ->constant]])
   #?(:cljs [cljs.tagged-literals :refer [JSValue]])
   #?(:cljs [goog.object :as gobj])
   #?(:cljs [sci.impl.types :as t :refer [->constant]])
   #?(:cljs [sci.impl.unrestrict :as unrestrict])
   [clojure.string :as str]
   [sci.impl.evaluator :as eval]
   [sci.impl.faster :as faster]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.load :as load]
   [sci.impl.macros :as macros]
   [sci.impl.records :as records]
   [sci.impl.resolve :as resolve]
   [sci.impl.utils :as utils :refer
    [ana-macros constant? macro? rethrow-with-location-of-node
     set-namespace! recur special-syms]]
   [sci.impl.vars :as vars]
   [sci.lang])
  #?(:clj (:import
           [sci.impl Reflector]))
  #?(:cljs
     (:require-macros
      [sci.impl.analyzer :refer [gen-return-recur
                                 gen-return-binding-call
                                 gen-return-call
                                 with-top-level-loc]])))

(defn recur-target [ctx]
  (:recur-target ctx))

(defn with-recur-target [ctx v]
  (assoc ctx :recur-target v))

(defn without-recur-target
  ([ctx]
   (assoc ctx :recur-target false))
  ([ctx reason]
   (assoc ctx :recur-target false :no-recur-reason reason)))

(defn recur-target? [ctx]
  (:recur-target ctx))

#?(:clj (set! *warn-on-reflection* true))

(defn- throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(declare analyze analyze-children analyze-call return-call return-map)

(defn analyze-children-tail [ctx children]
  (let [rt (recur-target ctx)
        non-tail-ctx (without-recur-target ctx)
        analyzed-children-non-tail (mapv #(analyze non-tail-ctx %) (butlast children))
        ret-child (analyze (with-recur-target ctx rt) (last children))]
    (conj analyzed-children-non-tail ret-child)))

(defn return-do
  [ctx expr children]
  (let [child-count (count children)]
    (if (> child-count 5)
      (let [node1 (return-do (without-recur-target ctx) expr (take 5 children))
            node2 (return-do ctx expr (drop 5 children))]
        (sci.impl.types/->Node (do (t/eval node1 ctx bindings)
                                   (t/eval node2 ctx bindings))
                               nil))
      (let [analyzed-children (analyze-children-tail ctx children)]
        (case child-count
          0 nil
          1 (nth analyzed-children 0)
          2 (let [node0 (nth analyzed-children 0)
                  node1 (nth analyzed-children 1)]
              (sci.impl.types/->Node
               (do (t/eval node0 ctx bindings)
                   (t/eval node1 ctx bindings)) nil))
          3 (let [node0 (nth analyzed-children 0)
                  node1 (nth analyzed-children 1)
                  node2 (nth analyzed-children 2)]
              (sci.impl.types/->Node
               (do (t/eval node0 ctx bindings)
                   (t/eval node1 ctx bindings)
                   (t/eval node2 ctx bindings)) nil))
          4 (let [node0 (nth analyzed-children 0)
                  node1 (nth analyzed-children 1)
                  node2 (nth analyzed-children 2)
                  node3 (nth analyzed-children 3)]
              (sci.impl.types/->Node
               (do (t/eval node0 ctx bindings)
                   (t/eval node1 ctx bindings)
                   (t/eval node2 ctx bindings)
                   (t/eval node3 ctx bindings)) nil))
          5 (let [node0 (nth analyzed-children 0)
                  node1 (nth analyzed-children 1)
                  node2 (nth analyzed-children 2)
                  node3 (nth analyzed-children 3)
                  node4 (nth analyzed-children 4)]
              (sci.impl.types/->Node
               (do (t/eval node0 ctx bindings)
                   (t/eval node1 ctx bindings)
                   (t/eval node2 ctx bindings)
                   (t/eval node3 ctx bindings)
                   (t/eval node4 ctx bindings)) nil)))))))

(defn return-or
  [ctx expr children]
  (let [child-count# (count children)]
    (if (> child-count# 5)
      (let [a0# (return-or ctx expr (take 5 children))
            a1# (return-or ctx expr (drop 5 children))]
        (sci.impl.types/->Node
         (or (t/eval a0# ctx bindings)
             (t/eval a1# ctx bindings))
         nil))
      (let [children (analyze-children-tail ctx children)]
        (case child-count#
          0 nil
          1 (analyze ctx (nth children 0))
          2 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))]
              (sci.impl.types/->Node
               (or (t/eval a0# ctx bindings)
                   (t/eval a1# ctx bindings))
               nil))
          3 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))
                  a2# (analyze ctx (nth children 2))]
              (sci.impl.types/->Node
               (or (t/eval a0# ctx bindings)
                   (t/eval a1# ctx bindings)
                   (t/eval a2# ctx bindings))
               nil))
          4 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))
                  a2# (analyze ctx (nth children 2))
                  a3# (analyze ctx (nth children 3))]
              (sci.impl.types/->Node
               (or (t/eval a0# ctx bindings)
                   (t/eval a1# ctx bindings)
                   (t/eval a2# ctx bindings)
                   (t/eval a3# ctx bindings))
               nil))
          5 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))
                  a2# (analyze ctx (nth children 2))
                  a3# (analyze ctx (nth children 3))
                  a4# (analyze ctx (nth children 4))]
              (sci.impl.types/->Node
               (or (t/eval a0# ctx bindings)
                   (t/eval a1# ctx bindings)
                   (t/eval a2# ctx bindings)
                   (t/eval a3# ctx bindings)
                   (t/eval a4# ctx bindings))
               nil)))))))

(defn return-and
  [ctx expr children]
  (let [child-count# (count children)]
    (if (> child-count# 5)
      (let [a0# (return-and ctx expr (take 5 children))
            a1# (return-and ctx expr (drop 5 children))]
        (sci.impl.types/->Node
         (and (t/eval a0# ctx bindings)
              (t/eval a1# ctx bindings))
         nil))
      (let [children (analyze-children-tail ctx children)]
        (case child-count#
          0 true
          1 (analyze ctx (nth children 0))
          2 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))]
              (sci.impl.types/->Node
               (and (t/eval a0# ctx bindings)
                    (t/eval a1# ctx bindings))
               nil))
          3 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))
                  a2# (analyze ctx (nth children 2))]
              (sci.impl.types/->Node
               (and (t/eval a0# ctx bindings)
                    (t/eval a1# ctx bindings)
                    (t/eval a2# ctx bindings))
               nil))
          4 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))
                  a2# (analyze ctx (nth children 2))
                  a3# (analyze ctx (nth children 3))]
              (sci.impl.types/->Node
               (and (t/eval a0# ctx bindings)
                    (t/eval a1# ctx bindings)
                    (t/eval a2# ctx bindings)
                    (t/eval a3# ctx bindings))
               nil))
          5 (let [a0# (analyze ctx (nth children 0))
                  a1# (analyze ctx (nth children 1))
                  a2# (analyze ctx (nth children 2))
                  a3# (analyze ctx (nth children 3))
                  a4# (analyze ctx (nth children 4))]
              (sci.impl.types/->Node
               (and (t/eval a0# ctx bindings)
                    (t/eval a1# ctx bindings)
                    (t/eval a2# ctx bindings)
                    (t/eval a3# ctx bindings)
                    (t/eval a4# ctx bindings))
               nil)))))))

(macros/deftime
  (defmacro gen-return-recur
    []
    (let [let-bindings (map (fn [i]
                              [i (vec (mapcat (fn [j]
                                                [(symbol (str "arg" j))
                                                 `(nth ~'analyzed-children ~j)
                                                 (symbol (str "param" j))
                                                 `(nth ~'params ~j)])
                                              (range i)))])
                            (range 1 20))
          recur-sym (gensym "recur")]
      `(defn ~'return-recur
         ~'[ctx expr analyzed-children]
         (when-not (recur-target? ~'ctx)
           (throw-error-with-location
            (case (:no-recur-reason ~'ctx)
              :try "Cannot recur across try"
              "Can only recur from tail position") ~'expr))
         (let [~'params (:params ~'ctx)]
           (case (count ~'analyzed-children)
             ~@(concat
                [0 `(let [recur# recur] (sci.impl.types/->Node recur# nil))]
                (mapcat (fn [[i binds]]
                          [i `(let ~(conj binds recur-sym `recur)
                                (sci.impl.types/->Node
                                 ;; important, recur vals must be evaluated with old bindings!
                                 (let [~@(mapcat (fn [j]
                                                   [(symbol (str "eval-" j))
                                                    `(t/eval ~(symbol (str "arg" j)) ~'ctx ~'bindings)])
                                                 (range i))]
                                   (do ~@(map (fn [j]
                                                `(aset
                                                  ~(with-meta 'bindings
                                                     {:tag 'objects}) ~j
                                                  ~(symbol (str "eval-" j))))
                                              (range i)))
                                   ~recur-sym)
                                 nil))])
                        let-bindings))))))))

;; (require 'clojure.pprint)
;; (clojure.pprint/pprint
;;  (clojure.core/macroexpand '(gen-return-recur)))

(declare return-recur) ;; for clj-kondo
(gen-return-recur)

(defn analyze-children [ctx children]
  (mapv #(analyze ctx %) children))

(defrecord FnBody [params body fixed-arity var-arg-name self-ref-idx iden->invoke-idx])

(declare update-parents)

(defn expand-fn-args+body [{:keys [fn-expr] :as ctx} [binding-vector & body-exprs] _macro? fn-name fn-id]
  (when-not binding-vector
    (throw-error-with-location "Parameter declaration missing." fn-expr))
  (when-not (vector? binding-vector)
    (throw-error-with-location "Parameter declaration should be a vector" fn-expr))
  (let [[fixed-args [_ var-arg-name]] (split-with #(not= '& %) binding-vector)
        fixed-args (vec fixed-args)
        fixed-arity (count fixed-args)
        ;; param-names = all simple symbols, no destructuring
        param-names (cond-> fixed-args
                      var-arg-name (conj var-arg-name))
        ctx (assoc ctx :params param-names)
        param-count (count param-names)
        param-idens (repeatedly param-count gensym)
        param-bindings (zipmap param-names param-idens)
        iden->invoke-idx (zipmap param-idens (range))
        bindings (apply dissoc (:bindings ctx) param-names)
        ctx (assoc ctx :bindings (merge bindings param-bindings))
        ctx (assoc ctx :iden->invoke-idx iden->invoke-idx)
        ctx (update ctx :parents conj (or var-arg-name fixed-arity))
        _ (vswap! (:closure-bindings ctx) assoc-in (conj (:parents ctx) :syms) (zipmap param-idens (range)))
        self-ref-idx (when fn-name (update-parents ctx (:closure-bindings ctx) fn-id))
        body (return-do (with-recur-target ctx true) fn-expr body-exprs)
        iden->invoke-idx (get-in @(:closure-bindings ctx) (conj (:parents ctx) :syms))]
    (cond-> (->FnBody binding-vector body fixed-arity var-arg-name self-ref-idx iden->invoke-idx)
      var-arg-name
      (assoc :vararg-idx (get iden->invoke-idx (last param-idens))))))

(defn analyzed-fn-meta [ctx m]
  (let [;; seq expr has location info with 2 keys
        meta-needs-eval? (> (count m) 2)
        m (if meta-needs-eval? (-> (analyze (assoc ctx :meta true) m)
                                   (vary-meta assoc :sci.impl/op :eval))
              m)]
    m))

(defn single-arity-fn [bindings-fn fn-body fn-name self-ref-in-enclosed-idx self-ref? nsm fn-meta macro?]
  (let [fixed-arity (:fixed-arity fn-body)
        copy-enclosed->invocation (:copy-enclosed->invocation fn-body)
        invoc-size (:invoc-size fn-body)
        body (:body fn-body)
        vararg-idx (:vararg-idx fn-body)]
    (sci.impl.types/->Node
     (let [enclosed-array (bindings-fn bindings)
           f (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                      body invoc-size nsm vararg-idx)
           f (if (nil? fn-meta) f
                 (let [fn-meta (t/eval fn-meta ctx bindings)]
                   (vary-meta f merge fn-meta)))
           f (if macro?
               (vary-meta f
                          #(assoc %
                                  :sci/macro macro?
                                  ;; added for better error reporting
                                  :sci.impl/inner-fn f))
               f)]
       (when self-ref?
         (aset ^objects enclosed-array
               self-ref-in-enclosed-idx
               f))
       f)
     nil)))

(defn multi-arity-fn-body [fn-body fn-name nsm]
  (let [fixed-arity (:fixed-arity fn-body)
        copy-enclosed->invocation (:copy-enclosed->invocation fn-body)
        invoc-size (:invoc-size fn-body)
        body (:body fn-body)
        vararg-idx (:vararg-idx fn-body)]
    (fn [enclosed-array]
      (sci.impl.types/->Node
       (let [f (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                        body invoc-size nsm vararg-idx)]
         f)
       nil))))

(defn analyze-fn* [ctx [_fn name? & body :as fn-expr]]
  (let [fn-expr-m (meta fn-expr)
        fn-extra-m (:sci.impl/fn fn-expr-m)
        macro? (:macro fn-extra-m)
        defn-name (:fn-name fn-extra-m)
        fn-expr-m (dissoc fn-expr-m :sci.impl/fn)
        ctx (assoc ctx :fn-expr fn-expr)
        fn-name (when (symbol? name?) name?)
        body (if fn-name
               body
               (cons name? body))
        bodies (if (seq? (first body))
                 body
                 [body])
        fn-id (gensym)
        parents ((fnil conj []) (:parents ctx) fn-id)
        ctx (assoc ctx :parents parents)
        ctx (if fn-name (-> ctx
                            (assoc-in [:bindings fn-name] fn-id))
                ctx)
        fn-name (or defn-name fn-name)
        bindings (:bindings ctx)
        bound-idens (set (vals bindings))
        ;; reverse-bindings (zipmap binding-vals (keys bindings))
        ctx (assoc ctx :outer-idens bound-idens)
        closure-bindings (:closure-bindings ctx)
        analyzed-bodies (reduce
                         (fn [{:keys [:max-fixed :min-varargs] :as acc} body]
                           (let [orig-body body
                                 arglist (first body)
                                 body (expand-fn-args+body ctx body macro? fn-name fn-id)
                                 ;; body (assoc body :sci.impl/arglist arglist)
                                 var-arg-name (:var-arg-name body)
                                 fixed-arity (:fixed-arity body)
                                 new-min-varargs (when var-arg-name fixed-arity)]
                             (when (and var-arg-name min-varargs)
                               (throw-error-with-location "Can't have more than 1 variadic overload" fn-expr))
                             (when (and (not var-arg-name) min-varargs (> fixed-arity min-varargs))
                               (throw-error-with-location
                                "Can't have fixed arity function with more params than variadic function" fn-expr))
                             (-> acc
                                 (assoc :min-varargs new-min-varargs
                                        :max-fixed (max fixed-arity
                                                        max-fixed))
                                 (update :bodies conj (assoc body :orig orig-body))
                                 (update :arglists conj arglist))))
                         {:bodies []
                          :arglists []
                          :min-var-args nil
                          :max-fixed -1} bodies)
        cb-idens-by-arity (get-in @closure-bindings parents)
        ;; all let-bound idens + closed over idens
        cb-idens (apply merge (map :syms (vals cb-idens-by-arity)))
        self-ref? (when fn-name (contains? cb-idens fn-id))
        ;; all closed over idens
        closed-over-idens (filter bound-idens (keys cb-idens))
        iden->invoke-idx (get-in @closure-bindings (conj (pop parents) :syms))
        ;; this represents the indices of enclosed values in old bindings
        ;; we need to copy those to a new array, the enclosed-array
        closed-over-iden->binding-idx (when iden->invoke-idx
                                        (zipmap closed-over-idens
                                                (mapv iden->invoke-idx closed-over-idens)))
        ;; here we decide which iden will be installed in which index in the enclosed array
        closed-over-cnt (count closed-over-idens)
        iden->enclosed-idx (zipmap closed-over-idens (range closed-over-cnt))
        iden->enclosed-idx (if fn-name
                             (assoc iden->enclosed-idx fn-id closed-over-cnt)
                             iden->enclosed-idx)
        [bindings-fn enclosed-array-cnt]
        (if (or self-ref? (seq closed-over-iden->binding-idx))
          (let [enclosed-array-cnt (cond-> closed-over-cnt
                                     fn-name (inc))
                ^objects binding->enclosed
                (into-array (keep (fn [iden]
                                    ;; for fn-id usage there is no outer binding idx
                                    (when-let [binding-idx (get iden->invoke-idx iden)]
                                      (let [enclosed-idx (get iden->enclosed-idx iden)]
                                        ;; (prn :copying binding-idx '-> enclosed-idx)
                                        (doto (object-array 2)
                                          (aset 0 binding-idx)
                                          (aset 1 enclosed-idx)))))
                                  closed-over-idens))]
            [(fn [^objects bindings]
               (areduce binding->enclosed idx ret (object-array enclosed-array-cnt)
                        (let [^objects idxs (aget binding->enclosed idx)
                              binding-idx (aget idxs 0)
                              binding-val (aget bindings binding-idx)
                              enclosed-idx (aget idxs 1)]
                          (aset ret enclosed-idx binding-val)
                          ret)))
             enclosed-array-cnt])
          [(constantly nil)])
        bodies (:bodies analyzed-bodies)
        bodies (mapv (fn [body]
                       (let [iden->invocation-idx (:iden->invoke-idx body)
                             invocation-self-idx (:self-ref-idx body)
                             enclosed->invocation
                             (into-array (keep (fn [iden]
                                                 (when-let [invocation-idx (iden->invocation-idx iden)]
                                                   (doto (object-array 2)
                                                     (aset 0 (iden->enclosed-idx iden))
                                                     (aset 1 invocation-idx))))
                                               closed-over-idens))
                             invoc-size (count iden->invocation-idx)
                             copy-enclosed->invocation
                             (when (pos? (alength ^objects enclosed->invocation))
                               (fn [^objects enclosed-array ^objects invoc-array]
                                 (areduce ^objects enclosed->invocation idx ret invoc-array
                                          (let [^objects idxs (aget ^objects enclosed->invocation idx)
                                                enclosed-idx (aget ^objects idxs 0)
                                                enclosed-val (aget ^objects enclosed-array enclosed-idx)
                                                invoc-idx (aget idxs 1)]
                                            (aset ^objects ret invoc-idx enclosed-val)
                                            ret))))]
                         (assoc body
                                :invoc-size invoc-size
                                :invocation-self-idx invocation-self-idx
                                :copy-enclosed->invocation copy-enclosed->invocation)))
                     bodies)
        ;; arglists (:arglists analyzed-bodies)
        fn-meta (dissoc fn-expr-m :line :column)
        fn-meta (when (seq fn-meta) (analyze ctx fn-meta))
        single-arity (when (= 1 (count bodies))
                       (first bodies))
        nsm (utils/current-ns-name)
        self-ref-in-enclosed-idx (some-> enclosed-array-cnt dec)
        ret-node (if single-arity
                   (single-arity-fn bindings-fn single-arity fn-name self-ref-in-enclosed-idx self-ref? nsm fn-meta macro?)
                   (let [arities (reduce
                                  (fn [arity-map fn-body]
                                    (let [f (multi-arity-fn-body fn-body fn-name nsm)
                                          var-arg? (:var-arg-name fn-body)
                                          fixed-arity (:fixed-arity fn-body)]
                                      (if var-arg?
                                        (assoc arity-map :variadic f)
                                        (assoc arity-map fixed-arity f))))
                                  {}
                                  bodies)]
                     (sci.impl.types/->Node
                      (let [enclosed-array (bindings-fn bindings)
                            f (fn [& args]
                                (let [arg-count (count args)]
                                  (if-let [f (fns/lookup-by-arity arities arg-count)]
                                    (let [f (f enclosed-array)
                                          f (t/eval f ctx bindings)]
                                      (apply f args))
                                    (throw (new #?(:clj Exception
                                                   :cljs js/Error)
                                                (let [actual-count (if macro? (- arg-count 2)
                                                                       arg-count)]
                                                  (str "Cannot call " fn-name " with " actual-count " arguments")))))))
                            f (if (nil? fn-meta) f
                                  (let [fn-meta (t/eval fn-meta ctx bindings)]
                                    (vary-meta f merge fn-meta)))
                            f (if macro?
                                (vary-meta f
                                           #(assoc %
                                                   :sci/macro macro?
                                                   ;; added for better error reporting
                                                   :sci.impl/inner-fn f))
                                f)]
                        (when self-ref?
                          (aset ^objects enclosed-array
                                self-ref-in-enclosed-idx
                                f))
                        f)
                      nil)))
        tag (:tag fn-expr-m)
        arglists (when defn-name (:arglists analyzed-bodies))]
    (cond-> ret-node
      (or tag arglists)
      (with-meta {:arglists arglists
                  :tag tag}))))

(defn update-parents
  ":syms = closed over values"
  [ctx closure-bindings ob]
  (let [parents (:parents ctx)
        new-cb (vswap! closure-bindings
                       (fn [cb]
                         (update-in cb (conj parents :syms)
                                    (fn [iden->invoke-idx]
                                      (if (contains? iden->invoke-idx ob)
                                        iden->invoke-idx
                                        (assoc iden->invoke-idx ob (count iden->invoke-idx)))))))
        closure-idx (get-in new-cb (conj parents :syms ob))]
    closure-idx))

(defn analyze-let*
  [ctx expr destructured-let-bindings exprs]
  (if (> (count destructured-let-bindings)
         10)
    (analyze-let* ctx expr
                  (take 10 destructured-let-bindings)
                  [(with-meta
                     (list* 'let* (vec (drop 10 destructured-let-bindings))
                            exprs)
                     (meta expr))])
    (let [rt (recur-target ctx)
          ctx (without-recur-target ctx)
          stack (utils/make-stack (meta expr) true)
          [ctx let-nodes idens]
          (reduce
           (fn [[ctx let-nodes idens] [binding-name binding-value]]
             (let [m (meta binding-value)
                   t (when m (:tag m))
                   binding-name (if t (vary-meta binding-name
                                                 assoc :tag t)
                                    binding-name)
                   v (analyze ctx binding-value)
                   new-iden (gensym)
                   cb (:closure-bindings ctx)
                   idx (update-parents ctx cb new-iden)
                   iden->invoke-idx (:iden->invoke-idx ctx)
                   iden->invoke-idx (assoc iden->invoke-idx new-iden idx)
                   ctx (assoc ctx :iden->invoke-idx iden->invoke-idx)]
               [(update ctx :bindings #(-> %
                                           (dissoc binding-name)
                                           (assoc binding-name new-iden)))
                (conj let-nodes v)
                (conj idens new-iden)]))
           [ctx [] []]
           (partition 2 destructured-let-bindings))
          body (return-do (with-recur-target ctx rt) expr exprs)
          iden->invoke-idx (:iden->invoke-idx ctx)
          idxs (mapv iden->invoke-idx idens)]
      ;; (prn :params params :idens idens :idxs idxs)
      (case (count idxs)
        0 (sci.impl.types/->Node
           (t/eval body ctx bindings)
           stack)
        1 (let [node0 (nth let-nodes 0)
                idx0 (nth idxs 0)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset ^objects bindings idx0 val0)
               (t/eval body ctx bindings))
             stack))
        2 (let [node0 (nth let-nodes 0)
                node1 (nth let-nodes 1)
                idx0 (nth idxs 0)
                idx1 (nth idxs 1)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset ^objects bindings idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset ^objects bindings idx1 val1)
                 (t/eval body ctx bindings)))
             stack))
        3 (let [node0 (nth let-nodes 0)
                node1 (nth let-nodes 1)
                node2 (nth let-nodes 2)
                idx0 (nth idxs 0)
                idx1 (nth idxs 1)
                idx2 (nth idxs 2)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset ^objects bindings idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset ^objects bindings idx1 val1)
                 (let [val2 (t/eval node2 ctx bindings)]
                   (aset ^objects bindings idx2 val2)
                   (t/eval body ctx bindings))))
             stack))
        4 (let [node0 (nth let-nodes 0)
                node1 (nth let-nodes 1)
                node2 (nth let-nodes 2)
                node3 (nth let-nodes 3)
                idx0 (nth idxs 0)
                idx1 (nth idxs 1)
                idx2 (nth idxs 2)
                idx3 (nth idxs 3)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset ^objects bindings idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset ^objects bindings idx1 val1)
                 (let [val2 (t/eval node2 ctx bindings)]
                   (aset ^objects bindings idx2 val2)
                   (let [val3 (t/eval node3 ctx bindings)]
                     (aset ^objects bindings idx3 val3)
                     (t/eval body ctx bindings)))))
             stack))
        5 (let [node0 (nth let-nodes 0)
                node1 (nth let-nodes 1)
                node2 (nth let-nodes 2)
                node3 (nth let-nodes 3)
                node4 (nth let-nodes 4)
                idx0 (nth idxs 0)
                idx1 (nth idxs 1)
                idx2 (nth idxs 2)
                idx3 (nth idxs 3)
                idx4 (nth idxs 4)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset ^objects bindings idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset ^objects bindings idx1 val1)
                 (let [val2 (t/eval node2 ctx bindings)]
                   (aset ^objects bindings idx2 val2)
                   (let [val3 (t/eval node3 ctx bindings)]
                     (aset ^objects bindings idx3 val3)
                     (let [val4 (t/eval node4 ctx bindings)]
                       (aset ^objects bindings idx4 val4)
                       (t/eval body ctx bindings))))))
             stack))))))

(defn init-var! [ctx name expr]
  (let [cnn (utils/current-ns-name)
        env (:env ctx)
        the-current-ns (get-in @env [:namespaces cnn]
                               ;; namespace could be absent in config
                               {})
        refers (:refers the-current-ns)
        the-current-ns (if-let [x (and refers (.get ^java.util.Map refers name))]
                         (throw-error-with-location
                          (str name " already refers to "
                               x " in namespace "
                               cnn)
                          expr)
                         (if-let [the-var #?(:clj (.get ^java.util.Map the-current-ns name)
                                             :cljs (get the-current-ns name))]
                           (let [cur-file @utils/current-file]
                             (when-not (= cur-file (:file (meta the-var)))
                               (alter-meta! the-var assoc :file cur-file))
                             the-current-ns)
                           (assoc the-current-ns name
                                  (doto (sci.lang.Var. nil (symbol (str cnn)
                                                                   (str name))
                                                       {:name name
                                                        :ns @utils/current-ns
                                                        :file @utils/current-file}
                                                       false
                                                       false
                                                       nil)
                                    (vars/unbind)))))]
    (swap! env
           (fn [env]
             (update env :namespaces assoc cnn the-current-ns))))
  nil)

(defn analyze-def
  [ctx expr]
  (let [ctx (without-recur-target ctx)
        [_def var-name ?docstring ?init] expr
        curr-ns @utils/current-ns
        simple? (simple-symbol? var-name)]
    (when-not (or simple?
                  (= (namespace var-name)
                     (str (t/getName curr-ns))))
      (throw-error-with-location "Var name should be simple symbol." expr))
    (let [var-name (if simple? var-name (symbol (name var-name)))]
      (init-var! ctx var-name expr)
      (let [arg-count (count expr)
            docstring (when (and (= 4 arg-count)
                                 (string? ?docstring))
                        ?docstring)
            expected-arg-count (if docstring 4 3)]
        (when-not (<= arg-count expected-arg-count)
          (throw (new #?(:clj IllegalArgumentException
                         :cljs js/Error)
                      "Too many arguments to def")))
        (let [init (if docstring ?init ?docstring)
              init (if (= 2 arg-count)
                     utils/var-unbound
                     (analyze ctx init))
              expr-loc (meta expr)
              expr-loc? (:line expr-loc)
              var-meta (meta var-name)
              m (if expr-loc?
                  (-> var-meta
                      (assoc :line (:line expr-loc))
                      (assoc :column (:column expr-loc)))
                  (let [top-level-loc utils/*top-level-location*]
                    (-> var-meta
                        (assoc :line (:line top-level-loc))
                        (assoc :column (:column top-level-loc)))))
              m-needs-eval? var-meta
              m (assoc m :ns curr-ns)
              m (if docstring (assoc m :doc docstring) m)
              m (if m-needs-eval?
                  (analyze ctx m)
                  (->constant m))]
          (sci.impl.types/->Node
           (eval/eval-def ctx bindings var-name init m)
           nil))))))

#_(defn analyze-defn [ctx [op fn-name & body :as expr]]
    ;; TODO: re-use analyze-def
    (when-not (simple-symbol? fn-name)
      (throw-error-with-location "Var name should be simple symbol." expr))
    (init-var! ctx fn-name expr)
    (let [macro? (= "defmacro" (name op))
          [pre-body body] (split-with (comp not sequential?) body)
          _ (when (empty? body)
              (throw-error-with-location "Parameter declaration missing." expr))
          docstring (when-let [ds (first pre-body)]
                      (when (string? ds) ds))
          meta-map (when-let [m (last pre-body)]
                     (when (map? m) m))
          [meta-map2 body] (if (seq? (first body))
                             (let [lb (last body)]
                               (if (map? lb)
                                 [lb (butlast body)]
                                 [nil body]))
                             [nil body])
          expr-loc (meta expr)
          meta-map (-> (meta fn-name)
                       (assoc :line (:line expr-loc))
                       (assoc :column (:column expr-loc))
                       (cond-> meta-map (merge meta-map)))
          meta-map (if meta-map2 (merge meta-map meta-map2)
                       meta-map)
          fn-body (cons 'fn body)
          f (analyze-fn* ctx fn-body macro? fn-name)
          arglists (list 'quote (seq (:arglists (meta f))))
          meta-map (assoc meta-map
                          :ns @utils/current-ns
                          :arglists arglists)
          meta-map (cond-> meta-map
                     docstring (assoc :doc docstring)
                     macro? (assoc :macro true))
          meta-map (analyze ctx meta-map)]
      (sci.impl.types/->Node
       (eval/eval-def ctx bindings fn-name f meta-map)
       nil)))

(defn analyze-loop*
  [ctx expr]
  (let [bv (second expr)
        syms (take-nth 2 bv)
        body (nnext expr)
        expansion `(let* ~bv
                     ~(list* `(fn* ~(vec syms) ~@body)
                             syms))]
    (analyze ctx expansion)))

(defn analyze-lazy-seq
  [ctx expr]
  (let [body (rest expr)
        ctx (with-recur-target ctx true) ;; body is analyzed in context of implicit no-arg fn
        ana (return-do ctx expr body)]
    (sci.impl.types/->Node
     (lazy-seq (t/eval ana ctx bindings))
     nil)))

(defn return-if
  [ctx expr]
  (let [exprs (rest expr)
        children (analyze-children ctx exprs)
        stack (assoc (meta expr)
                     :ns @utils/current-ns
                     :file @utils/current-file
                     :special true)]
    (case (count children)
      (0 1) (throw-error-with-location "Too few arguments to if" expr)
      2 (let [condition (nth children 0)
              then (nth children 1)]
          (cond (not condition) nil
                (constant? condition) then
                :else (sci.impl.types/->Node
                       (when (t/eval condition ctx bindings)
                         (t/eval then ctx bindings))
                       stack)))
      3 (let [condition (nth children 0)
              then (nth children 1)
              else (nth children 2)]
          (cond (not condition) else
                (constant? condition) then
                :else (sci.impl.types/->Node
                       (if (t/eval condition ctx bindings)
                         (t/eval then ctx bindings)
                         (t/eval else ctx bindings))
                       stack)))
      (throw-error-with-location "Too many arguments to if" expr))))

(defn analyze-case*
  [ctx expr]
  (let [ctx-wo-rt (without-recur-target ctx)
        case-val (analyze ctx-wo-rt (second expr))
        clauses (nnext expr)
        match-clauses (take-nth 2 clauses)
        result-clauses (analyze-children ctx (take-nth 2 (rest clauses)))
        [default? case-default] (when (odd? (count clauses))
                                  [true (analyze ctx (last clauses))])
        cases (interleave match-clauses result-clauses)
        assoc-new (fn [m k v]
                    (if-not (contains? m k)
                      (assoc m k v)
                      (throw-error-with-location (str "Duplicate case test constant " k)
                                                 expr)))
        case-map (loop [cases (seq cases)
                        ret-map {}]
                   (if cases
                     (let [[k v & cases] cases]
                       (if (seq? k)
                         (recur
                          cases
                          (reduce (fn [acc k]
                                    (assoc-new acc k v))
                                  ret-map
                                  k))
                         (recur
                          cases
                          (assoc-new ret-map k v))))
                     ret-map))
        f (if default?
            (sci.impl.types/->Node
             (eval/eval-case ctx bindings case-map case-val case-default)
             nil)
            (sci.impl.types/->Node
             (eval/eval-case ctx bindings case-map case-val)
             nil))]
    f))

(defn analyze-try
  [ctx expr]
  (let [ctx (without-recur-target ctx :try)
        body (next expr)
        stack (utils/make-stack (meta expr) true)
        [body-exprs
         catches
         finally]
        (loop [exprs body
               body-exprs []
               catch-exprs []
               finally-expr nil]
          (if exprs
            (let [expr (first exprs)
                  exprs (next exprs)]
              (cond (and (seq? expr) (= 'catch (first expr)))
                    (recur exprs body-exprs (conj catch-exprs expr) finally-expr)
                    (and (not exprs) (seq? expr) (= 'finally (first expr)))
                    [body-exprs catch-exprs expr]
                    :else
                    ;; TODO: cannot add body expression when catch is not empty
                    ;; TODO: can't have finally as non-last expression
                    (recur exprs (conj body-exprs expr) catch-exprs finally-expr)))
            [body-exprs catch-exprs finally-expr]))
        body (analyze ctx (cons 'do body-exprs))
        catches (mapv (fn [c]
                        (let [[_ ex binding & body] c]
                          (if-let [clazz #?(:clj (interop/resolve-class ctx ex)
                                            :cljs (case ex
                                                    js/Error js/Error
                                                    js/Object js/Object
                                                    :default :default
                                                    (analyze ctx ex)))]
                            (let [ex-iden (gensym)
                                  closure-bindings (:closure-bindings ctx)
                                  ex-idx (update-parents ctx closure-bindings ex-iden)
                                  ctx (-> ctx
                                          (update-in [:bindings] (fn [bindings]
                                                                   (-> bindings
                                                                       (dissoc binding)
                                                                       (assoc binding ex-iden))))
                                          (assoc-in [:iden->invoke-idx ex-iden] ex-idx))
                                  analyzed-body (analyze ctx
                                                         (cons 'do body))]
                              {:class clazz
                               :ex-idx ex-idx
                               :body analyzed-body
                               :ex ex})
                            (throw-error-with-location (str "Unable to resolve classname: " ex) ex))))
                      catches)
        sci-error (let [fst (when (= 1 (count catches))
                              (nth catches 0))
                        ex (:ex fst)]
                    (and (= #?(:clj 'Exception
                               :cljs 'js/Error) ex)
                         (some-> ex meta :sci/error)))
        finally (when finally
                  (analyze ctx (cons 'do (rest finally))))]
    (sci.impl.types/->Node
     (eval/eval-try ctx bindings body catches finally sci-error)
     stack)))

(defn analyze-throw [ctx [_throw ex :as expr]]
  (when-not (= 2 (count expr))
    (throw-error-with-location
     #?(:clj "Too many arguments to throw, throw expects a single Throwable instance"
        :cljs "Too many arguments to throw")
     expr))
  (let [ctx (without-recur-target ctx)
        ana (analyze ctx ex)
        stack (assoc (meta expr)
                     :ns @utils/current-ns
                     :file @utils/current-file
                     :special true)]
    (sci.impl.types/->Node
     (rethrow-with-location-of-node ctx bindings (t/eval ana ctx bindings) this)
     stack)))

;;;; Interop

(defn analyze-dot [ctx [_dot instance-expr method-expr & args :as expr]]
  (let [ctx (without-recur-target ctx)
        [method-expr & args] (if (seq? method-expr) method-expr
                                 (cons method-expr args))
        instance-expr (analyze ctx instance-expr)
        #?@(:clj [instance-expr (utils/vary-meta*
                                 instance-expr
                                 (fn [m]
                                   (if-let [t (:tag m)]
                                     (let [clazz (or (interop/resolve-class ctx t)
                                                     (records/resolve-record-class ctx t)
                                                     (throw-error-with-location
                                                      (str "Unable to resolve classname: " t) t))]
                                       (assoc m :tag-class clazz))
                                     m)))])
        method-name (name method-expr)
        args (when args (analyze-children ctx args))
        res
        (let [field-access (str/starts-with? method-name "-")
              meth-name (if field-access
                          (subs method-name 1)
                          method-name)
              meth-name* meth-name
              meth-name (munge meth-name)
              stack (assoc (meta expr)
                           :ns @utils/current-ns
                           :file @utils/current-file)]
          #?(:clj (if (class? instance-expr)
                    (let [static-method
                          #(let [arg-count (count args)
                                 args (object-array args)
                                 class-expr (:class-expr (meta expr))]
                             ;; prefab static-methods
                             (if-let [f (some-> ctx :env deref
                                                :class->opts :static-methods
                                                (get (interop/fully-qualify-class ctx class-expr))
                                                (get method-expr))]
                               (return-call ctx expr f (cons instance-expr args) stack nil)
                               (sci.impl.types/->Node
                                (interop/invoke-static-method ctx bindings instance-expr method-name
                                                              args arg-count)
                                stack)))]
                      (if (nil? args)
                        (if field-access
                          (let [method-name (subs method-name 1)]
                            (sci.impl.types/->Node
                             (interop/get-static-field instance-expr method-name)
                             stack))
                          ;; https://clojure.org/reference/java_interop
                          ;; If the second operand is a symbol and no args are
                          ;; supplied it is taken to be a field access - the
                          ;; name of the field is the name of the symbol, and
                          ;; the value of the expression is the value of the
                          ;; field, unless there is a no argument public method
                          ;; of the same name, in which case it resolves to a
                          ;; call to the method.
                          (if-let [_
                                   (try (Reflector/getStaticField ^Class instance-expr ^String method-name)
                                        (catch IllegalArgumentException _ nil))]
                            (sci.impl.types/->Node
                             (interop/get-static-field instance-expr method-name)
                             stack)
                            (static-method)))
                        (static-method)))
                    (let [arg-count #?(:cljs nil :clj (count args))
                          args (object-array args)
                          #?@(:clj [^"[Ljava.lang.Class;" arg-types (when (pos? arg-count)
                                                                      (make-array Class arg-count))
                                    has-types? (volatile! nil)])]
                      #?(:clj (when arg-types
                                (areduce args idx _ret nil
                                         (let [arg (aget args idx)
                                               arg-meta (meta arg)]
                                           (when-let [t (:tag arg-meta)]
                                             (when-let [t (interop/resolve-type-hint ctx t)]
                                               (do (vreset! has-types? true)
                                                   (aset arg-types idx t))))))))
                      (with-meta (sci.impl.types/->Node
                                  (eval/eval-instance-method-invocation
                                   ctx bindings instance-expr meth-name meth-name* field-access args arg-count
                                   #?(:cljs nil
                                      :clj (when @has-types?
                                             arg-types)))
                                  stack)
                        {::instance-expr instance-expr
                         ::method-name method-name
                         :tag (:tag (meta expr))})))
             :cljs (let [allowed? (or unrestrict/*unrestricted*
                                      (identical? method-expr utils/allowed-append)
                                      (-> ctx :env deref :class->opts :allow))
                         args (into-array args)]
                     (with-meta
                       (case [(boolean allowed?) (boolean field-access)]
                         [true true]
                         (sci.impl.types/->Node
                          (eval/allowed-instance-field-invocation ctx bindings instance-expr meth-name)
                          stack)
                         [true false]
                         (sci.impl.types/->Node
                          (eval/allowed-instance-method-invocation ctx bindings instance-expr meth-name args nil)
                          stack)
                         ;; default case
                         (sci.impl.types/->Node
                          (eval/eval-instance-method-invocation
                           ctx bindings instance-expr meth-name meth-name* field-access args allowed? nil nil)
                          stack))
                       {::instance-expr instance-expr
                        ::method-name method-name}))))]
    res))

(defn expand-dot**
  "Expands (. x method)"
  [ctx expr]
  (when (< (count expr) 3)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (analyze-dot ctx expr))

(defn expand-dot*
  "Expands (.foo x)"
  [ctx [method-name obj & args :as expr]]
  (when (< (count expr) 2)
    (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (analyze-dot ctx (with-meta (list '. obj (cons (symbol (subs (name method-name) 1)) args)) (meta expr))))

#?(:clj
   (defn- invoke-constructor-node [ctx class args]
     (let [ctx (without-recur-target ctx)
           args (analyze-children ctx args)]
       (sci.impl.types/->Node
        (interop/invoke-constructor class (mapv #(t/eval % ctx bindings) args))
        nil))))

(defn analyze-new [ctx [_new class-sym & args :as expr]]
  (let [ctx (without-recur-target ctx)]
    #?(:clj (if-let [class (:class (interop/resolve-class-opts ctx class-sym))]
              (invoke-constructor-node ctx class args)
              (if-let [record (records/resolve-record-class ctx class-sym)]
                (let [args (analyze-children ctx args)]
                  ;; _ctx expr f analyzed-children stack
                  (return-call ctx
                               ;; for backwards compatibility with error reporting
                               expr
                               (:sci.impl/constructor (meta record))
                               args
                               (assoc (meta expr)
                                      :ns @utils/current-ns
                                      :file @utils/current-file)
                               nil))
                (throw-error-with-location (str "Unable to resolve classname: " class-sym) class-sym)))
       :cljs (if (symbol? class-sym)
               ;; try to statically analyze class for better performance
               (if-let [class (or
                               (when-let [clazz (when-let [opts (interop/resolve-class-opts ctx class-sym)]
                                                  (or
                                                   ;; TODO: deprecate
                                                   (:constructor opts)
                                                   (:class opts)))]
                                 clazz)
                               (resolve/resolve-symbol ctx class-sym false))]
                 (let [args (analyze-children ctx args)
                       var? (utils/var? class)
                       maybe-var (when var? class)
                       maybe-record (cond
                                      var?
                                      (deref maybe-var)
                                      ;; symbol = already deref-ed record coming in via :import
                                      (symbol? class)
                                      class)
                       maybe-record-constructor
                       (when maybe-record
                         (-> maybe-record
                             meta :sci.impl/constructor))]
                   (cond maybe-record-constructor
                         (return-call ctx
                                      ;; for backwards compatibility with error reporting
                                      expr
                                      maybe-record-constructor
                                      args
                                      (assoc (meta expr)
                                             :ns @utils/current-ns
                                             :file @utils/current-file)
                                      nil)
                         var?
                         (let [args (into-array args)]
                           (sci.impl.types/->Node
                            (interop/invoke-js-constructor* ctx bindings (deref maybe-var)
                                                            args)
                            nil))
                         (instance? sci.impl.types/NodeR class)
                         (let [args (into-array args)]
                           (sci.impl.types/->Node
                            (interop/invoke-js-constructor* ctx bindings
                                                            (t/eval class ctx bindings)
                                                            args)
                            nil))
                         :else
                         (let [args (into-array args)]
                           (sci.impl.types/->Node
                            (interop/invoke-js-constructor* ctx bindings class ;; no eval needed
                                                            args)
                            nil))))
                 (if-let [record (records/resolve-record-class ctx class-sym)]
                   (let [args (analyze-children ctx args)]
                     (return-call ctx
                                  ;; for backwards compatibility with error reporting
                                  expr
                                  (:sci.impl/constructor (meta record))
                                  args
                                  (assoc (meta expr)
                                         :ns @utils/current-ns
                                         :file @utils/current-file)
                                  nil))
                   (throw-error-with-location (str "Unable to resolve classname: " class-sym) class-sym)))
               (let [class (analyze ctx class-sym)
                     args (analyze-children ctx args)
                     args (into-array args)]
                 (sci.impl.types/->Node
                  (interop/invoke-js-constructor*
                   ctx bindings (t/eval class ctx bindings)
                   args)
                  nil))))))

(defn expand-constructor [ctx [constructor-sym & args]]
  (let [constructor-name (name constructor-sym)
        class-sym (with-meta (symbol (namespace constructor-sym)
                                     (subs constructor-name 0
                                           (dec (count constructor-name))))
                    (meta constructor-sym))]
    (analyze-new ctx (with-meta (list* 'new class-sym args)
                       (meta constructor-sym)))))

;;;; End interop

;;;; Namespaces

(defn return-ns-op [_ctx f expr analyzed-args]
  (let [stack (assoc (meta expr)
                     :file @utils/current-file
                     :ns @utils/current-ns)]
    (sci.impl.types/->Node
     (try
       (apply f ctx analyzed-args)
       (catch #?(:clj Throwable :cljs js/Error) e
         (rethrow-with-location-of-node ctx bindings e this)))
     stack)))

(defn analyze-ns-form [ctx [_ns ns-name & exprs :as expr]]
  (when-not (symbol? ns-name)
    (throw (new #?(:clj IllegalArgumentException
                   :cljs js/Error)
                (str "Namespace name must be symbol, got: " (pr-str ns-name)))))
  (let [[docstring exprs]
        (let [fexpr (first exprs)]
          (if (string? fexpr)
            [fexpr (next exprs)]
            [nil exprs]))
        ;; skip attr-map
        [attr-map exprs]
        (let [m (first exprs)]
          (if (map? m)
            [m (next exprs)]
            [nil exprs]))
        attr-map (if docstring
                   (assoc attr-map :doc docstring)
                   attr-map)]
    (set-namespace! ctx ns-name attr-map)
    (loop [exprs exprs
           ret []]
      (if exprs
        (let [[k & args :as expr] (first exprs)]
          (case k
            (:require :require-macros :use :import :refer-clojure)
            (recur (next exprs)
                   (conj ret
                         (return-ns-op
                          ctx (case k
                                :require load/eval-require
                                #?@(:cljs [:require-macros load/eval-require-macros])
                                :use load/eval-use
                                :import eval/eval-import
                                :refer-clojure (fn [ctx & args]
                                                 (apply load/eval-refer ctx 'clojure.core args)))
                          expr args)))
            :gen-class ;; ignore
            (recur (next exprs) ret)))
        (return-do
         ctx
         expr
         (conj ret
               (sci.impl.types/->Node
                (do (load/add-loaded-lib @(:env ctx) ns-name) nil)
                nil)))))))

;;;; End namespaces


;;;; Vars

(defn analyze-var [ctx [_ var-name]]
  (resolve/resolve-symbol ctx var-name))

(defn analyze-set! [ctx [_ obj v :as expr]]
  (cond
    #?@(:cljs [(and (= 4 (count expr))
                    (str/starts-with? (str v) "-"))
               (let [obj (analyze ctx obj)
                     prop (munge (subs (str v) 1))
                     v (analyze ctx (nth expr 3))]
                 (sci.impl.types/->Node
                  (let [obj (t/eval obj ctx bindings)
                        v (t/eval v ctx bindings)]
                    (gobj/set obj prop v))
                  nil))])
    (symbol? obj) ;; assume dynamic var
    (let [sym obj
          obj (resolve/resolve-symbol ctx obj)
          v (analyze ctx v)]
      (cond (utils/var? obj)
            (sci.impl.types/->Node
             (let [v (t/eval v ctx bindings)]
               (t/setVal obj v))
             nil)
            (:mutable (meta obj))
            (let [instance (resolve/resolve-symbol ctx '__sci_this)
                  mutator (get (:local->mutator ctx) sym)]
              (sci.impl.types/->Node
               (let [v (t/eval v ctx bindings)
                     instance (t/eval instance ctx bindings)]
                 (mutator instance v))
               nil))
            :else (throw-error-with-location "Invalid assignment target" expr)))
    #?@(:cljs [(seq? obj)
               (let [obj (analyze ctx obj)
                     v (analyze ctx v)
                     info (meta obj)
                     k (subs (::method-name info) 1)
                     obj (::instance-expr info)]
                 (sci.impl.types/->Node
                  (let [obj (t/eval obj ctx bindings)
                        v (t/eval v ctx bindings)]
                    (gobj/set obj k v))
                  nil))])
    :else (throw-error-with-location "Invalid assignment target" expr)))

;;;; End vars

(macros/deftime
  (defmacro gen-return-binding-call
    "Creates returning-binding-call function, optimizes calling a local
  binding as function."
    []
    (let [let-bindings (map (fn [i]
                              [i (vec (mapcat (fn [j]
                                                [(symbol (str "arg" j))
                                                 `(nth ~'analyzed-children ~j)])
                                              (range i)))])
                            (range 20))]
      `(defn ~'return-binding-call
         ~'[_ctx expr idx f analyzed-children stack]
         (case (count ~'analyzed-children)
           ~@(concat
              (mapcat (fn [[i binds]]
                        [i `(let ~binds
                              (sci.impl.types/->Node
                               (try
                                 ((aget ~(with-meta 'bindings
                                           {:tag 'objects}) ~'idx)
                                  ~@(map (fn [j]
                                           `(t/eval ~(symbol (str "arg" j)) ~'ctx ~'bindings))
                                         (range i)))
                                 (catch ~(macros/? :clj 'Throwable :cljs 'js/Error) e#
                                   (rethrow-with-location-of-node ~'ctx ~'bindings e# ~'this)))
                               ~'stack))])
                      let-bindings)
              `[(fn [~'ctx ~'bindings]
                  (eval/fn-call ~'ctx ~'bindings (aget ~(with-meta 'bindings
                                                          {:tag 'objects}) ~'idx) ~'analyzed-children))]))))))

(declare return-binding-call) ;; for clj-kondo
(gen-return-binding-call)

;; NOTE: there is a small perf win (about 3%) when checking if all
;; analyzed-children are EvalFn and then using those fns directly. See
;; inline-evals branch.

(macros/deftime
  (defmacro gen-return-call
    []
    (let [let-bindings (map (fn [i]
                              [i (vec (mapcat (fn [j]
                                                [(symbol (str "arg" j))
                                                 `(nth ~'analyzed-children ~j)])
                                              (range i)))])
                            (range 20))]
      `(defn ~'return-call
         ~'[_ctx expr f analyzed-children stack wrap]
         (let [node#
               (case (count ~'analyzed-children)
                 ~@(concat
                    (mapcat (fn [[i binds]]
                              [i `(let ~binds
                                    (if ~'wrap
                                      (sci.impl.types/->Node
                                       (try
                                         ((~'wrap ~'ctx ~'bindings ~'f)
                                          ~@(map (fn [j]
                                                   `(t/eval ~(symbol (str "arg" j)) ~'ctx ~'bindings))
                                                 (range i)))
                                         (catch ~(macros/? :clj 'Throwable :cljs 'js/Error) e#
                                           (rethrow-with-location-of-node ~'ctx ~'bindings e# ~'this)))
                                       ~'stack)
                                      (sci.impl.types/->Node
                                       (try
                                         (~'f
                                          ~@(map (fn [j]
                                                   `(t/eval ~(symbol (str "arg" j)) ~'ctx ~'bindings))
                                                 (range i)))
                                         (catch ~(macros/? :clj 'Throwable :cljs 'js/Error) e#
                                           (rethrow-with-location-of-node ~'ctx ~'bindings e# ~'this)))
                                       ~'stack)))])
                            let-bindings)
                    `[(if ~'wrap
                        (sci.impl.types/->Node
                         (eval/fn-call ~'ctx ~'bindings (~'wrap ~'ctx ~'bindings ~'f) ~'analyzed-children)
                         ~'stack)
                        (sci.impl.types/->Node
                         (eval/fn-call ~'ctx ~'bindings ~'f ~'analyzed-children)
                         ~'stack))]))
               tag# ~'(:tag (meta expr))]
           (cond-> node#
             tag# (with-meta {:tag tag#})))))))

(declare return-call) ;; for clj-kondo
(gen-return-call)

(defn analyze-quote [_ctx expr]
  (when-not (= 2 (count expr))
    (throw-error-with-location "Wrong number of args (0) passed to quote" expr))
  (let [snd (second expr)]
    (->constant snd)))

(defn analyze-import [_ctx expr]
  (let [args (rest expr)
        stack (assoc (meta expr)
                     :ns @utils/current-ns
                     :file @utils/current-file)]
    (sci.impl.types/->Node
     (try (apply eval/eval-import ctx args)
          (catch #?(:clj Throwable :cljs js/Error) e
            (rethrow-with-location-of-node ctx bindings e this)))
     stack)))

(macros/deftime
  (defmacro with-top-level-loc [top-level? m & body]
    `(let [m# ~m
           loc# (when (and ~top-level? m# (:line m#))
                  {:line (:line m#)
                   :column (:column m#)})]
       (when loc#
         (macros/? :clj
                   (push-thread-bindings {#'utils/*top-level-location* loc#})
                   :cljs (set! utils/*top-level-location* loc#)))
       (try ~@body
            (finally
              (when loc#
                (macros/? :clj
                          (pop-thread-bindings)
                          :cljs (set! utils/*top-level-location* nil))))))))

(defn dispatch-special [ctx expr f]
  (case f
    do (return-do ctx expr (rest expr))
    let* (analyze-let* ctx expr (second expr) (nnext expr))
    fn* (analyze-fn* ctx expr)
    def (analyze-def ctx expr)
    loop* (analyze-loop* ctx expr)
    if (return-if ctx expr)
    ;; case macro expands into case* with no changes via fast-path
    (case case*) (analyze-case* ctx expr)
    try (analyze-try ctx expr)
    throw (analyze-throw ctx expr)
    expand-dot* (expand-dot* ctx expr)
    . (expand-dot** ctx expr)
    expand-constructor (expand-constructor ctx expr)
    new (analyze-new ctx expr)
    var (analyze-var ctx expr)
    set! (analyze-set! ctx expr)
    quote (analyze-quote ctx expr)
    import (analyze-import ctx expr)
    recur (return-recur ctx expr (analyze-children (without-recur-target ctx) (rest expr)))
    ;; Available as macro, but here for optimized version
    or (return-or ctx expr (rest expr))
    and (return-and ctx expr (rest expr))
    ns (analyze-ns-form ctx expr)
    lazy-seq (analyze-lazy-seq ctx expr)))


#?(:clj
   (defn analyze-interop [_ctx expr [^Class clazz meth]]
     (let [meth (str meth)
           stack (assoc (meta expr)
                        :ns @utils/current-ns
                        :file @utils/current-file)]
       (if-let [_fld (try (Reflector/getStaticField ^Class clazz ^String meth)
                          (catch IllegalArgumentException _
                            nil))]
         (sci.impl.types/->Node
          (interop/get-static-field clazz meth)
          stack)
         (if (str/starts-with? meth ".")
           (let [meth (subs meth 1)
                 f (fn [obj & args]
                     (Reflector/invokeInstanceMethodOfClass
                      obj clazz meth
                      ^objects (into-array Object args)))]
             (sci.impl.types/->Node
              f
              stack))
           (sci.impl.types/->Node
            (fn [& args]
              (Reflector/invokeStaticMethod
               clazz meth
               ^objects (into-array Object args)))
            stack))))))

(defn analyze-call [ctx expr m top-level?]
  (with-top-level-loc top-level? m
    (try
      (let [f (first expr)]
        (cond (symbol? f)
              (let [fsym f
                    ;; in call position Clojure prioritizes special symbols over
                    ;; bindings
                    special-sym (get special-syms f)
                    _ (when (and special-sym
                                 (:check-permissions ctx))
                        (resolve/check-permission! ctx f [special-sym nil]))
                    f (or special-sym
                          (resolve/resolve-symbol ctx f true))
                    f-meta (meta f)
                    eval? (and f-meta (:sci.impl/op f-meta))
                    fast-path (-> f-meta :sci.impl/fast-path)
                    f (or fast-path f)]
                (cond (and f-meta (::static-access f-meta))
                      #?(:clj
                         (let [[clazz meth class-expr] f]
                           (analyze-dot ctx (with-meta (list* '. clazz meth (rest expr))
                                              (assoc m :class-expr class-expr))))
                         :cljs
                         (let [[class method-path] f
                               last-path (last method-path)
                               ctor? (= "" last-path)
                               method-len (count method-path)
                               subpath (.slice method-path 0 (dec method-len))
                               lookup-fn (if (= 1 method-len)
                                           (constantly #js [class last-path])
                                           ;; This might fail at analysis time
                                           (fn []
                                             #js [(interop/get-static-fields class subpath)
                                                  last-path]))
                               [class method-name] (try (lookup-fn)
                                                        (catch :default _ nil))
                               children (analyze-children ctx (rest expr))
                               children (into-array children)]
                           (if class
                             ;; if class isn't found at analysis time, we
                             ;; delay lookup to runtime the performance
                             ;; difference isn't that great, so if turns out
                             ;; to be a problem that we're eagerly looking up
                             ;; the invoked class here, we can switch to the
                             ;; else branch by default
                             (if ctor?
                               (let [ctor class]
                                 (sci.impl.types/->Node
                                  (interop/invoke-js-constructor* ctx bindings ctor children)
                                  nil))
                               (if (instance? t/NodeR class)
                                 (sci.impl.types/->Node
                                  (let [class (t/eval class ctx bindings)
                                        method (unchecked-get class method-name)]
                                    (interop/invoke-static-method ctx bindings class method children))
                                  nil)
                                 (let [method (unchecked-get class method-name)]
                                   (sci.impl.types/->Node
                                    (interop/invoke-static-method ctx bindings class method children)
                                    nil))))
                             (if ctor?
                               (sci.impl.types/->Node
                                (let [arr (lookup-fn)
                                      ctor (aget arr 0)]
                                  (interop/invoke-js-constructor* ctx bindings ctor children))
                                nil)
                               (sci.impl.types/->Node
                                (let [arr (lookup-fn)
                                      class (aget arr 0)
                                      method-name (aget arr 1)
                                      method (unchecked-get class method-name)]
                                  (interop/invoke-static-method ctx bindings class method children))
                                nil)))))
                      #?@(:clj [(and f-meta (:sci.impl.analyzer/interop f-meta))
                                (let [[obj & children] (analyze-children ctx (rest expr))
                                      meth (-> (second f)
                                               str
                                               (subs 1))
                                      clazz (first f)
                                      children (into-array Object children)
                                      child-count (count children)
                                      stack (assoc m
                                                   :ns @utils/current-ns
                                                   :file @utils/current-file
                                                   :sci.impl/f-meta f-meta)]
                                  (sci.impl.types/->Node
                                   (let [obj (sci.impl.types/eval obj ctx bindings)]
                                     (interop/invoke-instance-method ctx bindings obj clazz
                                                                     meth
                                                                     children child-count nil))
                                   stack))])
                      #?@(:clj [(and f-meta (:sci.impl.analyzer/invoke-constructor f-meta))
                                (invoke-constructor-node ctx (first f) (rest expr))])
                      (and (not eval?) ;; the symbol is not a binding
                           (symbol? f)
                           (or
                            special-sym
                            (contains? ana-macros f)))
                      (dispatch-special ctx expr f)
                      :else
                      (try
                        (if (macro? f)
                          (let [;; Fix for #603
                                #?@(:cljs [f (if (utils/var? f)
                                               @f
                                               f)
                                           f (or (.-afn ^js f) f)])
                                v (apply f expr (:bindings ctx) (rest expr))
                                v (if (seq? v)
                                    (with-meta v (merge m (meta v)))
                                    v)
                                expanded (cond (:sci.impl/macroexpanding ctx) v
                                               (and top-level? (seq? v) (= 'do (first v)))
                                               ;; hand back control to eval-form for
                                               ;; interleaved analysis and eval
                                               (t/->EvalForm v)
                                               :else (analyze ctx v top-level?))]
                            expanded)
                          (if-let [f (:sci.impl/inlined f-meta)]
                            (return-call ctx
                                         expr
                                         f (analyze-children ctx (rest expr))
                                         (assoc m
                                                :ns @utils/current-ns
                                                :file @utils/current-file
                                                :sci.impl/f-meta f-meta)
                                         nil)
                            (if-let [op (:sci.impl/op (meta f))]
                              (case op
                                :resolve-sym
                                (return-binding-call ctx
                                                     expr
                                                     (:sci.impl/idx (meta f))
                                                     f (analyze-children ctx (rest expr))
                                                     (assoc m
                                                            :ns @utils/current-ns
                                                            :file @utils/current-file
                                                            :sci.impl/f-meta f-meta))
                                (let [children (analyze-children ctx (rest expr))]
                                  (return-call ctx
                                               expr
                                               f children (assoc m
                                                                 :ns @utils/current-ns
                                                                 :file @utils/current-file
                                                                 :sci.impl/f-meta f-meta)
                                               nil)))
                              (let [self-ref? (:self-ref? ctx)]
                                (if (and self-ref? (self-ref? f))
                                  (let [children (analyze-children ctx (rest expr))]
                                    (return-call ctx
                                                 expr
                                                 f children (assoc m
                                                                   :ns @utils/current-ns
                                                                   :file @utils/current-file
                                                                   :sci.impl/f-meta f-meta)
                                                 (fn [_ bindings _]
                                                   (deref
                                                    (eval/resolve-symbol bindings fsym)))))
                                  (let [children (analyze-children ctx (rest expr))]
                                    (return-call ctx
                                                 expr
                                                 f children (assoc m
                                                                   :ns @utils/current-ns
                                                                   :file @utils/current-file
                                                                   :sci.impl/f-meta f-meta)
                                                 #?(:cljs (when (utils/var? f) (fn [_ _ v]
                                                                                 (deref v))) :clj nil))))))))
                        (catch #?(:clj Exception :cljs js/Error) e
                          ;; we pass a ctx-fn because the rethrow function calls
                          ;; stack on it, the only interesting bit it the map
                          ;; with :ns and :file
                          (rethrow-with-location-of-node ctx e
                                                         (let [stack (assoc m
                                                                            :ns @utils/current-ns
                                                                            :file @utils/current-file
                                                                            :sci.impl/f-meta f-meta)]
                                                           (sci.impl.types/->Node nil stack)))))))
              (keyword? f)
              (let [children (analyze-children ctx (rest expr))
                    ccount (count children)]
                (case ccount
                  1 (let [arg (nth children 0)]
                      (sci.impl.types/->Node
                       (f (t/eval arg ctx bindings))
                       nil))
                  2 (let [arg0 (nth children 0)
                          arg1 (nth children 1)]
                      (sci.impl.types/->Node
                       (f (t/eval arg0 ctx bindings)
                          (t/eval arg1 ctx bindings))
                       nil))
                  (throw-error-with-location (str "Wrong number of args (" ccount ") passed to: " f) expr)))
              :else
              (let [f (analyze ctx f)
                    children (analyze-children ctx (rest expr))
                    stack (assoc m
                                 :ns @utils/current-ns
                                 :file @utils/current-file)]
                (return-call ctx
                             expr
                             f children stack
                             #?(:cljs (if (utils/var? f)
                                        (fn [ctx bindings f]
                                          (t/eval @f ctx bindings))
                                        (fn [ctx bindings f]
                                          (t/eval f ctx bindings)))
                                :clj (fn [ctx bindings f]
                                       (t/eval f ctx bindings)))))))
      (catch #?(:clj Exception
                :cljs :default) e
        (utils/rethrow-with-location-of-node ctx e (sci.impl.types/->Node nil (utils/make-stack m)))))))

(defn map-fn [children-count]
  (if (<= children-count 16)
    #?(:clj #(let [^objects arr (into-array Object %&)]
               (clojure.lang.PersistentArrayMap/createWithCheck arr))
       :cljs #(.createWithCheck PersistentArrayMap (into-array %&))
       :default array-map)
    #?(:clj #(let [^clojure.lang.ISeq s %&]
               (clojure.lang.PersistentHashMap/createWithCheck s))
       :cljs #(.createWithCheck PersistentHashMap (into-array %&))
       :default hash-map)))

(defn return-map [ctx the-map analyzed-children]
  (let [mf (map-fn (count analyzed-children))]
    (return-call ctx the-map mf analyzed-children nil nil)
    (return-call ctx the-map mf analyzed-children nil nil)))

(defn constant-node? [x]
  #?(:clj (instance? sci.impl.types.ConstantNode x)
     :cljs (not (instance? sci.impl.types.NodeR x))))

#?(:clj (defn unwrap-children [children]
          (-> (reduce (fn [acc x]
                        (conj! acc (t/eval x nil nil)))
                      (transient [])
                      children)
              persistent!)))

(defn analyze-map
  [ctx expr m]
  (let [ctx (without-recur-target ctx)
        children (into [] cat expr)
        analyzed-children (analyze-children ctx children)
        const? (every? constant-node? analyzed-children)
        #?@(:clj [analyzed-children (if const?
                                      (unwrap-children analyzed-children)
                                      analyzed-children)])
        same? (when const? (= children analyzed-children))
        const-val (when const?
                    (if same?
                      expr
                      (let [mf (map-fn (count analyzed-children))]
                        (apply mf analyzed-children))))
        analyzed-map (if const?
                       (->constant const-val)
                       (return-map ctx expr analyzed-children))
        analyzed-meta (when m (analyze ctx m))
        ret (if analyzed-meta
              (sci.impl.types/->Node
               (let [coll (t/eval analyzed-map ctx bindings)
                     md (t/eval analyzed-meta ctx bindings)]
                 (with-meta coll md))
               nil)
              analyzed-map)]
    ret))

(defn analyze-vec-or-set
  "Returns analyzed vector or set"
  [ctx f1 f2 expr m]
  (let [ctx (without-recur-target ctx)
        analyzed-meta (when m (analyze ctx m))
        analyzed-children (analyze-children ctx expr)
        const? (every? constant-node? analyzed-children)
        #?@(:clj [analyzed-children (if const?
                                      (unwrap-children analyzed-children)
                                      analyzed-children)])
        set-expr? (set? expr)
        same? (and const? (= (if set-expr?
                               (or (seq expr) [])
                               expr) analyzed-children))
        const-val (when const?
                    (if same?
                      (if (empty? expr)
                        (if set-expr? #{} [])
                        expr)
                      (f1 analyzed-children)))
        analyzed-coll (if const?
                        (->constant const-val)
                        (return-call ctx expr f2 analyzed-children nil nil))
        ret (if analyzed-meta
              (sci.impl.types/->Node
               (let [coll (t/eval analyzed-coll ctx bindings)
                     md (t/eval analyzed-meta ctx bindings)]
                 (with-meta coll md))
               nil)
              analyzed-coll)]
    ret))

#?(:cljs
   (defn analyze-js-obj [ctx js-val]
     (let [v (.-val ^js js-val)]
       (if (map? v)
         (let [ks (keys v)
               ks (map name ks)
               vs (vals v)
               vs (analyze-children ctx vs)]
           (sci.impl.types/->Node
            (apply js-obj (interleave ks (map #(t/eval % ctx bindings) vs)))
            nil))
         (let [vs (analyze-children ctx v)]
           (sci.impl.types/->Node
            (let [arr (array)]
              (run! #(.push arr (t/eval % ctx bindings)) vs)
              arr)
            nil))))))

;; This could be a protocol, but there's not a clear win in doing so:
;; https://github.com/babashka/sci/issues/848
(defn analyze
  ([ctx expr]
   (analyze ctx expr false))
  ([ctx expr top-level?]
   (let [m (meta expr)]
     (cond
       (constant? expr) (->constant expr)
       (symbol? expr) (let [v (resolve/resolve-symbol ctx expr false m)
                            mv (meta v)]
                        (cond (constant? v) (->constant v)
                              (utils/var? v)
                              (if (:const mv)
                                @v
                                (if (vars/isMacro v)
                                  (throw (new #?(:clj IllegalStateException :cljs js/Error)
                                              (str "Can't take value of a macro: " v "")))
                                  (sci.impl.types/->Node
                                   (faster/deref-1 v)
                                   nil)))
                              #?@(:clj
                                  [(:sci.impl.analyzer/interop mv)
                                   (analyze-interop ctx expr v)])
                              :else v))
       ;; don't evaluate records, this check needs to go before map?
       ;; since a record is also a map
       (record? expr) expr
       (map? expr) (analyze-map ctx expr m)
       #?@(:cljs [(instance? JSValue expr) (analyze-js-obj ctx expr)])
       (vector? expr) (analyze-vec-or-set ctx
                                          ;; relying on analyze-children to
                                          ;; return a vector
                                          identity
                                          vector expr m)
       (set? expr) (analyze-vec-or-set ctx set
                                       #?(:clj #(clojure.lang.PersistentHashSet/createWithCheck ^clojure.lang.ISeq %&)
                                          :cljs #(.createWithCheck PersistentHashSet (into-array %&))
                                          :default vector)
                                       expr m)
       (seq? expr) (if (seq expr)
                     (analyze-call ctx expr m top-level?)
                     ;; the empty list
                     expr)
       :else expr))))

(vreset! utils/analyze analyze)

;;;; Scratch

(comment
  ;; _ctx expr f analyzed-children
  )
