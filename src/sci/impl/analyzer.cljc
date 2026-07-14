(ns sci.impl.analyzer
  {:no-doc true
   :clj-kondo/config '{:linters {:unresolved-symbol {:exclude [ctx this bindings]}}}}
  (:require
   #?@(:cljd [] :clj [[sci.impl.reflector :as reflector]])
   #?(:cljd [sci.impl.types :as t :refer [->constant]]
      :clj [sci.impl.types :as t :refer [#?(:cljs ->Node) ->constant]])
   #?(:cljs [cljs.tagged-literals :refer [JSValue]])
   #?(:cljs [goog.object :as gobj])
   #?(:cljs [sci.impl.types :as t :refer [->constant]])
   #?(:cljs [sci.impl.jit :as jit])
   [clojure.string :as str]
   [sci.ctx-store :as store]
   #?(:cljs [sci.impl.async-macro :as async-macro])
   [sci.impl.deftype]
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
   [sci.lang :as lang])
  #?(:cljs
     (:require-macros
      [sci.impl.analyzer :refer [gen-return-recur
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

#?(:cljd nil :clj (set! *warn-on-reflection* true))

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
            node2 (return-do ctx expr (drop 5 children))
            node (sci.impl.types/->Node (do (t/eval node1 ctx bindings)
                                            (t/eval node2 ctx bindings))
                                        nil)]
        (t/attach-ast node [:do [node1 node2]]))
      (let [analyzed-children (analyze-children-tail ctx children)
            node (case child-count
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
                   (t/eval node4 ctx bindings)) nil)))]
        ;; child-count <= 1 returns the child itself; attaching would
        ;; overwrite the child's own ast and make the jit walker loop
        (if (> child-count 1)
          (t/attach-ast node [:do analyzed-children])
          node)))))

(defn return-or
  [ctx expr children]
  (let [child-count# (count children)]
    (if (> child-count# 5)
      (let [a0# (return-or ctx expr (take 5 children))
            a1# (return-or ctx expr (drop 5 children))
            node (sci.impl.types/->Node
                  (or (t/eval a0# ctx bindings)
                      (t/eval a1# ctx bindings))
                  nil)]
        (t/attach-ast node [:or [a0# a1#]]))
      (let [children (analyze-children-tail ctx children)
            node (case child-count#
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
               nil)))]
        ;; child-count <= 1 returns nil or the child itself (see return-do)
        (if (> child-count# 1)
          (t/attach-ast node [:or children])
          node)))))

(defn return-and
  [ctx expr children]
  (let [child-count# (count children)]
    (if (> child-count# 5)
      (let [a0# (return-and ctx expr (take 5 children))
            a1# (return-and ctx expr (drop 5 children))
            node (sci.impl.types/->Node
                  (and (t/eval a0# ctx bindings)
                       (t/eval a1# ctx bindings))
                  nil)]
        (t/attach-ast node [:and [a0# a1#]]))
      (let [children (analyze-children-tail ctx children)
            node (case child-count#
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
               nil)))]
        ;; child-count <= 1 returns true or the child itself (see return-do)
        (if (> child-count# 1)
          (t/attach-ast node [:and children])
          node)))))

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
          recur-sym (gensym "recur")
          expected-count-sym (gensym "expected-count")
          arg-count-sym (gensym "arg-count")]
      `(defn ~'return-recur
         ~'[ctx expr analyzed-children]
         (when-not (recur-target? ~'ctx)
           (throw-error-with-location
            (case (:no-recur-reason ~'ctx)
              :try "Cannot recur across try"
              "Can only recur from tail position") ~'expr))
         (let [~'params (:params ~'ctx)
               ~expected-count-sym (count ~'params)
               ~arg-count-sym (count ~'analyzed-children)]
           (when-not (= ~expected-count-sym ~arg-count-sym)
             (throw-error-with-location
              (str "Mismatched argument count to recur, expected: "
                   ~expected-count-sym " args, got: " ~arg-count-sym)
              ~'expr))
           (case ~arg-count-sym
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
                                                     #?(:cljd {:tag 'List} :default {:tag 'objects})) ~j
                                                  ~(symbol (str "eval-" j))))
                                              (range i)))
                                   ~recur-sym)
                                 nil))])
                        let-bindings))
             ;; default case for 20+ args
             (let [recur# recur]
               (sci.impl.types/->Node
                (let [evaled# (mapv (fn [a#] (t/eval a# ~'ctx ~'bindings)) ~'analyzed-children)]
                  (dotimes [i# ~arg-count-sym]
                    (aset ~(with-meta 'bindings #?(:cljd {:tag 'List} :default {:tag 'objects})) (int i#) (nth evaled# i#)))
                  recur#)
                nil))))))))

;; (require 'clojure.pprint)
;; (clojure.pprint/pprint
;;  (clojure.core/macroexpand '(gen-return-recur)))

(declare return-recur) ;; for clj-kondo
(gen-return-recur)

(defn analyze-children [ctx children]
  (mapv #(analyze ctx %) children))

(defrecord FnBody [params body fixed-arity var-arg-name self-ref-idx iden->invoke-idx])

(declare update-parents)

(defn expand-fn-args+body [{:keys [fn-expr] :as ctx} [binding-vector & body-exprs] _macro? fn-name fn-id #?(:cljs async? :default _async?)]
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
        ;; Transform async bodies before analysis
        body-exprs #?(:cljd body-exprs
                      :clj body-exprs
                      :cljs (if async?
                             (let [locals (set (keys (:bindings ctx)))]
                               (async-macro/transform-async-fn-body ctx locals body-exprs))
                             body-exprs))
        #?@(:cljs [this-as-vol (volatile! false)
                   ctx (assoc ctx :this-as this-as-vol)])
        body (return-do (with-recur-target ctx true) fn-expr body-exprs)
        iden->invoke-idx (get-in @(:closure-bindings ctx) (conj (:parents ctx) :syms))]
    (cond-> (->FnBody binding-vector body fixed-arity var-arg-name self-ref-idx iden->invoke-idx)
      var-arg-name
      (assoc :vararg-idx (get iden->invoke-idx (last param-idens)))
      #?@(:cljs [@this-as-vol (assoc :this-as-idx @this-as-vol)]))))

(defn analyzed-fn-meta [ctx m]
  (let [;; seq expr has location info with 2 keys
        meta-needs-eval? (> (count m) 2)
        m (if meta-needs-eval? (-> (analyze (assoc ctx :meta true) m)
                                   (vary-meta assoc :sci.impl/op :eval))
              m)]
    m))

(defn single-arity-fn [bindings-fn fn-body fn-name self-ref-in-enclosed-idx self-ref? nsm fn-meta macro?
                       #?@(:cljs [capture-pairs enclosed-array-cnt])]
  (let [fixed-arity (:fixed-arity fn-body)
        copy-enclosed->invocation (:copy-enclosed->invocation fn-body)
        invoc-size (:invoc-size fn-body)
        body (:body fn-body)
        vararg-idx (:vararg-idx fn-body)
        #?@(:cljs [this-as-idx (:this-as-idx fn-body)])
        node
        (sci.impl.types/->Node
         (let [enclosed-array (bindings-fn bindings)
               f #?(:cljs (jit/make-fn fn-body ctx enclosed-array
                                       (fn [] (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                                                       body invoc-size nsm vararg-idx this-as-idx)))
                    :default (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                                      body invoc-size nsm vararg-idx))
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
             (aset #?(:cljd ^List enclosed-array :default ^objects enclosed-array)
                   self-ref-in-enclosed-idx
                   f))
           f)
         nil)]
    #?(:cljs
       ;; fn-creation ast: the emitter builds the enclosed array from its
       ;; own slots via the static capture pairs and calls mk, so creating
       ;; a closure no longer escapes (nor forces array mode). fn-meta
       ;; needs the creating scope's bindings, macros never occur in
       ;; jitted positions: both keep the escape.
       (if (or fn-meta macro?)
         node
         (t/attach-ast
          node
          [:mkfn (fn [ctx enclosed-array]
                   (let [f (jit/make-fn fn-body ctx enclosed-array
                                        (fn [] (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                                                        body invoc-size nsm vararg-idx this-as-idx)))]
                     (when self-ref?
                       (aset ^objects enclosed-array self-ref-in-enclosed-idx f))
                     f))
           capture-pairs enclosed-array-cnt]))
       :default node)))

(defn multi-arity-fn-body [fn-body fn-name nsm]
  (let [fixed-arity (:fixed-arity fn-body)
        copy-enclosed->invocation (:copy-enclosed->invocation fn-body)
        invoc-size (:invoc-size fn-body)
        body (:body fn-body)
        vararg-idx (:vararg-idx fn-body)]
    (fn [enclosed-array]
      (sci.impl.types/->Node
       (let [f #?(:cljs (jit/make-fn fn-body ctx enclosed-array
                                     (fn [] (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                                                     body invoc-size nsm vararg-idx (:this-as-idx fn-body))))
                  :default (fns/fun ctx enclosed-array body fn-name macro? fixed-arity copy-enclosed->invocation
                                    body invoc-size nsm vararg-idx))]
         f)
       nil))))

(defn analyze-fn* [ctx [fn-sym name? & body :as fn-expr]]
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
        ;; Check both expr metadata and fn symbol metadata for :async
        async? (or (:async fn-expr-m)
                   (:async (meta fn-sym)))
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
                                 body (expand-fn-args+body ctx body macro? fn-name fn-id async?)
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
        [bindings-fn enclosed-array-cnt #?(:cljs capture-pairs :default _capture-pairs)]
        (if (or self-ref? (seq closed-over-iden->binding-idx))
          (let [enclosed-array-cnt (cond-> closed-over-cnt
                                     fn-name (inc))
                #?@(:cljd [binding->enclosed] :default [^objects binding->enclosed])
                (into-array (keep (fn [iden]
                                    ;; for fn-id usage there is no outer binding idx
                                    (when-let [binding-idx (get iden->invoke-idx iden)]
                                      (let [enclosed-idx (get iden->enclosed-idx iden)]
                                        ;; (prn :copying binding-idx '-> enclosed-idx)
                                        (doto #?(:cljd (#/(List/filled dynamic) 2 nil)
                                                 :default (object-array 2))
                                          (aset 0 binding-idx)
                                          (aset 1 enclosed-idx)))))
                                  closed-over-idens))]
            [(fn [#?(:cljd bindings :clj ^objects bindings :cljs ^objects bindings)]
               (areduce binding->enclosed idx ret #?(:cljd (#/(List/filled dynamic) enclosed-array-cnt nil)
                                                     :default (object-array enclosed-array-cnt))
                        (let [#?@(:cljd [idxs] :default [^objects idxs]) (aget binding->enclosed idx)
                              binding-idx (aget idxs 0)
                              binding-val (aget bindings binding-idx)
                              enclosed-idx (aget idxs 1)]
                          (aset #?(:cljd ^List ret :default ret) enclosed-idx binding-val)
                          ret)))
             enclosed-array-cnt
             #?(:cljs (vec (keep (fn [iden]
                                   (when-let [binding-idx (get iden->invoke-idx iden)]
                                     [binding-idx (get iden->enclosed-idx iden)]))
                                 closed-over-idens)))])
          [(constantly nil)])
        bodies (:bodies analyzed-bodies)
        bodies (mapv (fn [body]
                       (let [iden->invocation-idx (:iden->invoke-idx body)
                             invocation-self-idx (:self-ref-idx body)
                             enclosed->invocation
                             (into-array (keep (fn [iden]
                                                 (when-let [invocation-idx (iden->invocation-idx iden)]
                                                   (doto #?(:cljd (#/(List/filled dynamic) 2 nil)
                                                            :default (object-array 2))
                                                     (aset 0 (iden->enclosed-idx iden))
                                                     (aset 1 invocation-idx))))
                                               closed-over-idens))
                             invoc-size (count iden->invocation-idx)
                             copy-enclosed->invocation
                             (when (pos? (alength #?(:cljd ^List enclosed->invocation :default ^objects enclosed->invocation)))
                               (fn [#?(:cljd enclosed-array :clj ^objects enclosed-array :cljs ^objects enclosed-array)
                                    #?(:cljd invoc-array :clj ^objects invoc-array :cljs ^objects invoc-array)]
                                 (areduce #?(:cljd ^List enclosed->invocation :default ^objects enclosed->invocation) idx ret invoc-array
                                          (let [#?@(:cljd [idxs] :default [^objects idxs])
                                                (aget #?(:cljd ^List enclosed->invocation :default ^objects enclosed->invocation) idx)
                                                enclosed-idx (aget idxs 0)
                                                enclosed-val (aget #?(:cljd ^List enclosed-array :default ^objects enclosed-array) enclosed-idx)
                                                invoc-idx (aget idxs 1)]
                                            (aset #?(:cljd ^List ret :default ^objects ret) invoc-idx enclosed-val)
                                            ret))))
                             body (assoc body
                                         :invoc-size invoc-size
                                         :invocation-self-idx invocation-self-idx
                                         :copy-enclosed->invocation copy-enclosed->invocation
                                         #?@(:cljs [:enclosed->invocation-idxs enclosed->invocation]))]
                         ;; lazy: pay the new Function only when a closure
                         ;; over this body is first created, so fn bodies
                         ;; (incl. loops) inside never-called code don't
                         ;; compile at load time
                         #?(:cljs (if @t/jit-enabled
                                    (assoc body :jit-template
                                           (delay (jit/compile-template body)))
                                    body)
                            :default body)))
                     bodies)
        ;; arglists (:arglists analyzed-bodies)
        fn-meta (dissoc fn-expr-m :line :column)
        fn-meta (when (seq fn-meta) (analyze ctx fn-meta))
        single-arity (when (= 1 (count bodies))
                       (first bodies))
        nsm (utils/current-ns-name)
        self-ref-in-enclosed-idx (some-> enclosed-array-cnt dec)
        ret-node (if single-arity
                   (single-arity-fn bindings-fn single-arity fn-name self-ref-in-enclosed-idx self-ref? nsm fn-meta macro?
                                    #?@(:cljs [capture-pairs enclosed-array-cnt]))
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
                                    (throw (new #?(:cljd Exception
                                                   :clj Exception
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
                          (aset #?(:cljd ^List enclosed-array :default ^objects enclosed-array)
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
             (let [#?@(:cljs [this-as-binding?
                              (identical? fns/this-as-sentinel binding-value)])
                   m (meta binding-value)
                   t (when m (:tag m))
                   binding-name (if t (vary-meta binding-name
                                                 assoc :tag t)
                                    binding-name)
                   new-iden (gensym)
                   cb (:closure-bindings ctx)
                   idx (update-parents ctx cb new-iden)
                   _ #?(:cljd nil
                        :cljs (when this-as-binding?
                                (when-let [ta (:this-as ctx)]
                                  (vreset! ta idx)))
                        :clj nil)
                   v #?(:cljd (analyze ctx binding-value)
                        :clj (analyze ctx binding-value)
                        :cljs (if this-as-binding?
                                (sci.impl.types/->Node
                                 (aget #?(:cljd ^List bindings :default ^objects bindings) idx)
                                 nil)
                                (analyze ctx binding-value)))
                   ;; Propagate inferred tag from analyzed value to binding name
                   #?@(:clj [name-tag (-> binding-name meta :tag)
                             binding-tag (or name-tag (-> v meta :tag))
                             binding-name (if (and binding-tag (not name-tag))
                                            (vary-meta binding-name assoc :tag binding-tag)
                                            binding-name)])
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
          idxs (mapv iden->invoke-idx idens)
          ;; (prn :params params :idens idens :idxs idxs)
          node (case (count idxs)
        0 (sci.impl.types/->Node
           (t/eval body ctx bindings)
           stack)
        1 (let [node0 (nth let-nodes 0)
                idx0 (nth idxs 0)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset #?(:cljd ^List bindings :default ^objects bindings) idx0 val0)
               (t/eval body ctx bindings))
             stack))
        2 (let [node0 (nth let-nodes 0)
                node1 (nth let-nodes 1)
                idx0 (nth idxs 0)
                idx1 (nth idxs 1)]
            (sci.impl.types/->Node
             (let [val0 (t/eval node0 ctx bindings)]
               (aset #?(:cljd ^List bindings :default ^objects bindings) idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset #?(:cljd ^List bindings :default ^objects bindings) idx1 val1)
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
               (aset #?(:cljd ^List bindings :default ^objects bindings) idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset #?(:cljd ^List bindings :default ^objects bindings) idx1 val1)
                 (let [val2 (t/eval node2 ctx bindings)]
                   (aset #?(:cljd ^List bindings :default ^objects bindings) idx2 val2)
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
               (aset #?(:cljd ^List bindings :default ^objects bindings) idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset #?(:cljd ^List bindings :default ^objects bindings) idx1 val1)
                 (let [val2 (t/eval node2 ctx bindings)]
                   (aset #?(:cljd ^List bindings :default ^objects bindings) idx2 val2)
                   (let [val3 (t/eval node3 ctx bindings)]
                     (aset #?(:cljd ^List bindings :default ^objects bindings) idx3 val3)
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
               (aset #?(:cljd ^List bindings :default ^objects bindings) idx0 val0)
               (let [val1 (t/eval node1 ctx bindings)]
                 (aset #?(:cljd ^List bindings :default ^objects bindings) idx1 val1)
                 (let [val2 (t/eval node2 ctx bindings)]
                   (aset #?(:cljd ^List bindings :default ^objects bindings) idx2 val2)
                   (let [val3 (t/eval node3 ctx bindings)]
                     (aset #?(:cljd ^List bindings :default ^objects bindings) idx3 val3)
                     (let [val4 (t/eval node4 ctx bindings)]
                       (aset #?(:cljd ^List bindings :default ^objects bindings) idx4 val4)
                       (t/eval body ctx bindings))))))
             stack)))]
        (t/attach-ast node [:let idxs let-nodes body]))))

(defn init-var! [ctx name expr]
  (let [cnn (utils/current-ns-name)
        env (:env ctx)
        the-current-ns (get-in @env [:namespaces cnn]
                               ;; namespace could be absent in config
                               {})
        refers (:refers the-current-ns)
        the-current-ns (if-let [x (and refers #?(:cljd (get refers name)
                                                 :clj (.get ^java.util.Map refers name)
                                                 :cljs (.get refers name)))]
                         (throw-error-with-location
                          (str name " already refers to "
                               x " in namespace "
                               cnn)
                          expr)
                         (if #?(:cljd (get the-current-ns name)
                                :clj (.get ^java.util.Map the-current-ns name)
                                :cljs (.get the-current-ns name))
                           the-current-ns
                           (assoc the-current-ns name
                                  (doto (lang/->Var nil name
                                                    {:name name
                                                     :ns @utils/current-ns
                                                     :file @utils/current-file}
                                                    false
                                                    false
                                                    nil
                                                    @utils/current-ns)
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
          (throw (new #?(:cljd ArgumentError
                         :clj IllegalArgumentException
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
                  (->constant m))
              file @utils/current-file]
          (sci.impl.types/->Node
           (eval/eval-def ctx bindings var-name init m file)
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
                     ~(with-meta
                        (list* `(fn* ~(vec syms) ~@body)
                               syms)
                        ;; the synthesized call carries the loop's location,
                        ;; so errors thrown inside locate at the loop form
                        (meta expr)))]
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
        stack (utils/stack-frame (meta expr) nil true)]
    (case (count children)
      (0 1) (throw-error-with-location "Too few arguments to if" expr)
      2 (let [condition (nth children 0)
              then (nth children 1)]
          (cond (not condition) nil
                (constant? condition) then
                :else (sci.impl.types/->Node
                       (when (t/eval condition ctx bindings)
                         (t/eval then ctx bindings))
                       stack
                       [:if condition then nil])))
      3 (let [condition (nth children 0)
              then (nth children 1)
              else (nth children 2)]
          (cond (not condition) else
                (constant? condition) then
                :else (sci.impl.types/->Node
                       (if (t/eval condition ctx bindings)
                         (t/eval then ctx bindings)
                         (t/eval else ctx bindings))
                       stack
                       [:if condition then else])))
      (throw-error-with-location "Too many arguments to if" expr))))

(defn analyze-case*
  ;; JVM case* format: (case* ge shift mask default imap switch-type check-type skip-check)
  ;; imap: {key [test-constant result-expr], ...}
  [ctx expr]
  (let [[_ ge _shift _mask default imap] expr
        ctx-wo-rt (without-recur-target ctx)
        case-val (analyze ctx-wo-rt ge)
        case-default (analyze ctx default)
        case-map (reduce-kv
                   (fn [m _k [test result]]
                     (assoc m test (analyze ctx result)))
                   {} imap)
        #?@(:cljs [tests (vec (keys case-map))
                   ;; the jit dispatches through the same map lookup as
                   ;; eval-case (structural equality), then switches on
                   ;; the branch index; -1 = default
                   idx-map (zipmap tests (range))
                   branches (mapv case-map tests)])]
    (sci.impl.types/->Node
     (eval/eval-case ctx bindings case-map case-val case-default)
     nil
     #?(:cljs [:case case-val idx-map branches case-default]
        :default nil))))

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
                          (if-let [clazz #?(:cljd (case ex
                                                    :default :default
                                                    (or (interop/resolve-class-opts ctx ex)
                                                        (analyze ctx ex)))
                                            :clj (interop/resolve-class ctx ex)
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
                                                         (cons 'do body))
                                  sci-error (some-> ex meta :sci/error)]
                              {:class clazz
                               :ex-idx ex-idx
                               :body analyzed-body
                               :ex ex
                               :sci-error sci-error})
                            (throw-error-with-location (str "Unable to resolve classname: " ex) ex))))
                      catches)
        sci-error (some :sci-error catches)
        finally (when finally
                  (analyze ctx (cons 'do (rest finally))))
        node (sci.impl.types/->Node
              (eval/eval-try ctx bindings body catches finally sci-error)
              stack)]
    ;; hybrid try: body and finally compile, catch dispatch stays
    ;; eval-catches (exception path only). The interrupt-fn masking
    ;; logic (#1044) stays interpreted.
    #?(:cljs (if (nil? (:interrupt-fn ctx))
               (t/attach-ast node [:try body catches finally sci-error])
               node)
       :default node)))

(defn analyze-throw [ctx [_throw ex :as expr]]
  (when-not (= 2 (count expr))
    (throw-error-with-location
     #?(:cljd "Too many arguments to throw"
        :clj "Too many arguments to throw, throw expects a single Throwable instance"
        :cljs "Too many arguments to throw")
     expr))
  (let [ctx (without-recur-target ctx)
        ana (analyze ctx ex)
        stack (utils/stack-frame (meta expr) nil true)]
    (sci.impl.types/->Node
     (rethrow-with-location-of-node ctx bindings (t/eval ana ctx bindings) this)
     stack)))

;;;; Interop

#?(:cljd nil
   :clj
   (defn- resolve-tag-class [ctx instance-expr]
     (utils/vary-meta*
      instance-expr
      (fn [m]
        (if-let [t (:tag m)]
          (let [clazz (or (interop/resolve-class ctx t)
                          (records/resolve-record-class ctx t)
                          (throw-error-with-location
                           (str "Unable to resolve classname: " t) t))]
            (assoc m :tag-class clazz))
          m)))))

#?(:cljd nil
   :clj
   (defn- maybe-wrap-fi-adapter
     "Wrap instance-expr with FI adaptation if tag-class is a functional interface.
      Analysis-time check, eval-time adaptation (mirrors Compiler.java line 7072)."
     [instance-expr]
     (let [tag-class (-> instance-expr meta :tag-class)]
       (if (and (instance? Class tag-class) (reflector/maybe-fi-method tag-class))
         (let [orig instance-expr]
           (with-meta
             (sci.impl.types/->Node
              (let [val (t/eval orig ctx bindings)]
                (if (and (instance? clojure.lang.IFn val)
                         (not (instance? tag-class val)))
                  (reflector/box-arg tag-class val)
                  val))
              nil)
             (meta instance-expr)))
         instance-expr))))

#?(:cljd nil
   :clj
   (defn- analyze-instance-method [ctx instance-expr method-expr args expr]
     (let [method-name (name method-expr)
           field-access (str/starts-with? method-name "-")
           meth-name (if field-access (subs method-name 1) method-name)
           meth-name* meth-name
           meth-sym (symbol meth-name*)
           meth-name (#?(:clj munge :cljs utils/munge-str) meth-name)
           stack (utils/stack-frame (meta expr) nil)
           arg-count (count args)
           args (object-array args)
           ^"[Ljava.lang.Class;" arg-types (when (pos? arg-count)
                                             (make-array Class arg-count))
           has-types? (volatile! nil)]
       (when arg-types
         (areduce args idx _ret nil
                  (let [arg (aget args idx)
                        arg-meta (meta arg)]
                    (when-let [t (:tag arg-meta)]
                      (when-let [t (interop/resolve-type-hint ctx t)]
                        (do (vreset! has-types? true)
                            (aset arg-types idx t)))))))
       (with-meta (let [cache (volatile! nil)]
                    (sci.impl.types/->Node
                     (eval/eval-instance-method-invocation
                      ctx bindings instance-expr meth-name meth-name* meth-sym field-access args arg-count
                      (when @has-types? arg-types) cache)
                     stack))
         {::instance-expr instance-expr
          ::method-name method-name
          :tag (:tag (meta expr))}))))

(defn analyze-dot [ctx [_dot instance-expr method-expr & args :as expr]]
  (let [ctx (without-recur-target ctx)
        [method-expr & args] (if (seq? method-expr) method-expr
                                 (cons method-expr args))
        ;; (. DateTime parse ...) on a configured class is static, like DateTime/parse
        #?@(:cljd [static-class-sym
                   (when (and (symbol? instance-expr)
                              (not (:class-expr (meta expr)))
                              (contains? (some-> ctx :env deref :class->opts)
                                         instance-expr))
                     instance-expr)])
        instance-expr (analyze ctx instance-expr)
        #?@(:clj [instance-expr (resolve-tag-class ctx instance-expr)])
        #?@(:clj [instance-expr (maybe-wrap-fi-adapter instance-expr)])
        method-name (name method-expr)
        args (when args (analyze-children ctx args))
        res
        (let [field-access (str/starts-with? method-name "-")
              meth-name (if field-access
                          (subs method-name 1)
                          method-name)
              #?@(:cljs [meth-name* meth-name])
              #?@(:cljs [meth-sym (symbol meth-name*)])
              #?@(:cljs [meth-name (utils/munge-str meth-name)])
              stack (utils/stack-frame (meta expr) nil)]
          #?(:cljd
             (if-let [class-expr (or (:class-expr (meta expr)) static-class-sym)]
               ;; static Class/method (or Class/-FIELD): Dart has no reflection,
               ;; so only override fns dispatch; unlisted/reflect throw
               (let [class->opts (some-> ctx :env deref :class->opts)
                     class-opts (get class->opts class-expr)
                     override (if field-access
                                (get (:static-fields class-opts) (symbol meth-name))
                                (some-> class->opts :static-methods
                                        (get class-expr) (get method-expr)))
                     section (if field-access :static-fields :static-methods)]
                 (case (interop/member-disposition override class-opts section)
                   :override (if field-access
                               (sci.impl.types/->Node (override instance-expr) stack)
                               (sci.impl.types/->Node
                                (apply override instance-expr
                                       (map #(sci.impl.types/eval % ctx bindings) args))
                                stack))
                   (utils/throw-error-with-location
                    (str (if field-access "Field " "Method ") meth-name " on " class-expr
                         " not allowed (no static reflection on cljd)") expr)))
               ;; instance
               (with-meta
                 (sci.impl.types/->Node
                  (eval/eval-instance-method-invocation
                   ctx bindings instance-expr meth-name meth-name (symbol meth-name) field-access (vec args) (count args) nil nil)
                  stack)
                 {::instance-expr instance-expr
                  ::method-name method-name}))
             :clj (if (class? instance-expr)
                    (let [static-method
                          #(let [arg-count (count args)
                                 args (object-array args)
                                 class-expr (:class-expr (meta expr))]
                             ;; prefab static-methods
                             (let [class->opts (some-> ctx :env deref :class->opts)
                                   fq-class (interop/fully-qualify-class ctx class-expr)
                                   override (some-> class->opts :static-methods
                                                    (get fq-class)
                                                    (get method-expr))]
                               (case (interop/member-disposition override (get class->opts fq-class) :static-methods)
                                 :override (return-call ctx expr override (cons instance-expr args) stack nil)
                                 :deny (utils/throw-error-with-location
                                        (str "Method " meth-name " on " fq-class " not allowed!") expr)
                                 :reflect (sci.impl.types/->Node
                                           (interop/invoke-static-method ctx bindings instance-expr meth-name
                                                                         args arg-count)
                                           stack))))]
                      ;; class known at analysis, resolve :static-fields config here
                      (let [sf-opts (get (some-> ctx :env deref :class->opts)
                                         (symbol (.getName ^Class instance-expr)))
                            static-field
                            (fn [field-name-str]
                              (let [override (get (:static-fields sf-opts) (symbol field-name-str))]
                                (case (interop/member-disposition override sf-opts :static-fields)
                                  :override (sci.impl.types/->Node (override instance-expr) stack)
                                  :deny (utils/throw-error-with-location
                                         (str "Field " field-name-str " on " instance-expr " not allowed!") expr)
                                  :reflect (sci.impl.types/->Node
                                            (interop/get-static-field instance-expr field-name-str)
                                            stack))))]
                        (if (nil? args)
                          (if field-access
                            (static-field meth-name)
                            ;; https://clojure.org/reference/java_interop
                            ;; If the second operand is a symbol and no args are
                            ;; supplied it is taken to be a field access - the
                            ;; name of the field is the name of the symbol, and
                            ;; the value of the expression is the value of the
                            ;; field, unless there is a no argument public method
                            ;; of the same name, in which case it resolves to a
                            ;; call to the method.
                            (if-let [_
                                     (try (reflector/get-static-field ^Class instance-expr ^String method-name)
                                          (catch IllegalArgumentException _ nil))]
                              (static-field method-name)
                              (static-method)))
                          (static-method))))
                    (analyze-instance-method ctx instance-expr method-expr args expr))
             :cljs (let [;; only unconditional allows skip config resolution;
                         ;; :allow :all routes to the config-aware node so :closed wins
                         allowed? (or (:unrestricted ctx)
                                      (identical? method-expr utils/allowed-append))
                         args (into-array args)]
                     (with-meta
                       (case [(boolean allowed?) field-access]
                         [true true]
                         (sci.impl.types/->Node
                          (eval/allowed-instance-field-invocation ctx bindings instance-expr meth-name)
                          stack
                          [:iget instance-expr meth-name stack])
                         [true false]
                         (sci.impl.types/->Node
                          (eval/allowed-instance-method-invocation ctx bindings instance-expr meth-name args nil)
                          stack
                          [:imeth instance-expr meth-name (vec args) stack])
                         ;; default case
                         (let [cache (volatile! nil)]
                           (sci.impl.types/->Node
                            (eval/eval-instance-method-invocation
                             ctx bindings instance-expr meth-name meth-name* meth-sym field-access args allowed? nil nil cache)
                            stack)))
                       {::instance-expr instance-expr
                        ::method-name method-name}))))]
    res))

(defn expand-dot**
  "Expands (. x method)"
  [ctx expr]
  (when (< (count expr) 3)
    (throw (new #?(:cljd ArgumentError :clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  (analyze-dot ctx expr))

(defn expand-dot*
  "Expands (.foo x)"
  [ctx [method-name obj & args :as expr]]
  (when (< (count expr) 2)
    (throw (new #?(:cljd ArgumentError :clj IllegalArgumentException :cljs js/Error)
                "Malformed member expression, expecting (.member target ...)")))
  #?(:cljd (analyze-dot ctx (with-meta (list '. obj (cons (symbol (subs (name method-name) 1)) args)) (meta expr)))
     :clj (let [ctx (without-recur-target ctx)
                method-sym (symbol (subs (name method-name) 1))
                instance-expr (maybe-wrap-fi-adapter (resolve-tag-class ctx (analyze ctx obj)))
                args (when args (analyze-children ctx args))]
            (analyze-instance-method ctx instance-expr method-sym args expr))
     :cljs (analyze-dot ctx (with-meta (list '. obj (cons (symbol (subs (name method-name) 1)) args)) (meta expr)))))

#?(:cljd nil
   :clj
   (defn- invoke-constructor-node [ctx class args]
     (let [ctx (without-recur-target ctx)
           args (analyze-children ctx args)]
       (sci.impl.types/->Node
        (interop/invoke-constructor class (mapv #(t/eval % ctx bindings) args))
        nil))))

(defn analyze-new [ctx [_new class-sym & args :as expr]]
  (let [ctx (without-recur-target ctx)]
    #?(:cljd (if-let [ctor (:constructor (interop/resolve-class-opts ctx class-sym))]
               (let [args (analyze-children ctx args)]
                 (return-call ctx
                              expr
                              ctor
                              args
                              (utils/stack-frame (meta expr) nil)
                              nil))
               (if-let [record (records/resolve-record-class ctx class-sym)]
                 (let [args (analyze-children ctx args)]
                   (return-call ctx
                                expr
                                (:sci.impl/constructor (meta record))
                                args
                                (utils/stack-frame (meta expr) nil)
                                nil))
                 (throw-error-with-location (str "Unable to resolve classname: " class-sym) class-sym)))
       :clj (if-let [class (:class (interop/resolve-class-opts ctx class-sym))]
              (invoke-constructor-node ctx class args)
              (if-let [record (records/resolve-record-class ctx class-sym)]
                (let [args (analyze-children ctx args)]
                  ;; _ctx expr f analyzed-children stack
                  (return-call ctx
                               ;; for backwards compatibility with error reporting
                               expr
                               (:sci.impl/constructor (meta record))
                               args
                               (utils/stack-frame (meta expr) nil)
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
                                      class
                                      ;; Type value from :refers
                                      (instance? sci.lang.Type class)
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
                                      (utils/stack-frame (meta expr) nil)
                                      nil)
                         var?
                         (let [args (into-array args)]
                           (sci.impl.types/->Node
                            (interop/invoke-js-constructor* ctx bindings (deref maybe-var)
                                                            args)
                            nil))
                         (t/eval-node? class)
                         (let [args (into-array args)]
                           (sci.impl.types/->Node
                            (interop/invoke-js-constructor* ctx bindings
                                                            (t/eval class ctx bindings)
                                                            args)
                            nil))
                         :else
                         (let [children args
                               args (into-array args)]
                           (sci.impl.types/->Node
                            (interop/invoke-js-constructor* ctx bindings class ;; no eval needed
                                                            args)
                            nil
                            ;; registered-class ctor (incl. required JS libs);
                            ;; class resolved at analysis, same as js/X.
                            (when (:unrestricted ctx)
                              [:jsctor class (vec children)])))))
                 (if-let [record (records/resolve-record-class ctx class-sym)]
                   (let [args (analyze-children ctx args)]
                     (return-call ctx
                                  ;; for backwards compatibility with error reporting
                                  expr
                                  (:sci.impl/constructor (meta record))
                                  args
                                  (utils/stack-frame (meta expr) nil)
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
  (let [stack (utils/stack-frame (meta expr) nil)]
    (sci.impl.types/->Node
     (try
       (apply f ctx analyzed-args)
       (catch #?(:cljd Object :clj Throwable :cljs js/Error) e
         (rethrow-with-location-of-node ctx bindings e this)))
     stack)))

(defn analyze-ns-form [ctx [_ns ns-name & exprs :as expr]]
  (when-not (symbol? ns-name)
    (throw (new #?(:cljd ArgumentError
                   :clj IllegalArgumentException
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
    (set-namespace! ctx ns-name attr-map true)
    (loop [exprs exprs
           ret []]
      (if exprs
        (let [[k & args :as expr] (first exprs)]
          (case k
            (:require #?(:cljs :require-macros) :use :import :refer-clojure #?(:cljs :refer-global)
                      #?(:cljs :require-global))
            (recur (next exprs)
                   (conj ret
                         (return-ns-op
                          ctx (case k
                                :require load/eval-require
                                #?@(:cljs [:require-macros load/eval-require-macros])
                                #?@(:cljs [:refer-global load/eval-refer-global])
                                #?@(:cljs [:require-global load/eval-require-global])
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

(defn analyze-var [ctx [_ var-name :as expr]]
  (or (second
       (resolve/lookup (assoc ctx :bindings {}) var-name false nil true))
      (throw-error-with-location (str "Unable to resolve var: " var-name) expr)))

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
                    ;; set! returns the assigned value, like CLJS
                    (gobj/set obj prop v)
                    v)
                  nil
                  (when (:unrestricted ctx)
                    [:iset obj prop v])))])
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
                    ;; set! returns the assigned value, like CLJS
                    (gobj/set obj k v)
                    v)
                  nil
                  (when (:unrestricted ctx)
                    [:iset obj k v])))])
    :else (throw-error-with-location "Invalid assignment target" expr)))

;;;; End vars

(defn return-binding-call
  [ctx expr idx f analyzed-children stack]
  (let [node (return-call ctx expr f analyzed-children stack
                          (fn [_ctx bindings _f]
                            (aget #?(:cljd ^List bindings :default ^objects bindings) idx)))]
    (t/attach-ast node [:call-bind idx analyzed-children stack])))

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
                            (range 20))
          catch-clause `(catch ~#?(:cljd 'Object
                                   :default (macros/? :clj 'Throwable :cljs 'js/Error)) e#
                          (rethrow-with-location-of-node ~'ctx ~'bindings e# ~'this))
          eval-arg (fn [j]
                     `(t/eval ~(symbol (str "arg" j)) ~'ctx ~'bindings))
          gen-node (fn [i arg-fn]
                     `(sci.impl.types/->Node
                       (try (~'f ~@(map arg-fn (range i))) ~catch-clause)
                       ~'stack))
          idx-expr (fn [sym]
                     #?(:cljd `(.-idx ~(with-meta sym {:tag 'sci.impl.types/BindingNode}))
                        :default
                        (macros/? :clj `(.idx ~(with-meta sym {:tag 'sci.impl.types.BindingNode}))
                                  :cljs `(.-idx ~(with-meta sym {:tag 'sci.impl.types/BindingNode})))))
          get-idx (fn [j] (idx-expr (symbol (str "arg" j))))
          aget-expr (fn [j]
                      `(aget ~(with-meta 'bindings #?(:cljd {:tag 'List} :default {:tag 'objects}))
                             ~(symbol (str "bidx" j))))
          spec-fns-1 #?(:cljd nil
                        :default
                        (macros/? :clj
                               {'clojure.core/inc 'clojure.lang.Numbers/inc
                                'clojure.core/dec 'clojure.lang.Numbers/dec
                                'clojure.core/unchecked-inc 'clojure.lang.Numbers/unchecked_inc
                                'clojure.core/unchecked-dec 'clojure.lang.Numbers/unchecked_dec
                                'clojure.core/zero? 'clojure.lang.Numbers/isZero
                                'clojure.core/pos? 'clojure.lang.Numbers/isPos
                                'clojure.core/neg? 'clojure.lang.Numbers/isNeg
                                'clojure.core/nil? 'nil?
                                'clojure.core/not 'not}
                               :cljs
                               {'cljs.core/inc 'cljs.core/inc
                                'cljs.core/dec 'cljs.core/dec
                                'cljs.core/unchecked-inc 'cljs.core/unchecked-inc
                                'cljs.core/unchecked-dec 'cljs.core/unchecked-dec
                                'cljs.core/zero? 'cljs.core/zero?
                                'cljs.core/pos? 'cljs.core/pos?
                                'cljs.core/neg? 'cljs.core/neg?
                                'cljs.core/nil? 'nil?
                                'cljs.core/not 'not}))
          spec-fns-2 #?(:cljd nil
                        :default
                        (macros/? :clj
                               {'clojure.core/+ 'clojure.lang.Numbers/add
                                'clojure.core/- 'clojure.lang.Numbers/minus
                                'clojure.core/* 'clojure.lang.Numbers/multiply
                                'clojure.core/unchecked-add 'clojure.lang.Numbers/unchecked_add
                                'clojure.core/unchecked-subtract 'clojure.lang.Numbers/unchecked_minus
                                'clojure.core/unchecked-multiply 'clojure.lang.Numbers/unchecked_multiply
                                'clojure.core/rem 'clojure.lang.Numbers/remainder
                                'clojure.core/< 'clojure.lang.Numbers/lt
                                'clojure.core/> 'clojure.lang.Numbers/gt
                                'clojure.core/<= 'clojure.lang.Numbers/lte
                                'clojure.core/>= 'clojure.lang.Numbers/gte
                                'clojure.core/== 'clojure.lang.Numbers/equiv
                                'clojure.core/= 'clojure.lang.Util/equiv
                                'clojure.core/get 'clojure.lang.RT/get}
                               :cljs
                               {'cljs.core/+ 'cljs.core/+
                                'cljs.core/- 'cljs.core/-
                                'cljs.core/* 'cljs.core/*
                                'cljs.core/unchecked-add 'cljs.core/unchecked-add
                                'cljs.core/unchecked-subtract 'cljs.core/unchecked-subtract
                                'cljs.core/unchecked-multiply 'cljs.core/unchecked-multiply
                                'cljs.core/rem 'cljs.core/rem
                                'cljs.core/< 'cljs.core/<
                                'cljs.core/> 'cljs.core/>
                                'cljs.core/<= 'cljs.core/<=
                                'cljs.core/>= 'cljs.core/>=
                                'cljs.core/== 'cljs.core/==
                                'cljs.core/= 'cljs.core/=
                                'cljs.core/get 'cljs.core/get}))
          gen-specs (fn [spec-fns i arg-fn]
                      (mapcat (fn [[f-sym static-sym]]
                                [f-sym
                                 `(sci.impl.types/->Node
                                   (try (~static-sym ~@(map arg-fn (range i)))
                                        ~catch-clause)
                                   ~'stack)])
                              spec-fns))
          binding-instance? (fn [arg-sym]
                              #?(:cljd `(instance? sci.impl.types/BindingNode ~arg-sym)
                                 :default `(instance? sci.impl.types.BindingNode ~arg-sym)))
          all-bindings? (fn [i]
                          (cons `and (map (fn [j]
                                            (binding-instance? (symbol (str "arg" j))))
                                          (range i))))
          ;; binding-or-constant = BindingNode or constant (no t/eval dispatch needed)
          binding-or-constant? (fn [j]
                                 (let [arg-sym (symbol (str "arg" j))]
                                   `(or ~(binding-instance? arg-sym)
                                        ~#?(:cljd `(not (sci.impl.types/eval-node? ~arg-sym))
                                            :default
                                            (macros/? :clj `(instance? sci.impl.types.ConstantNode ~arg-sym)
                                                      :cljs `(not (t/eval-node? ~arg-sym)))))))
          all-binding-or-constant? (fn [i]
                                     (cons `and (map binding-or-constant? (range i))))
          ;; At analysis time, extract binding idx or constant value per arg.
          ;; At runtime, use captured boolean + value to pick aget vs constant.
          gen-bc-binds (fn [i]
                         (vec (mapcat (fn [j]
                                        (let [arg-sym (symbol (str "arg" j))]
                                          [(symbol (str "bnd" j))
                                           (binding-instance? arg-sym)
                                           (symbol (str "rv" j))
                                           `(if ~(symbol (str "bnd" j))
                                              ~(idx-expr arg-sym)
                                              ~#?(:cljd arg-sym
                                                  :default
                                                  (macros/? :clj `(.x ~(with-meta arg-sym {:tag 'sci.impl.types.ConstantNode}))
                                                            :cljs arg-sym)))]))
                                      (range i))))
          bc-arg-expr (fn [j]
                        `(if ~(symbol (str "bnd" j))
                           (aget ~(with-meta 'bindings #?(:cljd {:tag 'List} :default {:tag 'objects})) ~(symbol (str "rv" j)))
                           ~(symbol (str "rv" j))))
          gen-fused-node (fn [i specs]
                           (let [bidx-binds (vec (mapcat (fn [j]
                                                           [(symbol (str "bidx" j))
                                                            (get-idx j)])
                                                         (range i)))]
                             `(let ~bidx-binds
                                ~(if specs
                                   `(condp identical? ~'f
                                      ~@specs
                                      ~(gen-node i aget-expr))
                                   (gen-node i aget-expr)))))
          ;; Fused/specialized optimization for arities 1-2 only. Clojure
          ;; only inlines core functions at these arities (e.g. <=, + inline
          ;; at arity 2 but not 3). At arity 3+, calls go through IFn.invoke
          ;; regardless, so the fused path would only save t/eval dispatch,
          ;; not worth the extra generated code.
          gen-specialized-or-general (fn [i]
                                       (if (> i 2)
                                         (gen-node i eval-arg)
                                         (let [spec-fns (case (int i) 1 spec-fns-1 2 spec-fns-2 nil)
                                               fused-specs (when spec-fns (gen-specs spec-fns i aget-expr))
                                               ;; Only generate bc specs for arity 2+.
                                               ;; For arity 1, constants get folded at analysis time
                                               ;; (e.g. (inc 1) -> 2), so the condp is dead code.
                                               bc-specs (when (and spec-fns (> i 1))
                                                          (gen-specs spec-fns i bc-arg-expr))]
                                           (if spec-fns
                                             `(if ~(all-bindings? i)
                                                ~(gen-fused-node i fused-specs)
                                                ~(if bc-specs
                                                   `(if ~(all-binding-or-constant? i)
                                                      (let ~(gen-bc-binds i)
                                                        (condp identical? ~'f
                                                          ~@bc-specs
                                                          ~(gen-node i bc-arg-expr)))
                                                      ~(gen-node i eval-arg))
                                                   (gen-node i eval-arg)))
                                             `(if ~(all-bindings? i)
                                                ~(gen-fused-node i nil)
                                                ~(gen-node i eval-arg))))))]
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
                                          ~@(map eval-arg (range i)))
                                         ~catch-clause)
                                       ~'stack)
                                      ~(if (pos? i)
                                         (gen-specialized-or-general i)
                                         (gen-node i eval-arg))))])
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

(gen-return-call)

(defn analyze-quote [_ctx expr]
  (when-not (= 2 (count expr))
    (throw-error-with-location "Wrong number of args (0) passed to quote" expr))
  (let [snd (second expr)]
    (->constant snd)))

(defn analyze-import [_ctx expr]
  (let [args (rest expr)
        stack (utils/stack-frame (meta expr) nil)]
    (sci.impl.types/->Node
     (try (apply eval/eval-import ctx args)
          (catch #?(:cljd Object :clj Throwable :cljs js/Error) e
            (rethrow-with-location-of-node ctx bindings e this)))
     stack)))

#?(:cljd
   (defmacro with-top-level-loc [top-level? m & body]
     `(let [m# ~m
            loc# (when (and ~top-level? m# (:line m#))
                   {:line (:line m#)
                    :column (:column m#)})]
        (binding [utils/*top-level-location* (or loc# utils/*top-level-location*)]
          ~@body)))
   :default
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
                             :cljs (set! utils/*top-level-location* nil)))))))))

(defn dispatch-special [ctx expr f top-level?]
  (case f
    do (return-do ctx expr (rest expr))
    let* (analyze-let* ctx expr (second expr) (nnext expr))
    fn* (analyze-fn* ctx expr)
    def (analyze-def ctx expr)
    loop* (analyze-loop* ctx expr)
    if (return-if ctx expr)
    case* (analyze-case* ctx expr)
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
    recur (let [children (analyze-children (without-recur-target ctx) (rest expr))
                node (return-recur ctx expr children)]
            (t/attach-ast node [:recur children]))
    ;; Available as macro, but here for optimized version
    or (return-or ctx expr (rest expr))
    and (return-and ctx expr (rest expr))
    ns (analyze-ns-form ctx expr)
    lazy-seq (analyze-lazy-seq ctx expr)
    deftype* (sci.impl.deftype/analyze-deftype* ctx expr top-level?)))


#?(:cljd nil
   :clj
   (defn analyze-interop [ctx expr [^Class clazz meth]]
     (let [meth (str meth)
           stack (utils/stack-frame (meta expr) nil)]
       (cond (str/starts-with? meth ".")
             (let [meth (subs meth 1)
                   ;; class is explicit and is the dispatch class, resolve config here
                   class-opts (get (-> ctx :env deref :class->opts)
                                   (symbol (.getName clazz)))
                   override (get (:instance-methods class-opts) (symbol meth))
                   arg-types (when-let [param-tags (some-> (meta expr) :param-tags)]
                               (let [param-count (count param-tags)
                                     ^"[Ljava.lang.Class;" arg-types (when (pos? param-count)
                                                                       (make-array Class param-count))]
                                 (areduce arg-types idx _ret nil
                                          (when-let [t (nth param-tags idx)]
                                            (when-not (= '_ t)
                                              (when-let [t (interop/resolve-type-hint ctx t)]
                                                (aset arg-types idx t)))))
                                 arg-types))
                   reflect-f (fn [obj & args]
                               (let [args (object-array args)
                                     arg-count (alength args)
                                     ^java.util.List methods (interop/meth-cache ctx clazz meth arg-count #(reflector/get-methods clazz arg-count meth false) :instance-methods)]
                                 (reflector/invoke-matching-method meth methods clazz obj args arg-types)))]
               (case (interop/member-disposition override class-opts :instance-methods)
                 :override (sci.impl.types/->Node override stack)
                 :deny (utils/throw-error-with-location
                        (str "Method " meth " on class " (.getName clazz) " not allowed!") expr)
                 :reflect (sci.impl.types/->Node reflect-f stack)))
             (try (reflector/get-static-field ^Class clazz ^String meth)
                  (catch IllegalArgumentException _
                    nil))
             (let [sf-opts (get (some-> ctx :env deref :class->opts)
                                (symbol (.getName ^Class clazz)))
                   override (get (:static-fields sf-opts) (symbol meth))]
               (case (interop/member-disposition override sf-opts :static-fields)
                 :override (sci.impl.types/->Node (override clazz) stack)
                 :deny (utils/throw-error-with-location
                        (str "Field " meth " on class " (.getName ^Class clazz) " not allowed!") expr)
                 :reflect (sci.impl.types/->Node
                           (interop/get-static-field clazz meth)
                           stack)))
             :else (sci.impl.types/->Node
                    (fn [& args]
                      (reflector/invoke-static-method
                       clazz meth
                       ^objects (into-array Object args)))
                    stack)))))

#?(:cljd
   (defn- named-arg-sym? [x]
     (and (symbol? x)
          (let [n (name x)]
            ;; a real name after the dot: bare `.` is not a named arg
            (and (> (count n) 1) (identical? \. (nth n 0)))))))

#?(:cljd
   (defn- desugar-named-args
     "ClojureDart call-site sugar: trailing `.name val` pairs become `:name val`
      keyword args. A stray or unpaired trailing form is malformed."
     [args]
     (if (some named-arg-sym? args)
       (let [[pos named] (split-with (complement named-arg-sym?) args)
             named (if (identical? '.& (first named)) (next named) named)]
         (loop [pairs named
                acc (vec pos)]
           (if (seq pairs)
             (let [k (first pairs)
                   more (next pairs)]
               (when-not (named-arg-sym? k)
                 (throw (ex-info (str "Malformed named arguments: expected a .name but got "
                                      (pr-str k)) {})))
               (when-not more
                 (throw (ex-info (str "Malformed named arguments: missing value for " k) {})))
               (recur (next more) (conj acc (keyword (subs (name k) 1)) (first more))))
             acc)))
       args)))

(defn analyze-call [ctx expr m top-level?]
  (with-top-level-loc top-level? m
    (try
      (let [f* (first expr)]
        (cond (symbol? f*)
              (let [fsym f*
                    ;; in call position Clojure prioritizes special symbols over
                    ;; bindings
                    special-sym (get special-syms f*)
                    _ (when (and special-sym
                                 (:check-permissions ctx))
                        (resolve/check-permission! ctx f* [special-sym nil]))
                    f (or special-sym
                          (resolve/resolve-symbol ctx f* true))
                    f-meta (meta f)
                    eval? (and f-meta (:sci.impl/op f-meta))
                    fast-path (-> f-meta :sci.impl/fast-path)
                    f (or fast-path f)]
                (cond (and f-meta (::static-access f-meta))
                      #?(:cljs
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
                                  nil
                                  ;; direct emission only when no interop
                                  ;; checks apply, like instance interop
                                  (when (:unrestricted ctx)
                                    [:jsctor ctor (vec children)])))
                               (if (t/eval-node? class)
                                 (sci.impl.types/->Node
                                  (let [class (t/eval class ctx bindings)
                                        method (unchecked-get class method-name)]
                                    (interop/invoke-static-method ctx bindings class method children))
                                  nil)
                                 (let [method (unchecked-get class method-name)
                                       stack (utils/stack-frame m f-meta)]
                                   (sci.impl.types/->Node
                                    (try (interop/invoke-static-method ctx bindings class method children)
                                         (catch :default e
                                           (utils/rethrow-with-location-of-node ctx e this)))
                                    stack
                                    (when (:unrestricted ctx)
                                      [:jsstatic method class (vec children) stack])))))
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
                                nil))))
                         :default
                         (let [[clazz meth class-expr] f]
                           (analyze-dot ctx (with-meta (list* '. clazz meth (rest expr))
                                              (assoc m :class-expr class-expr)))))
                      #?@(:clj [(and f-meta (:sci.impl.analyzer/interop f-meta))
                                (let [[obj & args] (analyze-children ctx (rest expr))
                                      meth (-> (second f)
                                               str
                                               (subs 1))
                                      clazz (first f)
                                      ;; class is explicit and is the dispatch class, resolve config here
                                      class-opts (get (-> ctx :env deref :class->opts)
                                                      (symbol (.getName ^Class clazz)))
                                      override (get (:instance-methods class-opts) (symbol meth))
                                      args (object-array args)
                                      arg-count (count args)
                                      stack (utils/stack-frame m f-meta)
                                      ^"[Ljava.lang.Class;" arg-types (when (pos? arg-count)
                                                                        (make-array Class arg-count))
                                      has-types? (volatile! nil)]
                                  (when arg-types
                                    (or (when-let [param-tags (-> f* (some-> meta :param-tags))]
                                          (vreset! has-types? true)
                                          (areduce arg-types idx _ret nil
                                                   (when-let [t (nth param-tags idx)]
                                                     (when-not (= '_ t)
                                                       (when-let [t (interop/resolve-type-hint ctx t)]
                                                         (aset arg-types idx t))))))
                                        (areduce args idx _ret nil
                                                 (let [arg (aget args idx)
                                                       arg-meta (meta arg)]
                                                   (when-let [t (:tag arg-meta)]
                                                     (when-let [t (interop/resolve-type-hint ctx t)]
                                                       (do (vreset! has-types? true)
                                                           (aset arg-types idx t))))))))
                                  (case (interop/member-disposition override class-opts :instance-methods)
                                    :override
                                    (sci.impl.types/->Node
                                     (apply override (sci.impl.types/eval obj ctx bindings)
                                            (map #(sci.impl.types/eval % ctx bindings) args))
                                     stack)
                                    :deny
                                    (throw-error-with-location
                                     (str "Method " meth " on class " (.getName ^Class clazz) " not allowed!") expr)
                                    :reflect
                                    (sci.impl.types/->Node
                                     (let [obj (sci.impl.types/eval obj ctx bindings)]
                                       (interop/invoke-instance-method ctx bindings obj clazz
                                                                       meth
                                                                       args arg-count arg-types))
                                     stack)))])
                      #?@(:clj [(and f-meta (:sci.impl.analyzer/invoke-constructor f-meta))
                                (invoke-constructor-node ctx (first f) (rest expr))])
                      (and (not eval?) ;; the symbol is not a binding
                           (symbol? f)
                           (or
                            special-sym
                            (contains? ana-macros f)))
                      (dispatch-special ctx expr f top-level?)
                      :else
                      (try
                        (if (macro? f)
                          (let [;; Fix for #603
                                #?@(:cljs [f (if (utils/var? f)
                                               @f
                                               f)
                                           f (or (.-afn ^js f) f)])
                                v (store/with-ctx ctx
                                    ;; host macro fns are fixed-arity Dart closures
                                    #?(:cljd (try (apply f expr (:bindings ctx) (rest expr))
                                                  (catch NoSuchMethodError _
                                                    (let [op (first expr)
                                                          op (if (and (symbol? op) (not (namespace op)))
                                                               (symbol (str (utils/current-ns-name)) (str op))
                                                               op)]
                                                      (throw (ex-info (str "Wrong number of args ("
                                                                           (+ 2 (count (rest expr)))
                                                                           ") passed to: " op)
                                                                      {})))))
                                       :default (apply f expr (:bindings ctx) (rest expr))))
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
                          (let [rest-forms #?(:cljd (desugar-named-args (rest expr))
                                              :default (rest expr))]
                           (if-let [f (:sci.impl/inlined f-meta)]
                            (let [children (analyze-children ctx rest-forms)
                                  stack (utils/stack-frame m f-meta)
                                  node (return-call ctx
                                                    expr
                                                    f children
                                                    stack
                                                    nil)]
                              (t/attach-ast node [:call-direct f children stack]))
                            (if-let [op (:sci.impl/op (meta f))]
                              (case op
                                :resolve-sym
                                (return-binding-call ctx
                                                     expr
                                                     (:sci.impl/idx (meta f))
                                                     f (analyze-children ctx rest-forms)
                                                     (utils/stack-frame m f-meta))
                                (let [children (analyze-children ctx rest-forms)]
                                  (return-call ctx
                                               expr
                                               f children (utils/stack-frame m f-meta)
                                               nil)))
                              (let [self-ref? (:self-ref? ctx)]
                                (if (and self-ref? (self-ref? f))
                                  (let [children (analyze-children ctx rest-forms)]
                                    (return-call ctx
                                                 expr
                                                 f children (utils/stack-frame m f-meta)
                                                 (fn [_ bindings _]
                                                   (deref
                                                    (eval/resolve-symbol bindings fsym)))))
                                  (let [children (analyze-children ctx rest-forms)
                                        stack (utils/stack-frame m f-meta)
                                        node (return-call ctx
                                                          expr
                                                          f children stack
                                                          #?(:cljd nil
                                                             :cljs (when (utils/var? f) (fn [_ _ v]
                                                                                          (deref v))) :clj nil))]
                                    #?(:cljs (cond (utils/var? f)
                                                   (t/attach-ast node [:call-var f children stack])
                                                   (fn? f)
                                                   (t/attach-ast node [:call-direct f children stack])
                                                   :else node)
                                       :default node))))))))
                        (catch #?(:cljd Object :clj Exception :cljs js/Error) e
                          ;; we pass a ctx-fn because the rethrow function calls
                          ;; stack on it, the only interesting bit it the map
                          ;; with :ns and :file
                          (rethrow-with-location-of-node ctx e
                                                         (let [stack (utils/stack-frame m f-meta)]
                                                           (sci.impl.types/->Node nil stack)))))))
              (keyword? f*)
              (let [children (analyze-children ctx (rest expr))
                    ccount (count children)]
                (case ccount
                  1 (let [arg (nth children 0)]
                      (sci.impl.types/->Node
                       (f* (t/eval arg ctx bindings))
                       nil))
                  2 (let [arg0 (nth children 0)
                          arg1 (nth children 1)]
                      (sci.impl.types/->Node
                       (f* (t/eval arg0 ctx bindings)
                           (t/eval arg1 ctx bindings))
                       nil))
                  (throw-error-with-location (str "Wrong number of args (" ccount ") passed to: " f*) expr)))
              :else
              (let [f (analyze ctx f*)
                    children (analyze-children ctx (rest expr))
                    stack (utils/stack-frame m nil)
                    node (return-call ctx
                                      expr
                                      f children stack
                                      #?(:cljd (fn [ctx bindings f]
                                                 (t/eval f ctx bindings))
                                         :cljs (if (utils/var? f)
                                                 (fn [ctx bindings f]
                                                   (t/eval @f ctx bindings))
                                                 (fn [ctx bindings f]
                                                   (t/eval f ctx bindings)))
                                         :clj (fn [ctx bindings f]
                                                (t/eval f ctx bindings))))]
                #?(:cljs (if (utils/var? f)
                           node
                           ;; computed callee, e.g. ((add 1) 2): the callee
                           ;; node compiles like any child expression
                           (t/attach-ast node [:call-node f children stack]))
                   :default node))))
      (catch #?(:cljd Object
                :clj Exception
                :cljs :default) e
        (utils/rethrow-with-location-of-node ctx e (sci.impl.types/->Node nil (utils/make-stack m)))))))

(defn map-fn [children-count]
  #?(:cljd (fn [& kvs]
             (loop [m {} kvs (seq kvs)]
               (if kvs
                 (let [k (first kvs)]
                   (if (contains? m k)
                     (throw (ex-info (str "Duplicate key: " k) {}))
                     (recur (assoc m k (fnext kvs)) (nnext kvs))))
                 m)))
     :default
     (if (<= children-count 16)
       #?(:clj #(let [^objects arr (into-array Object %&)]
                  (clojure.lang.PersistentArrayMap/createWithCheck arr))
          :cljs #(.createWithCheck PersistentArrayMap (into-array %&))
          :default array-map)
       #?(:clj #(let [^clojure.lang.ISeq s %&]
                  (clojure.lang.PersistentHashMap/createWithCheck s))
          :cljs #(.createWithCheck PersistentHashMap (into-array %&))
          :default hash-map))))

(defn return-map [ctx the-map analyzed-children]
  (let [mf (map-fn (count analyzed-children))
        node (return-call ctx the-map mf analyzed-children nil nil)]
    (t/attach-ast node [:call-direct mf analyzed-children nil])))

(defn constant-node? [x]
  #?(:cljd (not (t/eval-node? x))
     :clj (instance? sci.impl.types.ConstantNode x)
     :cljs (not (t/eval-node? x))))

#?(:cljd nil
   :clj (defn unwrap-children [children]
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
                        (let [node (return-call ctx expr f2 analyzed-children nil nil)]
                          (t/attach-ast node [:call-direct f2 analyzed-children nil])))
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
                                  (throw #?(:cljd (ex-info (str "Can't take value of a macro: " v "") {})
                                            :clj (new IllegalStateException
                                                      (str "Can't take value of a macro: " v ""))
                                            :cljs (new js/Error
                                                       (str "Can't take value of a macro: " v ""))))
                                  (sci.impl.types/->Node
                                   (faster/deref-1 v)
                                   nil
                                   ;; value-position var read: emitted as a
                                   ;; plain deref, never cached (see the
                                   ;; retention note on the deref cache)
                                   [:vderef v])))
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
                                       #?(:cljd (fn [& xs]
                                                  (loop [s #{} xs (seq xs)]
                                                    (if xs
                                                      (let [x (first xs)]
                                                        (if (contains? s x)
                                                          (throw (ex-info (str "Duplicate key: " x) {}))
                                                          (recur (conj s x) (next xs))))
                                                      s)))
                                          :clj #(clojure.lang.PersistentHashSet/createWithCheck ^clojure.lang.ISeq %&)
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
