(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.types :as t]
            [sci.impl.utils :as utils]
            [sci.impl.macros :as macros :refer [?]])
  #?(:cljs (:require-macros [sci.impl.fns :refer [gen-run-fn
                                                  gen-run-fn*]])))

#?(:clj (set! *warn-on-reflection* true))

(defn throw-arity [ctx fn-name macro? args]
  (when-not (:disable-arity-checks ctx)
    (throw (new #?(:clj Exception
                   :cljs js/Error)
                (let [actual-count (if macro? (- (count args) 2)
                                       (count args))]
                  (str "Cannot call " fn-name " with " actual-count " arguments"))))))

(deftype Recur #?(:clj [val]
                  :cljs [val])
  t/IBox
  (getVal [_] val))

;; gen-run-fn expands into something like:

#_(let [p1 (first params)
        p2 (second params)]
    (fn run-fn [x y]
      (let [;; tried making bindings a transient, but saw no perf improvement (see #246)
            bindings (.get ^java.util.Map ctx :bindings)
            bindings (.assoc ^clojure.lang.Associative bindings p1 x)
            bindings (.assoc ^clojure.lang.Associative bindings p2 y)
            ctx #?(:clj (.assoc ctx :bindings bindings)
                   :cljs (-assoc ctx :bindings bindings))
            ret (return ctx)
            ;; m (meta ret)
            recur? (instance? Recur ret)]
        (if recur?
          (let [recur-val (t/getVal ret)]
            (recur (first recur-val) (second recur-val)))
          ret))))

(defmacro gen-run-fn [n]
  (let [locals (repeatedly n gensym)
        fn-params (vec (repeatedly n gensym))
        rnge (range n)
        nths (map (fn [n]
                    (? :clj `(.nth ~(with-meta 'params {:tag 'clojure.lang.Indexed}) ~n)
                            :cljs (list '-nth 'params n)))
                  rnge)
        let-vec (vec (mapcat (fn [local ith]
                               [local ith]) locals nths))
        assocs (mapcat (fn [local fn-param]
                         `[~'bindings ~(? :clj `(.assoc ~(with-meta 'bindings
                                                           {:tag 'clojure.lang.Associative})
                                                       ~local ~fn-param)
                                               :cljs `(~'-assoc ~'bindings ~local ~fn-param))])
                       locals fn-params)
        recurs (map (fn [n]
                      (? :clj `(.nth ~(with-meta 'recur-val {:tag 'clojure.lang.Indexed}) ~n)
                              :cljs `(~'-nth ~'recur-val ~n)))
                    rnge)]
    `(let ~let-vec
       (fn ~'run-fn ~fn-params
         (? :cljs (when-not (:disable-arity-checks ~'ctx)
                    (when-not (= ~n (.-length (~'js-arguments)))
                      (throw-arity ~'ctx ~'fn-name ~'macro? (vals (~'js->clj (~'js-arguments)))))))
         (let [;; tried making bindings a transient, but saw no perf improvement (see #246)
               ~'bindings (.get ~(with-meta 'ctx {:tag 'java.util.Map}) :bindings)
               ~@assocs
               ctx# (? :clj (.assoc ~(with-meta 'ctx {:tag 'clojure.lang.Associative})
                                    :bindings ~'bindings)
                            :cljs (~'-assoc ~'ctx :bindings ~'bindings))
               ret# (~'return ctx#)
               ;; m (meta ret)
               recur?# (instance? Recur ret#)]
           (if recur?#
             (let [~'recur-val (t/getVal ret#)]
               (recur ~@recurs))
             ret#))))))

#_(require '[clojure.pprint :as pprint])
#_(binding [*print-meta* true]
    (pprint/pprint (macroexpand '(gen-run-fn 2))))

(defmacro gen-run-fn* []
  '(fn run-fn [& args]
                (let [;; tried making bindings a transient, but saw no perf improvement (see #246)
                      bindings (.get ^java.util.Map ctx :bindings)
                      bindings
                      (loop [args* (seq args)
                             params (seq params)
                             ret bindings]
                        (if params
                          (let [fp (first params)]
                            (if (= '& fp)
                              (assoc ret (second params) args*)
                              (do
                                (when-not args*
                                  (throw-arity ctx fn-name macro? args))
                                (recur (next args*) (next params)
                                       (assoc ret fp (first args*))))))
                          (do
                            (when args*
                              (throw-arity ctx fn-name macro? args))
                            ret)))
                      ctx (? :clj (.assoc ctx :bindings bindings)
                                  :cljs (-assoc ctx :bindings bindings))
                      ret (return ctx)
                      ;; m (meta ret)
                      recur? (instance? Recur ret)]
                  (if recur?
                    (let [recur-val (t/getVal ret)]
                      (if min-var-args-arity
                        (let [[fixed-args [rest-args]]
                              [(subvec recur-val 0 min-var-args-arity)
                               (subvec recur-val min-var-args-arity)]]
                          (recur (into fixed-args rest-args)))
                        (recur recur-val)))
                    ret))))

(defn parse-fn-args+body
  [^clojure.lang.Associative ctx interpret eval-do*
   {:sci.impl/keys [fixed-arity var-arg-name
                    #_:clj-kondo/ignore params body] :as _m}
   #_:clj-kondo/ignore fn-name
   #_:clj-kondo/ignore macro?
   with-meta?]
  (let [min-var-args-arity (when var-arg-name fixed-arity)
        body-count (count body)
        return (if (= 1 body-count)
                 (let [fst (first body)]
                   #(interpret % fst))
                 #(eval-do* % body))
        f (if-not (or var-arg-name
                      #?(:clj (:disable-arity-checks ctx)))
            (case (int fixed-arity)
              0 (fn run-fn []
                  (let [ret (return ctx)
                        ;; m (meta ret)
                        recur? (instance? Recur ret)]
                    (if recur? (recur) ret)))
              1 (gen-run-fn 1)
              2 (gen-run-fn 2)
              3 (gen-run-fn 3)
              4 (gen-run-fn 4)
              5 (gen-run-fn 5)
              6 (gen-run-fn 6)
              7 (gen-run-fn 7)
              8 (gen-run-fn 8)
              9 (gen-run-fn 9)
              10 (gen-run-fn 10)
              11 (gen-run-fn 11)
              12 (gen-run-fn 11)
              13 (gen-run-fn 11)
              14 (gen-run-fn 11)
              15 (gen-run-fn 11)
              16 (gen-run-fn 11)
              17 (gen-run-fn 17)
              18 (gen-run-fn 18)
              19 (gen-run-fn 19)
              (gen-run-fn*))
            (gen-run-fn*))]
    (if with-meta?
      (with-meta
        f
        (if min-var-args-arity
          {:sci.impl/min-var-args-arity min-var-args-arity}
          {:sci.impl/fixed-arity fixed-arity}))
      f)))

(defn lookup-by-arity [arities arity]
  (some (fn [f]
          (let [{:sci.impl/keys [fixed-arity min-var-args-arity]} (meta f)]
            (when (or (= arity fixed-arity )
                      (and min-var-args-arity
                           (>= arity min-var-args-arity)))
              f))) arities))

(defn eval-fn [ctx interpret eval-do* {:sci.impl/keys [fn-bodies fn-name
                                                       var] :as f}]
  (let [macro? (:sci/macro f)
        self-ref (atom nil)
        ctx (if (and (not var)
                     fn-name)
              (assoc-in ctx [:bindings fn-name]
                        (fn call-self [& args]
                          (apply @self-ref args)))
              ctx)
        single-arity? (= 1 (count fn-bodies))
        f (if single-arity?
            (parse-fn-args+body ctx interpret eval-do* (first fn-bodies) fn-name macro? false)
            (let [arities (map #(parse-fn-args+body ctx interpret eval-do* % fn-name macro? true) fn-bodies)]
              (fn [& args]
                (let [arg-count (count args)]
                  (if-let [f (lookup-by-arity arities arg-count)]
                    (apply f args)
                    (throw (new #?(:clj Exception
                                   :cljs js/Error)
                                (let [actual-count (if macro? (- arg-count 2)
                                                       arg-count)]
                                  (str "Cannot call " fn-name " with " actual-count " arguments")))))))))
        f (if macro?
            (vary-meta f
                       #(assoc % :sci/macro macro?))
            f)]
    (reset! self-ref f)
    f))

(vreset! utils/eval-fn eval-fn)

;;;; Scratch

(comment
  )
