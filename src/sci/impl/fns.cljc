(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.faster :refer [nth-2 assoc-3 get-2]]
            [sci.impl.macros :as macros :refer [?]]
            [sci.impl.types :as t]
            [sci.impl.utils :as utils])
  #?(:cljs (:require-macros [sci.impl.fns :refer [gen-fn
                                                  gen-fn-varargs]])))

#?(:clj (set! *warn-on-reflection* true))

(defn throw-arity [ctx fn-name macro? args]
  (when-not (:disable-arity-checks ctx)
    (throw (new #?(:clj Exception
                   :cljs js/Error)
                (let [actual-count (if macro? (- (count args) 2)
                                       (count args))]
                  (str "Wrong number of args (" actual-count ") passed to: " fn-name))))))

(deftype Recur #?(:clj [val]
                  :cljs [val])
  t/IBox
  (getVal [_] val))

;; gen-run-fn expands into something like but using nth for better performance:

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

(defmacro gen-fn
  ([n]
   `(gen-fn ~n false))
  ([n disable-arity-checks]
   (let [locals (repeatedly n gensym)
         fn-params (vec (repeatedly n gensym))
         rnge (range n)
         nths (map (fn [n] `(nth-2 ~'params ~n)) rnge)
         let-vec (vec (mapcat (fn [local ith]
                                [local ith]) locals nths))
         assocs (mapcat (fn [local fn-param]
                          `[~'bindings (assoc-3 ~'bindings ~local ~fn-param)])
                        locals fn-params)
         recurs (map (fn [n]
                       `(nth-2 ~'recur-val ~n))
                     rnge)]
     `(let ~let-vec
        (fn ~(symbol (str "arity-" n)) ~fn-params
          ~@(? :cljs
               (when-not disable-arity-checks
                 `[(when-not (= ~n (.-length (~'js-arguments)))
                     (throw-arity ~'ctx ~'fn-name ~'macro? (vals (~'js->clj (~'js-arguments)))))]))
          (let [;; tried making bindings a transient, but saw no perf improvement (see #246)
                ~'bindings (get-2 ~'ctx :bindings)
                ~@assocs
                ctx# (assoc-3 ~'ctx :bindings ~'bindings)
                ret# (~'return ctx#)
                ;; m (meta ret)
                recur?# (instance? Recur ret#)]
            (if recur?#
              (let [~'recur-val (t/getVal ret#)]
                (recur ~@recurs))
              ret#)))))))

#_(require '[clojure.pprint :as pprint])
#_(binding [*print-meta* true]
    (pprint/pprint (macroexpand '(gen-run-fn 2))))

(defmacro gen-fn-varargs []
  '(fn varargs [& args]
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
                            (assoc-3 ret fp (first args*))))))
               (do
                 (when args*
                   (throw-arity ctx fn-name macro? args))
                 ret)))
           ctx (assoc-3 ctx :bindings bindings)
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

(defn fun
  [^clojure.lang.Associative ctx interpret eval-do*
   {:sci.impl/keys [fixed-arity var-arg-name
                    #_:clj-kondo/ignore params body] :as _m}
   #_:clj-kondo/ignore fn-name
   #_:clj-kondo/ignore macro?
   with-meta?]
  (let [disable-arity-checks? (get-2 ctx :disable-arity-checks)
        min-var-args-arity (when var-arg-name fixed-arity)
        body-count (count body)
        return (if (= 1 body-count)
                 (let [fst (first body)]
                   #(interpret % fst))
                 #(eval-do* % body))
        f (if-not (or var-arg-name
                      #?(:clj disable-arity-checks?))
            (case (int fixed-arity)
              0 (fn arity-0 []
                  (let [ret (return ctx)
                        ;; m (meta ret)
                        recur? (instance? Recur ret)]
                    (if recur? (recur) ret)))
              1 #?(:clj (gen-fn 1)
                   :cljs (if disable-arity-checks?
                           (gen-fn 1 true)
                           (gen-fn 1 false)))
              2 #?(:clj (gen-fn 2)
                   :cljs (if disable-arity-checks?
                           (gen-fn 2 true)
                           (gen-fn 2 false)))
              3 #?(:clj (gen-fn 3)
                   :cljs (if disable-arity-checks?
                           (gen-fn 3 true)
                           (gen-fn 3 false)))
              4 #?(:clj (gen-fn 4)
                   :cljs (if disable-arity-checks?
                           (gen-fn 4 true)
                           (gen-fn 4 false)))
              5 #?(:clj (gen-fn 5)
                   :cljs (if disable-arity-checks?
                           (gen-fn 5 true)
                           (gen-fn 5 false)))
              6 #?(:clj (gen-fn 6)
                   :cljs (if disable-arity-checks?
                           (gen-fn 6 true)
                           (gen-fn 6 false)))
              7 #?(:clj (gen-fn 7)
                   :cljs (if disable-arity-checks?
                           (gen-fn 7 true)
                           (gen-fn 7 false)))
              8 #?(:clj (gen-fn 8)
                   :cljs (if disable-arity-checks?
                           (gen-fn 8 true)
                           (gen-fn 8 false)))
              9 #?(:clj (gen-fn 9)
                   :cljs (if disable-arity-checks?
                           (gen-fn 9 true)
                           (gen-fn 9 false)))
              10 #?(:clj (gen-fn 10)
                    :cljs (if disable-arity-checks?
                            (gen-fn 10 true)
                            (gen-fn 10 false)))
              11 #?(:clj (gen-fn 11)
                    :cljs (if disable-arity-checks?
                            (gen-fn 11 true)
                            (gen-fn 11 false)))
              12 #?(:clj (gen-fn 12)
                    :cljs (if disable-arity-checks?
                            (gen-fn 12 true)
                            (gen-fn 12 false)))
              13 #?(:clj (gen-fn 13)
                    :cljs (if disable-arity-checks?
                            (gen-fn 13 true)
                            (gen-fn 13 false)))
              14 #?(:clj (gen-fn 14)
                    :cljs (if disable-arity-checks?
                            (gen-fn 15 true)
                            (gen-fn 15 false)))
              15 #?(:clj (gen-fn 3)
                    :cljs (if disable-arity-checks?
                            (gen-fn 15 true)
                            (gen-fn 15 false)))
              16 #?(:clj (gen-fn 16)
                    :cljs (if disable-arity-checks?
                            (gen-fn 16 true)
                            (gen-fn 16 false)))
              17 #?(:clj (gen-fn 17)
                    :cljs (if disable-arity-checks?
                            (gen-fn 17 true)
                            (gen-fn 17 false)))
              18 #?(:clj (gen-fn 18)
                    :cljs (if disable-arity-checks?
                            (gen-fn 18 true)
                            (gen-fn 18 false)))
              19 #?(:clj (gen-fn 19)
                    :cljs (if disable-arity-checks?
                            (gen-fn 19 true)
                            (gen-fn 19 false)))
              (gen-fn-varargs))
            (gen-fn-varargs))]
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
            (fun ctx interpret eval-do* (first fn-bodies) fn-name macro? false)
            (let [arities (map #(fun ctx interpret eval-do* % fn-name macro? true) fn-bodies)]
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
