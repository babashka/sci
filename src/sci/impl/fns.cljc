(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.evaluator :as eval]
            [sci.impl.faster :refer [nth-2 assoc-3 get-2]]
            [sci.impl.macros :as macros :refer [?]]
            [sci.impl.types :as t]
            [sci.impl.utils :as utils :refer [ctx-fn]]
            [sci.impl.vars :as vars])
  #?(:cljs (:require-macros [sci.impl.fns :refer [gen-fn
                                                  gen-fn-varargs]])))

#?(:clj (set! *warn-on-reflection* true))

(defn throw-arity [ctx nsm fn-name macro? args expected-arity]
  (when-not (:disable-arity-checks ctx)
    (throw (new #?(:clj Exception
                   :cljs js/Error)
                (let [actual-count (if macro? (- (count args) 2)
                                       (count args))]
                  (str "Wrong number of args (" actual-count ") passed to: " (if fn-name
                                                                               (str nsm "/" fn-name)
                                                                               (str "function of arity " expected-arity))))))))

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
   (if (zero? n)
     `(fn ~'arity-0 []
        ~@(? :cljs
             (when-not disable-arity-checks
               `[(when-not (zero? (.-length (~'js-arguments)))
                   (throw-arity ~'ctx ~'nsm ~'fn-name ~'macro? (vals (~'js->clj (~'js-arguments))) 0))]))
        (let [ret# (eval/eval ~'ctx ~'bindings ~'body)
              ;; m (meta ret)
              recur?# (instance? Recur ret#)]
           (if recur?# (recur) ret#)))
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
                       (throw-arity ~'ctx ~'nsm ~'fn-name ~'macro? (vals (~'js->clj (~'js-arguments))) ~n))]))
            (let [;; tried making bindings a transient, but saw no perf improvement
                  ;; it's even slower with less than ~10 bindings which is pretty uncommon
                  ;; see https://github.com/borkdude/sci/issues/559
                  ~@assocs
                  ret# (eval/eval ~'ctx ~'bindings ~'body)
                  ;; m (meta ret)
                  recur?# (instance? Recur ret#)]
              (if recur?#
                (let [~'recur-val (t/getVal ret#)]
                  (recur ~@recurs))
                ret#))))))))

#_(require '[clojure.pprint :as pprint])
#_(binding [*print-meta* true]
    (pprint/pprint (macroexpand '(gen-run-fn 2))))

(defmacro gen-fn-varargs []
  '(fn varargs [& args]
     (let [;; tried making bindings a transient, but saw no perf improvement
           ;; it's even slower with less than ~10 bindings which is pretty uncommon
           ;; see https://github.com/borkdude/sci/issues/559
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
                       (throw-arity ctx nsm fn-name macro? args (inc (count ret))))
                     (recur (next args*) (next params)
                            (assoc-3 ret fp (first args*))))))
               (do
                 (when args*
                   (throw-arity ctx nsm fn-name macro? args (inc (count ret))))
                 ret)))
           ret (eval/eval ctx bindings body)
           ;; m (meta ret)
           recur? (instance? Recur ret)]
       (if recur?
         (let [recur-val (t/getVal ret)
               min-var-args-arity (when var-arg-name fixed-arity)]
           (if min-var-args-arity
             (let [[fixed-args [rest-args]]
                   [(subvec recur-val 0 min-var-args-arity)
                    (subvec recur-val min-var-args-arity)]]
               (recur (into fixed-args rest-args)))
             (recur recur-val)))
         ret))))

(defn fun
  [#?(:clj ^clojure.lang.Associative ctx :cljs ctx)
   bindings
   fn-body
   #_:clj-kondo/ignore fn-name
   #_:clj-kondo/ignore macro?]
  (let [bindings-fn (:bindings-fn fn-body)
        bindings (bindings-fn bindings)
        fixed-arity (:fixed-arity fn-body)
        var-arg-name (:var-arg-name fn-body)
        #_:clj-kondo/ignore
        params (:params fn-body)
        body (:body fn-body)
        #_:clj-kondo/ignore nsm (vars/current-ns-name)
        disable-arity-checks? (get-2 ctx :disable-arity-checks)
        ;; body-count (count body)
        f (if-not #?(:clj (or var-arg-name
                              disable-arity-checks?)
                     :cljs var-arg-name)
            (case (int fixed-arity)
              0 #?(:clj (gen-fn 0)
                   :cljs (if disable-arity-checks?
                           (gen-fn 0 true)
                           (gen-fn 0 false)))
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
              20 #?(:clj (gen-fn 20)
                    :cljs (if disable-arity-checks?
                            (gen-fn 20 true)
                            (gen-fn 20 false)))
              (gen-fn-varargs))
            (gen-fn-varargs))]
    f))

(defn lookup-by-arity [arities arity]
  (or (get arities arity)
      (:variadic arities)))

(defn fn-arity-map [ctx bindings fn-name macro? fn-bodies]
  (reduce
   (fn [arity-map fn-body]
     (let [f (fun ctx bindings fn-body fn-name macro?)
           var-arg? (:var-arg-name fn-body)
           fixed-arity (:fixed-arity fn-body)]
       (if var-arg?
         (assoc arity-map :variadic f)
         (assoc arity-map fixed-arity f))))
   {}
   fn-bodies))

(defn eval-fn [ctx bindings fn-name fn-bodies macro? single-arity self-ref]
  (let [;; each evaluated fn should have its own self-ref!
        self-ref (when self-ref (volatile! nil))
        bindings (if self-ref
                   (assoc bindings fn-name self-ref)
                   bindings)
        ;; _ (when (and (not fn-name) self-ref) (prn :assoc fn-name self-refx))
        f (if single-arity
            (fun ctx bindings single-arity fn-name macro?)
            (let [arities (fn-arity-map ctx bindings fn-name macro? fn-bodies)]
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
                       #(assoc %
                               :sci/macro macro?
                               ;; added for better error reporting
                               :sci.impl/inner-fn f))
            f)]
    (when self-ref (vreset! self-ref f))
    f))

(vreset! utils/eval-fn eval-fn)

;;;; Scratch

(comment
  )
