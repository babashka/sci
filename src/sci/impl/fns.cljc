(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.types :as t]))

#?(:clj (set! *warn-on-reflection* true))

(defn throw-arity [fn-name macro? args]
  (throw (new #?(:clj Exception
                 :cljs js/Error)
              (let [actual-count (if macro? (- (count args) 2)
                                     (count args))]
                (str "Cannot call " fn-name " with " actual-count " arguments")))))

(deftype Recur #?(:clj [val]
                  :cljs [val])
  t/IBox
  (getVal [this] val))

#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn parse-fn-args+body
  [^clojure.lang.Associative ctx interpret eval-do*
   {:sci.impl/keys [fixed-arity var-arg-name ^clojure.lang.Indexed params body] :as _m}
   fn-name macro? with-meta?]
  (let [min-var-args-arity (when var-arg-name fixed-arity)
        body-count (count body)
        param-count (count params)
        return (if (= 1 body-count)
                 (let [fst (first body)]
                   #(interpret % fst))
                 #(eval-do* % body))
        f (fn run-fn [& args]
            (let [;; tried making bindings a transient, but saw no perf improvement (see #246)
                  bindings (.get ^java.util.Map ctx :bindings)
                  arg-count (count args)
                  ;;max-idx (dec arg-count)
                  args ^clojure.lang.Indexed (vec args)
                  bindings
                  (loop [idx 0
                         ret bindings]
                    (if (< idx param-count)
                      (let [fp (try (.nth params idx)
                                    (catch Exception _
                                      (throw-arity fn-name macro? args)))]
                        (if (= '& fp)
                          (assoc ret (try (.nth params (inc idx))
                                          (catch Exception _e
                                            (throw-arity fn-name macro? args)))
                                 (subvec args idx))
                          (recur (inc idx)
                                 (assoc ret fp (try (.nth args idx)
                                                    (catch Exception _e
                                                      (throw-arity fn-name macro? args)))))))
                      (do
                        (when (> arg-count idx)
                          (throw-arity fn-name macro? args))
                        ret)))
                  ctx (#?(:clj .assoc
                          :cljs -assoc) ctx :bindings bindings)
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
                ret)))]
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
                           (>= ^long arity ^long min-var-args-arity)))
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
            (vary-meta
             f
             #(assoc % :sci/macro macro?))
            f)]
    (reset! self-ref f)
    f))

;;;; Scratch

(comment
  )
