(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.utils :refer [mark-eval-call]]))

(defn parse-fn-args+body
  [interpret ctx
   {:sci/keys [fixed-arity fixed-names var-arg-name destructure-vec _arg-list body] :as _m} macro?]
  (let [;; _ (prn macro?)
        min-var-args-arity (when var-arg-name fixed-arity)
        m (if min-var-args-arity
            {:sci/min-var-args-arity min-var-args-arity}
            {:sci/fixed-arity fixed-arity})]
    (with-meta
      (fn [& args]
        (let [runtime-bindings (vec (interleave fixed-names (take fixed-arity args)))
              runtime-bindings (if var-arg-name
                                 (conj runtime-bindings var-arg-name
                                       (drop fixed-arity args))
                                 runtime-bindings)
              let-bindings (into runtime-bindings destructure-vec)
              form (list* 'let let-bindings body)
              ret (interpret ctx (mark-eval-call form))
              m (meta ret)
              recur? (:sci.impl/recur m)]
          (if recur? (recur ret) ret)))
      m)))

(defn lookup-by-arity [arities arity]
  (some (fn [f]
          (let [{:keys [:sci/fixed-arity :sci/min-var-args-arity]} (meta f)]
            (when (or (= arity fixed-arity )
                      (and min-var-args-arity
                           (>= arity min-var-args-arity)))
              f))) arities))

(defn eval-fn [ctx interpret {:sci/keys [fn-bodies fn-name] :as f}]
  (let [macro? (:sci/macro f)
        self-ref (atom nil)
        call-self (fn [& args]
                    (apply @self-ref args))
        ctx (if fn-name (assoc-in ctx [:bindings fn-name] call-self)
                ctx)
        arities (map #(parse-fn-args+body interpret ctx % macro?) fn-bodies)
        f (vary-meta
           (fn [& args]
             (let [arg-count (count args)]
               (if-let [f (lookup-by-arity arities arg-count)]
                 (apply f args)
                 (throw (new #?(:clj Exception
                                :cljs js/Error)
                             (let [actual-count (if macro? (- arg-count 2)
                                                    arg-count)]
                               (str "Cannot call " fn-name " with " actual-count " arguments")))))))
           #(assoc % :sci/macro macro?))]
    (reset! self-ref f)
    f))

;;;; Scratch

(comment
  )
