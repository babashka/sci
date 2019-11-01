(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.utils :refer [mark-eval-call]]))

(defn parse-fn-args+body
  [interpret ctx
   {:sci.impl/keys [fixed-arity fixed-names var-arg-name destructure-vec _arg-list body] :as _m}
   fn-name macro? single-arity?]
  (let [min-var-args-arity (when var-arg-name fixed-arity)
        m (if min-var-args-arity
            {:sci.impl/min-var-args-arity min-var-args-arity}
            {:sci.impl/fixed-arity fixed-arity})]
    (with-meta
      (fn [& args]
        (when single-arity?
          ;; no argument arity check has happened yet so we do it now
          (let [arity (count args)]
            (when-not (or (= arity fixed-arity)
                          (and min-var-args-arity
                               (>= arity min-var-args-arity)))
              (throw (new #?(:clj Exception
                             :cljs js/Error)
                          (let [actual-count (if macro? (- arity 2)
                                                 arity)]
                            (str "Cannot call " fn-name " with " actual-count " arguments")))))))
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
          (let [{:sci.impl/keys [fixed-arity min-var-args-arity]} (meta f)]
            (when (or (= arity fixed-arity )
                      (and min-var-args-arity
                           (>= arity min-var-args-arity)))
              f))) arities))

(defn eval-fn [ctx interpret {:sci.impl/keys [fn-bodies fn-name] :as f}]
  (let [macro? (:sci/macro f)
        self-ref (atom nil)
        call-self (fn [& args]
                    (apply @self-ref args))
        ctx (if fn-name (assoc-in ctx [:bindings fn-name] call-self)
                ctx)
        single-arity? (= 1 (count fn-bodies))
        arities (map #(parse-fn-args+body interpret ctx % fn-name macro? single-arity?) fn-bodies)
        f (vary-meta
           (if single-arity?
             (first arities)
             (fn [& args]
               (let [arg-count (count args)]
                 (if-let [f (lookup-by-arity arities arg-count)]
                   (apply f args)
                   (throw (new #?(:clj Exception
                                  :cljs js/Error)
                               (let [actual-count (if macro? (- arg-count 2)
                                                      arg-count)]
                                 (str "Cannot call " fn-name " with " actual-count " arguments"))))))))
           #(assoc % :sci/macro macro?))]
    (reset! self-ref f)
    f))

;;;; Scratch

(comment
  )
