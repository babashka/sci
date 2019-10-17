(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.utils :refer [mark-eval-call]]))

(defn parse-fn-args+body
  [interpret ctx
   {:sci/keys [fixed-arity fixed-names var-arg-name destructure-vec arg-list body] :as _m}]
  (let [;; _ (prn "M" _m)
        min-var-args-arity (when var-arg-name fixed-arity)
        m (if min-var-args-arity
            {:sci/min-var-args-arity min-var-args-arity}
            {:sci/fixed-arity fixed-arity})]
    (if #?(:cljs false :clj (and (not var-arg-name) fixed-arity (< fixed-arity 3)))
      ;; small optimization for fns with 0-2 args
      (case fixed-arity
        0 (with-meta (fn [] (interpret ctx (mark-eval-call (cons 'do body)))) m)
        1 (with-meta (fn [x]
                       (interpret ctx (mark-eval-call
                                       `(~'let [~(first arg-list) ~x]
                                         ~@body)))) m)
        2 (with-meta (let [[a1 a2] arg-list]
                       (fn [x y] (interpret ctx
                                            (mark-eval-call
                                             `(~'let [~a1 ~x
                                                      ~a2 ~y]
                                               ~@body)))))
            m))
      (with-meta
        (fn [& args]
          (if var-arg-name
            (when (< (count (take min-var-args-arity args))
                     min-var-args-arity)
              (throw (new #?(:clj Exception
                             :cljs js/Error)
                          (str "Wrong number of arguments. Expected at least: " min-var-args-arity ", got: " (count args)))))
            (when-not (= (count (take (inc fixed-arity) args))
                         fixed-arity)
              (throw (new #?(:clj Exception
                             :cljs js/Error) (str "Wrong number of arguments. Expected: " fixed-arity ", got: " (count args) ", " args)))))
          (let [runtime-bindings (vec (interleave fixed-names (take fixed-arity args)))
                runtime-bindings (if var-arg-name
                                   (conj runtime-bindings var-arg-name
                                         (drop fixed-arity args))
                                   runtime-bindings)
                let-bindings (into runtime-bindings destructure-vec)
                form (list* 'let let-bindings body)]
            ;; (prn "form" form)
            (interpret ctx (mark-eval-call form))))
        m))))

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
        arities (map #(parse-fn-args+body interpret ctx %) fn-bodies)
        f (vary-meta
            (if (= 1 (count arities))
              (first arities)
              (fn [& args]
                (let [arg-count (count args)]
                  (if-let [f (lookup-by-arity arities arg-count)]
                    (apply f args)
                    (throw (new #?(:clj Exception
                                   :cljs js/Error) (str "Cannot call " fn-name " with " arg-count " arguments.")))))))
            #(assoc %
                    :sci.impl/fn true
                    :sci/macro macro?))]
    (reset! self-ref f)
    f))

;;;; Scratch

(comment
  )
