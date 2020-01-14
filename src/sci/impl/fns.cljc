(ns sci.impl.fns
  {:no-doc true}
  (:require [sci.impl.utils :refer [mark-eval-call]]
            [sci.impl.types :as t]))

(defn throw-arity [fn-name macro? args]
  (throw (new #?(:clj Exception
                 :cljs js/Error)
              (let [actual-count (if macro? (- (count args) 2)
                                     (count args))]
                (str "Cannot call " fn-name " with " actual-count " arguments")))))

(deftype Recur #?(:clj [^:volatile-mutable val]
                 :cljs [^:mutable val])
  t/IBox
  (setVal [this v]
    (set! val v))
  (getVal [this] val))

(def recur-val (Recur. nil))

(defn parse-fn-args+body
  [interpret ctx
   {:sci.impl/keys [fixed-arity var-arg-name params body] :as _m}
   fn-name macro?]
  (let [min-var-args-arity (when var-arg-name fixed-arity)
        m (if min-var-args-arity
            {:sci.impl/min-var-args-arity min-var-args-arity}
            {:sci.impl/fixed-arity fixed-arity})]
    (with-meta
      (fn [& args]
        (let [runtime-bindings
              (loop [args (seq args)
                     params (seq params)
                     ret {}]
                (if params
                  (do
                    (when-not args
                      (throw-arity fn-name macro? args))
                    (let [fp (first params)]
                      (if (= '& fp)
                        (assoc ret (second params) args)
                        (recur (next args) (next params)
                               (assoc ret fp (first args))))))
                  (do
                    (when args
                      (throw-arity fn-name macro? args))
                    ret)))
              ctx (update ctx :bindings merge runtime-bindings)
              ret (if (= 1 (count body))
                    (interpret ctx (first body))
                    (interpret ctx (mark-eval-call `(do ~@body))))
              ;; m (meta ret)
              recur? (instance? Recur ret)]
          (if recur? (recur (t/getVal ret)) ret)))
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
        arities (map #(parse-fn-args+body interpret ctx % fn-name macro?) fn-bodies)
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
