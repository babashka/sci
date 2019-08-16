(ns sci.impl.fns
  (:refer-clojure :exclude [destructure])
  (:require [sci.impl.destructure :refer [destructure]]))

(defn parse-fn-args+body [interpret ctx [binding-vector & body]]
  (let [fixed-args (take-while #(not= '& %) binding-vector)
        var-arg-name (second (drop-while #(not= '& %) binding-vector))
        fixed-arity (count fixed-args)
        min-var-args-arity (when var-arg-name fixed-arity)
        m (if min-var-args-arity
            {:sci/min-var-args-arity min-var-args-arity}
            {:sci/fixed-arity fixed-arity})]
    (with-meta
      (fn [& args]
        (if var-arg-name
          (when (< (count (take min-var-args-arity args))
                   min-var-args-arity)
            (throw (new #?(:clj Exception
                           :cljs js/Error) "Wrong number of arguments. (varargs)")))
          (when-not (= (count (take (inc fixed-arity) args))
                       fixed-arity)
            (throw (new #?(:clj Exception
                           :cljs js/Error) (str "Wrong number of arguments. Expected: " fixed-arity ", got: " (count args) ", " args)))))
        (let [destructure-vec (vec (interleave binding-vector args))
              destructure-vec (if var-arg-name
                                (conj destructure-vec var-arg-name (list 'quote (drop fixed-arity args)))
                                destructure-vec)
              ;; arg-bindings (apply hash-map (interleave fixed-args args))
              form (list* 'let (destructure destructure-vec)
                          body)]
          ;; (prn "FORM" form)
          (interpret ctx form)
          #_(last (map #(interpret ctx %) body))))
      m)))

(defn index-by [f coll]
  (zipmap (map f coll) coll))

(defn lookup-by-arity [arities arity]
  (some (fn [f]
          (let [{:keys [:sci/fixed-arity :sci/min-var-args-arity]} (meta f)]
            (when (or (= arity fixed-arity )
                      (and min-var-args-arity
                           (>= arity min-var-args-arity)))
              f))) arities))

(defn eval-fn [ctx interpret [_fn name? & body :as e]]
  (let [fn-name (if (symbol? name?)
                  name?
                  nil)
        body (if fn-name
               body
               (cons name? body))
        fn-name (or fn-name (gensym "fn"))
        bodies (if (seq? (first body))
                 body
                 [body])
        self-ref (atom nil)
        call-self (fn [& args]
                    (apply @self-ref args))
        ctx (assoc-in ctx [:bindings fn-name] call-self)
        arities (map #(parse-fn-args+body interpret ctx %) bodies)
        f (if (= 1 (count arities))
            (first arities)
            (fn [& args]
              (let [arg-count (count args)]
                (if-let [f (lookup-by-arity arities arg-count)]
                  (apply f args)
                  (throw (new #?(:clj Exception
                                 :cljs js/Error) (str "Cannot call " fn-name " with " arg-count " arguments.")))))))]
    (reset! self-ref f)
    (with-meta f
      {:sci/name fn-name
       ;; :sci/arities (map meta arities)
       })))

(defn eval-defn [ctx interpret [defn fn-name docstring? & body :as expr]]
  (let [docstring (when (string? docstring?) docstring?)
        expr (if docstring (list* defn fn-name body)
                 expr)
        f (eval-fn ctx interpret expr)
        fn-name (-> f meta :sci/name)
        f (if docstring (vary-meta f assoc :sci/doc docstring)
              f)]
    (swap! (:env ctx) assoc fn-name f)
    f))

;;;; Scratch

(comment
  c
  e
  )
