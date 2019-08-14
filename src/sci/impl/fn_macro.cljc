(ns sci.impl.fn-macro
  (:refer-clojure :exclude [destructure])
  (:require [sci.impl.destructure :refer [destructure]]))

(defn parse-fn-args+body [interpret bindings [binding-vector & body]]
  (let [fixed-args (take-while #(not= '& %) binding-vector)
        var-arg (second (drop-while #(not= '& %) binding-vector))
        fixed-arity (count fixed-args)
        min-varargs-arity (when var-arg fixed-arity)
        m (if min-varargs-arity
            {:sci/min-varargs-arity min-varargs-arity}
            {:sci/fixed-arity fixed-arity})]
    (with-meta
      (fn [& args]
        (when-not (= (count args) (count binding-vector))
          (throw (new #?(:clj Exception
                         :cljs js/Error) "Wrong number of arguments.")))
        (let [fixed-names (repeatedly fixed-arity gensym)
              destructure-vec (vec (interleave binding-vector fixed-names))
              var-arg-name (when var-arg (gensym))
              destructure-vec (if var-arg
                                (conj destructure-vec var-arg var-arg-name)
                                destructure-vec)
              arg-bindings (apply hash-map (interleave fixed-names args))
              bindings (cond-> (merge bindings arg-bindings)
                         var-arg
                         (assoc var-arg-name (drop fixed-arity args)))
              form (list* 'let (destructure destructure-vec)
                          body)]
          (interpret form
                     bindings)))
      m)))

(defn index-by [f coll]
  (zipmap (map f coll) coll))

(defn lookup-by-arity [arities arity]
  (some (fn [f]
          (let [{:keys [:sci/fixed-arity :sci/min-varargs-arity]} (meta f)]
            (when (or (= arity fixed-arity )
                      (and min-varargs-arity
                           (>= arity min-varargs-arity)))
              f))) arities))

(defn eval-fn [interpret bindings [_fn name? & body]]
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
        bindings (assoc bindings fn-name call-self)
        arities (map #(parse-fn-args+body interpret bindings %) bodies)
        f (fn [& args]
            (let [arg-count (count args)]
              (if-let [f (lookup-by-arity arities arg-count)]
                (apply f args)
                (throw (new #?(:clj Exception
                               :cljs js/Error) (str "Cannot call " fn-name " with " arg-count " arguments."))))))]
    (reset! self-ref f)
    f))
