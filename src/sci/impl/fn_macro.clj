(ns sci.impl.fn-macro
  (:refer-clojure :exclude [destructure])
  (:require [sci.impl.destructure :refer [destructure]]))

(defn parse-fn-args+body [interpret bindings [binding-vector & body]]
  (let [arg-count (count binding-vector)]
    ^{:sci/arity arg-count}
    (fn [& args]
      (when-not (= (count args) (count binding-vector))
        (throw (Exception. "Wrong number of arguments.")))
      (let [arg-count (count binding-vector)
            names (repeatedly arg-count gensym)
            destructure-vec (vec (interleave binding-vector names))
            arg-bindings (apply hash-map (interleave names args))
            bindings (merge bindings arg-bindings)
            form (list* 'let (destructure destructure-vec)
                        body)]
        (interpret form
                   bindings)))))

(defn index-by [f coll]
  (zipmap (map f coll) coll))

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
        indexed (index-by (comp :sci/arity meta) arities)
        f (fn [& args]
            (let [arg-count (count args)]
              (if-let [f (get indexed (count args))]
                (apply f args)
                (throw (Exception. (str "Cannot call " fn-name " with " arg-count " arguments."))))))]
    (reset! self-ref f)
    f))
