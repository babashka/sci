(ns sci.impl.fns
  (:refer-clojure :exclude [destructure])
  (:require [sci.impl.destructure :refer [destructure]]
            [clojure.walk :refer [postwalk]]))

(defn expand-fn-literal [expr]
  (let [state (volatile! {:max-fixed 0 :var-args? false})
        _ (postwalk (fn [elt]
                      (if (symbol? elt)
                        (if-let [[_ m] (re-matches #"^%(.*)" (name elt))]
                          (cond (empty? m)
                                (vswap! state update :max-fixed max 1)
                                (= "&" m)
                                (vswap! state assoc :var-args? true)
                                :else (let [n #?(:clj (Integer/parseInt m)
                                                 :cljs (js/parseInt m))]
                                        (vswap! state update :max-fixed max n)))
                          elt)
                        elt))
                    expr)
        {:keys [:max-fixed :var-args?]} @state
        fixed-names (map #(symbol (str "%" %)) (range 1 (inc max-fixed)))
        arg-list (vec (concat fixed-names (when var-args?
                                            ['& '%&])))]
    (list 'fn arg-list
          (if (pos? max-fixed)
            (list 'let '[% %1] expr)
            expr))))

(defn parse-fn-args+body [interpret ctx [binding-vector & body]]
  (let [fixed-args (take-while #(not= '& %) binding-vector)
        var-arg (second (drop-while #(not= '& %) binding-vector))
        fixed-arity (count fixed-args)
        min-var-args-arity (when var-arg fixed-arity)
        m (if min-var-args-arity
            {:sci/min-var-args-arity min-var-args-arity}
            {:sci/fixed-arity fixed-arity})]
    (with-meta
      (fn [& args]
        (if var-arg
          (when (< (count (take min-var-args-arity args))
                   min-var-args-arity)
            (throw (new #?(:clj Exception
                           :cljs js/Error) "Wrong number of arguments.")))
          (when-not (= (count (take (inc fixed-arity) args))
                       fixed-arity)
            (throw (new #?(:clj Exception
                           :cljs js/Error) "Wrong number of arguments."))))
        (let [fixed-names (repeatedly fixed-arity gensym)
              destructure-vec (vec (interleave binding-vector fixed-names))
              var-arg-name (when var-arg (gensym))
              destructure-vec (if var-arg
                                (conj destructure-vec var-arg var-arg-name)
                                destructure-vec)
              arg-bindings (apply hash-map (interleave fixed-names args))
              ctx (cond-> (update ctx :bindings merge arg-bindings)
                    var-arg
                    (assoc-in [:bindings var-arg-name] (drop fixed-arity args)))
              form (list* 'let (destructure destructure-vec)
                          body)]
          (interpret ctx form)))
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

(defn eval-fn [ctx interpret [_fn name? & body]]
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
        f (fn [& args]
            (let [arg-count (count args)]
              (if-let [f (lookup-by-arity arities arg-count)]
                (apply f args)
                (throw (new #?(:clj Exception
                               :cljs js/Error) (str "Cannot call " fn-name " with " arg-count " arguments."))))))]
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
