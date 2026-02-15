(ns sci.impl.macroexpand
  {:no-doc true}
  (:refer-clojure :exclude [demunge var? macroexpand macroexpand-1])
  (:require [clojure.string :as str]
            [sci.ctx-store :as store]
            [sci.impl.interop :as interop]
            [sci.impl.resolve :as resolve]
            [sci.impl.utils :refer [kw-identical? var? macro? special-syms]]
            [sci.impl.vars :as vars]))

(defn macroexpand-1 [ctx expr]
  (let [ctx (assoc ctx :sci.impl/macroexpanding true)
        original-expr expr]
    (store/with-ctx ctx
      (if (seq? expr)
        (let [op (first expr)]
          (if (symbol? op)
            (cond (get special-syms op) expr
                  ;; (contains? #{'for} op) (analyze ctx expr)
                  (= 'clojure.core/defrecord op) expr
                  :else
                  (let [f (try (resolve/resolve-symbol ctx op true)
                               (catch #?(:clj Exception :cljs :default)
                                   _ ::unresolved))]
                    (if (kw-identical? ::unresolved f)
                      expr
                      (let [var? (var? f)
                            macro-var? (and var?
                                            (vars/isMacro f))
                            f (if macro-var? @f f)]
                        (if (or macro-var? (macro? f))
                          (apply f original-expr (:bindings ctx) (rest expr))
                          (if (str/starts-with? (str op) ".")
                            (let [target (second expr)
                                  target (if (and (symbol? target)
                                                  (interop/resolve-class ctx target))
                                           (list 'clojure.core/identity target)
                                           target)]
                              (list* '. target (symbol (subs (str op) 1)) (nnext expr)))
                            expr))))))
            expr))
        expr))))

(defn macroexpand
  [ctx form]
  (let [ex (macroexpand-1 ctx form)]
    (if (identical? ex form)
      form
      (macroexpand ctx ex))))
