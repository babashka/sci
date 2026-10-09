(ns sci.impl.macroexpand
  {:no-doc true}
  (:refer-clojure :exclude [demunge var? macroexpand macroexpand-1])
  (:require [clojure.string :as str]
            [sci.ctx-store :as store]
            [sci.impl.interop :as interop]
            [sci.impl.resolve :as resolve]
            [sci.impl.utils :refer [kw-identical? var? macro? special-syms]]
            [sci.impl.vars :as vars]))

#?(:clj
   (defn- expand-import [import-symbols-or-lists]
     (let [specs (map #(if (and (seq? %) (= 'quote (first %))) (second %) %)
                      import-symbols-or-lists)]
       (cons 'do
             (map #(list 'clojure.core/import* %)
                  (reduce (fn [v spec]
                            (if (symbol? spec)
                              (conj v (name spec))
                              (let [p (first spec) cs (rest spec)]
                                (into v (map #(str p "." %) cs)))))
                          [] specs))))))

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
                  (let [sname (str op)]
                    (if (and (str/ends-with? sname ".")
                             (not (str/starts-with? sname ".")))
                      ;; ClassName. constructor sugar -> (new ClassName args...)
                      (list* 'new (symbol (subs sname 0 (dec (count sname)))) (rest expr))
                      (let [f (try (resolve/resolve-symbol ctx op true)
                                   (catch #?(:cljd Object :clj Exception :cljs :default)
                                       _ ::unresolved))]
                        (if (kw-identical? ::unresolved f)
                          expr
                          (let [var? (var? f)
                                macro-var? (and var?
                                                (vars/isMacro f))
                                f (if macro-var? @f f)]
                            (cond
                              (or macro-var? (macro? f))
                              (apply f original-expr (:bindings ctx) (rest expr))
                              #?@(:clj [(= 'import f)
                                        (expand-import (rest expr))])
                              :else
                              (if (str/starts-with? sname ".")
                                (let [target (second expr)
                                      target (if (and (symbol? target)
                                                      (interop/resolve-class ctx target))
                                               (list 'clojure.core/identity target)
                                               target)]
                                  (list* '. target (symbol (subs sname 1)) (nnext expr)))
                                expr))))))))
            (if (and (var? op) (vars/isMacro op))
              (apply @op original-expr (:bindings ctx) (rest expr))
              expr)))
        expr))))

(defn macroexpand
  [ctx form]
  (let [ex (macroexpand-1 ctx form)]
    (if (identical? ex form)
      form
      (macroexpand ctx ex))))