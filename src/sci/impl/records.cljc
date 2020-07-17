(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn defrecord [_ _ ctx record-name fields & protocol-impls]
  (let [factory-fn-sym (symbol (str "->" record-name))
        keys (mapv keyword fields)
        protocol-impls (utils/split-when symbol? protocol-impls)
        protocol-impls
        (mapcat (fn [[protocol-name & impls]]
                  (map (fn [impl]
                         (let [protocol-var (@utils/eval-resolve-state ctx protocol-name)
                               protocol-ns (-> protocol-var deref :ns)
                               pns (str (vars/getName protocol-ns))
                               fq-meth-name #(symbol pns %)
                               args (second impl)
                               this (first args)
                               bindings (vec (mapcat (fn [field]
                                                       [field (list (keyword field) this)])
                                                     fields))]
                           `(defmethod ~(fq-meth-name (str (first impl))) '~record-name ~(second impl)
                              (let ~bindings
                                ~@(nnext impl)))))
                       impls))
                protocol-impls)]
    `(do
       ;; (prn '~record-name)
       (defn ~factory-fn-sym [& args#]
         (vary-meta (zipmap ~keys args#)
                    assoc
                    :sci.impl/record true
                    :sci.impl/type '~record-name))
       (def ~record-name (with-meta '~record-name
                           {:sci.impl/record true
                            :sci.impl.record/constructor ~factory-fn-sym}))
       ~@protocol-impls)))

(defn sci-record? [x]
  (or
   (when (map? x)
     (some-> x meta :sci.impl/record))
   (clojure.core/record? x)))

(defn resolve-record-class
  ([ctx sym]
   (let [sym-str (str sym)
         last-dot (str/last-index-of sym-str ".")
         class-name (if last-dot
                      (subs sym-str (inc last-dot) (count sym-str))
                      sym-str)
         namespace (if last-dot
                     (symbol (subs sym-str 0 last-dot))
                     (vars/current-ns-name))]
     (resolve-record-class ctx namespace (symbol class-name))))
  ([ctx package class]
   (let [namespace package]
     (when-let [sci-var (get-in @(:env ctx) [:namespaces namespace class])]
       (if (vars/var? sci-var)
         @sci-var
         sci-var)))))
