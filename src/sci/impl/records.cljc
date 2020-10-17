(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn defrecord [_ _ ctx record-name fields & protocol-impls]
  (let [factory-fn-str (str "->" record-name)
        factory-fn-sym (symbol factory-fn-str)
        map-factory-sym (symbol (str "map" factory-fn-str))
        keys (mapv keyword fields)
        protocol-impls (utils/split-when symbol? protocol-impls)
        protocol-impls
        (mapcat (fn [[protocol-name & impls]]
                  (let [impls (group-by first impls)
                        protocol (@utils/eval-resolve-state ctx protocol-name)
                        protocol (if (vars/var? protocol) @protocol protocol)
                        protocol-ns (:ns protocol)
                        pns (str (vars/getName protocol-ns))
                        fq-meth-name #(symbol pns %)]
                    (map (fn [[method-name bodies]]
                           (let [bodies (map rest bodies)
                                 bodies (mapv (fn [impl]
                                                (let [args (first impl)
                                                      this (first args)
                                                      bindings (vec (mapcat (fn [field]
                                                                              [field (list (keyword field) this)])
                                                                            fields))]
                                                  `(~args
                                                    (let ~bindings
                                                      ~@(next impl))))) bodies)]
                             `(defmethod ~(fq-meth-name (str method-name)) '~record-name ~@bodies)))
                         impls)))
                protocol-impls)]
    `(do
       (defn ~factory-fn-sym [& args#]
         (vary-meta (zipmap ~keys args#)
                    assoc
                    :sci.impl/record true
                    :sci.impl/type '~record-name))
       (defn ~map-factory-sym [m#]
         (vary-meta m#
                    assoc
                    :sci.impl/record true
                    :sci.impl/type '~record-name))
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

(defn resolve-record-or-protocol-class
  "A record class is represented by a symbol with metadata (currently). This is only an implementation detail.
   A protocol is represented by a map with :ns, :methods and optionally :class. This is also an implementation detail."
  ;; TODO: we should probably use munging here for namespaces with hyphens in them.
  ([ctx sym]
   (let [sym-str (str sym)
         last-dot (str/last-index-of sym-str ".")
         class-name (if last-dot
                      (subs sym-str (inc last-dot) (count sym-str))
                      sym-str)
         namespace (if last-dot
                     (symbol (subs sym-str 0 last-dot))
                     (vars/current-ns-name))]
     (resolve-record-or-protocol-class ctx namespace (symbol class-name))))
  ([ctx package class]
   (let [namespace (-> package str (str/replace "_" "-") symbol)]
     (when-let [sci-var (get-in @(:env ctx) [:namespaces namespace class])]
       (if (vars/var? sci-var)
         @sci-var
         sci-var)))))

(defn resolve-record-class
  [ctx class-sym]
  (when-let [x (resolve-record-or-protocol-class ctx class-sym)]
    (when (symbol? x) x)))
