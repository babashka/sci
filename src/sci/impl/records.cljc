(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record? deftype])
  (:require [clojure.string :as str]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

(defn deftype [form _ _ type-name fields & args]
  (let [dot-fn-sym (symbol (str type-name "."))
        factory-fn-sym (symbol (str "->" type-name))

        namespaced-type-sym (symbol (str (vars/current-ns-name)) (str type-name))

        {interfaces true methods false} (group-by symbol? args)
        methods (->> (group-by first methods)
                     (map (fn [[meth bodies]]
                            `['~meth (fn ~@(map rest bodies))]))
                     (into {}))]
    `(do
       (defn ~dot-fn-sym ~fields
         (vary-meta (clojure.core/reify* '~form
                                         ~interfaces
                                         ~methods)
                    assoc
                    :sci.impl/type true
                    :type '~namespaced-type-sym))
       (defn ~factory-fn-sym ~fields
         (~dot-fn-sym ~@fields))
       (def ~type-name (with-meta '~namespaced-type-sym
                                  {:sci.impl/type true
                                   :sci.impl.record/constructor ~dot-fn-sym})))))

(defn defrecord [_ _ ctx record-name fields & protocol-impls]
  (let [factory-fn-str (str "->" record-name)
        factory-fn-sym (symbol factory-fn-str)
        map-factory-sym (symbol (str "map" factory-fn-str))
        keys (mapv keyword fields)
        rec-type (symbol (str (vars/current-ns-name)) (str record-name))
        protocol-impls (utils/split-when symbol? protocol-impls)
        field-set (set fields)
        protocol-impls
        (mapcat
         (fn [[protocol-name & impls]]
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
                                               body (rest impl)
                                               destr (utils/maybe-destructured args body)
                                               args (:params destr)
                                               body (:body destr)
                                               this (first args)
                                               bindings
                                               (vec (mapcat (fn [field]
                                                              [field (list (keyword field) this)])
                                                            (reduce disj field-set args)))]
                                           `(~args
                                             (let ~bindings
                                               ~@body)))) bodies)]
                      `(defmethod ~(fq-meth-name (str method-name)) '~rec-type ~@bodies)))
                  impls)))
         protocol-impls)]
    `(do
       (defn ~map-factory-sym [m#]
         (vary-meta m#
                    assoc
                    :sci.impl/record true
                    :type '~rec-type))
       (defn ~factory-fn-sym [& args#]
         (vary-meta (zipmap ~keys args#)
                    assoc
                    :sci.impl/record true
                    :type '~rec-type))
       (def ~record-name (with-meta '~rec-type
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
