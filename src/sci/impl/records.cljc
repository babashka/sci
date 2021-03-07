(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]
            [sci.impl.types :as types]))

(defn -defrecord [form _ _ctx & args]
  (let [{classes true methods false} (group-by symbol? args)
        methods (->> (group-by first methods)
                     (map (fn [[meth bodies]]
                            `['~meth (fn ~@(map rest bodies))]))
                     (into {}))]
    `(clojure.core/defrecord* ~(vec classes) ~methods)))

(defn -defrecord*
  #?(:clj [ctx classes methods]
     :cljs [_ctx classes methods])
     #?(:clj (let [{interfaces true protocols false} (group-by class? classes)]
            (if-let [factory (:record-fn ctx)]
              (factory {:interfaces (set interfaces)
                        :methods methods
                        :protocols (set protocols)})
              (throw (ex-info (str "No reify factory for: " interfaces)
                              {:class class}))))
     ;; NOTE: in CLJS everything is a protocol in reify, except Object
     ;; So it's probably better if we dissoc-ed that from the set of classes
     ;; However, we only use that set to test in satisfies?
     :cljs (types/->Reified classes methods (set classes))))

(clojure.core/defrecord SciRecord []
  sci.impl.types.IReified
  (getInterfaces [this]
    (:sci.impl/interfaces (meta this)))
  (getMethods [this]
    (:sci.impl/methods (meta this)))
  (getProtocols [this]
    (:sci.impl/protocols (meta this)))
  clojure.lang.IFn
  (invoke [this a b]
    ((get (types/getMethods this) 'invoke) this a b)))

(defn ->>SciRecord [m interfaces protocols methods]
  (vary-meta (into (->SciRecord) m)
             assoc
             :sci.impl/interfaces interfaces
             :sci.impl/protocols protocols
             :sci.impl/methods methods))

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
