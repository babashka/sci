(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            [sci.lang]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn assert-no-jvm-interface [protocol protocol-name expr]
     (when (and (class? protocol)
                (not (= Object protocol)))
       (utils/throw-error-with-location
        (str "defrecord/deftype currently only support protocol implementations, found: " protocol-name)
        expr))))

(defmulti to-string types/type-impl)
(defmethod to-string :default [this]
  (let [t (types/type-impl this)]
    (str t "@"
         #?(:clj (Integer/toHexString (hash this))
            :cljs (.toString (hash this) 16)))))

(defn clojure-str [v]
  (let [t (types/type-impl v)]
    (str "#" t (into {} v))))

(defprotocol SciPrintMethod
  (-sci-print-method [x w]))

#?(:clj
   (deftype SciRecord [rec-name
                       type
                       basis-fields
                       type-meta
                       ext-map
                       ^:unsynchronized-mutable my_hash
                       ^:unsynchronized-mutable my_hasheq]
     clojure.lang.IRecord ;; marker interface

     clojure.lang.IHashEq
     (hasheq [_]
       (let [hq my_hasheq]
         (if (zero? hq)
           (let [type-hash (hash rec-name)
                 h (int (bit-xor type-hash (clojure.lang.APersistentMap/mapHasheq ext-map)))]
             (set! my_hasheq h)
             h)
           hq)))
     (hashCode [_]
       (let [hq my_hash]
         (if (zero? hq)
           (let [h (int (clojure.lang.APersistentMap/mapHash ext-map))]
             (set! my_hash h)
             h)
           hq)))
     (equals [this other]
       (clojure.lang.APersistentMap/mapEquals this other))

     clojure.lang.IObj
     (meta [_]
       (meta ext-map))
     (withMeta [_ m]
       (SciRecord. rec-name type basis-fields type-meta (with-meta ext-map m) 0 0))

     clojure.lang.ILookup
     (valAt [_this k]
       (.valAt ^clojure.lang.ILookup ext-map k))
     (valAt [_ k else]
       (.valAt ^clojure.lang.ILookup ext-map k else))

     clojure.lang.IPersistentMap
     (count [_]
       (.count ^clojure.lang.IPersistentMap ext-map))
     (empty [_]
       (throw (UnsupportedOperationException. (str "Can't create empty: " (str rec-name)))))
     (cons [this e]
       ((var clojure.core/imap-cons) this e))
     (equiv [this gs]
       (boolean
        (or (identical? this gs)
            (when (instance? SciRecord gs)
              (and (identical? rec-name (.-rec-name ^SciRecord gs))
                   (= ext-map (.-ext-map ^SciRecord gs)))))))
     (containsKey [_this k]
       (.containsKey ^clojure.lang.IPersistentMap ext-map k))
     (entryAt [_this k]
       (.entryAt ^clojure.lang.IPersistentMap ext-map k))
     (seq [_this] (.seq ^clojure.lang.IPersistentMap ext-map))
     (iterator [_this]
       (clojure.lang.RT/iter ext-map))
     (assoc [_this k v]
       (SciRecord. rec-name type basis-fields type-meta (assoc ext-map k v) 0 0))
     (without [_this k]
       (if (contains? basis-fields k)
         (dissoc ext-map k)
         (SciRecord. rec-name type basis-fields type-meta (dissoc ext-map k) 0 0)))

     java.util.Map
     java.io.Serializable
     (size [_this]
       (.size ^java.util.Map ext-map))
     (isEmpty [_this]
       (.isEmpty ^java.util.Map ext-map))
     (containsValue [_this v]
       (.containsValue ^java.util.Map ext-map v))
     (get [_this k]
       (.get ^java.util.Map ext-map k))
     (put [_this _k _v]
       (throw (UnsupportedOperationException.)))
     (remove [_this _k]
       (throw (UnsupportedOperationException.)))
     (putAll [_this _m]
       (throw (UnsupportedOperationException.)))
     (clear [_this]
       (throw (UnsupportedOperationException.)))
     (keySet [_this]
       (.keySet ^java.util.Map ext-map))
     (values [_this]
       (.values ^java.util.Map ext-map))
     (entrySet [_this]
       (.entrySet ^java.util.Map ext-map))

     Object
     (toString [this]
       (to-string this))

     SciPrintMethod
     (-sci-print-method [this w]
       (if-let [rv type-meta]
         (let [m (meta rv)]
           (if-let [pm (:sci.impl/print-method m)]
             (pm this w)
             (.write ^java.io.Writer w ^String (clojure-str this))))
         (.write ^java.io.Writer w ^String (clojure-str this))))

     sci.impl.types/SciTypeInstance
     (-get-type [_]
       type)))

;; See https://github.com/clojure/clojurescript/blob/9562ae11422243e0648a12c39e7c990ef3f94260/src/main/clojure/cljs/core.cljc#L1804
#?(:cljs
   (deftype SciRecord [rec-name
                       type
                       basis-fields
                       type-meta ext-map
                       ^:mutable my_hash]
     IRecord ;; marker interface

     ICloneable
     (-clone [_]
       (new SciRecord rec-name type basis-fields type-meta ext-map my_hash))

     IHash
     (-hash [_]
       (let [hq my_hash]
         (if (not (nil? hq))
           (let [type-hash (hash (-> rec-name munge str))
                 h (bit-xor type-hash (hash-unordered-coll ext-map))]
             (set! my_hash h)
             h)
           hq)))

     IEquiv
     (-equiv [this other]
       (and (some? other)
            (identical? (.-constructor this)
                        (.-constructor other))
            (= rec-name (.-rec-name ^SciRecord other))
            (= (.-ext-map this) (.-ext-map ^SciRecord other))))

     IMeta
     (-meta [_]
       (meta ext-map))

     IWithMeta
     (-with-meta [_ m]
       (new SciRecord
            rec-name type basis-fields type-meta (with-meta ext-map m) my_hash))

     ILookup
     (-lookup [_ k]
       (-lookup ext-map k))
     (-lookup [_ k else]
       (-lookup ext-map k else))

     ICounted
     (-count [_]
       (count ext-map))

     ICollection
     (-conj [this entry]
       (if (vector? entry)
         (-assoc this (-nth entry 0) (-nth entry 1))
         (reduce -conj
                 this
                 entry)))

     IAssociative
     (-contains-key? [_ k]
       (-contains-key? ext-map k))
     (-assoc [_ k v]
       (new SciRecord rec-name type basis-fields type-meta (assoc ext-map k v) nil))

     IMap
     (-dissoc [_ k]
       (if (contains? basis-fields k)
         (dissoc ext-map k)
         (new SciRecord rec-name type basis-fields type-meta (dissoc ext-map k) nil)))

     ISeqable
     (-seq [_]
       (-seq ext-map))

     IIterable
     (-iterator [_]
       (-iterator ext-map))

     IPrintWithWriter
     ;; see https://www.mail-archive.com/clojure@googlegroups.com/msg99560.html
     (-pr-writer [this w opts]
       (if-let [rv type-meta]
         (let [m (meta rv)]
           (if-let [pm (:sci.impl/print-method m)]
             (pm this w opts)
             (write-all w (clojure-str this))))
         (write-all w (clojure-str this))))

     IKVReduce
     (-kv-reduce [this f init]
       (reduce (fn [ret [k v]]
                 (f ret k v)) init this))

     sci.impl.types/SciTypeInstance
     (-get-type [_]
       type)

     Object
     (toString [this]
       (to-string this))))

#?(:clj
   (defmethod print-method SciRecord [v w]
     (-sci-print-method v w)))

#?(:clj  (defn ->record-impl [rec-name type basis-fields type-meta m]
           (SciRecord. rec-name type basis-fields type-meta m 0 0))
   :cljs (defn ->record-impl [rec-name type basis-fields type-meta m]
           (SciRecord. rec-name type basis-fields type-meta m nil)))

(defn defrecord-macro
  "Macro expansion for defrecord. Emits a (do (declare ...) (deftype* ...) (import ...))
   so that analyze-deftype* handles the actual code generation."
  [[_fname] _ record-name fields & raw-protocol-impls]
  (let [ns-name (utils/current-ns-name)
        tagged-name (symbol (str ns-name) (str record-name))
        class-name (symbol (str (munge ns-name) "." record-name))
        factory-fn-sym (symbol (str "->" record-name))
        constructor-fn-sym (symbol (str "__->" record-name "__ctor__"))
        map-factory-sym (symbol (str "map->" record-name))
        protocol-impls (utils/split-when symbol? raw-protocol-impls)
        interfaces (mapv first protocol-impls)
        method-counts (mapv #(count (rest %)) protocol-impls)
        methods (mapcat rest protocol-impls)]
    (list 'do
          (list 'declare factory-fn-sym constructor-fn-sym map-factory-sym)
          (list* 'deftype* tagged-name class-name fields
                 :implements (with-meta interfaces {:record true
                                                    :method-counts method-counts})
                 methods)
          (list 'import (list (symbol (str ns-name)) record-name)))))

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
                     (utils/current-ns-name))]
     (resolve-record-or-protocol-class ctx namespace (symbol class-name))))
  ([ctx package class]
   (let [namespace (-> package str (str/replace "_" "-") symbol)]
     (when-let [sci-var (let [ns (get-in @(:env ctx) [:namespaces namespace])]
                          (or (get (:types ns) class)
                              (get ns class)))]
       (if (utils/var? sci-var)
         @sci-var
         sci-var)))))

(defn resolve-record-class
  [ctx class-sym]
  (when-let [x (resolve-record-or-protocol-class ctx class-sym)]
    (when (instance? sci.lang.Type x) x)))
