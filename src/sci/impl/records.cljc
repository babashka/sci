(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.ctx-store :as store]
            [sci.impl.protocols :as protocols]
            [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]
            [sci.lang]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn assert-no-jvm-interface [protocol protocol-name expr]
     (when (and (class? protocol)
                (not (= Object protocol)))
       (utils/throw-error-with-location
        (str "defrecord/deftypecurrently only support protocol implementations, found: " protocol-name)
        expr))))

(defmulti to-string types/type-impl)
(defmethod to-string :default [this]
  (let [t (types/type-impl this)]
    (str (namespace t) "." (name t) "@"
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
                       var ext-map
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
       (SciRecord.
        rec-name type var (with-meta ext-map m) 0 0))

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
       (SciRecord. rec-name type var (assoc ext-map k v) 0 0))
     (without [_this k]
       (SciRecord. rec-name type var (dissoc ext-map k) 0 0))

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
       (if-let [rv var]
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
                       var ext-map
                       ^:mutable my_hash]
     IRecord ;; marker interface

     ICloneable
     (-clone [_]
       (new SciRecord rec-name type var ext-map my_hash))

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
            rec-name type var (with-meta ext-map m) my_hash))

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
       (new SciRecord rec-name type var (assoc ext-map k v) nil))

     IMap
     (-dissoc [_ k]
       (new SciRecord rec-name type var (dissoc ext-map k) nil))

     ISeqable
     (-seq [_]
       (-seq ext-map))

     IIterable
     (-iterator [_]
       (-iterator ext-map))

     IPrintWithWriter
     ;; see https://www.mail-archive.com/clojure@googlegroups.com/msg99560.html
     (-pr-writer [this w opts]
       (if-let [rv var]
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

#?(:clj (defn ->record-impl [rec-name type var m]
          (SciRecord. rec-name type var m 0 0))
   :cljs (defn ->record-impl [rec-name type var m]
           (SciRecord. rec-name type var m nil)))

(defn defrecord [[_fname & _ :as form] _ record-name fields & raw-protocol-impls]
  (let [ctx (store/get-ctx)]
    (if (:sci.impl/macroexpanding ctx)
      (cons 'clojure.core/defrecord (rest form))
      (let [factory-fn-str (str "->" record-name)
            factory-fn-sym (symbol factory-fn-str)
            constructor-fn-sym (symbol (str "__" factory-fn-str "__ctor__"))
            map-factory-sym (symbol (str "map" factory-fn-str))
            keys (mapv keyword fields)
            rec-type (symbol (str (munge (utils/current-ns-name)) "." record-name))
            protocol-impls (utils/split-when symbol? raw-protocol-impls)
            field-set (set fields)
            protocol-impls
            (mapcat
             (fn [[protocol-name & impls] #?(:clj expr :cljs expr)]
               (let [impls (group-by first impls)
                     protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                     ;; _ (prn :protocol protocol)
                     #?@(:cljs [protocol (or protocol
                                             (when (= 'Object protocol-name)
                                               ::object))])
                     _ (when-not protocol
                         (utils/throw-error-with-location
                          (str "Protocol not found: " protocol-name)
                          expr))
                     #?@(:clj [_ (assert-no-jvm-interface protocol protocol-name expr)])
                     protocol (if (utils/var? protocol) @protocol protocol)
                     protocol-var (:var protocol)
                     _ (when protocol-var
                         ;; TODO: not all externally defined protocols might have the :var already
                         (vars/alter-var-root protocol-var update :satisfies
                                              (fnil conj #{}) (protocols/type->str rec-type)))
                     protocol-ns (:ns protocol)
                     pns (cond protocol-ns (str (types/getName protocol-ns))
                               (= #?(:clj Object :cljs ::object) protocol) "sci.impl.records")
                     fq-meth-name #(if (simple-symbol? %)
                                     (symbol pns (str %))
                                     %)]
                 (map (fn [[method-name bodies]]
                        (let [bodies (map rest bodies)
                              bodies (mapv (fn [impl]
                                             (let [args (first impl)
                                                   body (rest impl)
                                                   destr (utils/maybe-destructured args body)
                                                   args (:params destr)
                                                   body (:body destr)
                                                   orig-this-sym (first args)
                                                   rest-args (rest args)
                                                   shadows-this? (some #(= orig-this-sym %) rest-args)
                                                   this-sym (if shadows-this?
                                                              (gensym "this_")
                                                              orig-this-sym)
                                                   args (if shadows-this?
                                                          (vec (cons this-sym rest-args))
                                                          args)
                                                   bindings (mapcat (fn [field]
                                                                      [field (list (keyword field) this-sym)])
                                                                    (reduce disj field-set args))
                                                   bindings (if shadows-this?
                                                              (concat bindings [orig-this-sym this-sym])
                                                              bindings)
                                                   bindings (vec bindings)]
                                               `(~args
                                                 (let ~bindings
                                                   ~@body)))) bodies)]
                          `(defmethod ~(fq-meth-name method-name) ~rec-type ~@bodies)))
                      impls)))
             protocol-impls
             raw-protocol-impls)
            arg-syms (mapv #(symbol (name %)) keys)
            nil-map (zipmap (map keyword field-set) (repeat nil))]
        `(do
           (declare ~record-name ~factory-fn-sym ~constructor-fn-sym ~map-factory-sym)
           (def ~(with-meta record-name
                   {:sci/record true})
             (sci.impl.records/-create-record-type
              ~{:sci.impl/type-name (list 'quote rec-type)
                :sci.impl/record true
                :sci.impl/constructor (list 'var constructor-fn-sym)
                :sci.impl/var (list 'var record-name)
                :sci.impl.record/map-constructor (list 'var map-factory-sym)}))
           (defn ~constructor-fn-sym
             ([~@arg-syms]
              (~constructor-fn-sym ~@arg-syms nil nil))
             ([~@arg-syms meta# ext#]
              (sci.impl.records/->record-impl '~rec-type ~rec-type (var ~record-name)
                                              (cond-> (zipmap ~keys ~arg-syms)
                                                ext# (merge ext#)
                                                meta# (with-meta meta#)))))
           (defn ~factory-fn-sym
             ([~@arg-syms]
              (~constructor-fn-sym ~@arg-syms nil nil)))
           (defn ~map-factory-sym [m#]
             (sci.impl.records/->record-impl '~rec-type ~rec-type (var ~record-name) (merge '~nil-map m#)))
           ~@protocol-impls
           ~record-name)))))

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
                          (or (get ns class)
                              (get (:refers ns) class)))]
       (if (utils/var? sci-var)
         @sci-var
         sci-var)))))

(defn resolve-record-class
  [ctx class-sym]
  (when-let [x (resolve-record-or-protocol-class ctx class-sym)]
    (when (instance? sci.lang.Type x) x)))
