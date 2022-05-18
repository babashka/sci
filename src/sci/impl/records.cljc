(ns sci.impl.records
  {:no-doc true}
  (:refer-clojure :exclude [defrecord record?])
  (:require [clojure.string :as str]
            [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

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
    (str (namespace t) "." (name t) "@"
         #?(:clj (Integer/toHexString (hash this))
            :cljs (.toString (hash this) 16)))))

(defn clojure-str [v]
  (let [t (types/type-impl v)]
    (str "#" (namespace t) "." (name t)
         (into {} v))))

(defprotocol SciPrintMethod
  (-sci-print-method [x w]))

;; #?(:cljs
;;    (clojure.core/defrecord SciRecord []
;;      Object
;;      (toString [this]
;;        (to-string this))
;;      SciPrintMethod
;;      (-sci-print-method [this w]
;;        (let [m (meta this)]
;;          (if-let [rv (:sci.impl/record-var m)]
;;            (let [m (meta @rv)]
;;              (if-let [pm (:sci.impl/print-method m)]
;;                (pm this w)
;;                (.write ^java.io.Writer w ^String (clojure-str this))))
;;            (.write ^java.io.Writer w ^String (clojure-str this)))))))

;; see https://gist.github.com/borkdude/19ac04ea0b2ef9d6643ba3de6817de57
;; TODO, port CLJS side too https://github.com/clojure/clojurescript/blob/9562ae11422243e0648a12c39e7c990ef3f94260/src/main/clojure/cljs/core.cljc#L1804
#?(:clj
   (deftype SciRecord [rec-name
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
       (or (meta ext-map)
           {:type rec-name}))
     (withMeta [_ m]
       (SciRecord.
        rec-name var (with-meta ext-map (assoc m :type rec-name)) 0 0))

     clojure.lang.ILookup
     (valAt [_this k]
       (.valAt ^clojure.lang.ILookup ext-map k))
     (valAt [_ k else]
       (.valAt ^clojure.lang.ILookup ext-map k else))

     ;; clojure.lang.IKeywordLookup
     ;; (getLookupThunk [_ k]
     ;;   (.getLookupThunk ^clojure.lang.ILookup ext-map k))

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
       (SciRecord. rec-name var (assoc ext-map k v) 0 0))
     (without [_this k]
       (SciRecord. rec-name var (dissoc ext-map k) 0 0))

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
         (let [m (meta @rv)]
           (if-let [pm (:sci.impl/print-method m)]
             (pm this w)
             (.write ^java.io.Writer w ^String (clojure-str this))))
         (.write ^java.io.Writer w ^String (clojure-str this))))))

#?(:cljs
   (deftype SciRecord [rec-name
                       var ext-map
                       ^:mutable my_hash]
     IRecord ;; marker interface

     ICloneable
     (-clone [_]
       (new SciRecord rec-name var ext-map my_hash))

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
            (= rec-name (.-rec-name other))
            (= (.-ext-map this) (.-ext-map other))))

     IMeta
     (-meta [_]
       (or (meta ext-map)
           {:type rec-name}))

     IWithMeta
     (-with-meta [_ m]
       (new SciRecord
            rec-name var (with-meta ext-map (assoc m :type rec-name)) my_hash))

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
       (new SciRecord rec-name var (assoc ext-map k v) nil))

     IMap
     (-dissoc [_ k]
       (new SciRecord rec-name var (dissoc ext-map k) nil))

     ISeqable
     (-seq [_]
       (-seq ext-map))

     IIterable
     (-iterator [_]
       (-iterator ext-map))

     IPrintWithWriter
     (-pr-writer [new-obj writer _]
       (write-all writer (clojure-str new-obj)))

     IKVReduce
     (-kv-reduce [this f init]
       (reduce (fn [ret [k v]]
                 (f ret k v)) init this))

     Object
     (toString [this]
       (to-string this))))

#?(:clj
   (defmethod print-method SciRecord [v w]
     (-sci-print-method v w)))

;; #?(:cljs ;; see https://www.mail-archive.com/clojure@googlegroups.com/msg99560.html
;;    (extend-type SciRecord
;;      IPrintWithWriter
;;      (-pr-writer [new-obj writer _]
;;        (write-all writer (clojure-str new-obj)))))

#?(:clj (defn ->record-impl [rec-name var m]
          (SciRecord. rec-name var m 0 0))
   :cljs (defn ->record-impl [rec-name var m]
           (SciRecord. rec-name var m nil)))

(defn assert-immutable-fields [fields expr]
  (run! (fn [field]
          (when-let [m (meta field)]
            (let [mutable? #?(:clj (or (:volatile-mutable m)
                                       (:unsynchronized-mutable m))
                              :cljs (:mutable m))]
              (when mutable?
                (utils/throw-error-with-location
                 "deftype currently does not support mutable fields yet"
                 expr)))))
        fields))

(defn defrecord [[fname & _ :as form] _ ctx record-name fields & raw-protocol-impls]
  (let [fname (name fname)
        deftype? (= "deftype" fname)]
    (when deftype?
      (assert-immutable-fields fields form))
    (if (:sci.impl/macroexpanding ctx)
      (cons 'clojure.core/defrecord (rest form))
      (let [factory-fn-str (str "->" record-name)
            factory-fn-sym (symbol factory-fn-str)
            map-factory-sym (when-not deftype?
                              (symbol (str "map" factory-fn-str)))
            keys (mapv keyword fields)
            rec-type (symbol (str (vars/current-ns-name)) (str record-name))
            protocol-impls (utils/split-when symbol? raw-protocol-impls)
            field-set (set fields)
            protocol-impls
            (mapcat
             (fn [[protocol-name & impls] #?(:clj expr :cljs expr)]
               (let [impls (group-by first impls)
                     protocol (@utils/eval-resolve-state ctx (:bindings ctx) protocol-name)
                     #?@(:cljs [protocol (or protocol
                                             (when (= 'Object protocol-name)
                                               ::object))])
                     _ (when-not protocol
                         (utils/throw-error-with-location
                          (str "Protocol not found: " protocol-name)
                          expr))
                     #?@(:clj [_ (assert-no-jvm-interface protocol protocol-name expr)])
                     protocol (if (vars/var? protocol) @protocol protocol)
                     protocol-ns (:ns protocol)
                     pns (cond protocol-ns (str (vars/getName protocol-ns))
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
                          `(defmethod ~(fq-meth-name method-name) '~rec-type ~@bodies)))
                      impls)))
             protocol-impls
             raw-protocol-impls)]
        `(do
           (declare ~record-name)
           ~(when-not deftype?
              `(defn ~map-factory-sym [m#]
                 (vary-meta (clojure.core/->record-impl '~rec-type (var ~record-name) m#)
                            assoc
                            ;; TODO: now that we're using the SciRecord type, we could move away from these metadata keys
                            :sci.impl/record true
                            :type '~rec-type)))
           (defn ~factory-fn-sym [& args#]
             (vary-meta (clojure.core/->record-impl '~rec-type (var ~record-name) (zipmap ~keys args#))
                        assoc
                        :sci.impl/record true
                        :type '~rec-type
                        :sci.impl/record-var ~(list 'var record-name)))
           (def ~record-name (with-meta '~rec-type
                               ~(cond-> {:sci.impl/record true
                                         :sci.impl.record/constructor factory-fn-sym
                                         :sci.impl/record-var (list 'var record-name)}
                                  (not deftype?)
                                  (assoc :sci.impl.record/map-constructor map-factory-sym))))
           ~@protocol-impls)))))

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
