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

(clojure.core/defrecord SciRecord []
  Object
  (toString [this]
    (to-string this))
  SciPrintMethod
  (-sci-print-method [this w]
    (let [m (meta this)]
      (if-let [rv (:sci.impl/record-var m)]
        (let [m (meta @rv)]
          (if-let [pm (:sci.impl/print-method m)]
            (pm this w)
            (.write ^java.io.Writer w ^String (clojure-str this))))
        (.write ^java.io.Writer w ^String (clojure-str this))))))

;; see https://gist.github.com/borkdude/19ac04ea0b2ef9d6643ba3de6817de57
#?(:clj
   (deftype SciRecord2 [rec-name
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
       (SciRecord2. rec-name var (with-meta ext-map m) nil nil))

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
            (when (identical? rec-name (.-rec-name ^SciRecord2 gs))
              (= ext-map (.-ext-map ^SciRecord2 gs))))))
     (containsKey [this k]
       (.containsKey ^clojure.lang.IPersistentMap ext-map k))
     (entryAt [this k]
       (.entryAt ^clojure.lang.IPersistentMap ext-map k))
     (seq [this] (.seq ^clojure.lang.IPersistentMap ext-map))
     (iterator [this]
       (clojure.lang.RT/iter ext-map))
     (assoc [this k v]
       (SciRecord2. rec-name var (assoc ext-map k v) nil nil))
     (without [this k]
       (SciRecord2. rec-name var (dissoc ext-map k) nil nil))

     java.util.Map
     java.io.Serializable
     (size [this]
       (.size ^java.util.Map ext-map))
     (isEmpty [this]
       (.isEmpty ^java.util.Map ext-map))
     (containsValue [this v]
       (.containsValue ^java.util.Map ext-map v))
     (get [this k]
       (.get ^java.util.Map ext-map k))
     (put [this k v]
       (throw (UnsupportedOperationException.)))
     (remove [this k]
       (throw (UnsupportedOperationException.)))
     (putAll [this m]
       (throw (UnsupportedOperationException.)))
     (clear [this]
       (throw (UnsupportedOperationException.)))
     (keySet [this]
       (.keySet ^java.util.Map ext-map))
     (values [this]
       (.values ^java.util.Map ext-map))
     (entrySet [this]
       (.entrySet ^java.util.Map ext-map))))

#?(:clj
   (defmethod print-method SciRecord [v w]
     (-sci-print-method v w)))

#?(:cljs ;; see https://www.mail-archive.com/clojure@googlegroups.com/msg99560.html
   (extend-type SciRecord
     IPrintWithWriter
     (-pr-writer [new-obj writer _]
       (write-all writer (clojure-str new-obj)))))

(defn ->record-impl [m]
  (map->SciRecord m))


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
                 (vary-meta (clojure.core/->record-impl m#)
                            assoc
                            ;; TODO: now that we're using the SciRecord type, we could move away from these metadata keys
                            :sci.impl/record true
                            :type '~rec-type)))
           (defn ~factory-fn-sym [& args#]
             (vary-meta (clojure.core/->record-impl (zipmap ~keys args#))
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
