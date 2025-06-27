(ns sci.lang
  (:require [clojure.string :as str]
            [sci.impl.types :as types]
            [sci.impl.vars :as vars]
            #?(:cljs [sci.impl.unrestrict :refer [*unrestricted*]]))
  (:refer-clojure :exclude [Var ->Var var? Namespace ->Namespace]))

#?(:clj (set! *warn-on-reflection* true))

;; marker interface for vars, clj only for now
#?(:clj (definterface ^{:doc "Marker interface for SCI vars."} IVar))

(defn- class-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s (inc i))
    s))

(defn- package-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s 0 i)
    s))

(deftype ^{:doc "Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly."}
    Type [^:volatile-mutable data
          ^:volatile-mutable namespace
          ^:volatile-mutable name]
  sci.impl.types/IBox
  (getVal [_] data)
  (setVal [_ v] (set! data v))
  Object
  (toString [_]
    (str (:sci.impl/type-name data)))

  ;; meta is only supported to get our implementation! keys out
  #?@(:clj
      [clojure.lang.IMeta
       (meta [_] data)]
      :cljs
      [IMeta
       (-meta [_] data)])

  ;; we need to support Named for `derive`
  #?@(:clj
      [clojure.lang.Named
       (getNamespace [this]
                     (if (nil? namespace)
                       (let [ns (package-name (str this))]
                         (set! namespace ns)
                         ns)
                       namespace))
       (getName [this]
                (if (nil? name)
                  (let [nom (class-name (str this))]
                    (set! name nom)
                    nom)
                  name))]
      :cljs
      [INamed
       (-namespace [this]
                   (if (nil? namespace)
                     (let [ns (package-name (str this))]
                       (set! namespace ns)
                       ns)
                     namespace))
       (-name [this]
              (if (nil? name)
                (let [nom (class-name (str this))]
                  (set! name nom)
                  nom)
                name))]))

#?(:clj (defmethod print-method Type [this w]
          (.write ^java.io.Writer w (str this))))

(defn- throw-root-binding [this]
  (throw (#?(:clj IllegalStateException. :cljs js/Error.)
                  (str "Can't change/establish root binding of " this " with set"))))

(defn notify-watches [ref watches old-val new-val]
  (when watches
    (when (pos? (count watches))
      (reduce-kv (fn [_ k f]
                   (f k ref old-val new-val)
                   nil)
                 nil
                 watches)))
  ref)

(deftype ^{:doc "Representation of a SCI var, created e.g. with `(defn foo [])`
    The fields of this type are implementation detail and should not be accessed
    directly."}
    Var [#?(:clj ^:volatile-mutable root
            :cljs ^:mutable root)
         sym
         #?(:clj ^:volatile-mutable meta
            :cljs ^:mutable meta)
         #?(:clj ^:volatile-mutable thread-bound
            :cljs ^:mutable thread-bound)
         #?(:clj ^:volatile-mutable needs-ctx
            :cljs ^:mutable needs-ctx)
         #?(:clj ^:volatile-mutable watches
            :cljs ^:mutable watches)]
  #?(:clj
     ;; marker interface, clj only for now
     sci.lang.IVar)
  types/HasName
  (getName [_this]
    (or (:name meta) sym))
  vars/IVar
  (bindRoot [this v]
    (let [old-root (.-root this)]
      (vars/with-writeable-var this meta
        (set! root v))
      (notify-watches this watches old-root v))
    ;; this is the return value for alter-var-root which should be the only place calling bindRoot directly
    v)
  (getRawRoot [_this]
    root)
  (toSymbol [_this]
    ;; if we have at least a name from metadata, then build the symbol from that
    (if-let [sym-name (some-> (:name meta) name)]
      (symbol (some-> (:ns meta) types/getName name) sym-name)
      ;; otherwise, fall back to the symbol
      sym))
  (isMacro [_]
    (or (:macro meta)
        (when-some [m (clojure.core/meta root)]
          (:sci/macro m))))
  (setThreadBound [this v]
    (set! (.-thread-bound this) v))
  (unbind [this]
    (vars/with-writeable-var this meta
      (set! (.-root this) (sci.impl.vars.SciUnbound. this))))
  (hasRoot [_this]
    (not (instance? sci.impl.vars.SciUnbound root)))
  vars/DynVar
  (dynamic? [_this]
    (:dynamic meta))
  types/IBox
  (setVal [this v]
    (if-let [b (vars/get-thread-binding this)]
      #?(:clj
         (let [t (.-thread b)]
           (if (not (identical? t (Thread/currentThread)))
             (throw (IllegalStateException.
                     (format "Can't set!: %s from non-binding thread" (vars/toSymbol this))))
             (types/setVal b v)))
         :cljs (types/setVal b v))
      #?(:clj (throw-root-binding this)
         :cljs (if *unrestricted*
                 (set! (.-root this) v)
                 (throw-root-binding this)))))
  (getVal [_this] root)
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref
      :cljs -deref) [this]
    (if thread-bound
      (if-let [tbox (vars/get-thread-binding this)]
        (types/getVal tbox)
        root)
      root))
  Object
  (toString [this]
    (str "#'" (vars/toSymbol this)))
  #?(:cljs IPrintWithWriter)
  #?(:cljs (-pr-writer [a writer opts]
                       (-write writer "#'")
                       (-pr-writer (vars/toSymbol a) writer opts)))
  #?(:clj clojure.lang.IMeta :cljs IMeta)
  #?(:clj (clojure.core/meta [_] meta) :cljs (-meta [_] meta))
  ;; #?(:clj Comparable :cljs IEquiv)
  ;; (-equiv [this other]
  ;;   (if (instance? Var other)
  ;;     (= (.-sym this) (.-sym other))
  ;;     false))
  ;; #?(:clj clojure.lang.IHashEq :cljs IHash)
  ;; (-hash [_]
  ;;   (hash-symbol sym))
  #?(:clj clojure.lang.IReference)
  #?(:clj (alterMeta [this f args]
                     (vars/with-writeable-var this meta
                       (locking this (set! meta (apply f meta args))))))
  #?(:clj (resetMeta [this m]
                     (vars/with-writeable-var this meta
                       (locking this (set! meta m)))))
  #?@(:clj [clojure.lang.IRef
            (addWatch [this key fn]
                      (vars/with-writeable-var this meta
                        (set! watches (assoc watches key fn)))
                      this)
            (removeWatch [this key]
                         (vars/with-writeable-var this meta
                           (set! watches (dissoc watches key)))
                         this)]
      :cljs [IWatchable
            (-add-watch [this key fn]
                        (vars/with-writeable-var this meta
                          (set! watches (assoc watches key fn)))
                        this)
            (-remove-watch [this key]
                           (vars/with-writeable-var this meta
                             (set! watches (dissoc watches key)))
                           this)])
  ;; #?(:cljs Fn) ;; In the real CLJS this is there... why?
  #?(:clj clojure.lang.IFn :cljs IFn)
  (#?(:clj invoke :cljs -invoke) [this]
    (@this))
  (#?(:clj invoke :cljs -invoke) [this a]
    (@this a))
  (#?(:clj invoke :cljs -invoke) [this a b]
    (@this a b))
  (#?(:clj invoke :cljs -invoke) [this a b c]
    (@this a b c))
  (#?(:clj invoke :cljs -invoke) [this a b c d]
    (@this a b c d))
  (#?(:clj invoke :cljs -invoke) [this a b c d e]
    (@this a b c d e))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f]
    (@this a b c d e f))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g]
    (@this a b c d e f g))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h]
    (@this a b c d e f g h))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i]
    (@this a b c d e f g h i))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j]
    (@this a b c d e f g h i j))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k]
    (@this a b c d e f g h i j k))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l]
    (@this a b c d e f g h i j k l))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m]
    (@this a b c d e f g h i j k l m))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n]
    (@this a b c d e f g h i j k l m n))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o]
    (@this a b c d e f g h i j k l m n o))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p]
    (@this a b c d e f g h i j k l m n o p))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q]
    (@this a b c d e f g h i j k l m n o p q))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r]
    (@this a b c d e f g h i j k l m n o p q r))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r s]
    (@this a b c d e f g h i j k l m n o p q r s))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r s t]
    (@this a b c d e f g h i j k l m n o p q r s t))
  (#?(:clj invoke :cljs -invoke) [this a b c d e f g h i j k l m n o p q r s t rest]
    (apply @this a b c d e f g h i j k l m n o p q r s t rest))
  #?(:clj
     (applyTo [this args]
              (apply @this args))))

#?(:clj
   ;; Use public interface for print-method so it can be overriden in bb itself
   (do (defmethod print-method sci.lang.IVar [o ^java.io.Writer w]
         (.write w (str "#'" (vars/toSymbol ^sci.impl.vars.IVar o))))
       (prefer-method print-method sci.lang.IVar clojure.lang.IDeref)))

(deftype
    ^{:doc
      "Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly."}
    Namespace [name #?(:clj ^:volatile-mutable meta
                       :cljs ^:mutable meta)]
  Object
  (toString [_]
    (str name))
  types/HasName
  (getName [_] name)
  #?(:clj clojure.lang.IMeta :cljs IMeta)
  #?(:clj (clojure.core/meta [_] meta) :cljs (-meta [_] meta))
  #?(:clj clojure.lang.IReference)
  #?(:clj (alterMeta [this f args]
                     (vars/with-writeable-namespace this meta
                       (locking this (set! meta (apply f meta args))))))
  #?(:clj (resetMeta [this m]
                     (vars/with-writeable-namespace this meta
                       (locking this (set! meta m))))))
