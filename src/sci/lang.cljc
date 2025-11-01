(ns sci.lang
  (:require [clojure.string :as str]
            [sci.impl.types :as types]
            [sci.impl.vars :as vars]
            #?(:cljs [sci.impl.unrestrict :refer [*unrestricted*]]))
  (:refer-clojure :exclude [Var ->Var var? Namespace ->Namespace])
  #?(:cljr (:import [System.Threading Thread])))

#?(:cljs nil :default (set! *warn-on-reflection* true))

;; marker interface for vars, clj{r} only for now
#?(:cljs nil :default (definterface ^{:doc "Marker interface for SCI vars."} IVar))

(defn- class-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s (inc i))
    s))

(defn- package-name [s]
  (if-let [i (str/last-index-of s ".")]
    (subs s 0 i)
    s))

(deftype ^{:doc "Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly."}
  #?(:cljr SciCustomType ;;Type is System.Type in ClojureCLR
     :default Type)
  [^:volatile-mutable data
   ^:volatile-mutable namespace
   ^:volatile-mutable name]
  sci.impl.types/IBox
  (getVal [_] data)
  (setVal [_ v] (set! data v))
  Object
  (#?(:cljr ToString :default toString) [_]
    (str (:sci.impl/type-name data)))

  ;; meta is only supported to get our implementation! keys out
  #?@(:cljs
      [IMeta
       (-meta [_] data)]
      :default
      [clojure.lang.IMeta
       (meta [_] data)])

  ;; we need to support Named for `derive`
  #?@(:cljs
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
                name))]
      :default
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
                  name))]))

#?(:cljs nil
   :clj (defmethod print-method Type [this w] (.write ^java.io.Writer w (str this)))
   :cljr (defmethod print-method SciCustomType [this w] (.Write ^System.IO.TextWriter w (str this))))

(defn- throw-root-binding [this]
  (throw (#?(:clj IllegalStateException. :cljs js/Error. :cljr InvalidOperationException.)
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
    Var [#?(:cljs ^:mutable root
            :default ^:volatile-mutable root)
         sym
         #?(:cljs ^:mutable meta
            :default ^:volatile-mutable meta)
         #?(:cljs ^:mutable thread-bound
            :default ^:volatile-mutable thread-bound)
         #?(:cljs ^:mutable needs-ctx
            :default ^:volatile-mutable needs-ctx)
         #?(:cljs ^:mutable watches
            :default ^:volatile-mutable watches)]
  #?@(:cljs [] :default
      ;; marker interface, clj only for now
      [sci.lang.IVar])
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
      #?(:cljs (types/setVal b v)
         :default
         (let [t (.-thread b)]
           (if (not (identical? t #?(:clj (Thread/currentThread)
                                     :cljr (Thread/CurrentThread))))
             (throw (#?(:clj IllegalStateException. :cljr InvalidOperationException.)
                     (format "Can't set!: %s from non-binding thread" (vars/toSymbol this))))
             (types/setVal b v))))
      #?(:cljs (if *unrestricted*
                 (set! (.-root this) v)
                 (throw-root-binding this))
         :default (throw-root-binding this))))
  (getVal [_this] root)
  #?(:cljs IDeref :default clojure.lang.IDeref)
  (#?(:cljs -deref
      :default deref) [this]
    (if thread-bound
      (if-let [tbox (vars/get-thread-binding this)]
        (types/getVal tbox)
        root)
      root))
  Object
  (#?(:cljr ToString :default toString) [this]
    (str "#'" (vars/toSymbol this)))
  #?(:cljs IPrintWithWriter)
  #?(:cljs (-pr-writer [a writer opts]
                       (-write writer "#'")
                       (-pr-writer (vars/toSymbol a) writer opts)))
  #?@(:cljs [IMeta
             (-meta [_] meta)]
      :default [clojure.lang.IMeta
                (meta [_] meta)])
  ;; #?(:clj Comparable :cljs IEquiv)
  ;; (-equiv [this other]
  ;;   (if (instance? Var other)
  ;;     (= (.-sym this) (.-sym other))
  ;;     false))
  ;; #?(:clj clojure.lang.IHashEq :cljs IHash)
  ;; (-hash [_]
  ;;   (hash-symbol sym))
  #?@(:cljs []
      :default
      [clojure.lang.IReference
       (alterMeta [this f args]
                  (vars/with-writeable-var this meta
                    (locking this (set! meta (apply f meta args)))))
       (resetMeta [this m]
                  (vars/with-writeable-var this meta
                    (locking this (set! meta m))))])
  #?@(:cljs [IWatchable
            (-add-watch [this key fn]
                        (vars/with-writeable-var this meta
                          (set! watches (assoc watches key fn)))
                        this)
            (-remove-watch [this key]
                           (vars/with-writeable-var this meta
                             (set! watches (dissoc watches key)))
                           this)]
      :default [clojure.lang.IRef
                (addWatch [this key fn]
                          (vars/with-writeable-var this meta
                            (set! watches (assoc watches key fn)))
                          this)
                (removeWatch [this key]
                             (vars/with-writeable-var this meta
                               (set! watches (dissoc watches key)))
                             this)])
  ;; #?(:cljs Fn) ;; In the real CLJS this is there... why?
  #?(:cljs IFn :default clojure.lang.IFn)
  (#?(:cljs -invoke :default invoke) [this]
    (@this))
  (#?(:cljs -invoke :default invoke) [this a]
    (@this a))
  (#?(:cljs -invoke :default invoke) [this a b]
    (@this a b))
  (#?(:cljs -invoke :default invoke) [this a b c]
    (@this a b c))
  (#?(:cljs -invoke :default invoke) [this a b c d]
    (@this a b c d))
  (#?(:cljs -invoke :default invoke) [this a b c d e]
    (@this a b c d e))
  (#?(:cljs -invoke :default invoke) [this a b c d e f]
    (@this a b c d e f))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g]
    (@this a b c d e f g))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h]
    (@this a b c d e f g h))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i]
    (@this a b c d e f g h i))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j]
    (@this a b c d e f g h i j))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k]
    (@this a b c d e f g h i j k))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l]
    (@this a b c d e f g h i j k l))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m]
    (@this a b c d e f g h i j k l m))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n]
    (@this a b c d e f g h i j k l m n))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o]
    (@this a b c d e f g h i j k l m n o))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o p]
    (@this a b c d e f g h i j k l m n o p))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o p q]
    (@this a b c d e f g h i j k l m n o p q))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o p q r]
    (@this a b c d e f g h i j k l m n o p q r))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o p q r s]
    (@this a b c d e f g h i j k l m n o p q r s))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o p q r s t]
    (@this a b c d e f g h i j k l m n o p q r s t))
  (#?(:cljs -invoke :default invoke) [this a b c d e f g h i j k l m n o p q r s t rest]
    (apply @this a b c d e f g h i j k l m n o p q r s t rest))
  #?@(:cljs []
      :default [(applyTo [this args] (apply @this args))]))

#?(:cljs nil :default
   ;; Use public interface for print-method so it can be overriden in bb itself
   (do (defmethod print-method sci.lang.IVar [o #?(:clj ^java.io.Writer w :cljr ^System.IO.TextWriter w :default w)]
         (#?(:clj .write :cljr :Write) w (str "#'" (vars/toSymbol ^sci.impl.vars.IVar o))))
       (prefer-method print-method sci.lang.IVar clojure.lang.IDeref)))

(deftype
    ^{:doc
      "Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly."}
    Namespace [name #?(:cljs ^:mutable meta
                       :default ^:volatile-mutable meta)]
  Object
  (#?(:cljr ToString :default toString) [_]
    (str name))
  types/HasName
  (getName [_] name)
  #?@(:cljs [IMeta 
             (-meta [_] meta)]
      :default [clojure.lang.IMeta
                (clojure.core/meta [_] meta)

                clojure.lang.IReference
                (alterMeta [this f args]
                           (vars/with-writeable-namespace this meta
                             (locking this (set! meta (apply f meta args)))))
                (resetMeta [this m]
                           (vars/with-writeable-namespace this meta
                             (locking this (set! meta m))))]))
