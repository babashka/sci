(ns sci.lang
  (:require [sci.impl.types :as types]
            [sci.impl.vars :as vars]
            #?(:cljd [sci.impl.unrestrict :refer [*unrestricted*]]
               :cljs [sci.impl.unrestrict :refer [*unrestricted*]]))
  (:refer-clojure :exclude [Var ->Var var? Namespace ->Namespace]))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

;; marker interface for vars, clj only for now
#?(:cljd nil :clj (definterface ^{:doc "Marker interface for SCI vars."} IVar))

(deftype ^{:doc "Representation of a SCI custom type, created e.g. with `(defrecord Foo [])`. The fields of this type are implementation detail and should not be accessed directly."}
    Type [#?(:cljd ^:mutable data :clj ^:volatile-mutable data :cljs ^:volatile-mutable data)]
  sci.impl.types/IBox
  (getVal [_] data)
  (setVal [_ v] (set! data v))
  Object
  ;; NOTE: returns "user.Foo" rather than "class user.Foo" (unlike java.lang.Class).
  ;; Changing this would break downstream libs (e.g. prismatic/schema).
  (toString [_]
    (str (:sci.impl/type-name data)))

  ;; meta is only supported to get our implementation! keys out
  #?@(:cljd
      [IMeta
       (-meta [_] data)]
      :clj
      [clojure.lang.IMeta
       (meta [_] data)]
      :cljs
      [IMeta
       (-meta [_] data)])

  ;; support alter-meta! for storing print-method etc.
  #?@(:cljd
      []
      :clj
      [clojure.lang.IReference
       (alterMeta [this f args]
                  (locking this
                    (set! data (apply f data args))))
       (resetMeta [this m]
                  (locking this
                    (set! data m)))])

  types/HasName
  (getName [this] (str this)))

#?(:cljd nil
   :clj (defmethod print-method Type [this w]
          (.write ^java.io.Writer w (str this))))

(defn- throw-root-binding [this]
  (throw (#?(:cljd StateError. :clj IllegalStateException. :cljs js/Error.)
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
    Var [#?(:cljd ^:mutable root
            :clj ^:volatile-mutable root
            :cljs ^:mutable root)
         sym
         #?(:cljd ^:mutable meta
            :clj ^:volatile-mutable meta
            :cljs ^:mutable meta)
         #?(:cljd ^:mutable thread-bound
            :clj ^:volatile-mutable thread-bound
            :cljs ^:mutable thread-bound)
         #?(:cljd ^:mutable needs-ctx
            :clj ^:volatile-mutable needs-ctx
            :cljs ^:mutable needs-ctx)
         #?(:cljd ^:mutable watches
            :clj ^:volatile-mutable watches
            :cljs ^:mutable watches)
         ns]
  ;; marker interface, clj only for now
  #?@(:cljd [] :clj [sci.lang.IVar])
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
    #?(:cljd (set! thread-bound v)
       :default (set! (.-thread-bound this) v)))
  (unbind [this]
    (vars/with-writeable-var this meta
      (set! (.-root this) #?(:cljd (vars/->SciUnbound this)
                             :clj (sci.impl.vars.SciUnbound. this)
                             :cljs (sci.impl.vars.SciUnbound. this)))))
  (hasRoot [_this]
    (not (instance? #?(:cljd vars/SciUnbound
                       :clj sci.impl.vars.SciUnbound
                       :cljs sci.impl.vars.SciUnbound) root)))
  vars/DynVar
  (dynamic? [_this]
    (:dynamic meta))
  types/IBox
  (setVal [this v]
    (if-let [b (vars/get-thread-binding this)]
      #?(:cljd (types/setVal b v)
         :clj
         (let [t (.-thread b)]
           (if (not (identical? t (Thread/currentThread)))
             (throw (IllegalStateException.
                     (format "Can't set!: %s from non-binding thread" (vars/toSymbol this))))
             (types/setVal b v)))
         :cljs (types/setVal b v))
      #?(:cljd (if *unrestricted*
                 (set! (.-root this) v)
                 (throw-root-binding this))
         :clj (throw-root-binding this)
         :cljs (if *unrestricted*
                 (set! (.-root this) v)
                 (throw-root-binding this)))))
  (getVal [_this] root)
  #?(:cljd IDeref :clj clojure.lang.IDeref :cljs IDeref)
  (#?(:cljd -deref
      :clj deref
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
  #?(:cljd IMeta :clj clojure.lang.IMeta :cljs IMeta)
  #?(:cljd (-meta [_] meta) :clj (clojure.core/meta [_] meta) :cljs (-meta [_] meta))
  ;; #?(:clj Comparable :cljs IEquiv)
  ;; (-equiv [this other]
  ;;   (if (instance? Var other)
  ;;     (= (.-sym this) (.-sym other))
  ;;     false))
  ;; #?(:clj clojure.lang.IHashEq :cljs IHash)
  ;; (-hash [_]
  ;;   (hash-symbol sym))
  #?@(:cljd [] :clj [clojure.lang.IReference
                     (alterMeta [this f args]
                                (vars/with-writeable-var this meta
                                  (locking this (set! meta (apply f meta args)))))
                     (resetMeta [this m]
                                (vars/with-writeable-var this meta
                                  (locking this (set! meta m))))])
  #?@(:cljd [IWatchable
             (-add-watch [this key fn]
                         (vars/with-writeable-var this meta
                           (set! watches (assoc watches key fn)))
                         this)
             (-remove-watch [this key]
                            (vars/with-writeable-var this meta
                              (set! watches (dissoc watches key)))
                            this)]
      :clj [clojure.lang.IRef
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
  #?(:cljd IFn :clj clojure.lang.IFn :cljs IFn)
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this]
    (@this))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a]
    (@this a))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b]
    (@this a b))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c]
    (@this a b c))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d]
    (@this a b c d))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e]
    (@this a b c d e))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f]
    (@this a b c d e f))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g]
    (@this a b c d e f g))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h]
    (@this a b c d e f g h))
  (#?(:cljd -invoke :clj invoke :cljs -invoke) [this a b c d e f g h i]
    (@this a b c d e f g h i))
  #?@(:cljd
      [(-invoke-more [this a b c d e f g h i rest]
         (apply @this a b c d e f g h i rest))
       (-apply [this args]
         (apply @this args))]
      :clj
      [(invoke [this a b c d e f g h i j]
         (@this a b c d e f g h i j))
       (invoke [this a b c d e f g h i j k]
         (@this a b c d e f g h i j k))
       (invoke [this a b c d e f g h i j k l]
         (@this a b c d e f g h i j k l))
       (invoke [this a b c d e f g h i j k l m]
         (@this a b c d e f g h i j k l m))
       (invoke [this a b c d e f g h i j k l m n]
         (@this a b c d e f g h i j k l m n))
       (invoke [this a b c d e f g h i j k l m n o]
         (@this a b c d e f g h i j k l m n o))
       (invoke [this a b c d e f g h i j k l m n o p]
         (@this a b c d e f g h i j k l m n o p))
       (invoke [this a b c d e f g h i j k l m n o p q]
         (@this a b c d e f g h i j k l m n o p q))
       (invoke [this a b c d e f g h i j k l m n o p q r]
         (@this a b c d e f g h i j k l m n o p q r))
       (invoke [this a b c d e f g h i j k l m n o p q r s]
         (@this a b c d e f g h i j k l m n o p q r s))
       (invoke [this a b c d e f g h i j k l m n o p q r s t]
         (@this a b c d e f g h i j k l m n o p q r s t))
       (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
         (apply @this a b c d e f g h i j k l m n o p q r s t rest))
       (applyTo [this args]
                (apply @this args))]
      :cljs
      [(-invoke [this a b c d e f g h i j]
         (@this a b c d e f g h i j))
       (-invoke [this a b c d e f g h i j k]
         (@this a b c d e f g h i j k))
       (-invoke [this a b c d e f g h i j k l]
         (@this a b c d e f g h i j k l))
       (-invoke [this a b c d e f g h i j k l m]
         (@this a b c d e f g h i j k l m))
       (-invoke [this a b c d e f g h i j k l m n]
         (@this a b c d e f g h i j k l m n))
       (-invoke [this a b c d e f g h i j k l m n o]
         (@this a b c d e f g h i j k l m n o))
       (-invoke [this a b c d e f g h i j k l m n o p]
         (@this a b c d e f g h i j k l m n o p))
       (-invoke [this a b c d e f g h i j k l m n o p q]
         (@this a b c d e f g h i j k l m n o p q))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
         (@this a b c d e f g h i j k l m n o p q r))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
         (@this a b c d e f g h i j k l m n o p q r s))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
         (@this a b c d e f g h i j k l m n o p q r s t))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
         (apply @this a b c d e f g h i j k l m n o p q r s t rest))]))

#?(:cljd nil
   :clj
   ;; Use public interface for print-method so it can be overriden in bb itself
   (do (defmethod print-method sci.lang.IVar [o ^java.io.Writer w]
         (.write w (str "#'" (vars/toSymbol ^sci.impl.vars.IVar o))))
       (prefer-method print-method sci.lang.IVar clojure.lang.IDeref)))

(deftype
    ^{:doc
      "Representation of a SCI namespace, created e.g. with `(create-ns 'foo)`.
      The fields of this type are implementation detail and should not be accessed
      directly."}
    Namespace [name #?(:cljd ^:mutable meta
                       :clj ^:volatile-mutable meta
                       :cljs ^:mutable meta)]
  Object
  (toString [_]
    (str name))
  types/HasName
  (getName [_] name)
  #?(:cljd IMeta :clj clojure.lang.IMeta :cljs IMeta)
  #?(:cljd (-meta [_] meta) :clj (clojure.core/meta [_] meta) :cljs (-meta [_] meta))
  #?@(:cljd [] :clj [clojure.lang.IReference
                     (alterMeta [this f args]
                                (vars/with-writeable-namespace this meta
                                  (locking this (set! meta (apply f meta args)))))
                     (resetMeta [this m]
                                (vars/with-writeable-namespace this meta
                                  (locking this (set! meta m))))]))
