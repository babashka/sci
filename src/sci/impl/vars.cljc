(ns sci.impl.vars
  {:no-doc true}
  (:refer-clojure :exclude [var? binding
                            push-thread-bindings
                            get-thread-bindings
                            pop-thread-bindings
                            with-bindings
                            thread-bound?
                            alter-var-root])
  (:require [sci.impl.macros :as macros]
            [sci.impl.types :as t]
            [sci.impl.unrestrict :refer [*unrestricted*]])
  #?(:cljs (:require-macros [sci.impl.vars :refer [with-bindings
                                                   with-writeable-namespace
                                                   with-writeable-var]])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol HasName ;; INamed was already taken by CLJS
  (getName [_]))

(macros/deftime
  (defmacro with-writeable-namespace
    [the-ns-object ns-meta & body]
    `(let [m# ~ns-meta]
       (if (or *unrestricted* (not (:sci.impl/built-in m#)))
         (do ~@body)
         (let [ns-obj# ~the-ns-object
               name# (getName ns-obj#)]
           (throw (ex-info (str "Built-in namespace " name# " is read-only.")
                           {:ns ns-obj#})))))))

(deftype SciNamespace [name #?(:clj ^:volatile-mutable meta
                               :cljs ^:mutable meta)]
  Object
  (toString [_]
    (str name))
  HasName
  (getName [_] name)
  #?(:clj clojure.lang.IMeta :cljs IMeta)
  #?(:clj (clojure.core/meta [_] meta) :cljs (-meta [_] meta))
  #?(:clj clojure.lang.IReference)
  #?(:clj (alterMeta [this f args]
                     (with-writeable-namespace this meta
                       (locking (set! meta (apply f meta args))))))
  #?(:clj (resetMeta [this m]
                     (with-writeable-namespace this meta
                       (locking (set! meta m))))))

(defn namespace? [x]
  (instance? sci.impl.vars.SciNamespace x))

(deftype Frame [bindings prev])

(def top-frame (Frame. {} nil))

#?(:clj
   (def ^ThreadLocal dvals (proxy [ThreadLocal] []
                             (initialValue [] top-frame)))
   :cljs
   (def dvals (atom top-frame)))

(defn get-thread-binding-frame ^Frame []
  #?(:clj (.get dvals)
     :cljs @dvals))

(deftype TBox #?(:clj [thread ^:volatile-mutable val]
                 :cljs [thread ^:mutable val])
  t/IBox
  (setVal [this v]
    (set! val v))
  (getVal [this] val))

(defn clone-thread-binding-frame ^Frame []
  (let [^Frame f #?(:clj (.get dvals)
                    :cljs @dvals)]
    (Frame. (.-bindings f) nil)))

(defn reset-thread-binding-frame [frame]
  #?(:clj (.set dvals frame)
     :cljs (reset! dvals frame)))

(declare var?)

(defn dynamic-var? [v]
  ;; TODO: make separate field
  (and (var? v)
       (:dynamic (meta v))))

(defprotocol IVar
  (bindRoot [this v])
  (getRawRoot [this])
  (toSymbol [this])
  (isMacro [this])
  (hasRoot [this])
  (setThreadBound [this v])
  (unbind [this]))

(defn push-thread-bindings [bindings]
  (let [frame (get-thread-binding-frame)
        bmap (.-bindings frame)
        bmap (reduce (fn [acc [var* val*]]
                       (when-not (dynamic-var? var*)
                         (throw (new #?(:clj IllegalStateException
                                        :cljs js/Error)
                                     (str "Can't dynamically bind non-dynamic var " var*))))
                       (setThreadBound var* true)
                       (assoc acc var* (TBox. #?(:clj (Thread/currentThread)
                                                 :cljs nil) val*)))
                     bmap
                     bindings)]
    (reset-thread-binding-frame (Frame. bmap frame))))

(defn pop-thread-bindings []
  (if-let [f (.-prev (get-thread-binding-frame))]
    (if (identical? top-frame f)
      #?(:clj (.remove dvals)
         :cljs (reset! dvals top-frame))
      (reset-thread-binding-frame f))
    (throw (new #?(:clj Exception :cljs js/Error) "No frame to pop."))))

(defn get-thread-bindings []
  (let [f (get-thread-binding-frame)]
    (loop [ret {}
           kvs (seq (.-bindings f))]
      (if kvs
        (let [[var* ^TBox tbox] (first kvs)
              tbox-val (t/getVal tbox)]
          (recur (assoc ret var* tbox-val)
                 (next kvs)))
        ret))))

(defn get-thread-binding ^TBox [sci-var]
  (when-let [^Frame f #?(:clj (.get dvals)
                         :cljs @dvals)]
    (when-let [bindings (.-bindings f)]
      (get bindings sci-var))))

(defn binding-conveyor-fn
  [f]
  (let [frame (clone-thread-binding-frame)]
    (fn
      ([]
       (reset-thread-binding-frame frame)
       (f))
      ([x]
       (reset-thread-binding-frame frame)
       (f x))
      ([x y]
       (reset-thread-binding-frame frame)
       (f x y))
      ([x y z]
       (reset-thread-binding-frame frame)
       (f x y z))
      ([x y z & args]
       (reset-thread-binding-frame frame)
       (apply f x y z args)))))

(defn throw-unbound-call-exception [the-var]
  (throw (new #?(:clj IllegalStateException
                 :cljs js/Error) (str "Attempting to call unbound fn: " the-var))))

(deftype SciUnbound [the-var]
  Object
  (toString [_]
    (str "Unbound: " the-var))
  #?@(:clj [clojure.lang.IFn] :cljs [IFn])
  (#?(:clj invoke :cljs -invoke) [_]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t]
    (throw-unbound-call-exception the-var))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t rest]
    (throw-unbound-call-exception the-var))
  #?(:clj
     (applyTo [_ args]
              (throw-unbound-call-exception the-var))))

;; adapted from https://github.com/clojure/clojurescript/blob/df1837048d01b157a04bb3dc7fedc58ee349a24a/src/main/cljs/cljs/core.cljs#L1118

(defn built-in-var? [var-meta]
  (:sci.impl/built-in var-meta))

(macros/deftime
  (defmacro with-writeable-var
    [the-var var-meta & body]
    `(let [vm# ~var-meta]
       (if (or *unrestricted* (not (:sci.impl/built-in vm#)))
         (do ~@body)
         (let [the-var# ~the-var
               ns# (:ns vm#)
               ns-name# (getName ns#)
               name# (getName the-var#)]
           (throw (ex-info (str "Built-in var #'" ns-name# "/" name# " is read-only.")
                           {:var ~the-var})))))))

(deftype SciVar [#?(:clj ^:volatile-mutable root
                    :cljs ^:mutable root)
                 sym
                 #?(:clj ^:volatile-mutable meta
                    :cljs ^:mutable meta)
                 #?(:clj ^:volatile-mutable thread-bound
                    :cljs ^:mutable thread-bound)]
  HasName
  (getName [this]
    sym)
  IVar
  (bindRoot [this v]
    (with-writeable-var this meta
      (set! (.-root this) v)))
  (getRawRoot [this]
    root)
  (toSymbol [this] sym)
  (isMacro [_]
    (:sci/macro (clojure.core/meta root)))
  (setThreadBound [this v]
    (set! (.-thread-bound this) v))
  (unbind [this]
    (with-writeable-var this meta
      (set! (.-root this) (SciUnbound. this))))
  (hasRoot [this]
    (not (instance? SciUnbound root)))
  t/IBox
  (setVal [this v]
    (if-let [b (get-thread-binding this)]
      #?(:clj
         (let [t (.-thread b)]
           (if (not (identical? t (Thread/currentThread)))
             (throw (new IllegalStateException
                         (format "Can't set!: %s from non-binding thread" sym)))
             (t/setVal b v)))
         :cljs (t/setVal b v))
      (throw (new #?(:clj IllegalStateException :cljs js/Error)
                  (str "Can't change/establish root binding of " this " with set")))))
  (getVal [this] root)
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref
      :cljs -deref) [this]
    (if thread-bound
      (if-let [tbox (get-thread-binding this)]
        (t/getVal tbox)
        root)
      root))
  Object
  (toString [_]
    (str "#'" sym))
  #?(:cljs IPrintWithWriter)
  #?(:cljs (-pr-writer [a writer opts]
                       (-write writer "#'")
                       (pr-writer sym writer opts)))
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
                     (with-writeable-var this meta
                       (locking (set! meta (apply f meta args))))))
  #?(:clj (resetMeta [this m]
                     (with-writeable-var this meta
                       (locking (set! meta m)))))
  #?(:clj clojure.lang.IRef) ;; added for multi-methods
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
   (do (defmethod print-method sci.impl.vars.IVar [o ^java.io.Writer w]
         (.write w (str "#'" (toSymbol o))))
       (prefer-method print-method sci.impl.vars.IVar clojure.lang.IDeref)))

(defn var? [x]
  (instance? sci.impl.vars.SciVar x))

(defn dynamic-var
  ([name]
   (dynamic-var name nil (meta name)))
  ([name init-val]
   (dynamic-var name init-val (meta name)))
  ([name init-val meta]
   (let [meta (assoc meta :dynamic true)]
     (SciVar. init-val name meta false))))

(defn binding
  [_ _ bindings & body]
  #_(assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (let [var-ize (fn [var-vals]
                  (loop [ret [] vvs (seq var-vals)]
                    (if vvs
                      (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                              (next (next vvs)))
                      (seq ret))))]
    `(let []
       ;; important: outside try
       (clojure.core/push-thread-bindings (hash-map ~@(var-ize bindings)))
       (try
         ~@body
         (finally
           (clojure.core/pop-thread-bindings))))))

(macros/deftime
  (defmacro with-bindings
    "Macro for binding sci vars for internal use."
    [bindings & body]
    `(do
       ;; important: outside try
       (vars/push-thread-bindings ~bindings)
       (try
         (do ~@body)
         (finally (vars/pop-thread-bindings))))))

(def current-file (dynamic-var '*file* nil))

(def user-ns (->SciNamespace 'user nil))

(def current-ns (dynamic-var '*ns* user-ns))

(defn current-ns-name []
  (getName @current-ns))

(defn alter-var-root [v f & args]
  #?(:clj
     (locking v (bindRoot v (apply f (getRawRoot v) args)))
     :cljs (bindRoot v (apply f (getRawRoot v) args))))

(comment
  (def v1 (SciVar. (fn [] 0) 'foo nil))
  @v1 ;; 0
  (push-thread-bindings {v1 2})
  (get-thread-binding v1) ;; 2
  (push-thread-bindings {v1 3})
  (get-thread-binding v1) ;; 3
  (pop-thread-bindings)
  (get-thread-binding v1) ;; 2
  (pop-thread-bindings)
  (get-thread-binding v1) ;; nil
  @v1 ;; 0
  (pop-thread-bindings) ;; exception
  )
