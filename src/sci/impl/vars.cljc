(ns sci.impl.vars
  {:no-doc true}
  (:refer-clojure :exclude [binding
                            push-thread-bindings
                            get-thread-bindings
                            pop-thread-bindings
                            with-bindings*
                            with-bindings
                            thread-bound?
                            alter-var-root
                            var-get
                            var-set
                            bound-fn*])
  (:require [sci.impl.macros :as macros]
            [sci.impl.types :as t]
            [sci.impl.unrestrict :refer [*unrestricted*]])
  #?(:cljs (:require-macros [sci.impl.vars :refer [with-bindings
                                                   with-writeable-namespace
                                                   with-writeable-var]])))

#?(:clj (set! *warn-on-reflection* true))

(macros/deftime
  (defmacro with-writeable-namespace
    [the-ns-object ns-meta & body]
    `(let [m# ~ns-meta]
       (if (or *unrestricted* (not (:sci/built-in m#)))
         (do ~@body)
         (let [ns-obj# ~the-ns-object
               name# (t/getName ns-obj#)]
           (throw (ex-info (str "Built-in namespace " name# " is read-only.")
                           {:ns ns-obj#})))))))

(deftype Frame [bindings prev])

(def top-frame (Frame. {} nil))

#?(:clj
   (def ^ThreadLocal dvals (proxy [ThreadLocal] []
                             (initialValue [] top-frame)))
   :cljs
   (def dvals (volatile! top-frame)))

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
     :cljs (vreset! dvals frame)))

(defprotocol IVar
  (bindRoot [this v])
  (getRawRoot [this])
  (toSymbol [this])
  (isMacro [this])
  (hasRoot [this])
  (setThreadBound [this v])
  (unbind [this]))

(defprotocol DynVar
  (dynamic? [this]))

(extend-type #?(:clj Object :cljs default)
  DynVar
  (dynamic? [_] false))

(defn push-thread-bindings [bindings]
  (let [^Frame frame (get-thread-binding-frame)
        bmap (.-bindings frame)
        bmap (reduce (fn [acc [var* val*]]
                       (when (not (dynamic? var*))
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
  ;; type hint needed to satisfy CLJS compiler / shadow
  (if-let [f (.-prev ^Frame (get-thread-binding-frame))]
    (if (identical? top-frame f)
      #?(:clj (.remove dvals)
         :cljs (vreset! dvals top-frame))
      (reset-thread-binding-frame f))
    (throw (new #?(:clj Exception :cljs js/Error) "No frame to pop."))))

(defn get-thread-bindings []
  (let [;; type hint added to prevent shadow-cljs warning, although fn has return tag
        ^Frame f (get-thread-binding-frame)]
    (loop [ret {}
           kvs (seq (.-bindings f))]
      (if kvs
        (let [[var* ^TBox tbox] (first kvs)
              tbox-val (t/getVal tbox)]
          (recur (assoc ret var* tbox-val)
                 (next kvs)))
        ret))))

(defn get-thread-binding ^TBox [sci-var]
  (when-let [;; type hint added to prevent shadow-cljs warning, although fn has return tag
             ^Frame f #?(:clj (.get dvals)
                         :cljs @dvals)]
    #?(:clj (.get ^java.util.Map (.-bindings f) sci-var)
       :cljs (.get (.-bindings f) sci-var))))

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
  (:sci/built-in var-meta))

(macros/deftime
  (defmacro with-writeable-var
    [the-var var-meta & body]
    `(let [vm# ~var-meta]
       (if (or *unrestricted* (not (:sci/built-in vm#)))
         (do ~@body)
         (let [the-var# ~the-var
               ns# (:ns vm#)
               ns-name# (t/getName ns#)
               name# (t/getName the-var#)]
           (throw (ex-info (str "Built-in var #'" ns-name# "/" name# " is read-only.")
                           {:var ~the-var})))))))

(defn var-get [v]
  (deref v))

(defn var-set [v val]
  (t/setVal v val))

(defn unqualify-symbol 
  "If sym is namespace-qualified, remove the namespace, else return sym"
  [sym]
  (if (qualified-symbol? sym)
    (symbol (name sym))
    sym))

(macros/deftime
  (defmacro with-bindings
    "Macro for binding sci vars for internal use."
    [bindings & body]
    `(do
       ;; important: outside try
       (vars/push-thread-bindings ~bindings)
       (try
         (do ~@body)
         (finally
           (vars/pop-thread-bindings))))))

(defn alter-var-root
  ([v f]
   #?(:clj
      (locking v (bindRoot v (f (getRawRoot v))))
      :cljs (bindRoot v (f (getRawRoot v)))))
  ([v f & args]
   #?(:clj
      (locking v (bindRoot v (apply f (getRawRoot v) args)))
      :cljs (bindRoot v (apply f (getRawRoot v) args)))))

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
