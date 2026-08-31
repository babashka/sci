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
  (:require [sci.ctx-store :as store]
            [sci.impl.execution :as execution]
            [sci.impl.macros :as macros]
            [sci.impl.types :as t]
            [sci.impl.world :as world])
  #?(:cljs (:require-macros [sci.impl.vars :refer [with-bindings
                                                   with-writeable-namespace
                                                   with-writeable-var
                                                   bumping-set!]])))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

(macros/deftime
  (defmacro with-writeable-namespace
    [the-ns-object ns-meta & body]
    `(let [m# ~ns-meta]
       (if (or (:unrestricted sci.ctx-store/*ctx*) (not (:sci/built-in m#)))
         (do ~@body)
         (let [ns-obj# ~the-ns-object
               name# (t/getName ns-obj#)]
           (throw (ex-info (str "Built-in namespace " name# " is read-only.")
                           {:ns ns-obj#})))))))

(deftype Frame [bindings prev scope prior-bindings])

(def top-frame (Frame. {} nil nil nil))

#?(:cljs
   (def var-epoch
     "Bumped on EVERY var mutation (root bind, unbind, set!, thread-binding
     push/pop). Jitted call sites cache var derefs keyed on this; a missed
     bump means a stale callee, so any new mutation path must bump."
     #js [0]))

#?(:cljs
   (defn bump-var-epoch! []
     (aset var-epoch 0 (inc (aget var-epoch 0)))))

;; The chokepoint for var value mutation: any write to a var root or
;; thread-binding box must go through this so jit deref caches can't go
;; stale. Expands to a bare set! off-CLJS; deftype fields can only be
;; set! inside the owning type's methods, hence a macro.
#?(:cljd
   (defmacro bumping-set! [target v]
     `(set! ~target ~v))
   :default
   (macros/deftime
     (defmacro bumping-set! [target v]
       (macros/?
        :clj `(set! ~target ~v)
        :cljs `(let [v# ~v]
                 (set! ~target v#)
                 (sci.impl.vars/bump-var-epoch!)
                 v#)))))

(defn get-thread-binding-frame ^Frame []
  (or (execution/binding-frame) top-frame))

(deftype TBox #?(:cljd [thread ^:mutable val]
                 :clj [thread ^:volatile-mutable val]
                 :cljs [thread ^:mutable val])
  t/IBox
  (setVal [_this v]
    (bumping-set! val v))
  (getVal [_this] val))

(defn clone-thread-binding-frame ^Frame []
  (let [state #?(:clj (.get ^ThreadLocal execution/current)
                 :default (execution/current-state))]
    (Frame. (execution/active-bindings state)
            nil
            (execution/binding-scope state)
            nil)))

(defn reset-thread-binding-frame [frame]
  (let [state #?(:clj (.get ^ThreadLocal execution/current)
                 :default (execution/current-state))
        scopes (loop [^Frame current frame
                      seen #{}
                      ret {}]
                 (if (nil? current)
                   ret
                   (let [scope (.-scope current)]
                     (if (contains? seen scope)
                       (recur (.-prev current) seen ret)
                       (recur (.-prev current)
                              (conj seen scope)
                              (assoc ret scope (.-bindings current)))))))]
    (execution/set-binding-frame! state frame)
    (execution/set-scope-bindings! state scopes)
    (execution/refresh-active-bindings! state)
    frame))

(defprotocol IVar
  (bindRoot [this v])
  (getRawRoot [this])
  (getDirectRoot [this])
  (selectRoot [this world-value])
  (getRootAt [this slot])
  (toSymbol [this])
  (isMacro [this])
  (hasRoot [this])
  (setThreadBound [this v])
  (unbind [this]))

(defprotocol DynVar
  (dynamic? [this]))

(extend-type #?(:cljd fallback :clj Object :cljs default)
  DynVar
  (dynamic? [_] false))

(defn push-thread-bindings [bindings]
  (let [state #?(:clj (.get ^ThreadLocal execution/current)
                 :default (execution/current-state))
        ^Frame frame (or (execution/binding-frame state) top-frame)
        scope (execution/binding-scope state)
        scopes (execution/scope-bindings state)
        prior-bindings (get scopes scope)
        bmap (or prior-bindings {})
        bmap (reduce (fn [acc [var* val*]]
                       (when (not (dynamic? var*))
                         (throw #?(:cljd (ex-info (str "Can't dynamically bind non-dynamic var " var*) {})
                                   :clj (new IllegalStateException
                                             (str "Can't dynamically bind non-dynamic var " var*))
                                   :cljs (new js/Error
                                              (str "Can't dynamically bind non-dynamic var " var*)))))
                       (setThreadBound var* true)
                       (assoc acc var* (TBox. #?(:cljd nil
                                                 :clj (Thread/currentThread)
                                                 :cljs nil) val*)))
                     bmap
                     bindings)]
    #?(:cljs (bump-var-epoch!))
    (let [new-frame (Frame. bmap frame scope prior-bindings)]
      (execution/set-binding-frame! state new-frame)
      (execution/set-scope-bindings! state (assoc scopes scope bmap))
      (execution/refresh-active-bindings! state)
      new-frame)))

(defn pop-thread-bindings []
  #?(:cljs (bump-var-epoch!))
  ;; type hint needed to satisfy CLJS compiler / shadow
  (let [state #?(:clj (.get ^ThreadLocal execution/current)
                 :default (execution/current-state))
        ^Frame frame (or (execution/binding-frame state) top-frame)]
    (if-let [previous (.-prev frame)]
      (let [scope (.-scope frame)
            prior-bindings (.-prior-bindings frame)
            scopes (execution/scope-bindings state)
            scopes (if (nil? prior-bindings)
                     (dissoc scopes scope)
                     (assoc scopes scope prior-bindings))]
        (execution/set-binding-frame! state
                                      (when-not (identical? top-frame previous)
                                        previous))
        (execution/set-scope-bindings! state scopes)
        (execution/refresh-active-bindings! state)
        nil)
      (throw (new #?(:cljd Exception :clj Exception :cljs js/Error) "No frame to pop.")))))

(defn get-thread-bindings []
  (let [bmap (execution/active-bindings)]
    (reduce-kv (fn [ret var* tbox]
                 (assoc ret var* (t/getVal tbox)))
               {}
               bmap)))

(defn get-thread-binding #?(:cljd [sci-var] :clj ^TBox [sci-var] :cljs ^TBox [sci-var])
  (let [state #?(:clj (.get ^ThreadLocal execution/current)
                 :default (execution/current-state))
        bmap #?(:clj (or (aget ^objects state execution/active-bindings-index) {})
                :default (execution/active-bindings state))]
    #?(:cljd (get bmap sci-var)
       :clj (.get ^java.util.Map bmap sci-var)
       :cljs (.get bmap sci-var))))

(defn- binding-frame-fn [frame ctx f]
  (let [invoke (fn [args]
                 (let [previous (get-thread-binding-frame)]
                   (try
                     (reset-thread-binding-frame frame)
                     (if (:sci.impl/world ctx)
                       (store/with-ctx ctx
                         (world/with-active-world ctx #(apply f args)))
                       (apply f args))
                     (finally
                       (reset-thread-binding-frame previous)))))]
    (fn
      ([]
       (invoke nil))
      ([x]
       (invoke [x]))
      ([x y]
       (invoke [x y]))
      ([x y z]
       (invoke [x y z]))
      ([x y z & args]
       (invoke (list* x y z args))))))

(defn binding-conveyor-fn
  "Convey the current binding values to an independent task. The shallow
  frame deliberately cannot pop scopes owned by the submitting execution."
  [f]
  (binding-frame-fn (clone-thread-binding-frame) store/*ctx* f))

(defn binding-continuation-fn
  "Convey the persistent binding frame chain to a continuation of the current
  execution. Unlike an independent task, the continuation may leave scopes
  entered before suspension."
  [f]
  (binding-frame-fn (get-thread-binding-frame) store/*ctx* f))

(defn throw-unbound-call-exception [the-var]
  (throw #?(:cljd (ex-info (str "Attempting to call unbound fn: " the-var) {})
            :clj (new IllegalStateException (str "Attempting to call unbound fn: " the-var))
            :cljs (new js/Error (str "Attempting to call unbound fn: " the-var)))))

#?(:cljd
   (deftype SciUnbound [the-var]
     Object
     (toString [_]
       (str "Unbound: " the-var))
     IFn
     (-invoke [_]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c d]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c d e]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c d e f]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c d e f g]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c d e f g h]
       (throw-unbound-call-exception the-var))
     (-invoke [_ a b c d e f g h i]
       (throw-unbound-call-exception the-var))
     (-invoke-more [_ a b c d e f g h i rest]
       (throw-unbound-call-exception the-var))
     (-apply [_ more]
       (throw-unbound-call-exception the-var))))

#?(:cljd nil
   :default
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
                 (throw-unbound-call-exception the-var)))))

;; adapted from https://github.com/clojure/clojurescript/blob/df1837048d01b157a04bb3dc7fedc58ee349a24a/src/main/cljs/cljs/core.cljs#L1118

(defn built-in-var? [var-meta]
  (:sci/built-in var-meta))

(macros/deftime
  (defmacro with-writeable-var
    [the-var var-meta & body]
    `(let [vm# ~var-meta]
       (if (or (:unrestricted sci.ctx-store/*ctx*) (not (:sci/built-in vm#)))
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
   #?(:cljd (bindRoot v (f (getRawRoot v)))
      :clj
      (locking v (bindRoot v (f (getRawRoot v))))
      :cljs (bindRoot v (f (getRawRoot v)))))
  ([v f & args]
   #?(:cljd (bindRoot v (apply f (getRawRoot v) args))
      :clj
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
