(ns sci.impl.execution
  "The execution-local state shared by world selection and dynamic bindings.")

(def ^:const active-world-index 0)
(def ^:const binding-frame-index 1)
(def ^:const binding-scope-index 2)
(def ^:const active-bindings-index 3)
(def ^:const scope-bindings-index 4)
(def ^:const active-cells-holder-index 5)
(def ^:const active-registry-index 6)

(defn- new-state []
  #?(:cljd (#/(List/filled dynamic) 7 nil)
     :default (object-array 7)))

#?(:cljd
   (def current (volatile! (new-state)))
   :clj
   (def ^ThreadLocal current
     (proxy [ThreadLocal] []
       (initialValue [] (new-state))))
   :cljs
   (def current (volatile! (new-state))))

(defn call-with-detached-state
  "Invoke `f` with an empty execution-local state, restoring the caller's
  interpreter state afterwards. This is the host boundary used when an SCI
  program constructs an independent interpreter recursively."
  [f]
  #?(:cljd
     (let [previous @current]
       (vreset! current (new-state))
       (try
         (f)
         (finally
           (vreset! current previous))))
     :clj
     (let [previous (.get ^ThreadLocal current)]
       (.set ^ThreadLocal current (new-state))
       (try
         (f)
         (finally
           (.set ^ThreadLocal current previous))))
     :cljs
     (let [previous @current]
       (vreset! current (new-state))
       (try
         (f)
         (finally
           (vreset! current previous))))))

(defn current-state []
  #?(:cljd @current
     :clj (.get ^ThreadLocal current)
     :cljs @current))

(defn active-world
  ([] (active-world (current-state)))
  ([state]
   #?(:cljd (aget ^List state active-world-index)
      :clj (aget ^objects state active-world-index)
      :cljs (aget state active-world-index))))

(defn binding-frame
  ([] (binding-frame (current-state)))
  ([state]
   #?(:cljd (aget ^List state binding-frame-index)
      :clj (aget ^objects state binding-frame-index)
      :cljs (aget state binding-frame-index))))

(defn binding-scope
  ([] (binding-scope (current-state)))
  ([state]
   #?(:cljd (aget ^List state binding-scope-index)
      :clj (aget ^objects state binding-scope-index)
      :cljs (aget state binding-scope-index))))

(defn active-bindings
  ([] (active-bindings (current-state)))
  ([state]
   (or #?(:cljd (aget ^List state active-bindings-index)
          :clj (aget ^objects state active-bindings-index)
          :cljs (aget state active-bindings-index))
       {})))

(defn scope-bindings
  ([] (scope-bindings (current-state)))
  ([state]
   (or #?(:cljd (aget ^List state scope-bindings-index)
          :clj (aget ^objects state scope-bindings-index)
          :cljs (aget state scope-bindings-index))
       {})))

(defn active-cells-holder
  ([] (active-cells-holder (current-state)))
  ([state]
   #?(:cljd (aget ^List state active-cells-holder-index)
      :clj (aget ^objects state active-cells-holder-index)
      :cljs (aget state active-cells-holder-index))))

(defn active-registry
  ([] (active-registry (current-state)))
  ([state]
   #?(:cljd (aget ^List state active-registry-index)
      :clj (aget ^objects state active-registry-index)
      :cljs (aget state active-registry-index))))

(defn set-active-world! [state value]
  #?(:cljd (aset ^List state active-world-index value)
     :clj (aset ^objects state active-world-index value)
     :cljs (aset state active-world-index value))
  value)

(defn set-binding-frame! [state value]
  #?(:cljd (aset ^List state binding-frame-index value)
     :clj (aset ^objects state binding-frame-index value)
     :cljs (aset state binding-frame-index value))
  value)

(defn set-binding-scope! [state value]
  #?(:cljd (aset ^List state binding-scope-index value)
     :clj (aset ^objects state binding-scope-index value)
     :cljs (aset state binding-scope-index value))
  value)

(defn set-active-bindings! [state value]
  #?(:cljd (aset ^List state active-bindings-index value)
     :clj (aset ^objects state active-bindings-index value)
     :cljs (aset state active-bindings-index value))
  value)

(defn set-scope-bindings! [state value]
  #?(:cljd (aset ^List state scope-bindings-index value)
     :clj (aset ^objects state scope-bindings-index value)
     :cljs (aset state scope-bindings-index value))
  value)

(defn set-active-cells-holder! [state value]
  #?(:cljd (aset ^List state active-cells-holder-index value)
     :clj (aset ^objects state active-cells-holder-index value)
     :cljs (aset state active-cells-holder-index value))
  value)

(defn set-active-registry! [state value]
  #?(:cljd (aset ^List state active-registry-index value)
     :clj (aset ^objects state active-registry-index value)
     :cljs (aset state active-registry-index value))
  value)

(defn refresh-active-bindings! [state]
  (let [scope (binding-scope state)
        scopes (scope-bindings state)
        host (get scopes nil {})
        scoped (if (nil? scope) {} (get scopes scope {}))
        effective (cond
                    (empty? host) scoped
                    (empty? scoped) host
                    :else (merge host scoped))]
    (set-active-bindings! state effective)))
