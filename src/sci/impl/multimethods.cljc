(ns sci.impl.multimethods
  {:no-doc true}
  (:refer-clojure :exclude [defmulti defmethod])
  (:require
   #?(:clj [clojure.string :as str])
   [sci.ctx-store :as store]
   [sci.fork :as fork]
   [sci.impl.world :as world]
   ;; no hierarchies on cljd, like the host
   #?@(:cljd [] :default [[sci.impl.hierarchies :refer [global-hierarchy]]])))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

#?(:cljd
   (defn- no-method [mm-name dv]
     (throw (ex-info (str "No method in multimethod '" mm-name "' for dispatch value: " dv) {}))))

#?(:cljd
   (do
     (def ^:private missing-method-table-value (Object.))

     (defn- cljd-method-table-value [method-table]
       (let [value (world/value method-table missing-method-table-value)]
         (if (identical? value missing-method-table-value)
           @method-table
           value)))

     (defn- cljd-swap-method-table! [method-table f & args]
       (if (world/tracked? method-table)
         (world/swap-value! method-table f args
                            (fn [_] nil) (fn [_ _] nil))
         (apply swap! method-table f args)))))

#?(:cljd nil
   :default
   (do
     (declare clone-multifn-state)

     (deftype MultiFnState [mm-name dispatch-fn default-dispatch-val hierarchy delegate]
       fork/Forkable
       (fork-value [this]
         (clone-multifn-state this)))

     (defn- new-host-multifn [mm-name dispatch-fn default hierarchy]
       #?(:clj (new clojure.lang.MultiFn mm-name dispatch-fn default hierarchy)
          :cljs (new cljs.core/MultiFn
                     mm-name dispatch-fn default hierarchy
                     (atom {}) (atom {}) (atom {}) (atom nil))))

     (defn- host-methods [multifn]
       #?(:clj (.getMethodTable ^clojure.lang.MultiFn multifn)
          :cljs (-methods multifn)))

     (defn- host-prefers [multifn]
       #?(:clj (.getPreferTable ^clojure.lang.MultiFn multifn)
          :cljs (-prefers multifn)))

     (defn- host-add-method! [multifn dispatch-val method]
       #?(:clj (.addMethod ^clojure.lang.MultiFn multifn dispatch-val method)
          :cljs (-add-method multifn dispatch-val method)))

     (defn- host-remove-method! [multifn dispatch-val]
       #?(:clj (.removeMethod ^clojure.lang.MultiFn multifn dispatch-val)
          :cljs (-remove-method multifn dispatch-val)))

     (defn- host-prefer-method! [multifn x y]
       #?(:clj (.preferMethod ^clojure.lang.MultiFn multifn x y)
          :cljs (-prefer-method multifn x y)))

     (defn- host-reset! [multifn]
       #?(:clj (.reset ^clojure.lang.MultiFn multifn)
          :cljs (-reset multifn)))

     (defn- host-get-method [multifn dispatch-val]
       #?(:clj (.getMethod ^clojure.lang.MultiFn multifn dispatch-val)
          :cljs (-get-method multifn dispatch-val)))

     (defn- clone-multifn-state [^MultiFnState state]
       (let [source (.-delegate state)
             target (new-host-multifn
                     (.-mm-name state) (.-dispatch-fn state)
                     (.-default-dispatch-val state) (.-hierarchy state))]
         (doseq [[dispatch-val method] (host-methods source)]
           (host-add-method! target dispatch-val method))
         (doseq [[x ys] (host-prefers source)
                 y ys]
           (host-prefer-method! target x y))
         (MultiFnState. (.-mm-name state) (.-dispatch-fn state)
                        (.-default-dispatch-val state) (.-hierarchy state) target)))

     (defn- initial-multifn-state [mm-name dispatch-fn default hierarchy]
       (MultiFnState. mm-name dispatch-fn default hierarchy
                      (new-host-multifn mm-name dispatch-fn default hierarchy)))

     (defn- state-delegate [^MultiFnState state]
       (.-delegate state))

     (defn- mutate-state [^MultiFnState state mutation]
       (let [^MultiFnState copy (clone-multifn-state state)]
         (mutation (.-delegate copy))
         copy))

     (declare sci-multifn-state mutate-sci-multifn!)

     #?(:clj
        (deftype SciMultiFn [home registry state-slot]
          fork/Forkable
          (fork-value [this] this)

          clojure.lang.IFn
          (invoke [_] ((state-delegate (world/managed-value home registry state-slot))))
          (invoke [_ a] ((state-delegate (world/managed-value home registry state-slot)) a))
          (invoke [_ a b] ((state-delegate (world/managed-value home registry state-slot)) a b))
          (invoke [_ a b c] ((state-delegate (world/managed-value home registry state-slot)) a b c))
          (invoke [_ a b c d] ((state-delegate (world/managed-value home registry state-slot)) a b c d))
          (invoke [_ a b c d e] ((state-delegate (world/managed-value home registry state-slot)) a b c d e))
          (invoke [_ a b c d e f] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f))
          (invoke [_ a b c d e f g] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g))
          (invoke [_ a b c d e f g h] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h))
          (invoke [_ a b c d e f g h i] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i))
          (invoke [_ a b c d e f g h i j] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j))
          (invoke [_ a b c d e f g h i j k] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k))
          (invoke [_ a b c d e f g h i j k l] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l))
          (invoke [_ a b c d e f g h i j k l m] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m))
          (invoke [_ a b c d e f g h i j k l m n] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n))
          (invoke [_ a b c d e f g h i j k l m n o] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o))
          (invoke [_ a b c d e f g h i j k l m n o p] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p))
          (invoke [_ a b c d e f g h i j k l m n o p q] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q))
          (invoke [_ a b c d e f g h i j k l m n o p q r] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q r))
          (invoke [_ a b c d e f g h i j k l m n o p q r s] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q r s))
          (invoke [_ a b c d e f g h i j k l m n o p q r s t] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q r s t))
          (applyTo [_ args]
            (apply (state-delegate (world/managed-value home registry state-slot)) args)))

        :cljs
        (deftype SciMultiFn [home registry state-slot]
          fork/Forkable
          (fork-value [this] this)

          IFn
          (-invoke [_] ((state-delegate (world/managed-value home registry state-slot))))
          (-invoke [_ a] ((state-delegate (world/managed-value home registry state-slot)) a))
          (-invoke [_ a b] ((state-delegate (world/managed-value home registry state-slot)) a b))
          (-invoke [_ a b c] ((state-delegate (world/managed-value home registry state-slot)) a b c))
          (-invoke [_ a b c d] ((state-delegate (world/managed-value home registry state-slot)) a b c d))
          (-invoke [_ a b c d e] ((state-delegate (world/managed-value home registry state-slot)) a b c d e))
          (-invoke [_ a b c d e f] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f))
          (-invoke [_ a b c d e f g] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g))
          (-invoke [_ a b c d e f g h] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h))
          (-invoke [_ a b c d e f g h i] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i))
          (-invoke [_ a b c d e f g h i j] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j))
          (-invoke [_ a b c d e f g h i j k] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k))
          (-invoke [_ a b c d e f g h i j k l] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l))
          (-invoke [_ a b c d e f g h i j k l m] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m))
          (-invoke [_ a b c d e f g h i j k l m n] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n))
          (-invoke [_ a b c d e f g h i j k l m n o] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o))
          (-invoke [_ a b c d e f g h i j k l m n o p] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p))
          (-invoke [_ a b c d e f g h i j k l m n o p q] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q))
          (-invoke [_ a b c d e f g h i j k l m n o p q r] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q r))
          (-invoke [_ a b c d e f g h i j k l m n o p q r s] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q r s))
          (-invoke [_ a b c d e f g h i j k l m n o p q r s t] ((state-delegate (world/managed-value home registry state-slot)) a b c d e f g h i j k l m n o p q r s t))

          IMultiFn
          (-reset [this]
            (mutate-sci-multifn! this host-reset!))
          (-add-method [this dispatch-val method]
            (mutate-sci-multifn!
             this #(host-add-method! % dispatch-val method)))
          (-remove-method [this dispatch-val]
            (mutate-sci-multifn!
             this #(host-remove-method! % dispatch-val)))
          (-prefer-method [this x y]
            (mutate-sci-multifn!
             this #(host-prefer-method! % x y)))
          (-get-method [this dispatch-val]
            (host-get-method (state-delegate (sci-multifn-state this))
                             dispatch-val))
          (-methods [this]
            (host-methods (state-delegate (sci-multifn-state this))))
          (-prefers [this]
            (host-prefers (state-delegate (sci-multifn-state this))))
          (-default-dispatch-val [this]
            (.-default-dispatch-val ^MultiFnState (sci-multifn-state this)))
          (-dispatch-fn [this]
            (.-dispatch-fn ^MultiFnState (sci-multifn-state this)))

          IEquiv
          (-equiv [this other] (identical? this other))
          IHash
          (-hash [this] (goog/getUid this))))

     (defn- sci-multifn-state [^SciMultiFn multifn]
       (world/managed-value (.-home multifn) (.-registry multifn)
                            (.-state-slot multifn)))

     (defn- mutate-sci-multifn! [^SciMultiFn multifn mutation]
       (world/managed-swap!
        (.-home multifn) (.-registry multifn) (.-state-slot multifn)
        #(mutate-state % mutation) nil
        (fn [_ _] nil) (fn [_ _ _ _] nil))
       multifn)

     (defn- managed-multifn [mm-name dispatch-fn default hierarchy]
       (let [{:keys [home registry slots managed-index]}
             (world/register-managed!
              :multifn
              [(initial-multifn-state mm-name dispatch-fn default hierarchy)]
              [0])]
         (world/attach-managed-owner!
          registry managed-index
          (SciMultiFn. home registry (nth slots 0)))))))

;; no hierarchies on cljd, dispatch is exact match with a default fallback
#?(:cljd
   (deftype SciMultiFn [mm-name dispatch-fn default method-table]
     fork/Forkable
     (fork-value [this] this)

     IFn
     (-invoke [_]
       (let [dv (dispatch-fn) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f) (no-method mm-name dv))))
     (-invoke [_ a]
       (let [dv (dispatch-fn a) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a) (no-method mm-name dv))))
     (-invoke [_ a b]
       (let [dv (dispatch-fn a b) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b) (no-method mm-name dv))))
     (-invoke [_ a b c]
       (let [dv (dispatch-fn a b c) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c) (no-method mm-name dv))))
     (-invoke [_ a b c d]
       (let [dv (dispatch-fn a b c d) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c d) (no-method mm-name dv))))
     (-invoke [_ a b c d e]
       (let [dv (dispatch-fn a b c d e) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c d e) (no-method mm-name dv))))
     (-invoke [_ a b c d e f*]
       (let [dv (dispatch-fn a b c d e f*) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f*) (no-method mm-name dv))))
     (-invoke [_ a b c d e f* g]
       (let [dv (dispatch-fn a b c d e f* g) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f* g) (no-method mm-name dv))))
     (-invoke [_ a b c d e f* g h]
       (let [dv (dispatch-fn a b c d e f* g h) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f* g h) (no-method mm-name dv))))
     (-invoke [_ a b c d e f* g h i]
       (let [dv (dispatch-fn a b c d e f* g h i) mt (cljd-method-table-value method-table) f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f* g h i) (no-method mm-name dv))))
     (-invoke-more [_ a b c d e f* g h i rest*]
       (let [dv (apply dispatch-fn a b c d e f* g h i rest*)
             mt (cljd-method-table-value method-table)
             f (or (get mt dv) (get mt default))]
         (if f (apply f a b c d e f* g h i rest*) (no-method mm-name dv))))
     (-apply [this args]
       (let [dv (apply dispatch-fn args)
             mt (cljd-method-table-value method-table)
             f (or (get mt dv) (get mt default))]
         (if f (apply f args) (no-method mm-name dv))))))

#?(:cljd
   (do
     ;; class values are registry maps, dispatch on the :class. Object is not
     ;; special here, protocol extension to Object maps to :default in the
     ;; protocol layer
     (defn normalize-dispatch-val [dispatch-val]
       (if (and (map? dispatch-val) (fn? (:instance? dispatch-val)))
         (or (:class dispatch-val)
             (throw (ex-info "Class cannot be used as a dispatch value" {})))
         dispatch-val))))

(defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (let [message (apply str "Only these options are valid: "
                         (first valid-keys)
                         (map #(str ", " %) (rest valid-keys)))]
      (throw
       #?(:cljd (ArgumentError. message)
          :clj (IllegalArgumentException. ^String message)
          :cljs (js/Error. ^string message))))))

(defn defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attr-map are optional.

  Options are key-value pairs and may be one of:

  :default

  The default dispatch value, defaults to :default

  :hierarchy

  The value used for hierarchical dispatch (e.g. ::square is-a ::shape)

  Hierarchies are type-like relationships that do not depend upon type
  inheritance. By default Clojure's multimethods dispatch off of a
  global hierarchy map.  However, a hierarchy relationship can be
  created with the derive function used to augment the root ancestor
  created with make-hierarchy.

  Multimethods expect the value of the hierarchy option to be supplied as
  a reference type e.g. a var (i.e. via the Var-quote dispatch macro #'
  or the var special form)."
  {:arglists '([name docstring? attr-map? dispatch-fn & options])}
  [_ _ mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
                      (next options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (next options)
                      options)
        dispatch-fn (first options)
        options     (next options)
        m           (if docstring
                      (assoc m :doc docstring)
                      m)
        m           (if (meta mm-name)
                      (conj (meta mm-name) m)
                      m)
        mm-name (with-meta mm-name m)]
    (when (= (count options) 1)
      (throw (new #?(:cljd Exception :clj Exception :cljs js/Error)
                  "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))

    (let [options   (apply hash-map options)
          default   (get options :default :default)
          hierarchy (get options :hierarchy #?(:cljd nil :default (global-hierarchy)))]
      (check-valid-options options :default :hierarchy)
      #?(:cljd `(~'defonce ~(with-meta mm-name m)
                 (clojure.core/multi-fn-impl ~(name mm-name) ~dispatch-fn ~default nil))
         :clj `(let [v# (def ~mm-name)]
                 (when-not (and (clojure.core/has-root-impl v#) (clojure.core/multi-fn?-impl (deref v#)))
                   (def ~mm-name
                     (clojure.core/multi-fn-impl ~(name mm-name) ~dispatch-fn ~default ~hierarchy))))
         :cljs `(defonce ~(with-meta mm-name m)
                  (clojure.core/multi-fn-impl
                   ~(symbol (name mm-name)) ~dispatch-fn ~default ~hierarchy))))))

(defn multi-fn?-impl [x]
  #?(:cljd (instance? SciMultiFn x)
     :clj (or (instance? SciMultiFn x)
              (instance? clojure.lang.MultiFn x))
     :cljs (or (instance? SciMultiFn x)
               (instance? cljs.core/MultiFn x))))

(defn multi-fn-impl [name dispatch-fn default hierarchy]
  #?(:cljd (let [method-table (atom {})]
             (world/register! method-table {})
             (SciMultiFn. name dispatch-fn default method-table))
     :default (managed-multifn name dispatch-fn default hierarchy)))

(defn multi-fn-add-method-impl
  [multifn dispatch-val f]
  #?(:cljd (do (cljd-swap-method-table!
                (.-method-table ^SciMultiFn multifn) assoc
                (normalize-dispatch-val dispatch-val) f)
               multifn)
     :default
     (if (instance? SciMultiFn multifn)
       (mutate-sci-multifn!
        multifn #(host-add-method! % dispatch-val f))
       (host-add-method! multifn dispatch-val f))))

(defn get-method-impl [multifn dispatch-val]
  #?(:cljd
     (let [mt (cljd-method-table-value
               (.-method-table ^SciMultiFn multifn))
           dispatch-val (normalize-dispatch-val dispatch-val)]
       (or (get mt dispatch-val) (get mt (.-default ^SciMultiFn multifn))))
     :default
     (host-get-method
      (if (instance? SciMultiFn multifn)
        (state-delegate (sci-multifn-state multifn))
        multifn)
      dispatch-val)))

(defn methods-impl [multifn]
  #?(:cljd (cljd-method-table-value (.-method-table ^SciMultiFn multifn))
     :default
     (host-methods
      (if (instance? SciMultiFn multifn)
        (state-delegate (sci-multifn-state multifn))
        multifn))))

(defn prefers-impl [multifn]
  #?(:cljd {}
     :default
     (host-prefers
      (if (instance? SciMultiFn multifn)
        (state-delegate (sci-multifn-state multifn))
        multifn))))

(defn remove-method-impl [multifn dispatch-val]
  #?(:cljd
     (do (cljd-swap-method-table!
          (.-method-table ^SciMultiFn multifn)
          dissoc (normalize-dispatch-val dispatch-val))
         multifn)
     :default
     (if (instance? SciMultiFn multifn)
       (mutate-sci-multifn!
        multifn #(host-remove-method! % dispatch-val))
       (host-remove-method! multifn dispatch-val))))

(defn remove-all-methods-impl [multifn]
  #?(:cljd
     (do (cljd-swap-method-table!
          (.-method-table ^SciMultiFn multifn) (constantly {}))
         multifn)
     :default
     (if (instance? SciMultiFn multifn)
       (mutate-sci-multifn! multifn host-reset!)
       (host-reset! multifn))))

(defn prefer-method-impl [multifn x y]
  #?(:cljd multifn
     :default
     (if (instance? SciMultiFn multifn)
       (mutate-sci-multifn!
        multifn #(host-prefer-method! % x y))
       (host-prefer-method! multifn x y))))

(defn defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [_x _y multifn dispatch-val & fn-tail]
  #?(:cljd
     (list 'clojure.core/multi-fn-add-method-impl multifn dispatch-val (list* 'fn fn-tail))
     :clj
     (let [multifn-str (str multifn)]
       (if (or (str/ends-with? multifn-str "print-method")
               (str/ends-with? multifn-str "simple-dispatch"))
         `(let [v# ~dispatch-val
                m# (meta v#)
                mf# (resolve '~multifn)]
            ;; TODO: what about deftype - how can we detect deftype at runtime?
            ;; Should we inject the ctx here to resolve the type? no, because type can really be dynamic?
            ;; thus we need a way to detect if a value is an instance of a record or type
            ;; or change `type` to return the deftype type, wouldn't this work?
            ;; no, because print-method looks at core type
            (if (instance? sci.lang.Type v#)
              (do
                (cond
                  (= (resolve 'clojure.pprint/simple-dispatch) mf#)
                  (alter-meta! v# assoc :sci.impl/pprint-simple-dispatch (fn ~@fn-tail))
                  (= (resolve 'clojure.core/print-method) mf#)
                  (alter-meta! v# assoc :sci.impl/print-method (fn ~@fn-tail))
                  :else (clojure.core/multi-fn-add-method-impl ~multifn ~dispatch-val (fn ~@fn-tail))))
              (clojure.core/multi-fn-add-method-impl ~multifn ~dispatch-val (fn ~@fn-tail))))
         `(clojure.core/multi-fn-add-method-impl ~multifn ~dispatch-val (fn ~@fn-tail))))
     :cljs
     (list 'clojure.core/multi-fn-add-method-impl multifn dispatch-val (list* 'fn fn-tail))))
