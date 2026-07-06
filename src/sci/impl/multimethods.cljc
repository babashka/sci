(ns sci.impl.multimethods
  {:no-doc true}
  (:refer-clojure :exclude [defmulti defmethod])
  (:require
   #?(:clj [clojure.string :as str])
   [sci.ctx-store :as store]
   ;; no hierarchies on cljd, like the host
   #?@(:cljd [] :default [[sci.impl.hierarchies :refer [global-hierarchy]]])))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

#?(:cljd
   (defn- no-method [mm-name dv]
     (throw (ex-info (str "No method in multimethod '" mm-name "' for dispatch value: " dv) {}))))

;; no hierarchies on cljd, dispatch is exact match with a default fallback
#?(:cljd
   (deftype SciMultiFn [mm-name dispatch-fn default method-table]
     IFn
     (-invoke [_]
       (let [dv (dispatch-fn) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f) (no-method mm-name dv))))
     (-invoke [_ a]
       (let [dv (dispatch-fn a) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a) (no-method mm-name dv))))
     (-invoke [_ a b]
       (let [dv (dispatch-fn a b) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b) (no-method mm-name dv))))
     (-invoke [_ a b c]
       (let [dv (dispatch-fn a b c) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c) (no-method mm-name dv))))
     (-invoke [_ a b c d]
       (let [dv (dispatch-fn a b c d) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c d) (no-method mm-name dv))))
     (-invoke [_ a b c d e]
       (let [dv (dispatch-fn a b c d e) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c d e) (no-method mm-name dv))))
     (-invoke [_ a b c d e f*]
       (let [dv (dispatch-fn a b c d e f*) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f*) (no-method mm-name dv))))
     (-invoke [_ a b c d e f* g]
       (let [dv (dispatch-fn a b c d e f* g) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f* g) (no-method mm-name dv))))
     (-invoke [_ a b c d e f* g h]
       (let [dv (dispatch-fn a b c d e f* g h) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f* g h) (no-method mm-name dv))))
     (-invoke [_ a b c d e f* g h i]
       (let [dv (dispatch-fn a b c d e f* g h i) mt @method-table f (or (get mt dv) (get mt default))]
         (if f (f a b c d e f* g h i) (no-method mm-name dv))))
     (-invoke-more [_ a b c d e f* g h i rest*]
       (let [dv (apply dispatch-fn a b c d e f* g h i rest*)
             mt @method-table
             f (or (get mt dv) (get mt default))]
         (if f (apply f a b c d e f* g h i rest*) (no-method mm-name dv))))
     (-apply [this args]
       (let [dv (apply dispatch-fn args)
             mt @method-table
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
         dispatch-val))
     (defn get-method-impl [^SciMultiFn multifn dispatch-val]
       (let [mt @(.-method-table multifn)
             dispatch-val (normalize-dispatch-val dispatch-val)]
         (or (get mt dispatch-val) (get mt (.-default multifn)))))
     (defn methods-impl [^SciMultiFn multifn]
       @(.-method-table multifn))
     (defn remove-method-impl [^SciMultiFn multifn dispatch-val]
       (swap! (.-method-table multifn) dissoc (normalize-dispatch-val dispatch-val))
       multifn)))

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
                  (let [method-table# (atom {})
                        prefer-table# (atom {})
                        method-cache# (atom {})
                        cached-hierarchy# (atom {})]
                    (clojure.core/multi-fn-impl ~(symbol (name mm-name)) ~dispatch-fn ~default ~hierarchy
                                                method-table# prefer-table# method-cache# cached-hierarchy#)))))))

(defn multi-fn?-impl [x]
  #?(:cljd (instance? SciMultiFn x)
     :clj (instance? clojure.lang.MultiFn x)
     :cljs (instance? cljs.core/MultiFn x)))

(defn multi-fn-impl #?(:cljd [name dispatch-fn default hierarchy]
                       :clj [name dispatch-fn default hierarchy]
                       :cljs [name dispatch-fn default hierarchy
                              method-table prefer-table method-cache cached-hierarchy])
  #?(:cljd (SciMultiFn. name dispatch-fn default (atom {}))
     :clj (new clojure.lang.MultiFn name dispatch-fn default hierarchy)
     :cljs (new cljs.core/MultiFn name dispatch-fn default hierarchy
                method-table prefer-table method-cache cached-hierarchy)))

(defn multi-fn-add-method-impl
  [multifn dispatch-val f]
  #?(:cljd (do (swap! (.-method-table ^SciMultiFn multifn) assoc
                      (normalize-dispatch-val dispatch-val) f)
               multifn)
     :clj (.addMethod ^clojure.lang.MultiFn multifn dispatch-val f)
     :cljs (-add-method multifn dispatch-val f)))

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
