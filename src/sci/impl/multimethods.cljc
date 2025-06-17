(ns sci.impl.multimethods
  {:no-doc true}
  (:refer-clojure :exclude [defmulti defmethod])
  (:require
   #?(:clj [clojure.string :as str])
   [sci.ctx-store :as store]
   [sci.impl.hierarchies :refer [global-hierarchy]]))

#?(:clj (set! *warn-on-reflection* true))

(defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (let [message (apply str "Only these options are valid: "
                         (first valid-keys)
                         (map #(str ", " %) (rest valid-keys)))]
      (throw
       #?(:clj (IllegalArgumentException. ^String message)
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
  [_ _ ctx mm-name & options]
  (let [[ctx mm-name options] (if (symbol? ctx)
                                [nil ctx (cons mm-name options)]
                                [ctx mm-name options])
        docstring   (if (string? (first options))
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
      (throw (new #?(:clj Exception :cljs js/Error)
                  "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))

    (let [options   (apply hash-map options)
          default   (get options :default :default)
          hierarchy (get options :hierarchy (global-hierarchy))]
      (check-valid-options options :default :hierarchy)
      #?(:clj `(let [v# (def ~mm-name)]
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
  (instance? #?(:clj clojure.lang.MultiFn
                :cljs cljs.core/MultiFn) x))

(defn multi-fn-impl #?(:clj [name dispatch-fn default hierarchy]
                       :cljs [name dispatch-fn default hierarchy
                              method-table prefer-table method-cache cached-hierarchy])
  (new #?(:clj clojure.lang.MultiFn
          :cljs cljs.core/MultiFn) name dispatch-fn default hierarchy
       #?@(:cljs [method-table prefer-table method-cache cached-hierarchy])))

(defn multi-fn-add-method-impl
  [multifn dispatch-val f]
  #?(:clj (.addMethod ^clojure.lang.MultiFn multifn dispatch-val f)
     :cljs (-add-method multifn dispatch-val f)))

(defn defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  [_x _y multifn dispatch-val & fn-tail]
  #?(:clj
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
                  (alter-meta! (:sci.impl/var m#) assoc :sci.impl/pprint-simple-dispatch (fn ~@fn-tail))
                  (= (resolve 'clojure.core/print-method) mf#)
                  (alter-meta! (:sci.impl/var m#) assoc :sci.impl/print-method (fn ~@fn-tail))
                  :else (clojure.core/multi-fn-add-method-impl ~multifn ~dispatch-val (fn ~@fn-tail))))
              (clojure.core/multi-fn-add-method-impl ~multifn ~dispatch-val (fn ~@fn-tail))))
         `(clojure.core/multi-fn-add-method-impl ~multifn ~dispatch-val (fn ~@fn-tail))))
     :cljs
     (list 'clojure.core/multi-fn-add-method-impl multifn dispatch-val (list* 'fn fn-tail))))
