(ns sci.impl.utils
  {:no-doc true}
  (:require [clojure.string :as str]))

(derive :sci.error/realized-beyond-max :sci/error)

(defn sci-error? [e]
  (isa? (:type e) :sci/error))

(defn constant? [x]
  (or (number? x) (string? x) (keyword? x)))

(defn mark-resolve-sym
  [sym]
  (vary-meta
   sym
   (fn [m]
     (assoc m
            :sci.impl/eval true))))

(defn kw-identical? [k v]
  (#?(:clj identical? :cljs keyword-identical?)
   k v))

(defn gensym*
  ([] (mark-resolve-sym (gensym)))
  ([prefix] (mark-resolve-sym (gensym prefix))))

(defn mark-eval-call
  [expr]
  (vary-meta
   expr
   (fn [m]
     (assoc m
            :sci.impl/eval-call true
            :sci.impl/eval true))))

(defn mark-eval
  [expr]
  (vary-meta
   expr
   (fn [m]
     (assoc m :sci.impl/eval true))))

(defn throw-error-with-location
  ([msg iobj] (throw-error-with-location msg iobj {}))
  ([msg iobj data]
   (let [{:keys [:row :col]} (meta iobj)
         msg (str msg
                  " [at line " row ", column " col "]") ]
     (throw (ex-info msg (merge {:type :sci/error
                                 :row row
                                 :col col} data))))))

(defn rethrow-with-location-of-node [^Exception e node]
  (if-let [m #?(:clj (.getMessage e)
                :cljs (.-message e))]
    (if (str/includes? m "[at line")
      (throw e)
      (let [{:keys [:row :col]} (meta node)]
        (if (and row col)
          (let [m (str m " [at line " row ", column " col "]")
                new-exception (let [d (ex-data e)]
                                (ex-info m (merge {:type :sci/error
                                                   :row row
                                                   :col col
                                                   :message m} d) e))]
            (throw new-exception))
          (throw e))))
    (throw e)))

(defn merge-meta
  "Only adds metadata to obj if d is not nil and if meta on obj isn't already nil."
  [obj d]
  (if d
    (if-let [m (meta obj)]
      (with-meta obj (merge m d))
      obj)
    obj))

(defn strip-core-ns [sym]
  (case (namespace sym)
    ("clojure.core" "cljs.core") (symbol (name sym))
    sym))
