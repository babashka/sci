(ns sci.impl.load-async
  (:require
   [clojure.string :as str]
   [sci.impl.faster :as faster :refer [get-2 deref-1]]
   [sci.impl.fns :as fns]
   [sci.impl.interop :as interop]
   [sci.impl.macros :as macros]
   [sci.impl.records :as records]
   [sci.impl.types :as t]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     rethrow-with-location-of-node
                                     set-namespace!
                                     kw-identical?
                                     ]]
   [sci.impl.vars :as vars]))

(declare eval-string*)

(defn handle-refer-all [the-current-ns the-loaded-ns include-sym? rename-sym only]
  (let [only (when only (set only))]
    (reduce (fn [ns [k v]]
              (if (and (symbol? k) (include-sym? k)
                       (or (not only)
                           (contains? only k)))
                (assoc ns (rename-sym k) v)
                ns))
            the-current-ns
            the-loaded-ns)))

(defn handle-require-libspec-env
  [ctx callback env current-ns the-loaded-ns lib-name
   {:keys [:as :refer :rename :exclude :only :use] :as _parsed-libspec}]
  (let [the-current-ns (get-in env [:namespaces current-ns]) ;; = ns-data?
        the-current-ns (if as (assoc-in the-current-ns [:aliases as] lib-name)
                           the-current-ns)
        rename-sym (if rename (fn [sym] (or (rename sym) sym))
                       identity)
        include-sym? (if exclude
                       (let [excludes (set exclude)]
                         (fn [sym]
                           (not (contains? excludes sym))))
                       (constantly true))
        the-current-ns
        (cond refer
              (cond (or (kw-identical? :all refer)
                        use)
                    (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym nil)
                    (sequential? refer)
                    (reduce (fn [ns sym]
                              (if (include-sym? sym)
                                (assoc ns (rename-sym sym)
                                       (if-let [[_k v] (find the-loaded-ns sym)]
                                         v
                                         (when-not (:uberscript ctx)
                                           (throw (new #?(:clj Exception :cljs js/Error)
                                                       (str sym " does not exist"))))))
                                ns))
                            the-current-ns
                            refer)
                    :else (throw (new #?(:clj Exception :cljs js/Error)
                                      (str ":refer value must be a sequential collection of symbols"))))
              use (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym only)
              :else the-current-ns)
        env (assoc-in env [:namespaces current-ns] the-current-ns)]
    (when-let [on-loaded (some-> the-loaded-ns :obj meta :sci.impl/required-fn)]
      (on-loaded {}))
    env))

(defn handle-require-libspec
  [ctx callback lib opts]
  (let [{:keys [:reload]} opts
        env* (:env ctx)
        env @env* ;; NOTE: loading namespaces is not (yet) thread-safe
        cnn (vars/current-ns-name)
        namespaces (get env :namespaces)
        uberscript (:uberscript ctx)
        reload* (or reload uberscript)]
    (if-let [the-loaded-ns (when-not reload* (get namespaces lib))]
      (reset! env* (handle-require-libspec-env ctx callback env cnn the-loaded-ns lib opts))
      (if-let [load-fn (:load-fn env)]
        (let [cb (fn [v]
                   (if (:error v)
                     (callback v)
                     (if-let [{:keys [:file :source]} (:value v)]
                       (do
                         (try (vars/with-bindings
                                {vars/current-ns @vars/current-ns
                                 vars/current-file file}
                                (@utils/eval-string* (assoc ctx :bindings {}) source))
                              (catch #?(:clj Exception :cljs js/Error) e
                                (swap! env* update :namespaces dissoc lib)
                                (throw e)))
                         (swap! env* (fn [env]
                                       (let [namespaces (get env :namespaces)
                                             the-loaded-ns (get namespaces lib)]
                                         (handle-require-libspec-env ctx callback env cnn
                                                                     the-loaded-ns
                                                                     lib opts))))
                         (callback {:value nil}))
                       (or (when reload*
                             (when-let [the-loaded-ns (get namespaces lib)]
                               (reset! env* (handle-require-libspec-env ctx callback env cnn the-loaded-ns lib opts))))
                           (throw (new #?(:clj Exception :cljs js/Error)
                                       (str "Could not find namespace: " lib ".")))))))]
          (load-fn {:namespace lib
                    :reload reload
                    :callback cb}))
        (throw (new #?(:clj Exception :cljs js/Error)

                    (str "Could not find namespace " lib ".")))))))

(defn load-lib [ctx callback prefix lib & options]
  (when (and prefix (pos? (.indexOf (name lib) #?(:clj (int \.)
                                                  :cljs \.))))
    (throw-error-with-location (str "Found lib name '" (name lib) "' containing period with prefix '"
                                    prefix "'.  lib names inside prefix lists must not contain periods")
                               lib))
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)]
    (handle-require-libspec ctx callback lib opts)))

(defn- prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn load-libs*
  [ctx callback prefix kw opts args]
  (if (seq args)
    (let [arg (first args)]
      (if (libspec? arg)
        (let [cb (fn [v]
                   (if (:error v)
                     (callback v)
                     (load-libs* ctx callback prefix kw opts (rest args))))]
          (apply load-lib ctx cb prefix (prependss arg opts)))
        (let [[prefix & args*] arg]
          (when (nil? prefix)
            (throw-error-with-location "prefix cannot be nil"
                                       args))
          (load-libs* ctx callback prefix kw opts args*))))
    (callback {:value nil})))

(defn- load-libs
  "Loads libs, evaling libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [ctx callback kw args]
  (let [args* (cons kw args)
        flags (filter keyword? args*)
        opts (interleave flags (repeat true))
        args* (filter (complement keyword?) args*)]
    ;; check for unsupported options
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer}
          unsupported (seq (remove supported flags))]
      (when unsupported
        (throw-error-with-location (apply str "Unsupported option(s) supplied: "
                                          (interpose \, unsupported))
                                   ;; best effort location
                                   args)))
    ;; check a load target was specified
    (when-not (seq args*)
      (throw-error-with-location "Nothing specified to load"
                                 args))
    (load-libs* ctx callback nil kw opts args)))

(defn eval-require
  [ctx callback args]
  (load-libs ctx callback :require args))

