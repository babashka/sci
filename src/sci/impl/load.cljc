(ns sci.impl.load
  {:no-doc true}
  (:require
   [clojure.string :as str]
   [sci.impl.utils :as utils :refer [throw-error-with-location
                                     kw-identical?]]
   [sci.impl.vars :as vars]))

(defn handle-refer-all [the-current-ns the-loaded-ns include-sym? rename-sym only]
  (let [referred (:refers the-current-ns)
        only (when only (set only))
        referred (reduce (fn [ns [k v]]
                           (if (and (symbol? k) (include-sym? k)
                                    (or (not only)
                                        (contains? only k)))
                             (assoc ns (rename-sym k) v)
                             ns))
                         referred
                         the-loaded-ns)]
    (assoc the-current-ns :refers referred)))

(defn handle-require-libspec-env
  [ctx env current-ns the-loaded-ns lib-name
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
                    (let [referred (:refers the-current-ns)
                          referred (reduce (fn [ns sym]
                                             (if (include-sym? sym)
                                               (assoc ns (rename-sym sym)
                                                      (if-let [[_k v] (find the-loaded-ns sym)]
                                                        v
                                                        (throw (new #?(:clj Exception :cljs js/Error)
                                                                    (str sym " does not exist")))))
                                               ns))
                                           referred
                                           refer)]
                      (assoc the-current-ns :refers referred))
                    :else (throw (new #?(:clj Exception :cljs js/Error)
                                      (str ":refer value must be a sequential collection of symbols"))))
              use (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym only)
              :else the-current-ns)
        env (assoc-in env [:namespaces current-ns] the-current-ns)]
    (when-let [on-loaded (some-> the-loaded-ns :obj meta :sci.impl/required-fn)]
      (on-loaded {}))
    env))

(defn add-loaded-lib [env lib]
  (swap! env update :loaded-libs (fn [loaded-libs]
                                   (if (nil? loaded-libs)
                                     #{lib}
                                     (conj loaded-libs lib))))
  nil)

(defn handle-require-libspec
  [ctx lib opts]
  (let [{:keys [:reload :reload-all]} opts
        env* (:env ctx)
        env @env* ;; NOTE: loading namespaces is not (yet) thread-safe
        cnn (vars/current-ns-name)
        namespaces (get env :namespaces)
        reload* (or reload reload-all (:reload-all ctx))]
    (if-let [the-loaded-ns (when-not reload* (get namespaces lib))]
      (let [loading (:loading ctx)]
        (if (and loading
                 (not (contains? (:loaded-libs env) lib))
                 (nat-int? #?(:clj (.indexOf ^clojure.lang.PersistentVector loading lib)
                              :cljs (.indexOf loading lib))))
          (throw-error-with-location
           (let [lib-emphasized (str "[ " lib " ]")
                 loading (conj loading lib)
                 loading (replace {lib lib-emphasized} loading)]
             (str "Cyclic load dependency: " (str/join "->" loading)))
           lib)
          (reset! env* (handle-require-libspec-env ctx env cnn the-loaded-ns lib opts))))
      (if-let [load-fn (:load-fn env)]
        (if-let [{:keys [:file :source]} (load-fn {:namespace lib
                                                   :reload (or reload reload-all)})]
          (do
            ;; (.println System/err "source")
            ;; (.println System/err source)
            (let [ctx (-> ctx
                          (assoc :bindings {})
                          (assoc :reload-all reload-all)
                          (update :loading (fn [loading]
                                             (if (nil? loading)
                                               [lib]
                                               (conj loading lib)))))]
              (try (vars/with-bindings
                     {vars/current-ns @vars/current-ns
                      vars/current-file file}
                     (@utils/eval-string* ctx source))
                   (catch #?(:clj Exception :cljs js/Error) e
                     (swap! env* update :namespaces dissoc lib)
                     (throw e))))
            (swap! env* (fn [env]
                          (let [namespaces (get env :namespaces)
                                the-loaded-ns (get namespaces lib)]
                            (handle-require-libspec-env ctx env cnn
                                                        the-loaded-ns
                                                        lib opts)))))
          (or (when reload*
                (when-let [the-loaded-ns (get namespaces lib)]
                  (reset! env* (handle-require-libspec-env ctx env cnn the-loaded-ns lib opts))))
              (throw (new #?(:clj Exception :cljs js/Error)
                          (str "Could not find namespace: " lib ".")))))
        (throw (new #?(:clj Exception :cljs js/Error)

                    (str "Could not find namespace " lib ".")))))
    (add-loaded-lib env* lib)
    nil))

(defn load-lib [ctx prefix lib & options]
  (when (and prefix (pos? (.indexOf (name lib) #?(:clj (int \.)
                                                  :cljs \.))))
    (throw-error-with-location (str "Found lib name '" (name lib) "' containing period with prefix '"
                                    prefix "'.  lib names inside prefix lists must not contain periods")
                               lib))
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)]
    (handle-require-libspec ctx lib opts)))

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

(defn- load-libs
  "Loads libs, evaling libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [ctx kw args]
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
    (doseq [arg args*]
      (if (libspec? arg)
        (apply load-lib ctx nil (prependss arg opts))
        (let [[prefix & args*] arg]
          (when (nil? prefix)
            (throw-error-with-location "prefix cannot be nil"
                                       args))
          (doseq [arg args*]
            (apply load-lib ctx prefix (prependss arg opts))))))))

(defn eval-require
  [ctx & args]
  (load-libs ctx :require args))

(vreset! utils/eval-require-state eval-require)

(defn eval-use
  [ctx & args]
  (load-libs ctx :use args))

(vreset! utils/eval-use-state eval-use)

(defn eval-refer-clojure [ctx exprs]
  (let [ns-sym 'clojure.core]
    (loop [exprs exprs]
      (when exprs
        (let [[k v] exprs]
          (case k
            :exclude
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (vars/current-ns-name)]
                       (update-in env [:namespaces cnn :refer ns-sym :exclude]
                                  (fnil into #{}) v))))
            :only
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (vars/current-ns-name)
                           other-ns (get-in env [:namespaces ns-sym])
                           other-vars (select-keys other-ns v)]
                       ;; TODO: this is wrong, don't merge these vars into the current namespace
                       (update-in env [:namespaces cnn]
                                  merge other-vars))))
            :rename
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (vars/current-ns-name)
                           namespaces (:namespaces env)
                           the-current-ns (get namespaces cnn)
                           other-ns (get-in env [:namespaces ns-sym])
                           the-current-ns
                           (reduce (fn [acc [original-name new-name]]
                                     (-> acc
                                         (assoc-in [:refers new-name] (get other-ns original-name))
                                         (update-in [:refer ns-sym :exclude] (fnil conj #{}) original-name)))
                                   the-current-ns
                                   v)]
                       (assoc-in env [:namespaces cnn] the-current-ns)))))
          (recur (nnext exprs)))))))

(defn eval-refer* [env ns-sym filters]
  env
  (let [cnn (vars/current-ns-name)
        namespaces (:namespaces env)
        ns (or (get namespaces ns-sym)
               (throw (new #?(:clj Exception :cljs js/Error)
                           (str "No namespace: " ns-sym))))
        fs (apply hash-map filters)
        public-keys (filter symbol? (keys ns))
        rename (or (:rename fs) {})
        exclude (set (:exclude fs))
        to-do (if (= :all (:refer fs))
                public-keys
                (or (:refer fs) (:only fs) public-keys))
        _ (when (and to-do (not (sequential? to-do)))
            (throw (new #?(:clj Exception :cljs js/Error)
                        ":only/:refer value must be a sequential collection of symbols")))
        the-current-ns (get namespaces cnn)
        referred (:refers the-current-ns)
        referred (reduce (fn [referred sym]
                           (if-not (exclude sym)
                             (let [v (get ns sym)]
                               (when-not v
                                 (throw (new #?(:clj java.lang.IllegalAccessError
                                                :cljs js/Error)
                                             ;; TODO: handle private vars
                                             (if false ;; (get (ns-interns ns) sym)
                                               (str sym " is not public")
                                               (str sym " does not exist")))))
                               (assoc referred (or (rename sym) sym) v ))
                             referred))
                         referred
                         to-do)
        the-current-ns (assoc the-current-ns :refers referred)
        namespaces (assoc namespaces cnn the-current-ns)
        env (assoc env :namespaces namespaces)]
    env))

(defn eval-refer
  "The function equivalent of :refer is handled differently than what we
  did before (this is more like what Clojure itself does.) For
  referring clojure.core we still use the old code."
  [ctx ns-sym & filters]
  (if (= 'clojure.core ns-sym)
    (eval-refer-clojure ctx filters)
    (swap! (:env ctx) eval-refer* ns-sym filters))
  nil)

(vreset! utils/eval-refer-state eval-refer)
