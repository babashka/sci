(ns sci.impl.load
  {:no-doc true}
  (:refer-clojure :exclude [loaded-libs load-reader load-string])
  (:require
   [clojure.string :as str]
   [clojure.tools.reader.reader-types :as r]
   [sci.ctx-store :as store]
   [sci.impl.parser :as parser]
   [sci.impl.types :as types]
   [sci.impl.utils :as utils :refer [kw-identical? throw-error-with-location]]
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

#?(:cljs
   (defn handle-js-lib [env opts lib cnn the-lib]
     (let [path (:path opts)
           the-lib (if path
                     (reduce (fn [the-lib path]
                               (js/Reflect.get the-lib path)) the-lib (.split path "."))
                     the-lib)
           clazz (symbol (munge (str lib (when path
                                           "$") path)))
           env (-> env
                   (assoc-in [:class->opts clazz :class] the-lib)
                   (assoc-in [:raw-classes clazz] the-lib))
           env (if-let [alias (:as opts)]
                 (assoc-in env [:namespaces cnn :imports alias] clazz)
                 env)
           env (if-let [refers (:refer opts)]
                 (let [rename (:rename opts)]
                   (reduce (fn [env refer]
                             (let [sub-sym (symbol (str lib "$$" (str refer)))
                                   the-sublib (js/Reflect.get the-lib (str refer))
                                   refer (get rename refer refer)]
                               (-> env
                                   (assoc-in [:namespaces cnn :imports refer] sub-sym)
                                   (update-in [:namespaces cnn :refer 'clojure.core :exclude] (fnil conj #{}) refer)
                                   (assoc-in [:class->opts sub-sym :class] the-sublib)
                                   (assoc-in [:raw-classes sub-sym] the-sublib))))
                           env refers))
                 env)]
       env)))

#?(:cljs
   (defn lib+path [lib]
     (str/split lib (re-pattern "\\$") 2)))

(defn handle-require-libspec-env
  [_ctx env current-ns the-loaded-ns lib-name
   {:keys [:as :refer #?(:cljs :refer-macros) :rename :exclude :only :use] :as #?(:clj _opts :cljs opts)}]
  (or
   #?(:cljs
      (when (string? lib-name)
        (let [[lib-name path] (lib+path lib-name)]
          (if-let [the-lib (get (:js-libs env) lib-name)]
            (handle-js-lib env (assoc opts :path path) lib-name current-ns the-lib)
            env))))
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
         #?@(:cljs [refer (if refer-macros
                            (if (or (nil? refer)
                                    (coll? refer))
                              (into refer refer-macros)
                              ;; assume :all
                              refer)
                            refer)])
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
                                       ":refer value must be a sequential collection of symbols")))
               use (handle-refer-all the-current-ns the-loaded-ns include-sym? rename-sym only)
               :else the-current-ns)
         env (assoc-in env [:namespaces current-ns] the-current-ns)]
     (when-let [on-loaded (some-> the-loaded-ns :obj meta :sci.impl/required-fn)]
       (on-loaded {}))
     env)))

(defn loaded-libs [env]
  @(get-in env '[:namespaces clojure.core *loaded-libs*]))

(defn add-loaded-lib [env lib]
  #?(:clj
     (dosync (alter (loaded-libs env) conj lib))
     :cljs
     (swap! (loaded-libs env) conj lib))
  nil)


(defn load-reader*
  "Low level load-reader* that doesn't install any bindings"
  [ctx reader]
  (let [reader
        ;; TODO: move this check to edamame
        (if #?(:clj (instance? clojure.tools.reader.reader_types.IndexingReader reader)
               :cljs (implements? r/IndexingReader reader))
          reader
          (r/indexing-push-back-reader reader))]
    (loop [ret nil]
      (let [x (parser/parse-next ctx reader)]
        (if (utils/kw-identical? parser/eof x)
          ret
          (recur (utils/eval ctx x)))))))

(defn load-reader [reader]
  (let [ctx (store/get-ctx)]
    (vars/with-bindings (utils/load-thread-bindings ctx {utils/current-ns @utils/current-ns})
      (load-reader* ctx reader))))

(defn load-string [s]
  (let [rdr (r/indexing-push-back-reader (r/string-push-back-reader s))]
    (load-reader rdr)))

(defn handle-require-libspec
  [ctx lib opts]
  (let [env* (:env ctx)
        env @env*
        cnn (utils/current-ns-name)
        lib (get (:ns-aliases env) lib lib)
        #?@(:cljs [js-lib? (string? lib)])]
    (or #?(:cljs
           (when js-lib?
             (let [[lib path] (lib+path lib)]
               (when-let [the-lib (get (:js-libs env) lib)]
                 (swap! env* (fn [env]
                               (handle-js-lib env (assoc opts :path path) lib cnn the-lib)))
                 {}))))
        (if-let [as-alias (:as-alias opts)]
          (reset! env* (handle-require-libspec-env ctx env cnn nil lib {:as as-alias}))
          (let [{:keys [:reload :reload-all]} opts
                namespaces (get env :namespaces)
                reload* (or reload reload-all (:reload-all ctx))]
            (if-let [the-loaded-ns (when-not reload* (get namespaces lib))]
              (let [loading (:loading ctx)]
                (if (and loading
                         (not (contains? @(loaded-libs env) lib))
                         (nat-int? #?(:clj (.indexOf ^clojure.lang.PersistentVector loading lib)
                                      :cljs (.indexOf loading lib))))
                  (throw-error-with-location
                   (let [lib-emphasized (str "[ " lib " ]")
                         loading (conj loading lib)
                         loading (replace {lib lib-emphasized} loading)]
                     (str "Cyclic load dependency: " (str/join "->" loading)))
                   lib)
                  (when-not (= lib cnn)
                    (reset! env* (handle-require-libspec-env ctx env cnn the-loaded-ns lib opts)))))
              (if-let [load-fn (:load-fn env)]
                (let [curr-ns @utils/current-ns]
                  (if-let [{:keys [file source handled]}
                           (load-fn {:namespace lib ;; old name
                                     :libname lib
                                     :ctx ctx
                                     :opts opts
                                     :ns (types/getName curr-ns)
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
                        (when source
                          (try (vars/with-bindings
                                 (utils/load-thread-bindings ctx {utils/current-ns curr-ns
                                                                  utils/current-file file})
                                 (@utils/eval-string* ctx source))
                               (catch #?(:clj Exception :cljs js/Error) e
                                 (swap! env* update :namespaces dissoc lib)
                                 (throw e)))))
                      (when-not handled
                        (swap! env* (fn [env]
                                      (let [namespaces (get env :namespaces)
                                            the-loaded-ns (get namespaces lib)]
                                        (handle-require-libspec-env ctx env cnn
                                                                    the-loaded-ns
                                                                    lib opts))))))
                    (or (when reload*
                          (when-let [the-loaded-ns (get namespaces lib)]
                            (reset! env* (handle-require-libspec-env ctx env cnn the-loaded-ns lib opts))))
                        (throw (new #?(:clj Exception :cljs js/Error)
                                    (str "Could not find namespace: " lib "."))))))
                (throw (new #?(:clj Exception :cljs js/Error)

                            (str "Could not find namespace " lib ".")))))
            #?(:clj (add-loaded-lib env lib)
               :cljs (when-not js-lib?
                       (add-loaded-lib env lib)))
            nil)))))

(defn load-lib* [ctx prefix lib options]
  (when (and prefix (pos? (.indexOf (name lib) #?(:clj (int \.)
                                                  :cljs \.))))
    (throw-error-with-location (str "Found lib name '" (name lib) "' containing period with prefix '"
                                    prefix "'.  lib names inside prefix lists must not contain periods")
                               lib))
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)]
    (handle-require-libspec ctx lib opts)))

#?(:clj
   (let [load-lock (Object.)]
     (defn load-lib [ctx prefix lib & options]
       (locking load-lock
         (load-lib* ctx prefix lib options))))
   :cljs
   (defn load-lib [ctx prefix lib & options]
     (load-lib* ctx prefix lib options)))

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
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer #?(:cljs :require-macros)}
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

(defn eval-require-macros
  [ctx & args]
  (load-libs ctx :require-macros args))

(defn eval-use
  [ctx & args]
  (load-libs ctx :use args))

(defn eval-refer-clojure [ctx exprs]
  (let [ns-sym 'clojure.core]
    (loop [exprs exprs]
      (when exprs
        (let [[k v] exprs]
          (case k
            :exclude
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (utils/current-ns-name)]
                       (update-in env [:namespaces cnn :refer ns-sym :exclude]
                                  (fnil into #{}) v))))
            :only
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (utils/current-ns-name)
                           other-ns (get-in env [:namespaces ns-sym])
                           other-vars (select-keys other-ns v)]
                       ;; TODO: this is wrong, don't merge these vars into the current namespace
                       (update-in env [:namespaces cnn]
                                  merge other-vars))))
            :rename
            (swap! (:env ctx)
                   (fn [env]
                     (let [cnn (utils/current-ns-name)
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
  (let [cnn (utils/current-ns-name)
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
                               (assoc referred (or (rename sym) sym) v))
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
