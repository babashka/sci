(ns sci.async
  (:require
   [sci.core :as sci]
   [sci.impl.copy-vars]
   [sci.impl.load :as load]
   [sci.impl.namespaces]
   [sci.impl.vars])
  (:refer-clojure :exclude [require]))

(declare eval-string*)

(defn- handle-libspecs [ctx libspecs]
  (let [last-ns (:last-ns ctx)]
    (if (seq libspecs)
      (let [fst (first libspecs)
            [libname & opts] (if (symbol? fst)
                               [fst] fst)
            libname (if (= 'cljs.core libname)
                      'clojure.core libname)
            env* (:env ctx)
            cnn (sci/ns-name @last-ns)]
        (if (and (symbol? libname)
                 (sci/find-ns ctx libname))
          ;; library already loaded, the only thing left to do is process the options, we can defer to load
          (do (sci/binding [sci/ns @last-ns]
                (apply load/load-lib ctx nil libname opts))
              (handle-libspecs ctx (rest libspecs)))
          (if-let [load-fn (:async-load-fn @env*)]
            (let [opts (apply hash-map opts)
                  [libname* path] (if (string? libname)
                                    (load/lib+path libname)
                                    libname)]
              (.then (js/Promise.resolve (load-fn {:ns (sci/ns-name @last-ns)
                                                   :ctx ctx
                                                   :libname libname*
                                                   :property-path path
                                                   :opts opts}))
                     (fn [res]
                       (let [ctx (or (:ctx res) ctx)]
                         (if (:handled res)
                           (handle-libspecs ctx (rest libspecs))
                           ;; TODO: handle return value
                           (let [handle-opts
                                 (fn [ctx res]
                                   (let [libname (or (:libname res) libname)
                                         env* (:env ctx)]
                                     (swap! env*
                                            (fn [env]
                                              (let [namespaces (get env :namespaces)
                                                    the-loaded-ns (get namespaces libname)]
                                                (load/handle-require-libspec-env ctx env cnn
                                                                                 the-loaded-ns
                                                                                 libname opts))))))]
                             (if-let [src (:source res)]
                               (let [curr-ns @last-ns]
                                 (.then (eval-string* ctx src)
                                        (fn []
                                          (vreset! last-ns curr-ns)
                                          (handle-opts ctx res)
                                          (handle-libspecs ctx (rest libspecs)))))
                               (do
                                 (handle-opts ctx res)
                                 (handle-libspecs ctx (rest libspecs))))))))))
            (do (sci/binding [sci/ns @last-ns]
                  (apply load/load-lib ctx nil libname opts))
                (handle-libspecs ctx (rest libspecs))))))
      (js/Promise.resolve nil #_ns-obj))))

(defn ^:private eval-ns-form [ctx ns-form]
  (let [last-ns (:last-ns ctx)
        [_ns ns-name & ns-forms] ns-form
        grouped (group-by (fn [ns-form]
                            (and (seq? ns-form)
                                 (= :require (first ns-form)))) ns-forms)
        require-forms (get grouped true)
        other-forms (get grouped false)
        ;; ignore all :require-macros for now
        other-forms (remove #(and (seq? %) (= :require-macros (first %)))
                            other-forms)
        ;; TODO: there might be a more efficient way
        ns-obj (sci/binding [sci/ns @last-ns]
                 (sci/eval-form ctx (list 'do (list* 'ns ns-name other-forms) '*ns*)))
        _ (vreset! last-ns ns-obj)
        libspecs (mapcat rest require-forms)]
    (handle-libspecs ctx libspecs)))

(declare await await?)

(defn eval-string*
  [ctx s]
  (let [rdr (sci/reader s)
        last-ns (or (:last-ns ctx) (volatile! @sci/ns))
        ctx (assoc ctx :last-ns last-ns)
        eval-next (fn eval-next [res]
                    (let [continue #(sci/binding [sci/ns @last-ns]
                                      (let [form (sci/parse-next ctx rdr)]
                                        (if (= :sci.core/eof form)
                                          (js/Promise.resolve res)
                                          (if (seq? form)
                                            (if (= 'ns (first form))
                                              (eval-next (await (eval-ns-form ctx form)))
                                              (eval-next
                                               (sci/eval-form ctx form)))
                                            (eval-next (sci/eval-form ctx form))))))]
                      (if (or (not (instance? js/Promise res))
                              (await? res))
                        ;; flatten awaited promise or non-promise
                        (.then (js/Promise.resolve res)
                               continue)
                        ;; do not flatten promise results from evaluated results
                        (continue))))]
    (eval-next nil)))

(defn eval-string+
  "Same as eval-string* but returns map with `:val`, the evaluation
  result, and `:ns`, the last active namespace. The return value can
  be passed back into `opts` to preserve the namespace state."
  ([ctx s] (eval-string+ ctx s nil))
  ([ctx s opts]
   (let [last-ns (volatile! (or (when opts (:ns opts))
                                @sci/ns))
         ctx (assoc ctx :last-ns last-ns)]
     (.then (eval-string* ctx s)
            (fn [v]
              {:val v
               :ns @last-ns})))))

(defn await
  "Mark promise to be flatteded into top level async evaluation, similar
  to top level await."
  [promise]
  (set! (.-__sci_await ^js promise) true)
  promise)

(defn await?
  "Check if promise was marked with `await`."
  [promise]
  (.-__sci_await ^js promise))

(defn- require* [ctx & libspecs]
  (let [ctx (assoc ctx :last-ns (or (:last-ns ctx)
                                    (volatile! @sci/ns)))
        p (handle-libspecs ctx libspecs)]
    (await p)))

(def require
  "Async require that can be substituted for sync require by
  `{:namespaces {'clojure.core {'require scia/require}}}`"
  (sci.impl.copy-vars/new-var 'require require* true))
