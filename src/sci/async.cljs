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
            cnn (sci/ns-name @last-ns)
            [libname* path] (if (string? libname)
                              (load/lib+path libname)
                              [libname])]
        (if (or (and (symbol? libname)
                     (sci/find-ns ctx libname))
                (and (string? libname)
                     (get (:js-libs @env*) libname*)))
          ;; library already loaded, the only thing left to do is process the options, we can defer to load
          (do (sci/binding [sci/ns @last-ns]
                (apply load/load-lib ctx nil libname opts))
              (handle-libspecs ctx (rest libspecs)))
          (if-let [load-fn (:async-load-fn @env*)]
            (let [opts (apply hash-map opts)]
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
    (.then (handle-libspecs ctx libspecs)
           (fn [_] nil))))

(declare await await?)

(defn- -eval-form
  ([ctx form] (-eval-form ctx form false false))
  ([ctx form wrap? no-bind?]
   (let [last-ns* (:last-ns ctx)
         last-ns (or last-ns* (volatile! @sci/ns))
         ctx (if last-ns* ctx (assoc ctx :last-ns last-ns))
         eval-next (fn eval-next [remaining]
                     (let [[form remaining] (when (seq remaining)
                                              [(first remaining) (next remaining)])
                           f (fn []
                               (js/Promise.resolve
                                (if (seq? form)
                                  (let [fst (first form)]
                                    (if (= 'ns fst)
                                      (if remaining
                                        (.then (eval-ns-form ctx form)
                                               #(eval-next remaining))
                                        (let [v (await (eval-ns-form ctx form))]
                                          (if wrap?
                                            (.then v
                                                   (fn [v]
                                                     {:val v
                                                      :ns @last-ns}))
                                            v)
                                          ))
                                      (if (= 'do fst)
                                        (eval-next (next form))
                                        (if remaining
                                          (let [v (sci/eval-form ctx form)]
                                            (if (await? v)
                                              (.then v #(eval-next remaining))
                                              (eval-next remaining)))
                                          (let [v (sci/eval-form ctx form)]
                                            (if wrap?
                                              {:val v
                                               :ns @last-ns}
                                              v))))))
                                  (if remaining
                                    (do (sci/eval-form ctx form)
                                        (eval-next remaining))
                                    (let [v (sci/eval-form ctx form)]
                                      (if wrap?
                                        {:val v
                                         :ns @last-ns}
                                        v))))))]
                       (if no-bind? (f)
                           (sci/binding [sci/ns @last-ns]
                             (f)))))]
     (eval-next [form]))))

(defn eval-form
  "Eval single form in ctx."
  [ctx form]
  (-eval-form ctx form))

(defn eval-form+
  "Eval single form in ctx, return map of `:val` and `:ns`."
  ([ctx s] (eval-form+ ctx s nil))
  ([ctx form opts]
   (let [last-ns (volatile! (or (when opts (:ns opts))
                                @sci/ns))
         ctx (assoc ctx :last-ns last-ns)]
     (-eval-form ctx form true false))))

(defn- -eval-string
  ([ctx s] (-eval-string ctx s false))
  ([ctx s wrap?]
   (let [rdr (sci/reader s)
         last-ns (or (:last-ns ctx) (volatile! @sci/ns))
         ctx (assoc ctx :last-ns last-ns)
         eval-next (fn eval-next [res]
                     (let [continue #(sci/binding [sci/ns @last-ns]
                                       (let [form (sci/parse-next ctx rdr)]
                                         (if (= :sci.core/eof form)
                                           (if wrap?
                                             (js/Promise.resolve {:val res
                                                                  :ns @last-ns})
                                             (js/Promise.resolve res))
                                           (.then (-eval-form ctx form true true)
                                                  (fn [v] (eval-next (:val v)))))))]
                       (if (or (not (instance? js/Promise res))
                               (await? res))
                         ;; flatten awaited promise or non-promise
                         (.then (js/Promise.resolve res)
                                continue)
                         ;; do not flatten promise results from evaluated results
                         (continue))))]
     (eval-next nil))))

(defn eval-string*
  [ctx s]
  (-eval-string ctx s))

(defn eval-string+
  "Same as eval-string* but returns map with `:val`, the evaluation
  result, and `:ns`, the last active namespace. The return value can
  be passed back into `opts` to preserve the namespace state."
  ([ctx s] (eval-string+ ctx s nil))
  ([ctx s opts]
   (let [last-ns (volatile! (or (when opts (:ns opts))
                                @sci/ns))
         ctx (assoc ctx :last-ns last-ns)]
     (-eval-string ctx s true))))

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
