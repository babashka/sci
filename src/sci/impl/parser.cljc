(ns sci.impl.parser
  {:no-doc true}
  (:refer-clojure :exclude [read-string eval])
  (:require
   [clojure.tools.reader.reader-types :as r]
   [edamame.core :as edamame]
   [sci.impl.interop :as interop]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:const eof :sci.impl.parser.edamame/eof)

(def read-eval
  (vars/new-var '*read-eval true {:ns vars/clojure-core-ns
                                  :dynamic true}))

(def data-readers
  (vars/new-var '*default-data-reader-fn* {}
                {:ns vars/clojure-core-ns
                 :dynamic true}))

(def default-data-reader-fn
  (vars/new-var '*default-data-reader-fn* nil
                {:ns vars/clojure-core-ns
                 :dynamic true}))

(def reader-resolver
  (vars/new-var '*reader-resolver* nil
                {:ns vars/clojure-core-ns
                 :dynamic true}))

(def default-opts
  (edamame/normalize-opts
   {:all true
    :read-eval false
    :row-key :line
    :col-key :column
    :read-cond :allow
    :location? (fn [obj]
                 (or
                  ;; for fine-grained error messages during analysis we also add
                  ;; locations to symbols. This adds about 10% to parse/analysis
                  ;; time.

                  ;; $ tmp/bb-only-seq-and-sym-locs tmp/meander.clj
                  ;; "Elapsed time: 120.448655 msecs"

                  ;; $ tmp/bb-only-seq-locs tmp/meander.clj
                  ;; "Elapsed time: 110.869661 msecs"

                  (symbol? obj)
                  ;; same as clojure
                  (seq? obj)))
    :end-location false}))

(defn var->sym [v]
  (when-let [m (meta v)]
    (when-let [var-name (:name m)]
      (when-let [ns (:ns m)]
        (symbol (str (vars/getName ns))
                (str var-name))))))

(defn fully-qualify [ctx sym]
  (let [env @(:env ctx)
        sym-ns (when-let [n (namespace sym)]
                 (symbol n))
        sym-name-str (name sym)
        current-ns (vars/current-ns-name)
        current-ns-str (str current-ns)
        namespaces (get env :namespaces)
        the-current-ns (get namespaces current-ns)
        aliases (:aliases the-current-ns)
        ret (if-not sym-ns
              (or (when-let [refers (:refers the-current-ns)]
                    (when-let [v (get refers sym)]
                      (var->sym v)))
                  (when-let [v (get the-current-ns sym)]
                    (var->sym v))
                  (when (or (and (contains? (get namespaces 'clojure.core) sym)
                                 ;; only valid when the symbol isn't excluded
                                 (not (some-> the-current-ns
                                              :refer
                                              (get 'clojure.core)
                                              :exclude
                                              (contains? sym ))))
                            (contains? utils/ana-macros sym))
                    (symbol "clojure.core" sym-name-str))
                  (interop/fully-qualify-class ctx sym)
                  ;; all unresolvable symbols all resolved in the current namespace
                  (symbol current-ns-str sym-name-str))
              (if (get-in env [:namespaces sym-ns])
                sym
                (if-let [ns (get aliases sym-ns)]
                  (symbol (str ns) sym-name-str)
                  sym)))]
    ret))

(defn throw-eval-read [_]
  (throw (ex-info "EvalReader not allowed when *read-eval* is false."
                  {:type :sci.error/parse})))

(defn auto-resolve [ctx opts]
  (or (:auto-resolve opts)
      (let [env (:env ctx)
            env-val @env
            current-ns (vars/current-ns-name)
            the-current-ns (get-in env-val [:namespaces current-ns])
            aliases (:aliases the-current-ns)
            auto-resolve (assoc aliases :current current-ns)]
        auto-resolve)))

(defn parse-next
  ([ctx r]
   (parse-next ctx r nil))
  ([ctx r opts]
   (let [features (:features ctx)
         readers (:readers ctx)
         readers (if (vars/var? readers) @readers readers)
         auto-resolve (auto-resolve ctx opts)
         parse-opts (cond-> (assoc default-opts
                                   :features features
                                   :auto-resolve auto-resolve
                                   :syntax-quote {:resolve-symbol #(fully-qualify ctx %)}
                                   :readers (fn [t]
                                              (or (and readers (readers t))
                                                  (@data-readers t)
                                                  (some-> (@utils/eval-resolve-state ctx {} t)
                                                          meta
                                                          :sci.impl.record/map-constructor)
                                                  (when-let [f @default-data-reader-fn]
                                                    (fn [form]
                                                      (f t form)))))
                                   :read-eval (if @read-eval
                                                (fn [x]
                                                  (utils/eval ctx x))
                                                throw-eval-read))
                      opts (merge opts))
         ret (try (let [v (edamame/parse-next r parse-opts)]
                    (if (utils/kw-identical? v :edamame.core/eof)
                      eof
                      v))
                  (catch #?(:clj clojure.lang.ExceptionInfo
                            :cljs cljs.core/ExceptionInfo) e
                    (throw (ex-info #?(:clj (.getMessage e)
                                       :cljs (.-message e))
                                    (assoc (ex-data e)
                                           :type :sci.error/parse
                                           :phase "parse"
                                           :file @vars/current-file)
                                    e))))]
     ret)))

(defn reader [x]
  #?(:clj (r/indexing-push-back-reader (r/push-back-reader x))
     :cljs (let [string-reader (r/string-reader x)
                 buf-len 1
                 pushback-reader (r/PushbackReader. string-reader
                                                    (object-array buf-len)
                                                    buf-len buf-len)]
             (r/indexing-push-back-reader pushback-reader))))

(defn get-line-number [reader]
  (r/get-line-number reader))

(defn get-column-number [reader]
  (r/get-column-number reader))

(defn parse-string
  ([ctx s]
   (let [r (reader s)
         v (parse-next ctx r)]
     (if (utils/kw-identical? eof v) nil v))))

;;;; Scratch

(comment
  )
