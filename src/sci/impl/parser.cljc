(ns sci.impl.parser
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   [clojure.tools.reader.reader-types :as r]
   [sci.impl.interop :as interop]
   [sci.impl.parser.edamame :as parser]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(def ^:const eof :sci.impl.parser.edamame/eof)

(def default-opts
  {:read-eval false
   :read-cond :allow})

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
              (or (when (or (and (contains? (get namespaces 'clojure.core) sym)
                                 ;; only valid when the symbol isn't excluded
                                 (not (some-> the-current-ns
                                              :refer
                                              (get 'clojure.core)
                                              :exclude
                                              (contains? sym ))))
                            (contains? utils/ana-macros sym))
                    (symbol "clojure.core" sym-name-str))
                  (interop/fully-qualify-class ctx sym)
                  (when-let [v (get the-current-ns sym)]
                    (when-let [m (meta v)]
                      (when-let [var-name (:name m)]
                        (when-let [ns (:ns m)]
                          (symbol (str (vars/getName ns))
                                  (str var-name))))))
                  ;; all unresolvable symbols all resolved in the current namespace
                  (symbol current-ns-str sym-name-str))
              (if (get-in env [:namespaces sym-ns])
                sym
                (if-let [ns (get aliases sym-ns)]
                  (symbol (str ns) sym-name-str)
                  sym)))]
    ret))

(defn parse-next
  ([r]
   (parser/parse-next default-opts r))
  ([ctx r]
   (parse-next ctx r nil))
  ([ctx r opts]
   (let [features (:features ctx)
         readers (:readers ctx)
         readers (if (vars/var? readers) @readers readers)
         env (:env ctx)
         env-val @env
         current-ns (vars/current-ns-name)
         the-current-ns (get-in env-val [:namespaces current-ns])
         aliases (:aliases the-current-ns)
         auto-resolve (assoc aliases :current current-ns)
         parse-opts (cond-> (assoc default-opts
                                   :features features
                                   :auto-resolve auto-resolve
                                   :syntax-quote {:resolve-symbol #(fully-qualify ctx %)}
                                   :readers readers)
                      opts (merge opts))
         ret (try (parser/parse-next parse-opts
                                     r)
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
