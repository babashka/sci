(ns sci.impl.parser
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   [edamame.impl.parser :as parser]
   [sci.impl.analyzer :as ana]
   [sci.impl.readers :as readers]))

#?(:clj (set! *warn-on-reflection* true))

(def opts
  (parser/normalize-opts
   {:all true
    :read-eval false
    :fn readers/read-fn}))

(defn fully-qualify [env sym]
  (let [sym-ns (when-let [n (namespace sym)]
                 (symbol n))
        sym-name-str (name sym)
        current-ns (:current-ns env)
        current-ns-str (str current-ns)
        namespaces (get env :namespaces)
        the-current-ns (get namespaces current-ns)
        aliases (:aliases the-current-ns)
        ret (if-not sym-ns
              (let [clojure-core (get namespaces 'clojure.core)]
                (if (or (get clojure-core sym)
                        (contains? ana/macros sym))
                  (symbol "clojure.core" sym-name-str)
                  (symbol current-ns-str sym-name-str)))
              (if (get-in env [:namespaces sym-ns])
                sym
                (if-let [ns (get aliases sym-ns)]
                  (symbol (str ns) sym-name-str)
                  sym)))]
    ret))

(defn parse-next
  ([r]
   (parser/parse-next opts r))
  ([ctx r]
   (let [features (:features ctx)
         env (:env ctx)
         env-val @env
         current-ns (:current-ns env-val)
         the-current-ns (get-in env-val [:namespaces current-ns])
         aliases (:aliases the-current-ns)
         auto-resolve (assoc aliases
                             :current current-ns)
         parse-opts (assoc opts
                           :read-cond :allow
                           :features features
                           :auto-resolve auto-resolve
                           :syntax-quote {:resolve-symbol #(fully-qualify env-val %)})
         ret (parser/parse-next parse-opts
                                r)]
     ;; (prn "ret" ret)
     ret)))

;;;; Scratch

(comment
  )
