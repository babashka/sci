(ns sci.impl.parser
  {:no-doc true}
  (:refer-clojure :exclude [read-string])
  (:require
   [edamame.impl.parser :as parser]
   [sci.impl.analyzer :as ana]
   [sci.impl.interop :as interop]
   [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(def opts
  (parser/normalize-opts
   {:all true
    :read-eval false
    :row-key :line
    :col-key :column
    :end-row-key :end-line
    :end-col-key :end-column}))

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
              (or (when (or (get (get namespaces 'clojure.core) sym)
                            (contains? ana/macros sym))
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
   (parser/parse-next opts r))
  ([ctx r]
   (let [features (:features ctx)
         readers (:readers ctx)
         readers (if (vars/var? readers) @readers readers)
         env (:env ctx)
         env-val @env
         current-ns (vars/current-ns-name)
         the-current-ns (get-in env-val [:namespaces current-ns])
         aliases (:aliases the-current-ns)
         auto-resolve (assoc aliases
                             :current current-ns)
         parse-opts (assoc opts
                           :read-cond :allow
                           :features features
                           :auto-resolve auto-resolve
                           :syntax-quote {:resolve-symbol #(fully-qualify ctx %)}
                           :tools.reader/opts {:readers readers})
         ret (parser/parse-next parse-opts
                                r)]
     ;; (prn "ret" ret)
     ret)))

;;;; Scratch

(comment
  )
