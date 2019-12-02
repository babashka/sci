(ns sci.impl.opts
  {:no-doc true}
  (:require [sci.impl.namespaces :as namespaces]
            [sci.impl.utils :as utils :refer [strip-core-ns]]
            [sci.impl.io :as io]
            [sci.impl.vars :as vars]
            #?(:cljs [goog.string])))

(defn init-env! [env bindings aliases namespaces imports in out err]
  (vars/bindRoot io/in (if in in (io/init-in)))
  (vars/bindRoot io/out (if out out (io/init-out)))
  (vars/bindRoot io/err (if err err (io/init-err)))
  (swap! env (fn [env]
               (let [namespaces (merge-with merge {'user bindings} namespaces/namespaces (:namespaces env) namespaces)
                     aliases (merge namespaces/aliases (:aliases env) aliases)
                     namespaces (update namespaces 'user assoc :aliases aliases)]
                 (assoc env
                        :namespaces namespaces
                        :imports imports
                        :current-ns 'user)))))

(def presets
  {:termination-safe
   {:deny '[loop recur trampoline]
    :realize-max 100}})

(defn process-permissions [& permissions]
  (not-empty (into #{} (comp cat (map strip-core-ns)) permissions)))

(def default-classes
  #?(:clj {'java.lang.AssertionError AssertionError
           'java.lang.Exception {:class Exception}
           'clojure.lang.ExceptionInfo clojure.lang.ExceptionInfo
           'clojure.lang.LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader
           'java.lang.String {:class String}
           'java.io.StringWriter java.io.StringWriter
           'java.io.StringReader java.io.StringReader
           'java.lang.Integer Integer
           'java.lang.Double Double
           'java.lang.ArithmeticException ArithmeticException}
     :cljs {'Error js/Error
            'goog.string.StringBuffer goog.string/StringBuffer}))

(def default-imports
  #?(:clj '{AssertionError java.lang.AssertionError
            Exception java.lang.Exception
            String java.lang.String
            ArithmeticException java.lang.ArithmeticException
            Integer java.lang.Integer
            Double java.lang.Double}
     :cljs {}))

(defn normalize-classes [classes]
  (loop [sym->class (transient {})
         class->opts (transient {})
         kvs classes]
    (if-let [[sym class-opts] (first kvs)]
      (let [[class class-opts] (if (map? class-opts)
                                 [(:class class-opts) class-opts]
                                 [class-opts {}])]
        (recur (assoc! sym->class sym class)
               ;; storing the physical class as key didn't work well with
               ;; GraalVM
               (assoc! class->opts sym class-opts)
               (rest kvs)))
      {:sym->class (persistent! sym->class)
       :class->opts (persistent! class->opts)})))

(defn init
  "Initializes options"
  [{:keys [:bindings :env
           :allow :deny
           :realize-max
           :preset ;; used by malli
           :aliases
           :namespaces
           :classes
           :imports
           :features
           :in
           :out
           :err]}]
  (let [preset (get presets preset)
        env (or env (atom {}))
        imports (merge default-imports imports)
        bindings bindings
        _ (init-env! env bindings aliases namespaces imports in out err)
        ctx (merge {:env env
                    :bindings {}
                    :allow (process-permissions (:allow preset) allow)
                    :deny (process-permissions (:deny preset) deny)
                    :realize-max (or realize-max (:realize-max preset))
                    :features features}
                   (normalize-classes (merge default-classes classes)))]
    ctx))
