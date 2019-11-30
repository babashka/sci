(ns sci.impl.opts
  {:no-doc true}
  (:require [sci.impl.namespaces :as namespaces]
            [sci.impl.utils :as utils :refer [strip-core-ns]]))

(defn init-env! [env bindings aliases namespaces imports]
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
           'java.lang.String {:class String}
           'java.lang.Integer Integer
           'java.lang.Double Double
           'java.lang.ArithmeticException ArithmeticException}
     :cljs {'Error js/Error}))

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
