(ns sci.impl.opts
  {:no-doc true}
  (:require
   #?(:cljs [goog.string])
   [sci.impl.namespaces :as namespaces]
   [sci.impl.types]
   [sci.impl.utils :as utils :refer [strip-core-ns]]
   [sci.impl.vars :as vars]
   [sci.lang])
  #?(:clj (:import [sci.impl.types IReified])))

#?(:clj
   (defrecord Env [namespaces imports load-fn]))

(defn init-env! [env bindings aliases namespaces imports load-fn]
  (swap! env (fn [env]
               (let [env-nss (:namespaces env)
                     namespaces (merge-with merge
                                            (or
                                             ;; either the env has already got namespaces
                                             env-nss
                                             ;; or we need to install the default namespaces
                                             namespaces/namespaces)
                                            (when-not env-nss
                                              ;; can skip when env has already got namespaces
                                              {'user (assoc bindings
                                                            :obj vars/user-ns)})
                                            namespaces)
                     aliases (merge namespaces/aliases aliases
                                    (get-in env [:namespaces 'user :aliases]))
                     namespaces (-> namespaces
                                    (update 'user assoc :aliases aliases)
                                    (update 'clojure.core assoc 'global-hierarchy
                                            (vars/->SciVar (make-hierarchy) 'global-hierarchy
                                              {:ns vars/clojure-core-ns} false)))
                     imports (if-let [env-imports (:imports env)]
                               (merge env-imports imports)
                               imports)]
                 ;; TODO: is the first case ever hit?
                 (if-not env
                   #?(:clj (->Env namespaces imports load-fn)
                      :cljs {:namespaces namespaces
                             :imports imports
                             :load-fn load-fn})
                   (assoc env
                          :namespaces namespaces
                          :imports imports
                          :load-fn load-fn))))))

(defn process-permissions [prev-perms & permissions]
  (not-empty (into prev-perms (comp cat (map strip-core-ns)) permissions)))

(def default-classes
  #?(:clj {'java.lang.AssertionError AssertionError
           'java.lang.Exception {:class Exception}
           'java.lang.IllegalArgumentException java.lang.IllegalArgumentException
           'clojure.lang.Delay clojure.lang.Delay
           'clojure.lang.ExceptionInfo clojure.lang.ExceptionInfo
           'clojure.lang.LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader
           'java.lang.String {:class String}
           'java.io.StringWriter java.io.StringWriter
           'java.io.StringReader java.io.StringReader
           'java.lang.Integer Integer
           'java.lang.Number Number
           'java.lang.Double Double
           'java.lang.ArithmeticException ArithmeticException
           'java.lang.Object Object
           'sci.lang.IVar sci.lang.IVar}
     :cljs {'Error {:class js/Error :constructor (fn
                                                   ([msg] (js/Error. msg))
                                                   ([msg filename] (js/Error. msg filename))
                                                   ([msg filename line] (js/Error. msg filename line)))}
            'cljs.core.Delay {:class cljs.core/Delay
                              :constructor #(cljs.core/Delay. % nil)}
            'goog.string.StringBuffer {:class goog.string/StringBuffer
                                       :constructor #(goog.string/StringBuffer. %)}}))

(def default-imports
  #?(:clj '{AssertionError java.lang.AssertionError
            Exception java.lang.Exception
            String java.lang.String
            ArithmeticException java.lang.ArithmeticException
            Integer java.lang.Integer
            Number java.lang.Number
            Double java.lang.Double
            Object java.lang.Object}
     :cljs {}))

(defn normalize-classes [classes]
  (loop [class->opts (transient (select-keys classes [:allow]))
         kvs classes]
    (if-let [[sym class-opts] (first kvs)]
      (recur ;; storing the physical class as key didn't work well with
       ;; GraalVM
       (assoc! class->opts sym (if (map? class-opts)
                                 class-opts
                                 {:class class-opts}))
       (rest kvs))
      {:public-class (:public-class classes)
       :class->opts (persistent! class->opts)})))

(def default-reify-fn
  #?(:clj (fn [{:keys [interfaces methods protocols]}]
            (reify
              Object
              (toString [this]
                ((get methods 'toString) this))
              IReified
              (getInterfaces [this]
                interfaces)
              (getMethods [this]
                methods)
              (getProtocols [this]
                protocols)))
     :cljs (fn [_ _ _])))

#?(:clj (defrecord Ctx [bindings env
                        features readers
                        reload-all
                        check-permissions]))

(defn ->ctx [bindings env features readers check-permissions?]
  #?(:cljs {:bindings bindings
            :env env
            :features features
            :readers readers
            :check-permissions check-permissions?}
     :clj (->Ctx bindings env features readers false check-permissions?)))

(defn init
  "Initializes options"
  [{:keys [:bindings :env
           :allow :deny
           :aliases
           :namespaces
           :classes
           :imports
           :features
           :load-fn
           :readers
           :reify-fn
           :proxy-fn
           :disable-arity-checks]}]
  (let [env (or env (atom {}))
        imports (merge default-imports imports)
        bindings bindings
        _ (init-env! env bindings aliases namespaces imports load-fn)
        raw-classes (merge default-classes classes)
        classes (normalize-classes raw-classes)
        ctx (assoc (->ctx {} env features readers (or allow deny))
                   :allow (when allow (process-permissions #{} allow))
                   :deny (when deny (process-permissions #{} deny))
                   :reify-fn (or reify-fn default-reify-fn)
                   :proxy-fn proxy-fn
                   :disable-arity-checks disable-arity-checks
                   :public-class (:public-class classes)
                   :raw-classes raw-classes ;; hold on for merge-opts
                   :class->opts (:class->opts classes)
                   #?@(:clj [:main-thread-id (.getId (Thread/currentThread))]))]
    ctx))

(defn merge-opts [ctx opts]
  (let [{:keys [:bindings
                :allow :deny
                :aliases
                :namespaces
                :classes
                :imports
                :features
                :load-fn
                :readers
                :reify-fn
                :disable-arity-checks]
         :or {disable-arity-checks (:disable-arity-checks ctx)}} opts
        env (:env ctx)
        _ (init-env! env bindings aliases namespaces imports load-fn)
        raw-classes (merge (:raw-classes ctx) classes)
        classes (normalize-classes raw-classes)
        ctx (assoc (->ctx {} env features readers (or (:check-permissions ctx) allow deny))
                   :allow (when allow (process-permissions (:allow ctx) allow))
                   :deny (when deny (process-permissions (:deny ctx) deny))
                   :reify-fn reify-fn
                   :disable-arity-checks disable-arity-checks
                   :public-class (:public-class classes)
                   :raw-classes raw-classes
                   :class->opts (:class->opts classes)
                   :main-thread-id (:main-thread-id ctx))]
    ctx))
