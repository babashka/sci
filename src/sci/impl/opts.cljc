(ns sci.impl.opts
  {:no-doc true}
  (:require
   #?(:cljs [goog.string])
   [sci.impl.namespaces :as namespaces]
   [sci.impl.types]
   [sci.impl.utils :as utils :refer [strip-core-ns]]
   [sci.lang])
  #?(:clj (:import
           [sci.impl.types IReified])))

#?(:clj
   (defrecord Env [namespaces imports load-fn]))

(def namespace-syms (keys namespaces/namespaces))

(defn init-env! [env bindings aliases namespaces classes raw-classes imports
                 load-fn #?(:cljs async-load-fn) #?(:cljs js-libs) ns-aliases]
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
                                                            :obj utils/user-ns)})
                                            namespaces)
                     aliases (merge aliases
                                    (get-in env [:namespaces 'user :aliases]))
                     namespaces (-> namespaces
                                    (update 'user assoc :aliases aliases)
                                    (update 'clojure.core assoc
                                            'global-hierarchy
                                            (utils/new-var 'global-hierarchy (make-hierarchy)
                                                           {:ns utils/clojure-core-ns})
                                            '*loaded-libs* (namespaces/loaded-libs**
                                                            (concat (keys env-nss)
                                                                    namespace-syms))))
                     imports (if-let [env-imports (:imports env)]
                               (merge env-imports imports)
                               imports)
                     ns-aliases (merge (:ns-aliases env) ns-aliases)
                     #?@(:cljs [js-libs (merge (:js-libs env) js-libs)])]
                 ;; TODO: is the first case ever hit?
                 (if-not env
                   #?(:clj (->Env namespaces imports load-fn)
                      :cljs {:namespaces namespaces
                             :imports imports
                             :load-fn load-fn
                             :async-load-fn async-load-fn})
                   (assoc env
                          :namespaces namespaces
                          :imports imports
                          :load-fn load-fn
                          #?@(:cljs [:async-load-fn async-load-fn
                                     :js-libs js-libs])
                          :public-class (:public-class classes)
                          :class->opts (:class->opts classes)
                          :raw-classes raw-classes
                          :ns-aliases ns-aliases))))))

(defn process-permissions [prev-perms & permissions]
  (not-empty (into prev-perms (comp cat (map strip-core-ns)) permissions)))

(def default-classes
  #?(:clj {'java.lang.AssertionError AssertionError
           'java.lang.Exception {:class Exception}
           'java.lang.IllegalArgumentException java.lang.IllegalArgumentException
           'clojure.lang.Delay clojure.lang.Delay
           'clojure.lang.ExceptionInfo clojure.lang.ExceptionInfo
           'clojure.lang.LineNumberingPushbackReader clojure.lang.LineNumberingPushbackReader
           'clojure.lang.LazySeq clojure.lang.LazySeq
           'java.lang.String {:class String}
           'java.io.StringWriter java.io.StringWriter
           'java.io.StringReader java.io.StringReader
           'java.lang.Integer Integer
           'java.lang.Number Number
           'java.lang.Double Double
           'java.lang.ArithmeticException ArithmeticException
           'java.lang.Object Object
           'sci.lang.IVar sci.lang.IVar ;; deprecated
           'sci.lang.Type sci.lang.Type
           'sci.lang.Var sci.lang.Var}
     :cljs {'Error {:class js/Error :constructor (fn
                                                   ([msg] (js/Error. msg))
                                                   ([msg filename] (js/Error. msg filename))
                                                   ([msg filename line] (js/Error. msg filename line)))}
            ;; this is here to satisfy the queue reader literal + advanced compilation
            'cljs.core.PersistentQueue.EMPTY cljs.core/PersistentQueue.EMPTY
            'goog.string.StringBuffer {:class goog.string/StringBuffer
                                       :constructor #(goog.string/StringBuffer. %)}
            'sci.lang.Type sci.lang.Type}))

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

(defn stringify-keys [m]
  (persistent!
   (reduce-kv (fn [m k v]
                (assoc! m (name k) v)) (transient {}) m)))

(comment
  (stringify-keys {:foo 1})
  )

(defn normalize-classes [classes]
  (loop [class->opts (transient (select-keys classes [:allow]))
         kvs classes]
    (if-let [[sym class-opts] (first kvs)]
      (recur ;; storing the physical class as key didn't work well with
       ;; GraalVM
       (if (map? class-opts)
         (if-let [sm (:static-methods class-opts)]
           (-> (assoc! class->opts sym class-opts)
               (assoc! :static-methods (assoc (:static-methods class->opts) sym sm)))
           (assoc! class->opts sym class-opts))
         (assoc! class->opts sym {:class class-opts}))
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
              (getInterfaces [_this]
                interfaces)
              (getMethods [_this]
                methods)
              (getProtocols [_this]
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

(def default-ns-aliases
  #?(:clj {}
     :cljs {;; in SCI the core namespace is always called clojure.core
            'cljs.core 'clojure.core}))

(defn init
  "Initializes options"
  [{:keys [bindings env
           allow deny
           aliases
           namespaces
           classes
           imports
           features
           load-fn
           readers
           reify-fn
           proxy-fn
           #?(:cljs async-load-fn)
           #?(:cljs js-libs)
           ns-aliases
           load-thread-bindings]}]
  (let [env (or env (atom {}))
        imports (merge default-imports imports)
        ns-aliases (merge default-ns-aliases ns-aliases)
        bindings bindings
        raw-classes (merge default-classes classes)
        classes (normalize-classes raw-classes)
        _ (init-env! env bindings aliases namespaces classes raw-classes imports
                     load-fn #?(:cljs async-load-fn) #?(:cljs js-libs) ns-aliases)
        ctx (assoc (->ctx {} env features readers (or allow deny))
                   :allow (when allow (process-permissions #{} allow))
                   :deny (when deny (process-permissions #{} deny))
                   :reify-fn (or reify-fn default-reify-fn)
                   :proxy-fn proxy-fn
                   :load-thread-bindings load-thread-bindings
                   #?@(:clj [:main-thread-id (.getId (Thread/currentThread))]))]
    ctx))

(defn merge-opts [ctx opts]
  (let [!env (:env ctx)
        env @!env
        {:keys [bindings
                allow deny
                aliases
                namespaces
                classes
                imports
                features
                load-fn
                readers
                reify-fn
                #?(:cljs async-load-fn)
                #?(:cljs js-libs)
                ns-aliases
                load-thread-bindings]
         :or {load-fn (:load-fn env)
              #?@(:cljs [async-load-fn (:async-load-fn env)])
              features (:features ctx)
              load-thread-bindings (:load-thread-bindings ctx)}} opts
        raw-classes (merge (:raw-classes @!env) classes)
        classes (normalize-classes raw-classes)
        _ (init-env! !env bindings aliases namespaces classes raw-classes imports load-fn #?(:cljs async-load-fn) #?(:cljs js-libs) ns-aliases)
        ctx (assoc (->ctx {} !env features readers (or (:check-permissions ctx) allow deny))
                   :allow (when allow (process-permissions (:allow ctx) allow))
                   :deny (when deny (process-permissions (:deny ctx) deny))
                   :reify-fn reify-fn
                   :main-thread-id (:main-thread-id ctx)
                   :load-thread-bindings load-thread-bindings)]
    ctx))
