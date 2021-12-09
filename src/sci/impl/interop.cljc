(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import [sci.impl Reflector]
                   [java.lang.reflect Field Modifier]))
  (:require #?(:cljs [goog.object :as gobject])
            #?(:cljs [clojure.string :as str])
            [sci.impl.vars :as vars]))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:clj (set! *warn-on-reflection* true))

(defn invoke-instance-field
  #?@(:cljs [[obj _target-class field-name]
             ;; gobject/get didn't work here
             (aget obj field-name)]
      :clj
      [([obj ^Class target-class method]
        (let [^Field field (.getField target-class method)
              mod (.getModifiers field)]
          (if (and (not (Modifier/isStatic mod))
                   (Modifier/isPublic mod))
            (.get field obj)
            (throw (ex-info (str "Not found or accessible instance field: " method) {})))))]))

(defn invoke-instance-method
  #?@(:cljs [[obj _target-class method-name args]
             ;; gobject/get didn't work here
             (if-let [method (aget obj method-name)]
               (.apply method obj (into-array args) #_(js-object-array args))
               (throw (js/Error. (str "Could not find instance method: " method-name))))]
      :clj
      [([obj ^Class target-class method args]
        (if-not target-class
          (Reflector/invokeInstanceMethod obj method (object-array args))
          (let [methods (Reflector/getMethods target-class (count args) method false)]
            (Reflector/invokeMatchingMethod method methods obj (object-array args)))))]))

(defn get-static-field #?(:clj [[^Class class field-name-sym]]
                          :cljs [[class field-name-sym]])
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (if (str/includes? (str field-name-sym) ".")
             (apply gobject/getValueByKeys class (str/split (str field-name-sym) #"\."))
             (gobject/get class field-name-sym))))

#?(:cljs
   (defn invoke-js-constructor [constructor args]
     (let [ctor (js/Function.prototype.bind.apply constructor)
           args (vec args)]
       (case (count args)
         0 (new ctor)
         1 (new ctor (nth args 0))
         2 (new ctor (nth args 0) (nth args 1))
         3 (new ctor (nth args 0) (nth args 1) (nth args 2))
         4 (new ctor (nth args 0) (nth args 1) (nth args 2) (nth args 3))
         5 (new ctor (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4))
         6 (new ctor (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5))
         7 (new ctor (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6))

         (throw (ex-info "Constructors with more than 7 arguments are not supported" {:constructor constructor}))))))

(defn invoke-constructor #?(:clj [^Class class args]
                            :cljs [constructor args])
  #?(:clj (Reflector/invokeConstructor class (object-array args))
     :cljs (invoke-js-constructor constructor args)))

(defn invoke-static-method #?(:clj [[^Class class method-name] args]
                              :cljs [[class method-name] args])
  #?(:clj
     (Reflector/invokeStaticMethod class (str method-name) (object-array args))
     :cljs (if-let [method (gobject/get class method-name)]
             (.apply method class (into-array args))
             (let [method-name (str method-name)
                   field (get-static-field [class method-name])]
               (cond
                 (not field)
                 (throw (js/Error. (str "Could not find static method " method-name)))
                 (clojure.string/ends-with? method-name ".")
                 (invoke-js-constructor field args)
                 :else
                 ;; why is this here??
                 (apply field args))))))

(defn fully-qualify-class [{:keys [:env :class->opts]} sym]
  (or #?(:clj (when (contains? class->opts sym) sym)
         :cljs (if-let [ns* (namespace sym)]
                 (when (identical? "js" ns*)
                   (when (contains? class->opts (symbol (name sym)))
                     sym))
                 (when (contains? class->opts sym)
                   sym)))
      (let [env @env]
        (or (get (:imports env) sym)
            (let [cnn (vars/current-ns-name)]
              (get-in env [:namespaces cnn :imports sym]))))))

(defn resolve-class-opts [{:keys [:env :class->opts]} sym]
  (let [class-opts (or #?(:clj (get class->opts sym)
                          :cljs (if-let [ns* (namespace sym)]
                                  (when (identical? "js" ns*)
                                    (get class->opts (symbol (name sym))))
                                  (get class->opts sym)))
                       (let [env @env
                             cnn (vars/current-ns-name)
                             imports (get-in env [:namespaces cnn :imports])]
                         (if-let [[_ v] (find imports sym)]
                           ;; finding a nil v means the object was unmapped
                           (get class->opts v)
                           (when-let [v (get-in env [:imports sym])]
                             (get class->opts v)))))]
    class-opts))

(defn resolve-class [ctx sym]
  (:class (resolve-class-opts ctx sym)))
