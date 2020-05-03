(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import [sci.impl Reflector]))
  (:require #?(:cljs [goog.object :as gobj])
            [sci.impl.vars :as vars]
            #?(:cljs [clojure.string])))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:clj (set! *warn-on-reflection* true))


#?(:cljs
   (do
     (defn toJS [v]
       (if (instance? MetaFn v)
         ;; when returning a function, make it callable from JS
         (.-afn v)
         (clj->js v)))

     (defn js-object-array [args]
       (to-array (map toJS args)))))

(defn invoke-instance-method
  #?@(:cljs [[obj _target-class method-name args]
             ;; gobj/get didn't work here
             (if (identical? \- (.charAt method-name 0))
               (aget obj (subs method-name 1))
               (if-let [method (aget obj method-name)]
                 (.apply method obj (js-object-array args))
                 (throw (js/Error. (str "Could not find instance method: " method-name)))))]
      :clj
      [#_([obj method args]
        (invoke-instance-method obj nil method args))
       ([obj target-class method args]
        (if-not target-class
          (Reflector/invokeInstanceMethod obj method (object-array args))
          (let [methods (Reflector/getMethods target-class (count args) method false)]
            (Reflector/invokeMatchingMethod method methods obj (object-array args)))))]))

(defn get-static-field #?(:clj [[^Class class field-name-sym]]
                          :cljs [[class field-name-sym]])
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (gobj/get class field-name-sym)))

#?(:cljs
   (defn invoke-js-constructor [constructor args]
     (let [ctor (js/Function.prototype.bind.apply constructor)
           args (js-object-array args)]
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
     :cljs (if-let [method (gobj/get class method-name)]
             (.apply method class (js-object-array args))
             (let [method-name (str method-name)
                   [field sub-method-name] (clojure.string/split method-name #"\.")]
               (cond
                 sub-method-name
                 (invoke-instance-method (get-static-field [class field]) nil sub-method-name args)

                 (clojure.string/ends-with? method-name ".")
                 (invoke-js-constructor (get-static-field [class field]) args)

                 :else
                 (throw (js/Error. (str "Could not find static method " method-name))))))))

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
                       (let [env @env]
                         (or
                          (when-let [v (get-in env [:imports sym])]
                            (get class->opts v))
                          (let [cnn (vars/current-ns-name)]
                            (when-let [v (get-in env [:namespaces cnn :imports sym])]
                              (get class->opts v))))))]
    class-opts))

(defn resolve-class [ctx sym]
  (:class (resolve-class-opts ctx sym)))
