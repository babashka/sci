(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import [sci.impl Reflector]))
  #?(:cljs (:require [goog.object :as gobj])))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:clj (set! *warn-on-reflection* true))

(defn invoke-instance-method
  #?@(:cljs [[obj _target-class method args]
             ;; gobj/get didn't work here
             (if-let [method (aget obj method)]
               (apply method obj args)
               (throw (js/Error. "Could not find method" method)))]
      :clj
      [#_([obj method args]
        (invoke-instance-method obj nil method args))
       ([obj target-class method args]
        (if-not target-class
          (Reflector/invokeInstanceMethod obj method (object-array args))
          (let [methods (Reflector/getMethods target-class (count args) method false)]
            (Reflector/invokeMatchingMethod method methods obj (object-array args)))))]))

(defn invoke-static-method #?(:clj [[^Class class method-name] args]
                              :cljs [[class method-name] args])
  #?(:clj
     (Reflector/invokeStaticMethod class (str method-name) (object-array args))
     :cljs (if-let [method (gobj/get class method-name)]
             (apply method args)
             (throw (js/Error. "Could not find method" method-name)))))

(defn get-static-field #?(:clj [[^Class class field-name-sym]]
                          :cljs [_])
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (throw (js/Error. "Not implemented yet."))))

(defn invoke-constructor #?(:clj [^Class class args]
                            :cljs [constructor args])
  #?(:clj (Reflector/invokeConstructor class (object-array args))
     :cljs (apply constructor args)))

(defn fully-qualify-class [{:keys [:env :class->opts]} sym]
  (or #?(:clj (when (contains? class->opts sym) sym)
         :cljs (if-let [ns* (namespace sym)]
                 (when (identical? "js" ns*)
                   (when (contains? class->opts (symbol (name sym)))
                     sym))
                 (when (contains? class->opts sym)
                   sym)))
      (get (:imports @env) sym)))

(defn resolve-class-opts [{:keys [:env :class->opts]} sym]
  (let [class-opts (or #?(:clj (get class->opts sym)
                     :cljs (if-let [ns* (namespace sym)]
                             (when (identical? "js" ns*)
                               (get class->opts (symbol (name sym))))
                             (get class->opts sym)))
                  (when-let [v (get (:imports @env) sym)]
                    (get class->opts v)))]
    class-opts))

(defn resolve-class [ctx sym]
  (:class (resolve-class-opts ctx sym)))
