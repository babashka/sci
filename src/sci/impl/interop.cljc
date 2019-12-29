(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import [clojure.lang Reflector])))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:clj (set! *warn-on-reflection* true))

(defn invoke-instance-method #?(:clj [_ctx obj method args]
                                :cljs [_ctx _obj _method _args])
  #?(:clj
     (Reflector/invokeInstanceMethod obj method (object-array args))
     :cljs (throw (js/Error. "Not implemented yet."))))

(defn invoke-static-method #?(:clj [_ctx [[^Class class method-name] & args]]
                              :cljs [_ctx & _args])
  #?(:clj
     (Reflector/invokeStaticMethod class (str method-name) (object-array args))
     :cljs (throw (js/Error. "Not implemented yet."))))

(defn get-static-field #?(:clj [_ctx [^Class class field-name-sym]]
                          :cljs [_ctx _])
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (throw (js/Error. "Not implemented yet."))))

(defn invoke-constructor #?(:clj [_ctx ^Class class args]
                            :cljs [_ctx constructor args])
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
