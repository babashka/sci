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
     :cljs (throw (js/Error. "Not imlemented yet."))))

(defn invoke-static-method #?(:clj [_ctx [[^Class class method-name] & args]]
                              :cljs [_ctx & _args])
  #?(:clj
     (Reflector/invokeStaticMethod class (str method-name) (object-array args))
     :cljs (throw (js/Error. "Not imlemented yet."))))

(defn get-static-field #?(:clj [_ctx [^Class class field-name-sym]]
                          :cljs [_ctx _])
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (throw (js/Error. "Not imlemented yet."))))

(defn invoke-constructor #?(:clj [_ctx ^Class class args]
                            :cljs [_ctx _ _])
  #?(:clj (Reflector/invokeConstructor class (object-array args))
     :cljs (throw (js/Error. "Not imlemented yet."))))

(defn resolve-class [{:keys [:env :sym->class]} sym]
  (or (get sym->class sym)
      (when-let [v (get (:imports @env) sym)]
        (get sym->class v))))
