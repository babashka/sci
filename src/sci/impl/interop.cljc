(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import [clojure.lang Reflector])))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java#L141-L142
;; see invokeStaticMethodVariadic
;; see invokeStaticMethod
;; see getStaticField

#?(:clj (set! *warn-on-reflection* true))

(defn invoke-instance-method #?(:clj [obj method args]
                                :cljs [_obj _method _args])
  #?(:clj (Reflector/invokeInstanceMethod obj method args)
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

(defn dot-macro [_ _ & [obj [method-symbol & args]]]
  (let [method (str method-symbol)
        args (vec args)
        res `(~'__invoke-instance-method__ ~obj ~method (object-array ~args))]
    res))
