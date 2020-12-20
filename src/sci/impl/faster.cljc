(ns sci.impl.faster
  {:no-doc true}
  (:require [sci.impl.macros :refer [?]])
  #?(:cljs (:require-macros [sci.impl.faster :refer [nth-2 assoc-3 get-2]])))

(defmacro nth-2
  [c i]
  (?
   :clj `(.nth ~(with-meta c {:tag 'clojure.lang.Indexed}) ~i)
   :cljs `(~'-nth ~c ~i)))

(defmacro assoc-3
  [m k v]
  (?
   :clj `(.assoc ~(with-meta m {:tag 'clojure.lang.Associative}) ~k ~v)
   :cljs `(~'-assoc ~m ~k ~v)))

(defmacro get-2
  [m k]
  (?
   :clj `(.get ~(with-meta m {:tag 'java.util.Map}) ~k)
   :cljs `(.get ~m ~k)))

(defmacro deref-1
  [ref]
  (?
   :clj `(.deref ~(with-meta ref
                    {:tag 'clojure.lang.IDeref}))
   :cljs `(~'-deref ~ref)))
