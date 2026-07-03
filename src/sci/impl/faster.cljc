(ns sci.impl.faster
  {:no-doc true}
  (:require #?@(:cljd [] :default [[sci.impl.macros :refer [?]]]))
  #?(:cljs (:require-macros [sci.impl.faster :refer [nth-2 assoc-3 get-2]])))

#?(:cljd
   (defmacro nth-2 [c i]
     `(nth ~c ~i))
   :default
   (defmacro nth-2
     [c i]
     (?
      :clj `(.nth ~(with-meta c {:tag 'clojure.lang.Indexed}) ~i)
      :cljs `(~'-nth ~c ~i))))

#?(:cljd
   (defmacro assoc-3 [m k v]
     `(assoc ~m ~k ~v))
   :default
   (defmacro assoc-3
     [m k v]
     (?
      :clj `(.assoc ~(with-meta m {:tag 'clojure.lang.Associative}) ~k ~v)
      :cljs `(~'-assoc ~m ~k ~v))))

#?(:cljd
   (defmacro get-2 [m k]
     `(get ~m ~k))
   :default
   (defmacro get-2
     [m k]
     (?
      :clj `(.get ~(with-meta m {:tag 'java.util.Map}) ~k)
      :cljs `(.get ~m ~k))))

#?(:cljd
   (defmacro deref-1 [ref]
     `(deref ~ref))
   :default
   (defmacro deref-1
     [ref]
     (?
      :clj `(.deref ~(with-meta ref
                       {:tag 'clojure.lang.IDeref}))
      :cljs `(~'-deref ~ref))))
