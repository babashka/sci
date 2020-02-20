(ns sci.impl.protocols
  {:no-doc true})

(defprotocol IInterpret
  (-interpret [expr ctx]))

;; Protocol to support the transition to a protocol based dispatcher, probably not needed later
(defprotocol IOp
  (-op [expr]))

(defn- impl [expr]
  (when-let [m (meta expr)]
    [m (.get ^java.util.Map m :sci.impl/op)]))

(extend-protocol IOp
  nil
  (-op [expr]
    nil)

  #?(:clj Object :cljs object)
  (-op [expr] (impl expr)))

#?(:cljs
   (extend-protocol IOp
     function (-op [expr] (impl expr))
     string   (-op [expr] (impl expr))
     number   (-op [expr] (impl expr))))
