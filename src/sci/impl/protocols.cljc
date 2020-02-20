(ns sci.impl.protocols
  {:no-doc true})

(defprotocol IInterpret
  (-interpret [expr ctx]))
