(ns sci.impl.types)

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

(defprotocol IEvalOp
  (getType [_this]))

(deftype EvalOp [type v]
  IEvalOp
  (getType [_] type)
  IBox
  (getVal [_] v))
