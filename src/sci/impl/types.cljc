(ns sci.impl.types)

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

#_(defprotocol IEvalOp
  (getType [_this]))

#_(deftype EvalOp [type v]
  IEvalOp
  (getType [_] type)
  IBox
  (getVal [_] v))
