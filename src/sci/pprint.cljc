(ns sci.pprint
  "Extensible pprinting for built-in SCI types."
  (:require [clojure.pprint :as pprint]
            [sci.impl.records]
            [sci.impl.types :as types]))

(defprotocol SciPrettyPrint
  (-sci-pprint-simple-dispatch [obj]))

;; TODO, if record implements protocol and protocol has pprint method, we use that

(extend-protocol SciPrettyPrint
  sci.impl.records.SciRecord
  (-sci-pprint-simple-dispatch [obj]
    (if-let [rv (types/type-impl obj)]
      (let [m (meta rv)]
        (if-let [pm (:sci.impl/pprint-simple-dispatch m)]
          (pm obj)
          (pprint/simple-dispatch (into {} obj))))
      (pprint/simple-dispatch (into {} obj)))))

(defmethod pprint/simple-dispatch sci.impl.records.SciRecord [obj]
  (-sci-pprint-simple-dispatch obj))
