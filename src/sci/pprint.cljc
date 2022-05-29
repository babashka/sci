(ns sci.pprint
  "Require this namespace if you want to extend pretty-printing to
  records created with SCI."
  (:require [clojure.pprint :as pprint]
            [sci.impl.records]
            [sci.impl.types :as types]
            [sci.lang]))

(defmethod pprint/simple-dispatch sci.impl.records.SciRecord [obj]
  (if-let [rv (types/type-impl obj)]
    (let [m (meta rv)]
      (if-let [pm (:sci.impl/pprint-simple-dispatch m)]
        (pm obj)
        (pprint/simple-dispatch (into {} obj))))
    (pprint/simple-dispatch (into {} obj))))
