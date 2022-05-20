(ns sci.pprint
  "Extensible pprinting for built-in SCI types."
  (:require [clojure.pprint :as pprint]
            [sci.impl.records]))

#?(:clj (prefer-method pprint/simple-dispatch sci.impl.records.SciRecord clojure.lang.IRecord))
#?(:clj (prefer-method pprint/simple-dispatch sci.impl.records.SciRecord java.util.Map))
#?(:clj (prefer-method pprint/simple-dispatch sci.impl.records.SciRecord clojure.lang.IPersistentMap))

(defprotocol SciPrettyPrint
  (-sci-pprint-simple-dispatch [obj]))

(extend-protocol SciPrettyPrint
  sci.impl.records.SciRecord
  (-sci-pprint-simple-dispatch [obj]
    (let [m (meta obj)
          var (:sci.impl/record-var m)]
      (if-let [rv var]
        (let [m (meta @rv)]
          (if-let [pm (:sci.impl/pprint-simple-dispatch m)]
            (pm obj)
            (pprint/simple-dispatch (into {} obj))))
        (pprint/simple-dispatch (into {} obj))))))

(defmethod pprint/simple-dispatch sci.impl.records.SciRecord [obj]
  (-sci-pprint-simple-dispatch obj))
