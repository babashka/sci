(ns sci.impl.unrestrict
  {:no-doc true})

(def ^:dynamic *unrestricted* false)

(defn unrestricted?
  "PLACEHOLDER-DOCSTRING: ctx :unrestricted wins over the global when set."
  [ctx]
  (if-some [u (:unrestricted ctx)]
    u
    *unrestricted*))
