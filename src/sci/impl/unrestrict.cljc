(ns sci.impl.unrestrict
  {:no-doc true})

;; internal conduit from the ctx :unrestricted flag to runtime checks that
;; have no ctx in hand (built-in var mutation); bound by eval-form, never a
;; source of truth on its own
(def ^:dynamic *unrestricted* false)

(defn unrestricted?
  "PLACEHOLDER-DOCSTRING: the ctx :unrestricted flag is the only source."
  [ctx]
  (true? (:unrestricted ctx)))
