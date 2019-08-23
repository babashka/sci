(ns sci.impl.utils)

(defn gensym*
  ([] (with-meta (gensym)
        {:sci.impl/unresolved true}))
  ([prefix] (with-meta (gensym prefix)
              {:sci.impl/unresolved true})))

(defn mark-eval-call
  [expr]
  (vary-meta
   expr
   (fn [m]
     (assoc m :sci.impl/eval-call true))))
