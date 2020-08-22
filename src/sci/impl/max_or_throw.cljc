(ns sci.impl.max-or-throw
  {:no-doc true})

(defprotocol MaxOrThrow
  (max-or-throw [this ctx n]))

(defn bottom [n data]
  (lazy-seq (throw (ex-info (str "Maximum number of elements realized: " n)
                            data))))

(defn take*
  ([n coll err-val]
   (lazy-seq
    (if (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take* (dec n) (rest s) err-val)))
      err-val))))

(defn take-or-throw [coll ctx n]
  (take* n coll
         (bottom n (merge {;;:column (delay coll)
                           :type :sci.error/realized-beyond-max
                           :realize-max n
                           :expression (:expression ctx)}))))

(extend-protocol MaxOrThrow

  nil
  (max-or-throw
    ([this ctx n] this))

  #?(:clj Object :cljs default)
  (max-or-throw
    ([this ctx n] this))

  #?(:clj clojure.lang.LazySeq :cljs LazySeq)
  (max-or-throw
    ([this ctx n]
     (take-or-throw this ctx n)))

  #?(:clj clojure.lang.Cons :cljs Cons)
  (max-or-throw
    ([this ctx n]
     (take-or-throw this ctx n)))

  #?(:clj clojure.lang.Range :cljs Range)
  (max-or-throw
    ([this ctx n]
     (take-or-throw this ctx n)))

  #?@(:clj [clojure.lang.LongRange
            (max-or-throw
             ([this ctx n]
              (take-or-throw this ctx n)))])

  #?(:clj clojure.lang.Iterate :cljs Iterate)
  (max-or-throw
    ([this ctx n]
     (take-or-throw this ctx n)))

  #?(:clj clojure.lang.Repeat :cljs Repeat)
  (max-or-throw
    ([this ctx n]
     ;; (prn "TYPE" (type this))
     (take-or-throw this ctx n)))
  )
