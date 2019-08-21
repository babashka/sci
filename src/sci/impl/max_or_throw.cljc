(ns sci.impl.max-or-throw)

(defprotocol MaxOrThrow
  (max-or-throw [this] [this n]))

(defn bottom [n data]
  (lazy-seq (throw (ex-info (str "Maximum number of elements realized: " n)
                            (assoc data :n n)))))

(defn take*
  ([n coll err-val]
   (lazy-seq
    (if (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take* (dec n) (rest s) err-val)))
      err-val))))

(defn take-or-throw [n coll]
  (take* n coll (bottom n {:coll (delay coll)})))

(extend-protocol MaxOrThrow

  nil
  (max-or-throw
    ([this] this)
    ([this n] this))

  #?(:clj Object :cljs default)
  (max-or-throw
    ([this] (prn (type this)) this)
    ([this n] this))

  #?(:clj clojure.lang.LazySeq :cljs LazySeq)
  (max-or-throw
    ([this]
     (max-or-throw this 100))
    ([this n]
     (take-or-throw n this)))

  #?(:clj clojure.lang.Cons :cljs Cons)
  (max-or-throw
    ([this]
     (max-or-throw this 100))
    ([this n]
     (take-or-throw n this)))

  #?(:clj clojure.lang.Range :cljs Range)
  (max-or-throw
    ([this]
     (max-or-throw this 100))
    ([this n]
     (take-or-throw n this)))

  #?@(:clj [clojure.lang.LongRange
            (max-or-throw
             ([this]
              (max-or-throw this 100))
             ([this n]
              (take-or-throw n this)))])

  #?(:clj clojure.lang.Iterate :cljs Iterate)
  (max-or-throw
    ([this]
     (max-or-throw this 100))
    ([this n]
     (take-or-throw n this)))

  #?(:clj clojure.lang.Repeat :cljs Repeat)
  (max-or-throw
    ([this]
     (max-or-throw this 100))
    ([this n]
     ;; (prn "TYPE" (type this))
     (take-or-throw n this)))

  )
