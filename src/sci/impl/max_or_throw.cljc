(ns sci.impl.max-or-throw)

(defprotocol MaxOrThrow
  (max-or-throw [this] [this n]))

(defn bottom [n]
  (lazy-seq (throw (new #?(:clj Exception :clj js/Error)
                        (str "max number of elements reached: " n)))))

(defn take*
  ([n coll err-val]
   (lazy-seq
    (if (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take* (dec n) (rest s) err-val)))
      err-val))))

(defn take-or-throw [n xs]
  (take* n xs (bottom n)))

(extend-protocol MaxOrThrow

  nil
  (max-or-throw
    ([this] this)
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
     (take-or-throw n this)))

  #?(:clj Object :cljs default)
  (max-or-throw
    ([this] #_(prn (type this)) this)
    ([this n] this))

  )
