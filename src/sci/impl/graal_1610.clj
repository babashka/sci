(ns sci.impl.graal-1610
  "Patches clojure.core with fix for oracle/graal #1610."
  (:refer-clojure :exclude [rand rand-int rand-nth random-sample])
  (:import [java.util Random]))

(def native? (= "executable" (System/getProperty "org.graalvm.nativeimage.kind")))

(when native?
  (defn set-var-root [v x]
    (alter-var-root v (constantly x)))

  (def random (delay (java.util.Random.)))

  (defn rand
    ([] (.nextDouble ^Random @random))
    ([n] (* n (rand))))
  (set-var-root #'clojure.core/rand rand)

  (defn rand-int
    ([n] (int (rand n))))
  (set-var-root #'clojure.core/rand-int rand-int)

  (defn rand-nth
    [coll]
    (nth coll (rand-int (count coll))))
  (set-var-root #'clojure.core/rand-nth rand-nth)

  (defn random-sample
    ([prob]
     (filter (fn [_] (< (rand) prob))))
    ([prob coll]
     (filter (fn [_] (< (rand) prob)) coll)))
  (set-var-root #'clojure.core/random-sample random-sample))
