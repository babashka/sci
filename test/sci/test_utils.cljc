(ns sci.test-utils
  (:require #?(:clj [edamame.core :as edamame])
            #?(:clj [me.raynes.conch :refer [let-programs] :as sh])
            [clojure.test :refer [is]]
            [sci.core :refer [eval-string]]))

(def native? #?(:clj (= "native" (System/getenv "SCI_TEST_ENV"))
                :cljs false))

(when native? (println "Testing native version."))

(defn eval* [form ctx]
  (if #?(:clj (not native?)
         :cljs true)
    (eval-string (str form) ctx)
    #?(:clj
       (let [v (let-programs [sci "./sci"]
                 (try (sci (str form) (str ctx))
                      (catch #?(:clj Exception :cljs :default) e
                        (throw (ex-info (:stderr (ex-data e))
                                        (or (ex-data e) {}))))))]
         (edamame/parse-string v {:all true}))
       :cljs nil)))

(defn submap?
  "Is m1 a subset of m2? Taken from
  https://github.com/clojure/spec-alpha2, clojure.test-clojure.spec"
  [m1 m2]
  (cond
    (and (map? m1) (map? m2))
    (every? (fn [[k v]] (and (contains? m2 k)
                             (submap? v (get m2 k))))
            m1)
    (instance? java.util.regex.Pattern m1)
    (re-find m1 m2)
    :else (= m1 m2)))

(defmacro assert-submap [m r]
  `(is (submap? ~m ~r)))

;;;; Scratch

(comment
  (eval* "*in*" {'*in* 1}) ;;=> 1
  )

