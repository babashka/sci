(ns sci.test-utils
  (:require #?@(:cljs [] :default [[edamame.core :as edamame]])
            #?(:clj  [me.raynes.conch :refer [let-programs] :as sh])
            [clojure.test :as test :refer [is]]
            [sci.core :refer [eval-string]]
            #?@(:cljs [] :default [sci.test-utils.macros]) ;; defines thrown-with-data?
            [sci.test-utils.utils :as u])
  #?(:cljs (:require-macros [sci.test-utils.macros])))

(def native? #?(:clj (= "native" (System/getenv "SCI_TEST_ENV"))
                :default false))

(when native? (println "Testing native version."))

(defn eval* [form ctx]
  (if #?(:clj (not native?)
         :default true)
    (eval-string (str form) ctx)
    #?(:clj
       (let [v (let-programs [sci "./sci"]
                 (try (sci (str form) (str ctx))
                      (catch Exception e
                        (throw (ex-info (:stderr (ex-data e))
                                        (or (ex-data e) {}))))))]
         (edamame/parse-string v {:all true
                                  :location? (constantly false)}))
       :default nil)))

(def submap? u/submap?)

(defmacro assert-submap [m r]
  `(is (submap? ~m ~r)))

#?(:cljs
   (defn planck-env? []
     (exists? js/PLANCK_EXIT_WITH_VALUE)))

;;;; Scratch

(comment
  (eval* "*in*" {'*in* 1}) ;;=> 1
  )

