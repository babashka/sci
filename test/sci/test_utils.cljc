(ns sci.test-utils
  (:require [sci.core :refer [eval-string]]
            #?(:clj [me.raynes.conch :refer [let-programs] :as sh])
            #?(:clj [clojure.edn :as edn])))

(def native? #?(:clj (= "native" (System/getenv "SCI_TEST_ENV"))
                :cljs false))

(when native? (println "Testing native version."))

(defn eval* [form ctx]
  (if #?(:clj (not native?)
         :cljs true)
    (eval-string (str form) ctx)
    #?(:clj
       (let-programs [sci "./sci"]
         (try (edn/read-string (sci (str form) (str ctx)))
              (catch #?(:clj Exception :cljs :default) e
                (throw (new Exception
                            (:stderr (ex-data e))))))))))

;;;; Scratch

(comment
  (eval* "*in*" {'*in* 1}) ;;=> 1
  )

