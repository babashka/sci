(ns sci.test-utils
  (:require [sci.core :refer [eval-string]]
            #?(:clj [me.raynes.conch :refer [let-programs] :as sh])
            #?(:clj [clojure.edn :as edn])))

(def native? #?(:clj (= "native" (System/getenv "SCI_TEST_ENV"))
                :cljs false))

(when native? (println "Testing native version."))

(defn eval* [form bindings]
  (if #?(:clj (not native?)
         :cljs true)
    (eval-string (str form) {:bindings bindings})
    #?(:clj
       (let-programs [sci "./sci"]
         ;; (prn ">>>" (str form) (str bindings) (sci (str form) (str bindings)))
         (try (edn/read-string (sci (str form) (str bindings)))
              (catch #?(:clj Exception :cljs :default) e
                (throw (new #?(:clj Exception :cljs js/Error)
                            (:stderr (ex-data e))))))))))

;;;; Scratch

(comment
  (eval* "*in*" {'*in* 1}) ;;=> 1
  )

