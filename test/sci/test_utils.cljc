(ns sci.test-utils
  (:require [sci.core :refer [eval-string]]
            #?(:clj [me.raynes.conch :refer [let-programs] :as sh])
            #?(:clj [clojure.edn :as edn])))

(def native? #?(:clj (= "native" (System/getenv "SCI_TEST_ENV"))
                :cljs false))

(defn eval* [form bindings]
  (if #?(:clj (not native?)
         :cljs true)
    (eval-string (str form) {:bindings bindings})
    #?(:clj
       (let-programs [sci "./sci"]
         (try (edn/read-string (sci (str form) (str bindings)))
              (catch #?(:clj Exception :cljs :default) e
                (ex-data e)))))))

;;;; Scratch

(comment
  (eval* "*in*" {'*in* 1}) ;;=> 1
  )

