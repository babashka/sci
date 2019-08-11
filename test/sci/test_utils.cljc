(ns sci.test-utils
  (:require [sci.core :refer [eval-string]]
            #?(:clj [me.raynes.conch :refer [let-programs] :as sh])
            #?(:clj [clojure.edn :as edn])))

(defn eval* [form bindings]
  (if #?(:clj (not= "native" (System/getenv "SCI_TEST_ENV"))
         :cljs true)
    (eval-string (str form) {:bindings bindings})
    #?(:clj
       (let-programs [sci "./sci"]
         (binding [sh/*throw* false]
           (edn/read-string (sci (str form) (str bindings))))))))

;;;; Scratch

(comment
  (eval* "*in*" {'*in* 1}) ;;=> 1
  )

