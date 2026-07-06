(ns sci.test-utils
  (:require #?@(:cljd [["dart:io" :as io]
                       [edamame.core :as edamame]]
                :clj [[edamame.core :as edamame]])
            #?@(:cljd [] :clj [[me.raynes.conch :refer [let-programs] :as sh]])
            [clojure.test :as test :refer [is]]
            ;; :refer of fns from ported namespaces breaks the cljd host pass
            #?(:cljd [sci.core :as sci]
               :default [sci.core :refer [eval-string]])
            #?(:cljd [sci.test-utils.macros]
               :clj [sci.test-utils.macros]) ;; defines thrown-with-data?
            [sci.test-utils.utils :as u])
  #?(:cljs (:require-macros [sci.test-utils.macros])))

(def native? #?(:cljd (= "native" (get io/Platform.environment "SCI_TEST_ENV"))
                :clj (= "native" (System/getenv "SCI_TEST_ENV"))
                :cljs false))

(when native? (println "Testing native version."))

(defn eval* [form ctx]
  (if (not native?)
    (#?(:cljd sci/eval-string :default eval-string) (str form) ctx)
    #?(:cljd
       ;; shell out to the AOT-compiled sci.aot-main binary, like the JVM
       ;; SCI_TEST_ENV=native path shells out to ./sci
       (let [bin (get io/Platform.environment "SCI_NATIVE_BIN")
             result (io/Process.runSync bin [(str form) (str ctx)])]
         (if (zero? (.-exitCode result))
           (let [out (.trim (str (.-stdout result)))]
             (when-not (= "" out)
               (edamame/parse-string out {:all true :location? (constantly false)})))
           (throw (ex-info (str (.-stderr result)) {}))))
       :clj
       (let [v (let-programs [sci "./sci"]
                 (try (sci (str form) (str ctx))
                      (catch #?(:clj Exception :cljs :default) e
                        (throw (ex-info (:stderr (ex-data e))
                                        (or (ex-data e) {}))))))]
         (edamame/parse-string v {:all true
                                  :location? (constantly false)}))
       :cljs nil)))

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
