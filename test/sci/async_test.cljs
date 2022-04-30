(ns sci.async-test
  (:require [sci.core :as sci]
            [sci.async :as scia]
            [clojure.test :as test :refer [deftest is testing async]]
            [clojure.string :as str]
            [promesa.core :as p]))

(deftest async-eval-string-test
  (async done
   (p/let [ctx (sci/init {:async-load-fn
                          (fn [{:keys [libname opts ctx ns]}]
                            (case libname
                              "some_js_lib"
                              (-> (js/Promise.resolve #js {:libfn (fn [] "yes")})
                                  (.then (fn [mod]
                                           (sci/add-class! ctx (:as opts) mod)
                                           (sci/add-import! ctx ns (:as opts) (:as opts))
                                           {:handled true})))))})
           code (str/join
                 "\n"
                 (map pr-str '[(ns dude (:require ["some_js_lib" :as my-lib]))
                               (my-lib/libfn)]))
           res (scia/eval-string* ctx code)]
     (is (= "yes" res))
     (done))))
