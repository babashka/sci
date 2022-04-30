(ns sci.async-test
  (:require [clojure.string :as str]
            [clojure.test :as test :refer [deftest is testing async]]
            [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(deftest async-eval-string-js-lib-test
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

(deftest async-eval-string-cljs-source-lib-test
  (async done
         (p/let [ctx (sci/init {:async-load-fn
                                (fn [{:keys [libname opts ctx ns]}]
                                  {:source "(ns foobar) (defn hello [] :hello)"})})
                 code (str/join
                       "\n"
                       (map pr-str '[(ns dude (:require [foobar :as my-lib]))
                                     (my-lib/hello)]))
                 res (scia/eval-string* ctx code)]
           (is (= :hello res))
           (done))))

(deftest async-eval-string-cljs-source-lib-error-test
  (async done
         (-> (p/let [ctx (sci/init {:async-load-fn
                                    (fn [{:keys [libname opts ctx ns]}]
                                      (js/Promise.reject (js/Error. "Not found")))})
                     code (str/join
                           "\n"
                           (map pr-str '[(ns dude (:require [foobar :as my-lib]))
                                         (my-lib/hello)]))
                     res (scia/eval-string* ctx code)])
             (p/catch (fn [err]
                        (is (= "Not found" (.-message err)))
                        (done))))))
