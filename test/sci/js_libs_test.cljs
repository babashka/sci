(ns sci.js-libs-test
  (:require ["fs" :as fs]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing async]]
            [sci.core :as sci]
            [sci.async :as scia]
            [promesa.core :as p]))

(deftest js-libs-test
  (testing "alias"
    (let [ctx (sci/init {})]
      (sci/add-js-lib! ctx "fs" fs)
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs" :as fs] '[clojure.string :as str])
                                   (first (str/split-lines (fs/readFileSync "README.md" "utf-8")))))
           "img"))))
  (testing "refer"
    (let [ctx (sci/init {})]
      (sci/add-js-lib! ctx "fs" fs)
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs" :refer [readFileSync]]
                                            '[clojure.string :as str])
                                   (first (str/split-lines (readFileSync "README.md" "utf-8")))))
           "img"))))
  (testing "load-fn"
    (let [ctx-holder (atom nil)
          ctx (sci/init {:load-fn (fn [_]
                                    (sci/add-js-lib! @ctx-holder "fs" fs)
                                    {})})]
      (reset! ctx-holder ctx)
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs" :refer [readFileSync]]
                                            '[clojure.string :as str])
                                   (first (str/split-lines (readFileSync "README.md" "utf-8")))))
           "img"))))
  )

(deftest async-eval-string-js-lib-test
  (async done
         (p/let [ctx (sci/init {:async-load-fn
                                (fn [{:keys [libname ctx]}]
                                  (case libname
                                    "fs"
                                    (do
                                      (sci/add-js-lib! ctx "fs" fs)
                                      (js/Promise.resolve {}))))})
                 code (str/join
                       "\n"
                       (map pr-str '[(ns dude (:require ["fs" :as fs]
                                                        [clojure.string :as str]))
                                     (first (str/split-lines (readFileSync "README.md" "utf-8")))]))
                 res (scia/eval-string* ctx code)]
           (is (str/includes? res "img"))
           (done))))
