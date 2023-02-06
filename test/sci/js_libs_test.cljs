(ns sci.js-libs-test
  (:require ["fs" :as fs]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing]]
            [sci.core :as sci]))

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
           "img")))))
