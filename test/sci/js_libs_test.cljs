(ns sci.js-libs-test
  (:require ["fs" :as fs]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing async]]
            [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(deftest js-libs-test
  (testing "init"
    (let [ctx (sci/init {:js-libs {"fs" fs}})]
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs" :as fs] '[clojure.string :as str])
                                   (first (str/split-lines (fs/readFileSync "README.md" "utf-8")))))
           "img"))))

  (testing "merge"
    (let [ctx (sci/init {})
          ctx2 (sci/merge-opts ctx {:js-libs {"fs" fs}})]
      (is (str/includes?
           (sci/eval-form ctx2 '(do (require '["fs" :as fs] '[clojure.string :as str])
                                    (first (str/split-lines (fs/readFileSync "README.md" "utf-8")))))
           "img"))))

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
           "img"))
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs" :refer [readFileSync] :rename {readFileSync exists?}]
                                            '[clojure.string :as str])
                                   (first (str/split-lines (exists? "README.md" "utf-8")))))
           "img"))))
  (testing "rename"
    (let [ctx (sci/init {})]
      (sci/add-js-lib! ctx "fs" fs)
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs" :refer [readFileSync] :rename {readFileSync slurp}]
                                            '[clojure.string :as str])
                                   (first (str/split-lines (slurp "README.md" "utf-8")))))
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
  (testing "property access pattern"
    (let [ctx-holder (atom nil)
          ctx (sci/init {:load-fn (fn [_]
                                    (sci/add-js-lib! @ctx-holder "fs" fs)
                                    {})})]
      (reset! ctx-holder ctx)
      (is (str/includes?
           (sci/eval-form ctx '(do (require '["fs$readFileSync" :as slurp]
                                            '[clojure.string :as str])
                                   (first (str/split-lines (slurp "README.md" "utf-8")))))
           "img")))))

(deftest async-eval-string-js-lib-alias-test
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
                                     (first (str/split-lines (fs/readFileSync "README.md" "utf-8")))]))
                 res (scia/eval-string* ctx code)]
           (is (str/includes? res "img"))
           (done))))

(deftest async-eval-string-js-lib-refer-test
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
                       (map pr-str '[(ns dude (:require ["fs" :refer [readFileSync]]
                                                        [clojure.string :as str]))
                                     (first (str/split-lines (readFileSync "README.md" "utf-8")))]))
                 res (scia/eval-string* ctx code)]
           (is (str/includes? res "img"))
           (done))))

(deftest async-eval-string-js-lib-property-access-test
  (async done
         (p/let [ctx (sci/init {:async-load-fn
                                (fn [{:keys [libname ctx]}]
                                  (case libname
                                    "fs"
                                    (do
                                      (sci/add-js-lib! ctx "fs" fs)
                                      (js/Promise.resolve {}))))})
                 code (str/join "\n" (map pr-str '[(ns foo (:require ["fs$readFileSync" :as slurp]
                                                                     ["fs$existsSync" :as exists?]
                                                                     [clojure.string :as str]))
                                                   [(first (str/split-lines (slurp "README.md" "utf-8")))
                                                    (exists? "README.md")]]))
                 res (scia/eval-string* ctx code)]
           (is (str/includes? (first res) "img"))
           (is (true? (second res)))
           (done))))
