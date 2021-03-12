(ns sci.proxy-test
  (:require [clojure.test :refer [deftest is]]
            [sci.core :as sci]))

(deftest APersistentMap-proxy-test
  (let [obj (sci/eval-string
             "
(proxy [clojure.lang.APersistentMap] []
 (valAt
   ([k] [:k k])
   ([k default] [:k k :default default])))
"
             {:classes {'clojure.lang.APersistentMap clojure.lang.APersistentMap}
              :proxy-fn (fn [{:keys [:class :methods]}]
                          (case (.getName ^Class class)
                            "clojure.lang.APersistentMap"
                            (proxy [clojure.lang.APersistentMap] []
                              (valAt
                                ([k] ((get methods 'valAt) k))
                                ([k default] ((get methods 'valAt) k default))))))})]

    (is (= [:k :f] (obj :f)))
    (is (= [:k :f :default :def] (obj :f :def)))))



