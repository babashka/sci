(ns sci.proxy-test
  (:require [clojure.test :refer [deftest is]]
            [sci.core :as sci]))

(deftest APersistentMap-proxy-test
  (let [obj (sci/eval-string
             "
(proxy [clojure.lang.APersistentMap] []
 (valAt
   ([k] (and (instance? clojure.lang.APersistentMap this) [:k k]))
   ([k default] (and (instance? clojure.lang.APersistentMap this) [:k k :default default]))))
"
             {:classes {'clojure.lang.APersistentMap clojure.lang.APersistentMap}
              :proxy-fn (fn [{:keys [:class :methods]}]
                          (case (.getName ^Class class)
                            "clojure.lang.APersistentMap"
                            (proxy [clojure.lang.APersistentMap] []
                              (valAt
                                ([k] ((get methods 'valAt) this k))
                                ([k default] ((get methods 'valAt) this k default))))))})]

    (is (= [:k :f] (obj :f)))
    (is (= [:k :f :default :def] (obj :f :def)))))



