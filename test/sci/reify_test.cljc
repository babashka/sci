(ns sci.reify-test
  (:require [clojure.test :refer #?(:clj [deftest is testing]
                                    :cljs [deftest])]
            [sci.test-utils :as tu])
  (:import [sci.impl.types IReified]))

(defn eval* [prog & {:as opts}]
  (tu/eval* prog opts))

(deftest reify-test
  #?(:clj
     (testing "reifying Object"
       (is (= "this!" (eval* "(str (reify Object (toString [this] \"this!\")))"))))))

(deftest reify-mixed-protocol-class-test
  #?(:clj
     (testing "reifying Object and IDeref"
       (is (= ["obj-str" "ideref-deref" "protocol-custom"]
              (eval* "
(defprotocol Protocol
  (custom [this]))
(let [r (reify
          Object (toString [this] \"obj-str\")
          clojure.lang.IDeref (deref [this] \"ideref-deref\")
          Protocol (custom [this] \"protocol-custom\")
          )]
  [(str r) (deref r) (custom r)])"
                     :reify {'#{java.lang.Object sci.impl.types.IReified}
                             (fn [methods]
                               (reify
                                 Object
                                 (toString [this]
                                   ((get-in methods '[java.lang.Object toString]) this))

                                 IReified
                                 (getMethods [this]
                                   ((get-in methods '[sci.impl.types.IReified getMethods]) this))
                                 (getInterfaces [this]
                                   ((get-in methods '[sci.impl.types.IReified getInterfaces]) this))))}))))))
