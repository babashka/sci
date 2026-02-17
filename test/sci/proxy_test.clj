(ns sci.proxy-test
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.types :as types]))

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
    (is (= [:k :f :default :def] (obj :f :def))))
  (testing "qualified method names"
    (let [obj (sci/eval-string
               "
(proxy [clojure.lang.APersistentMap] []
 (foobar/valAt
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
      (is (= [:k :f :default :def] (obj :f :def))))))

(defn- object-proxy-fn [{:keys [class methods]}]
  (case (.getName ^Class class)
    "java.lang.Object"
    (proxy [Object] []
      (toString []
        (if-let [f (get methods 'toString)]
          (f this)
          (proxy-super toString)))
      (hashCode []
        (if-let [f (get methods 'hashCode)]
          (f this)
          (proxy-super hashCode)))
      (equals [other]
        (if-let [f (get methods 'equals)]
          (f this other)
          (proxy-super equals other))))
))

(def ^:private proxy-super-opts
  {:classes {:allow :all}
   :proxy-fn object-proxy-fn})

(deftest proxy-super-toString-test
  (testing "proxy-super calls superclass toString"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (toString [] (str \"prefix:\" (proxy-super toString))))]
                     (.startsWith (str p) \"prefix:\"))"
                  proxy-super-opts)]
      (is (true? result))))
  (testing "proxy-super toString returns the default Object toString"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (toString [] (proxy-super toString)))]
                     (.contains (str p) \"@\"))"
                  proxy-super-opts)]
      (is (true? result)))))

(deftest proxy-super-without-override-test
  (testing "proxy without toString override uses default"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] [])]
                     (.contains (str p) \"@\"))"
                  proxy-super-opts)]
      (is (true? result)))))

(deftest proxy-super-hashCode-test
  (testing "proxy-super hashCode calls superclass"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (hashCode [] (+ 1000 (proxy-super hashCode))))]
                     (> (.hashCode p) 999))"
                  proxy-super-opts)]
      (is (true? result)))))

(deftest proxy-super-equals-test
  (testing "proxy-super equals calls superclass"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (equals [other]
                               (proxy-super equals other)))]
                     (and (.equals p p)
                          (not (.equals p (Object.)))))"
                  proxy-super-opts)]
      (is (true? result)))))

(deftest proxy-super-with-args-test
  (testing "proxy-super passes arguments to superclass equals"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (equals [other]
                               (proxy-super equals other)))]
                     ;; equals with self should be true, with different object false
                     [(boolean (.equals p p))
                      (boolean (.equals p (Object.)))])"
                  proxy-super-opts)]
      (is (= [true false] result)))))

(deftest proxy-mappings-test
  (testing "proxy-mappings returns the fn map"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (toString [] \"hello\"))]
                     (map? (proxy-mappings p)))"
                  proxy-super-opts)]
      (is (true? result))))
  (testing "proxy-mappings contains the overridden method"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (toString [] \"hello\"))]
                     (contains? (proxy-mappings p) \"toString\"))"
                  proxy-super-opts)]
      (is (true? result)))))

(deftest update-proxy-test
  (testing "update-proxy changes method behavior"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (toString [] \"original\"))]
                     (let [before (str p)]
                       (update-proxy p {\"toString\" (fn [this] \"updated\")})
                       [before (str p)]))"
                  proxy-super-opts)]
      (is (= ["original" "updated"] result)))))

(deftest proxy-super-restores-mappings-test
  (testing "proxy-super restores original mappings after call"
    (let [result (sci/eval-string
                  "(let [p (proxy [Object] []
                             (toString []
                               (str \"custom:\" (proxy-super toString))))]
                     ;; call twice to verify mappings are restored
                     (let [r1 (str p)
                           r2 (str p)]
                       (and (.startsWith r1 \"custom:\")
                            (.startsWith r2 \"custom:\"))))"
                  proxy-super-opts)]
      (is (true? result)))))
