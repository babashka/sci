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
          (proxy-super equals other))))))

(defn- apersistentmap-proxy-fn [{:keys [class methods]}]
  (case (.getName ^Class class)
    "clojure.lang.APersistentMap"
    (proxy [clojure.lang.APersistentMap] []
      (iterator [] ((get methods 'iterator) this))
      (containsKey [k] ((get methods 'containsKey) this k))
      (entryAt [k] ((get methods 'entryAt) this k))
      (valAt
        ([k] ((get methods 'valAt) this k))
        ([k default] ((get methods 'valAt) this k default)))
      (cons [v]
        (if-let [m (get methods 'cons)]
          (m this v)
          (proxy-super cons v)))
      (count [] ((get methods 'count) this))
      (assoc [k v] ((get methods 'assoc) this k v))
      (without [k] ((get methods 'without) this k))
      (seq [] ((get methods 'seq) this))
      (meta [] ((get methods 'meta) this))
      (withMeta [md] ((get methods 'withMeta) this md))
      (toString []
        (if-let [m (get methods 'toString)]
          (m this)
          (proxy-super toString))))))

(def ^:private proxy-super-opts
  {:classes {:allow :all}
   :proxy-fn object-proxy-fn})

(def ^:private apersistentmap-opts
  {:classes {'clojure.lang.APersistentMap clojure.lang.APersistentMap
             :allow :all}
   :proxy-fn apersistentmap-proxy-fn})

(deftest proxy-super-Object-test
  (testing "proxy-super on Object: toString, hashCode, equals"
    (is (= {:starts-with-prefix true
            :hashcode-gt-999 true
            :equals-self true
            :not-equals-other true}
           (sci/eval-string
            "(let [p (proxy [Object] []
                        (toString [] (str \"prefix:\" (proxy-super toString)))
                        (hashCode [] (+ 1000 (proxy-super hashCode)))
                        (equals [other] (proxy-super equals other)))]
                {:starts-with-prefix (.startsWith (str p) \"prefix:\")
                 :hashcode-gt-999 (> (.hashCode p) 999)
                 :equals-self (.equals p p)
                 :not-equals-other (not (.equals p (Object.)))})"
            proxy-super-opts))))
  (testing "proxy without override uses default"
    (is (true? (sci/eval-string
                "(let [p (proxy [Object] [])]
                   (.contains (str p) \"@\"))"
                proxy-super-opts))))
  (testing "proxy-super restores mappings after call"
    (is (true? (sci/eval-string
                "(let [p (proxy [Object] []
                            (toString [] (str \"custom:\" (proxy-super toString))))]
                   (let [r1 (str p) r2 (str p)]
                     (and (.startsWith r1 \"custom:\")
                          (.startsWith r2 \"custom:\"))))"
                proxy-super-opts)))))

(deftest proxy-super-APersistentMap-test
  (testing "proxy-super cons delegates to APersistentMap.cons which calls assoc"
    (is (= {:assoc-called [:a 1]}
           (sci/eval-string
            "(let [m (proxy [clojure.lang.APersistentMap] []
                       (iterator [] (.iterator {}))
                       (containsKey [k] false)
                       (entryAt [k] nil)
                       (valAt ([k] nil) ([k d] d))
                       (cons [v] (proxy-super cons v))
                       (count [] 0)
                       (assoc [k v] {:assoc-called [k v]})
                       (without [k] nil)
                       (seq [] nil)
                       (meta [] nil)
                       (withMeta [md] nil))]
               (conj m [:a 1]))"
            apersistentmap-opts)))))

(deftest proxy-mappings-test
  (testing "proxy-mappings returns a map containing the overridden method"
    (is (= {:is-map true :has-toString true}
           (sci/eval-string
            "(let [p (proxy [Object] []
                        (toString [] \"hello\"))]
               {:is-map (map? (proxy-mappings p))
                :has-toString (contains? (proxy-mappings p) \"toString\")})"
            proxy-super-opts)))))

(deftest update-proxy-test
  (testing "update-proxy changes method behavior"
    (is (= ["original" "updated"]
           (sci/eval-string
            "(let [p (proxy [Object] []
                        (toString [] \"original\"))]
               (let [before (str p)]
                 (update-proxy p {\"toString\" (fn [this] \"updated\")})
                 [before (str p)]))"
            proxy-super-opts)))))
