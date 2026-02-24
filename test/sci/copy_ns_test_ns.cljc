(ns sci.copy-ns-test-ns)

(defn foo
  "YOLO"
  {:copy-this true}
  [] :foo)

(defn bar [] :bar)

(defn baz
  {:awesome-meta true}
  [])

(defn quux {:exclude-this true} [])

(defn skip-wiki {:skip-wiki true} [])

(defprotocol ITest
  (x [y]))

(defn ^:sci/macro vec-macro
  [_ _ x]
  `[~x ~x])

(defn- private-fn [] :private-from-other-ns)
