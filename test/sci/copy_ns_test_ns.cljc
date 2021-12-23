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
