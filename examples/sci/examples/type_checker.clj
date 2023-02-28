(ns sci.examples.type-checker
  (:require [sci.core :as sci]))

(def ^:dynamic *type-errors* (atom []))

(defn inct [_&env &form t]
  (if-not (= :number t)
    (do (swap! *type-errors* conj {:message "Type error: inc received non-number"
                                   :loc (meta &form)})
        :any)
    t))

(defn assoct [_&env _&form m k v & kvs]
  (case m :map (apply assoc {} k v kvs)
        (if (map? m)
          (apply assoc m k v kvs)
          :any)))

(defn my-fnt [_ &form m]
  (when-not (:required m)
    (swap! *type-errors* conj {:message "Expected required key :required"
                               :loc (meta &form)})
    m))

(def typechecked-clojure {'clojure.core {'inc inct
                                         'str (fn [& _args] :string)}
                          'my-ns {'my-fn my-fnt}})

(binding [*type-errors* (atom [])]
  (sci/eval-string "

(inc (str :foo))
(my-ns/my-fn (assoc {} :without-required (inc \"foo\")))
(my-ns/my-fn (assoc {} :required 1)) ;; no error"

                   {:namespaces
                    typechecked-clojure })
  *type-errors*)

