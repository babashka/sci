(ns sci.test-utils.utils)

(defn submap?
  "Is m1 a submap of m2? Adapted from
  https://github.com/clojure/spec-alpha2, clojure.test-clojure.spec"
  [m1 m2]
  (cond
    (and (map? m1) (map? m2))
    (every? (fn [[k v]] (and (contains? m2 k)
                             (submap? v (get m2 k))))
            m1)
    #?(:cljs (regexp? m1)
       :default (instance? #?(:clj java.util.regex.Pattern :cljr System.Text.RegularExpressions.Regex) m1))
    (re-find m1 m2)
    :else (= m1 m2)))
