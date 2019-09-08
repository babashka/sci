(ns sci.impl.readers
  {:no-doc true}
  (:require [clojure.walk :refer [postwalk]]
            [sci.impl.utils :refer [mark-resolve-sym]]))

(defn read-fn [expr]
  (let [state (volatile! {:max-fixed 0 :var-args? false})
        expr (postwalk (fn [elt]
                         (if (symbol? elt)
                           (if-let [[_ m] (re-matches #"^%(.*)" (name elt))]
                             (cond (empty? m)
                                   (do (vswap! state update :max-fixed max 1)
                                       '%1)
                                   (= "&" m)
                                   (do (vswap! state assoc :var-args? true)
                                       elt)
                                   :else (do (let [n #?(:clj (Integer/parseInt m)
                                                        :cljs (js/parseInt m))]
                                               (vswap! state update :max-fixed max n))
                                             elt))
                             elt)
                           elt))
                       expr)
        {:keys [:max-fixed :var-args?]} @state
        fixed-names (map #(symbol (str "%" %)) (range 1 (inc max-fixed)))
        fixed-names (map mark-resolve-sym fixed-names)
        var-args-sym (mark-resolve-sym '%&)
        arg-list (vec (concat fixed-names (when var-args?
                                            ['& var-args-sym])))
        destructure-vec (vec (interleave fixed-names fixed-names))
        destructure-vec (if var-args?
                          (conj destructure-vec var-args-sym var-args-sym)
                          destructure-vec)
        form {:sci/fn true
              :sci/fn-bodies
              [{:sci/binding-vector arg-list
                ;; body gets macroexpanded after read phase
                :sci/body [expr]
                :sci/fixed-arity (count fixed-names)
                :sci/destructure-vec destructure-vec
                :sci/arg-list arg-list
                :sci/fixed-names fixed-names
                :sci/var-arg-name (when var-args? '%&)}]}]
    form))

;;;; Scratch

(comment)
