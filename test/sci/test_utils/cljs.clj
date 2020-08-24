(ns sci.test-utils.cljs
  (:require [cljs.test :as cljs-test]))

(defmethod cljs-test/assert-expr 'thrown-with-data?
  [_menv msg [_ data expr]]
  `(let [expected# ~data
         msg# ~msg]
     (try
       ~expr
       (test/do-report
        {:type :fail
         :message msg#
         :expected expected#
         :actual nil})
       (catch cljs.core/ExceptionInfo ex#
         (let [data# (ex-data ex#)]
           (test/do-report
            {:type (if (= expected# (select-keys data# (keys expected#)))
                     :pass
                     :fail)
             :message msg#
             :expected expected#
             :actual data#}))))))

