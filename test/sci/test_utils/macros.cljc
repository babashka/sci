(ns sci.test-utils.macros
  (:require
   [sci.test-utils.utils]
   #?(:clj [clojure.test :as test])
   [cljs.test :as cljs-test]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
  (defmethod test/assert-expr 'thrown-with-data?
    [msg [_ msg-re data expr]]
    (let [[msg-re expected expr]
          (if expr [msg-re data expr]
              [nil msg-re data])]
      `(let [msg-re# ~msg-re
             expected# ~expected
             msg# ~msg]
         (try
           ~expr
           (test/do-report
            {:type :fail
             :message msg#
             :expected expected#
             :actual nil})
           (catch Exception ex#
             (let [data# (ex-data ex#)
                   ex-msg# (.getMessage ex#)]
               (test/do-report
                (if msg-re#
                  (if (re-find msg-re# ex-msg#)
                    {:type (if (sci.test-utils.utils/submap? expected# data#)
                             :pass
                             :fail)
                     :message msg#
                     :expected expected#
                     :actual data#}
                    {:type :fail
                     :message msg#
                     :expected msg-re#
                     :actual ex-msg#}))))))))))

(defmacro deftime
  "From macrovich"
  [& body]
  (when #?(:clj (not (:ns &env)) :cljs (re-matches #".*\$macros" (name (ns-name *ns*))))
    `(do ~@body)))

(deftime
  (defmethod #?(:clj cljs.test/assert-expr
                :cljs cljs.test$macros/assert-expr)
    'thrown-with-data?
    [_menv msg [_ msg-re data expr]]
    (let [[msg-re expected expr]
          (if expr [msg-re data expr]
              [nil msg-re data])]
      `(let [msg-re# ~msg-re
             expected# ~expected
             msg# ~msg]
         (cljs.test/do-report
          (try
            ~expr
            {:type :fail
             :message msg#
             :expected expected#
             :actual nil}
            (catch cljs.core/ExceptionInfo ex#
              (let [data# (ex-data ex#)
                    ex-msg# (.-message ex#)]
                (if msg-re#
                  (if (re-find msg-re# ex-msg#)
                    {:type (if (sci.test-utils.utils/submap? expected# data#)
                             :pass
                             :fail)
                     :message msg#
                     :expected expected#
                     :actual data#}
                    {:type :fail
                     :message msg#
                     :expected msg-re#
                     :actual ex-msg#}))))))))))
