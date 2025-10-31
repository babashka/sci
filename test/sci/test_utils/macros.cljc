(ns sci.test-utils.macros
  (:require
   [sci.test-utils.utils]
   #?@(:cljs [] :default [[clojure.test :as test]])
   #?@(:cljr [] :default [[cljs.test :as cljs-test]])))

#?(:cljs nil :default (set! *warn-on-reflection* true))

#?(:cljs nil :default
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
                   ex-msg# (#?(:clj .getMessage :default ex-message) ex#)]
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
  (when #?(:clj (not (:ns &env))
           :cljs (re-matches #".*\$macros" (name (ns-name *ns*)))
           :default true)
    `(do ~@body)))

#?(:cljr nil :default
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
)
