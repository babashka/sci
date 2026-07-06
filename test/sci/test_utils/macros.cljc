(ns sci.test-utils.macros
  (:require
   #?@(:cljd [] :default [[cljs.test :as cljs-test]])
   #?(:clj [clojure.test :as test])
   [sci.test-utils.utils]))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

;; cljd assert-expr is not extensible, use a boolean-returning macro
#?(:cljd
   (defmacro thrown-with-data? [& args]
     (let [[msg-re expected expr] (if (= 3 (count args))
                                    args
                                    [nil (first args) (second args)])]
       `(try
          ~expr
          false
          (catch cljd.core/ExceptionInfo e#
            (let [data# (ex-data e#)
                  msg# (ex-message e#)]
              (and (or (nil? ~msg-re) (re-find ~msg-re msg#))
                   (sci.test-utils.utils/submap? ~expected data#))))))))

#?(:cljd nil
   :clj
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
                 (if (and msg-re# (not (re-find msg-re# ex-msg#)))
                   {:type :fail
                    :message msg#
                    :expected msg-re#
                    :actual ex-msg#}
                   {:type (if (sci.test-utils.utils/submap? expected# data#)
                            :pass
                            :fail)
                    :message msg#
                    :expected expected#
                    :actual data#})))))))))

#?(:cljd nil
   :default
   (do
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
                     (if (and msg-re# (not (re-find msg-re# ex-msg#)))
                       {:type :fail
                        :message msg#
                        :expected msg-re#
                        :actual ex-msg#}
                       {:type (if (sci.test-utils.utils/submap? expected# data#)
                                :pass
                                :fail)
                        :message msg#
                        :expected expected#
                        :actual data#})))))))))))
