(ns ^:no-doc sci.impl.macros
  #?(:cljs
     (:require-macros
      [sci.impl.macros :refer [deftime ?]])))

(defmacro deftime
  "Private. deftime macro from https://github.com/cgrand/macrovich"
  [& body]
  (when #?(:clj (not (:ns &env))
           :cljs (when-let [n (and *ns* (ns-name *ns*))]
                   (re-matches #".*\$macros" (name n)))
           :default true)
    `(do ~@body)))

(defmacro usetime
  "Private. usetime macro from https://github.com/cgrand/macrovich"
  [& body]
  (when #?(:clj true :cljs (not (re-matches #".*\$macros" (name (ns-name *ns*)))) :default true)
    `(do ~@body)))

(deftime
  (defmacro ?
    "Private. case macro from https://github.com/cgrand/macrovich"
    [& {:keys [cljs clj]}]
 #?(:cljr clj
    :default
    (if (contains? &env '&env)
      `(if (:ns ~'&env) ~cljs ~clj)
      (if #?(:clj (:ns &env) :cljs true)
        cljs
        clj)))))
