(ns example
  (:require
   [clojure.string :as str]
   [goog.object :as gobject]
   [promesa.core :as p]
   [sci.async :as scia]
   [sci.core :as sci]))

(defn async-load-fn
  [{:keys [libname opts ctx ns]}]
  (let [[libname suffix] (str/split libname "$")]
       (case libname
         "some_js_lib"
         (p/let [js-lib (p/resolved #js {:add +
                                         :subtract -
                                         :multiply *
                                         :default *})]
           (let [js-lib (if suffix
                          (gobject/getValueByKeys js-lib (.split suffix "."))
                          js-lib)
                 munged (symbol (munge libname))]
             ;; register class globally in context
             (sci/add-class! ctx munged js-lib)
             (let [{:keys [as refer]} opts]
               (when as
                 ;; import class in current namespace with reference to globally
                 ;; registed class
                 (sci/add-import! ctx ns munged as))
               (when refer
                 (doseq [sym refer]
                   (let [prop (gobject/get js-lib sym)
                         sub-libname (str munged "$" prop)]
                     ;; register sub-library globally
                     (sci/add-class! ctx sub-libname prop)
                     ;; add import to sub-library in current namespace
                     (sci/add-import! ctx ns sub-libname sym))))))
           {:handled true}))))

(def ctx (sci/init {:async-load-fn async-load-fn
                    ;; async require override
                    :namespaces {'clojure.core {'require scia/require}}
                    ;; allow JS interop globally
                    :classes {'js goog/global :allow :all}}))

;; allow printing
(sci/alter-var-root sci/print-fn (constantly *print-fn*))

(def code-1
  "
(ns example (:require [\"some_js_lib\" :as my-lib :refer [subtract]]))
[(my-lib/add 1 2) (subtract 3 2)]
")

(def code-2 "
(require '[\"some_js_lib$default\" :as awesome])
(require '[\"some_js_lib$add\" :as add])
[(awesome 3 2) (add 4 5)]
")

(p/let [result (scia/eval-string* ctx code-1)
        _ (println "Result:" result) ;; [3 1]
        result (scia/eval-string* ctx code-2)
        _ (println "Result:" result) ;; [5 9]
        ]
  )
