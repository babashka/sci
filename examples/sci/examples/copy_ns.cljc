(ns sci.examples.copy-ns
  (:refer-clojure :exclude [update-vals])
  (:require
   [edamame.core]
   [sci.core :as sci]))

(defn update-vals
  "Same as `update-vals` from clojure 1.11 but included here so tests
  can run with older versions of Clojure/Script."
  [m f]
  (with-meta
    (persistent!
     (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                (if #?(:clj (instance? clojure.lang.IEditableCollection m)
                       :cljs (implements? IEditableCollection m))
                  (transient m)
                  (transient {}))
                m))
    (meta m)))

(let [ens (sci/create-ns 'edamame.core)
      publics (ns-publics 'edamame.core)
      sci-ns (update-vals publics #(sci/copy-var* % ens))
      ctx (sci/init {:namespaces {'edamame.core sci-ns}})]
  (prn (sci/eval-string* ctx "(require '[edamame.core :as e]) (e/parse-string \"1\")"))
  ;;=> 1
  )

(let [ens (sci/create-ns 'edamame.core)
      sci-ns (sci/copy-ns edamame.core ens {:exclude [iobj?]})
      ctx (sci/init {:namespaces {'edamame.core sci-ns}})]
  (prn (sci/eval-string* ctx "(require '[edamame.core :as e]) (e/parse-string \"1\")"))
  ;;=> 1
  )
