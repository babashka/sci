(ns sci.opts
  (:require [sci.impl.vars :refer [dynamic-var]]
            [sci.impl.opts :refer [init-env!
                                   presets
                                   default-imports
                                   process-permissions
                                   normalize-classes
                                   default-classes]])
  #?(:cljs (:require-macros [sci.opts :refer [with-dynamic-var]])))

(defmacro with-dynamic-var
  ([m name] `(with-dynamic-var ~m ~name nil nil))
  ([m name init-val] `(with-dynamic-var ~m ~name ~init-val nil))
  ([m name init-val meta]
   `(let [n# '~name
          v# (dynamic-var n# ~init-val ~meta)]
      (assoc ~m n# v#))))

(defn initialize
  "Initializes options"
  [{:keys [:bindings :env
           :allow :deny
           :realize-max
           :preset ;; used by malli
           :aliases
           :namespaces
           :classes
           :imports
           :features]}]
  (let [preset (get presets preset)
        env (or env (atom {}))
        imports (merge default-imports imports)
        bindings bindings
        _ (init-env! env bindings aliases namespaces imports)
        ctx (merge {:env env
                    :bindings {}
                    :allow (process-permissions (:allow preset) allow)
                    :deny (process-permissions (:deny preset) deny)
                    :realize-max (or realize-max (:realize-max preset))
                    :features features}
                   (normalize-classes (merge default-classes classes)))]
    ctx))
