(ns sci.impl.reify
  {:no-doc true}
  ;; excluding reify breaks variadic fn compilation on cljd, macro fn named reify-macro there
  (:refer-clojure :exclude [#?@(:cljd [] :default [reify])])
  #?(:cljd (:require [sci.impl.types :as t])
     :clj (:require [sci.ctx-store :as store]))
  #?(:cljs (:require [sci.impl.deftype :as deftype]
                     [sci.impl.types :as t])))

(defn #?(:cljd reify-macro :clj reify :cljs reify) [form _ & args]
  (let [{classes true methods false} (group-by symbol? args)
        methods-by-name (group-by first methods)
        simple-meth #(if (simple-symbol? %) % (symbol (name %)))
        ;; per-method implemented arities (arg count includes this), used to
        ;; install native CLJS protocol slots only for implemented arities
        arities (into {}
                      (map (fn [[meth bodies]]
                             [(simple-meth meth)
                              (into #{} (map (comp count second)) bodies)]))
                      methods-by-name)
        methods (->> methods-by-name
                     (map (fn [[meth bodies]]
                            `['~(simple-meth meth) (fn ~@(map rest bodies))]))
                     (into {}))]
    `(clojure.core/reify* '~form ~(vec classes) ~methods '~arities)))

(defn reify*
  #?(:cljd [_form classes methods _arities]
     :clj [form classes methods _arities]
     :cljs [_form classes methods arities])
     #?(:cljd (t/->Reified classes methods (set classes))
        :clj (let [{interfaces true protocols false} (group-by class? classes)]
            (if-let [factory (:reify-fn (store/get-ctx))]
              (if-let [obj (factory {:interfaces (set interfaces)
                                     :methods methods
                                     :protocols (set protocols)})]
                (with-meta obj (meta form))
                (throw (ex-info (str "Unsupported interface in reify: " (first interfaces))
                                {:interfaces interfaces})))
              (throw (ex-info (str "No reify factory for: " interfaces)
                              {:class class}))))
     ;; NOTE: in CLJS everything is a protocol in reify, except Object
     ;; So it's probably better if we dissoc-ed that from the set of classes
     ;; However, we only use that set to test in satisfies?
     :cljs (let [obj (t/->Reified classes methods (set classes))]
             (doseq [c classes]
               ;; native CLJS protocol entry (copy-var/copy-ns on a protocol):
               ;; install the implemented slots directly on this instance
               (when (and (map? c) (:marker-setter c))
                 (deftype/-install-native-protocol-on!
                  obj c
                  (into {}
                        (keep (fn [[msym m]]
                                (when-let [f (get methods msym)]
                                  [msym {:arities (or (get arities msym)
                                                      (set (keys (:setters m))))
                                         :impl f}])))
                        (:native-methods c)))))
             obj)))
