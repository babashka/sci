(ns sci.impl.copy-vars
  {:no-doc true}
  (:require
   [sci.impl.cljs]
   [sci.impl.macros :as macros]
   [sci.impl.utils :as utils :refer [clojure-core-ns]]
   [sci.lang])
  #?(:cljs (:require-macros [sci.impl.copy-vars :refer [copy-var copy-core-var macrofy]])))

#?(:clj (set! *warn-on-reflection* true))

;; The following is produced with:
;; (def inlined (filter (comp :inline meta) (vals (ns-publics 'clojure.core))))
;; (map (comp :name meta) inlined)
(def inlined-vars
  '#{+' unchecked-remainder-int unchecked-subtract-int dec' short-array bit-shift-right aget = boolean bit-shift-left aclone dec < char unchecked-long unchecked-negate unchecked-inc-int floats pos? boolean-array alength bit-xor unsigned-bit-shift-right neg? unchecked-float num reduced? booleans int-array inc' <= -' * min get long double bit-and-not unchecked-add-int short quot unchecked-double longs unchecked-multiply-int int > unchecked-int unchecked-multiply unchecked-dec double-array float - byte-array zero? unchecked-dec-int rem nth nil? bit-and *' unchecked-add identical? unchecked-divide-int unchecked-subtract / bit-or >= long-array object-array doubles unchecked-byte unchecked-short float-array inc + chars ints bit-not byte max == count char-array compare shorts unchecked-negate-int unchecked-inc unchecked-char bytes})

(def cljs-resolve (resolve 'cljs.analyzer.api/resolve))

#?(:clj (def elide-vars (= "true" (System/getenv "SCI_ELIDE_VARS")))
   ;; for self-hosted
   :cljs (def elide-vars false))

(macros/deftime

  (defn ensure-quote [x]
    (if (and (seq? x) (= 'quote (first x)))
      x
      (list 'quote x)))

  (defn dequote [x]
    (if (and (seq? x) (= 'quote (first x)))
      (second x)
      x))

  (defn core-sym [sym]
    (symbol "clojure.core" (name sym)))

  (defn var-meta [&env sym opts & _a]
    (let [sym (dequote sym)
          macro (when opts (:macro opts))
          nm (when opts (:name opts))
          [fqsym sym] (if (qualified-symbol? sym)
                     [sym (symbol (name sym))]
                     [(symbol "clojure.core" (str sym)) sym])
          inline (contains? inlined-vars sym)
          fast-path (or (= 'or sym)
                        (= 'and sym)
                        (= 'case sym)
                        (= 'ns sym)
                        (= 'lazy-seq sym))
          varm (merge (cond-> {:name (or nm (list 'quote (symbol (name sym))))}
                    macro (assoc :macro true)
                    inline (assoc :sci.impl/inlined (:inlined opts fqsym)))
                  (let [#?@(:clj [the-var (macros/? :clj (resolve fqsym)
                                                    :cljs (atom nil))])]
                    (macros/? :clj #?(:clj  (let [m (meta the-var)
                                                  dyn (:dynamic m)
                                                  arglists (:arglists m)]
                                              (cond-> (if elide-vars {} {:doc (:doc m)})
                                                dyn (assoc :dynamic dyn)
                                                (if elide-vars false arglists)
                                                (assoc :arglists (list 'quote (:arglists m)))
                                                fast-path (assoc :sci.impl/fast-path (list 'quote sym))))
                                      :cljs nil)
                              :cljs (let [r (cljs-resolve &env fqsym)
                                          m (:meta r)
                                          dyn (:dynamic m)
                                          arglists (or (:arglists m) (:arglists r))]
                                      (cond-> {:arglists (ensure-quote arglists)
                                               :doc (or (:doc m) (:doc r))}
                                        dyn (assoc :dynamic dyn)
                                        arglists (assoc :arglists (ensure-quote arglists))
                                        fast-path (assoc :sci.impl/fast-path (list 'quote sym)))))))]
      #_(when (= 'inc sym)
        (prn varm))
      varm))

  (defmacro macrofy [& args]
    (let [[sym & args] args]
      `(macrofy* ~sym ~@args ~@(repeat (- 3 (count args)) nil) ~(var-meta &env sym nil))))


  ;; Note: self hosted CLJS can't deal with multi-arity macros so this macro is split in 2
  #?(:clj
     (if elide-vars
         (binding [*out* *err*]
           (println "SCI: eliding vars."))
       nil))
  (defmacro copy-var
    [sym ns & [opts]]
    (let [macro (:macro opts)
          #?@(:clj [the-var (macros/? :clj (resolve sym)
                                      :cljs (atom nil))])
          dyn (:dynamic opts)
          varm (cond-> (assoc (var-meta &env (or (:name opts)
                                                 (:copy-meta-from opts)
                                                 sym)
                                        opts)
                              :sci/built-in true
                              :ns ns)
                 dyn (assoc :dynamic dyn))
          nm (:name varm)
          ctx (:ctx opts)
          init (:init opts sym)]
      ;; NOTE: emit as little code as possible, so our JS bundle is as small as possible
      (if macro
        (macros/? :clj
                  #?(:clj  `(sci.lang.Var. ~(deref the-var) ~nm ~varm false ~ctx nil)
                     :cljs `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil))
                  :cljs `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil))
        (if elide-vars
            (if (or dyn ctx)
              `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil)
              sym)
           `(sci.lang.Var. ~init ~nm ~varm false ~ctx nil)))))
  (defmacro copy-core-var
    [sym]
    `(copy-var ~sym clojure-core-ns {:copy-meta-from ~(core-sym sym)}))

  )

(defn macrofy*
  ([f] (vary-meta f #(assoc % :sci/macro true)))
  ([sym f] (macrofy* sym f nil false))
  ([sym f ns] (macrofy* sym f ns false))
  ([sym f ns ctx?] (macrofy* sym f ns ctx? nil))
  ([sym f ns ctx? extra-meta]
   (let [ns (or ns clojure-core-ns)]
     (utils/new-var sym f (cond-> {:ns ns
                                   :macro true
                                   :sci/built-in true}
                            (and (not elide-vars)
                                 extra-meta)
                            (merge extra-meta))
                    ctx?))))

(defn new-var
  ([sym f] (new-var sym f nil false))
  ([sym f ns] (new-var sym f ns false))
  ([sym f ns ctx?] (new-var sym f ns ctx? nil))
  ([sym f ns ctx? extra-meta]
   (let [ctx? (or ctx? (true? ns))
         ns (if (true? ns)
              clojure-core-ns
              (or ns clojure-core-ns))]
     (assert (and (not (boolean? ns))
                  (instance? sci.lang.Namespace ns)) sym)
     (utils/new-var sym f (cond-> {:ns ns
                                   :sci/built-in true}
                            (and (not elide-vars)
                                 extra-meta)
                            (merge extra-meta))
                    ctx?))))
