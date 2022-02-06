(ns sci.impl.resolve
  {:no-doc true}
  (:require [clojure.string :as str]
            [sci.impl.evaluator :as eval]
            [sci.impl.faster :as faster]
            [sci.impl.interop :as interop]
            [sci.impl.records :as records]
            [sci.impl.utils :as utils :refer [strip-core-ns
                                              ana-macros
                                              ctx-fn
                                              kw-identical?]]
            [sci.impl.vars :as vars]))

(defn throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(defn mark-resolve-sym
  [sym]
  (vary-meta
   sym
   (fn [m]
     (assoc m :sci.impl/op :resolve-sym))))

(defn check-permission! [ctx sym [check-sym  v]]
  (or (identical? utils/allowed-loop sym)
      (identical? utils/allowed-recur sym)
      (let [check-sym (strip-core-ns check-sym)
            allow (:allow ctx)]
        (when-not (if allow (or (and (vars/var? v) (not (:sci/built-in (meta v))))
                                (contains? allow check-sym))
                      true)
          (throw-error-with-location (str sym " is not allowed!") sym))
        (let [deny (:deny ctx)]
          (when (if deny (contains? deny check-sym)
                    false)
            (throw-error-with-location (str sym " is not allowed!") sym))))))

(defn lookup*
  ([ctx sym call?] (lookup* ctx sym call? false))
  ([ctx sym call? only-var?]
   (let [sym-ns (some-> (namespace sym) symbol)
         sym-name (symbol (name sym))
         env (faster/get-2 ctx :env)
         env @env
         cnn (vars/current-ns-name)
         the-current-ns (-> env :namespaces cnn)
         ;; resolve alias
         sym-ns (when sym-ns (or (get-in the-current-ns [:aliases sym-ns])
                                 sym-ns))]
     (if sym-ns
       (or
        (when (or (= sym-ns 'clojure.core) (= sym-ns 'cljs.core))
          (or (some-> env :namespaces (get 'clojure.core) (find sym-name))
              (when-let [v (when call? (get ana-macros sym-name))]
                [sym v])))
        (or (some-> env :namespaces (get sym-ns) (find sym-name))
            (when-not only-var?
              (when-let [clazz (interop/resolve-class ctx sym-ns)]
                [sym (if call?
                       (with-meta
                         [clazz sym-name]
                         {:sci.impl.analyzer/static-access true})
                       (ctx-fn
                        (fn [_ctx _bindings]
                          (interop/get-static-field [clazz sym-name]))
                        nil
                        sym
                        (assoc (meta sym)
                               :file @vars/current-file
                               :ns @vars/current-ns)))]))))
       ;; no sym-ns
       (or
        ;; prioritize refers over vars in the current namespace, see 527
        (when-let [refers (:refers the-current-ns)]
          (find refers sym-name))
        (find the-current-ns sym) ;; env can contain foo/bar symbols from bindings
        (let [kv (some-> env :namespaces (get 'clojure.core) (find sym-name))]
          ;; only valid when the symbol isn't excluded
          (when-not (some-> the-current-ns
                            :refer
                            (get 'clojure.core)
                            :exclude
                            (contains? sym-name))
            kv))
        (when (when call? (get ana-macros sym))
          [sym sym])
        (when-not only-var?
          (or
           (when-let [c (interop/resolve-class ctx sym)]
             [sym c])
           ;; resolves record or protocol referenced as class
           ;; e.g. clojure.lang.IDeref which is really a var in clojure.lang/IDeref
           (when-let [x (records/resolve-record-or-protocol-class ctx sym)]
             [sym x]))))))))

(defn update-parents
  ":syms = closed over values"
  [ctx closure-bindings ob]
  (let [parents (:parents ctx)
        arity (:arity ctx)
        params (:params ctx)
        ;; TODO: we can make an explicit counter here
        offset (count params)
        ;; TODO: minor - we can shortcut when we detect a sym was already added on the lowest level
        ;; But then we'd have to start on the lowest level, which is fine too.
        new-cb (vswap! closure-bindings
                       (fn [cb]
                         (first (reduce
                                 (fn [[acc path] entry]
                                   (let [path (conj path entry)
                                         path-syms path]
                                     [(update-in acc path-syms (fn [entry]
                                                                 (let [syms (or (:syms entry)
                                                                                #{})
                                                                       new-syms (conj syms ob)
                                                                       entry (assoc entry :syms new-syms)
                                                                       entry (if (and (= path-syms parents)
                                                                                      ;; something was added
                                                                                      (not= syms new-syms))
                                                                               (let [idx (+ offset (dec (count new-syms)))]
                                                                                 (assoc-in entry [arity ob] idx))
                                                                               entry)]
                                                                   entry)))
                                      path]))
                                 [cb []]
                                 parents))))
        ;; TODO: closure-idx also depends on new let bindings introduced in the
        ;; body of a function, so we'll have to keep a separate counter for this
        ;; let's try without let first, shall we?
        closure-idx (get-in new-cb (conj parents arity ob))]
    closure-idx))

(defn lookup
  ([ctx sym call?] (lookup ctx sym call? nil))
  ([ctx sym call? tag]
   (let [bindings (faster/get-2 ctx :bindings)]
     (or
      ;; bindings are not checked for permissions
      (when-let [[k v]
                 (find bindings sym)]
        ;; (assert (symbol? v) (str "Not a symbol: " v))
        (let [idx (when-let [cb (:closure-bindings ctx)]
                    (when-let [oi (:outer-idens ctx)]
                      (when-let [ob (oi v)]
                        (update-parents ctx cb ob))))
              v (if call? ;; resolve-symbol is already handled in the call case
                  (mark-resolve-sym k)
                  (let [idx (or idx (get (:iden->idx ctx) v))
                        v (ctx-fn
                           (fn [_ctx ^objects bindings]
                             (aget bindings idx)
                             #_(eval/resolve-symbol bindings k))
                           nil
                           (if tag
                             (vary-meta k assoc :tag tag)
                             k))]
                    v))]
          [k v]))
      (when-let [kv (lookup* ctx sym call?)]
        (when (:check-permissions ctx)
          (check-permission! ctx sym kv))
        kv)))))

;; workaround for evaluator also needing this function
(vreset! utils/lookup lookup)

(defn resolve-symbol
  ([ctx sym] (resolve-symbol ctx sym false nil))
  ([ctx sym call?] (resolve-symbol ctx sym call? nil))
  ([ctx sym call? tag]
   (let [res (second
              (or
               (lookup ctx sym call? tag)
               (let [n (name sym)]
                 (cond
                   (and call?
                        (str/starts-with? n ".")
                        (> (count n) 1))
                   [sym 'expand-dot*] ;; method invocation
                   (and call?
                        (str/ends-with? n ".")
                        (> (count n) 1))
                   [sym 'expand-constructor]
                   :else
                   (throw-error-with-location
                    (str "Could not resolve symbol: " (str sym))
                    sym)))))]
     ;; (prn 'resolve sym '-> res (meta res))
     res)))
