(ns sci.impl.resolve
  {:no-doc true}
  (:require [clojure.string :as str]
            [sci.impl.faster :as faster]
            [sci.impl.interop :as interop]
            [sci.impl.records :as records]
            [sci.impl.types :refer [->Node]]
            [sci.impl.utils :as utils :refer [strip-core-ns
                                              ana-macros]]))

(defn throw-error-with-location [msg node]
  (utils/throw-error-with-location msg node {:phase "analysis"}))

(defn mark-resolve-sym
  [sym idx]
  (vary-meta
   sym
   (fn [m]
     (assoc m
            :sci.impl/op :resolve-sym
            :sci.impl/idx idx))))

(defn check-permission! [ctx sym kv]
  (or (identical? utils/allowed-loop sym)
      (identical? utils/allowed-recur sym)
      (let [check-sym (strip-core-ns sym)
            allow (:allow ctx)]
        (when-not (if allow
                    (let [[_ v] kv]
                      (or (and (utils/var? v) (not (:sci/built-in (meta v))))
                          (contains? allow check-sym)))
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
         sym-name-str (name sym)
         sym-name (symbol sym-name-str)
         env (faster/get-2 ctx :env)
         env @env
         cnn (utils/current-ns-name)
         the-current-ns (-> env :namespaces cnn)
         ;; resolve alias
         sym-ns (when sym-ns (or (get-in the-current-ns [:aliases sym-ns])
                                 sym-ns))
         sym-ns (get (:ns-aliases env) sym-ns sym-ns)]
     (if sym-ns
       (or
        #?(:clj
           (when (and (= 1 (.length sym-name-str))
                      (Character/isDigit (.charAt sym-name-str 0)))
             (when-let [clazz (interop/resolve-array-class ctx sym-ns sym-name-str)]
               [sym clazz])))
        (when
            #?(:clj (= 'clojure.core sym-ns)
               :cljs (or (= 'clojure.core sym-ns)
                         (= 'cljs.core sym-ns)))
          (or (some-> env :namespaces (get 'clojure.core) (find sym-name))
              (when-let [v (when call? (get ana-macros sym-name))]
                [sym v])))
        (some-> env :namespaces (get sym-ns) (find sym-name))
        (when-not only-var?
          (when-let [clazz (interop/resolve-class ctx sym-ns)]
            [sym (if (and call? #?(:clj (not (str/starts-with? sym-name-str "."))))
                   (with-meta
                     [clazz #?(:clj sym-name
                               :cljs (.split (str sym-name) "."))
                      sym-ns]
                     #?(:clj
                        (if (= "new" sym-name-str)
                          {:sci.impl.analyzer/invoke-constructor true}
                          {:sci.impl.analyzer/static-access true})
                        :cljs
                        {:sci.impl.analyzer/static-access true}))
                   #?(:clj
                      (with-meta
                        [clazz sym-name]
                        {:sci.impl.analyzer/interop true})
                      :cljs
                      (let [stack (assoc (meta sym)
                                         :file @utils/current-file
                                         :ns @utils/current-ns)
                            path (.split (str sym-name) ".")
                            len (alength path)]
                        (if (== 1 len)
                          (->Node
                           (interop/get-static-field clazz sym-name)
                           stack)
                          (->Node
                           (interop/get-static-fields clazz path)
                           stack))
                        )))])))
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
           #?(:clj
              (when-let [x (records/resolve-record-or-protocol-class ctx sym)]
                [sym x])
              :cljs
              (when-not (:dotted-access ctx)
                (when-let [x (records/resolve-record-or-protocol-class ctx sym)]
                  [sym x]))))))))))

(defn update-parents
  ":syms = closed over -> idx"
  [ctx closure-bindings ob]
  (let [parents (:parents ctx)
        new-cb (vswap! closure-bindings
                       (fn [cb]
                         (first
                          (reduce
                           (fn [[acc path] _idx]
                             (let [new-acc
                                   (update-in
                                    acc path
                                    (fn [entry]
                                      (let [iden->invoke-idx (or (:syms entry)
                                                                 {})
                                            added-before? (contains? iden->invoke-idx ob)]
                                        (if added-before?
                                          entry
                                          (assoc entry :syms
                                                 (assoc iden->invoke-idx
                                                        ob (count iden->invoke-idx)))))))
                                   new-res [new-acc
                                            (-> path pop pop)]]
                               (if (= acc new-acc)
                                 (reduced new-res)
                                 new-res)))
                           [cb
                            parents]
                           (range (/ (count parents) 2))))))
        closure-idx (get-in new-cb (conj parents :syms ob))]
    closure-idx))

(defn lookup
  ([ctx sym call?] (lookup ctx sym call? nil))
  ([ctx sym call? m] (lookup ctx sym call? m nil))
  ([ctx sym call? #?(:clj m :cljs _) only-var?]
   (let [bindings (faster/get-2 ctx :bindings)
         track-mutable? (faster/get-2 ctx :deftype-fields)]
     (or
      (when-let [[k v]
                 (find bindings sym)]
        (let [idx (or (get (:iden->invoke-idx ctx) v)
                      (let [oi (:outer-idens ctx)
                            ob (oi v)]
                        (update-parents ctx (:closure-bindings ctx) ob)))
              #?@(:clj [tag (or  (:tag m)
                                 (some-> k meta :tag))])
              mutable? (when track-mutable?
                         (when-let [m (some-> k meta)]
                           #?(:clj (or (:volatile-mutable m)
                                       (:unsynchronized-mutable m))
                              :cljs (or (:mutable m)
                                        (:volatile-mutable m)))))
              v (if call? ;; resolve-symbol is already handled in the call case
                  (mark-resolve-sym k idx)
                  (let [v (cond-> (if mutable?
                                    (let [ext-map (second (lookup ctx '__sci_this false))]
                                      (->Node
                                       (let [this (sci.impl.types/eval ext-map ctx bindings)
                                             inner (sci.impl.types/getVal this)]
                                         (get inner sym))
                                       nil))
                                    (->Node
                                     (aget ^objects bindings idx)
                                     nil))
                            #?@(:clj [tag (with-meta
                                            {:tag tag})])
                            mutable? (vary-meta assoc :mutable true))]
                    v))]
          [k v]))
      (when-let [kv (lookup* ctx sym call? only-var?)]
        (when (:check-permissions ctx)
          (check-permission! ctx sym kv))
        kv)))))

(defn resolve-symbol*
  [ctx sym call? m]
  (or
   (lookup ctx sym call? m)
   (let [n (name sym)]
     (cond
       ;; NOTE: move this to analyzer when resolve-symbol returns nil?
       (and call?
            (str/starts-with? n ".")
            (> (count n) 1))
       [sym 'expand-dot*] ;; method invocation
       (and call?
            (str/ends-with? n ".")
            (> (count n) 1))
       [sym 'expand-constructor]))))

#?(:cljs
   (defn resolve-prefix+path
     [ctx sym m]
     (let [sym-ns (namespace sym)
           sym-name (name sym)
           segments (.split sym-name ".")
           ctx (assoc ctx :dotted-access true)]
       (loop [prefix nil
              segments segments]
         (when-not (empty? segments)
           (let [fst-segment (first segments)
                 nxt-segments (next segments)
                 new-sym (symbol sym-ns (str prefix
                                             (when prefix ".") fst-segment))

                 new-sym-2 (when (and (not sym-ns)
                                      prefix)
                             (symbol prefix
                                     fst-segment))]
             (if-let [v (resolve-symbol* ctx new-sym false m)]
               [(second v) nxt-segments]
               (if-let [v2 (when new-sym-2
                             (resolve-symbol* ctx new-sym-2 false m))]
                 [(second v2) nxt-segments]
                 (recur (str new-sym) nxt-segments)))))))))

#?(:cljs (defn resolve-dotted-access [ctx sym call? m]
           #?(:cljs
              (when-let [[v segments] (resolve-prefix+path ctx sym m)]
                (let [v (if (utils/var? v) (deref v) v)
                      segments (into-array segments)]
                  ;; NOTE: there is a reloading implication here...
                  (if call?
                    [sym (with-meta
                           [v segments]
                           {:sci.impl.analyzer/static-access true})]
                    (if (instance? sci.impl.types/NodeR v)
                      [sym
                       (sci.impl.types/->Node
                        (interop/get-static-fields
                         (sci.impl.types/eval v ctx bindings)
                         segments)
                        sym)]
                      ;; This is x.a.b.c
                      [sym (interop/get-static-fields v segments)
                       ;; This would be the correct implementation if v would be mutated, but can be implemented as:
                       ;; (.. x -a -b c)
                       #_(sci.impl.types/->Node
                            (interop/get-static-fields v segments)
                            nil)])))))))

(defn resolve-symbol
  ([ctx sym] (resolve-symbol ctx sym false nil))
  ([ctx sym call?] (resolve-symbol ctx sym call? nil))
  ([ctx sym call? m]
   (second
    (or (resolve-symbol* ctx sym call? m)
        #?(:cljs (let [resolved (resolve-dotted-access ctx sym call? m)]
                   resolved))
        (throw-error-with-location
         (str "Could not resolve symbol: " sym)
         sym)))))
