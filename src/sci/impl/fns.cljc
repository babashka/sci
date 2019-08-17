(ns sci.impl.fns)

;; TODO: use this to improve arity error message
#_(defn show-arities [fixed-arities var-args-min-arity]
    (let [fas (vec (sort fixed-arities))
          max-fixed (peek fas)
          arities (if var-args-min-arity
                    (if (= max-fixed var-args-min-arity)
                      fas (conj fas var-args-min-arity))
                    fas)]
      (cond var-args-min-arity
            (str (str/join ", " arities) " or more")
            (= 1 (count fas)) (first fas)
            :else (str (str/join ", " (pop arities)) " or " (peek arities)))))

;; TODO: use this to improve arity error message
#_(defn arity-error-msg [ns-name fn-name arg-count fixed-arities var-args-min-arity]
    (str fn-name "<name> is called with "
         ;; (if ns-name (str ns-name "/" fn-name) fn-name)
         arg-count
         (if (= 1 arg-count) "arg" "args")
         (show-arities fixed-arities var-args-min-arity)))

(defn parse-fn-args+body [interpret ctx {:sci/keys [fixed-arity fixed-names var-arg-name destructure-vec arg-list body] :as _m}]
  (let [;; _ (prn "M" _m)
        min-var-args-arity (when var-arg-name fixed-arity)
        m (if min-var-args-arity
            {:sci/min-var-args-arity min-var-args-arity}
            {:sci/fixed-arity fixed-arity})]
    (if #?(:cljs false :clj (and (not var-arg-name) fixed-arity (< fixed-arity 3)))
      ;; small optimization for fns with 0-2 args
      (case fixed-arity
        0 (with-meta (fn [] (interpret ctx (cons 'do body))) m)
        1 (with-meta (fn [x]
                       (interpret ctx `(~'let [~(first arg-list) ~x]
                                        ~@body))) m)
        2 (with-meta (let [[a1 a2] arg-list]
                       (fn [x y] (interpret ctx
                                            `(~'let [~a1 ~x
                                                     ~a2 ~y]
                                              ~@body)))) m))
      (with-meta
        (fn [& args]
          (if var-arg-name
            (when (< (count (take min-var-args-arity args))
                     min-var-args-arity)
              (throw (new #?(:clj Exception
                             :cljs js/Error)
                          (str "Wrong number of arguments. Expected at least: " min-var-args-arity ", got: " (count args)))))
            (when-not (= (count (take (inc fixed-arity) args))
                         fixed-arity)
              (throw (new #?(:clj Exception
                             :cljs js/Error) (str "Wrong number of arguments. Expected: " fixed-arity ", got: " (count args) ", " args)))))
          (let [;; arg-list contains the gensymed symbols, so here we get:
                ;; [arg0 val1 arg1 val2]
                ;; _ (prn args "," (conj fixed-names var-arg-name))
                runtime-bindings (vec (interleave fixed-names (take fixed-arity args)))
                runtime-bindings (if var-arg-name
                                   (conj runtime-bindings var-arg-name
                                         (list 'quote (drop fixed-arity args)))
                                   runtime-bindings)
                ;; _ (prn "RT" runtime-bindings)
                ;; now we can substitute these values in the destructure-vec we
                ;; got at macroexpansion time, which is basically what let does
                ;; for us
                let-bindings (into runtime-bindings destructure-vec)
                ;; arg-bindings (apply hash-map (interleave fixed-args args))
                form (list* 'let let-bindings
                            body)]
            (interpret ctx form)))
        m))))

(defn lookup-by-arity [arities arity]
  (some (fn [f]
          (let [{:keys [:sci/fixed-arity :sci/min-var-args-arity]} (meta f)]
            (when (or (= arity fixed-arity )
                      (and min-var-args-arity
                           (>= arity min-var-args-arity)))
              f))) arities))

(defn eval-fn [ctx interpret {:sci/keys [fn-bodies fn-name]}]
  (let [self-ref (atom nil)
        call-self (fn [& args]
                    (apply @self-ref args))
        ctx (assoc-in ctx [:bindings fn-name] call-self)
        arities (map #(parse-fn-args+body interpret ctx %) fn-bodies)
        f (if (= 1 (count arities))
            (first arities)
            (fn [& args]
              (let [arg-count (count args)]
                (if-let [f (lookup-by-arity arities arg-count)]
                  (apply f args)
                  (throw (new #?(:clj Exception
                                 :cljs js/Error) (str "Cannot call " fn-name " with " arg-count " arguments.")))))))]
    (reset! self-ref f)
    f #_(with-meta f
          {:sci/name fn-name
           ;; :sci/arities (map meta arities)
           })))

(defn eval-defn [ctx interpret [defn fn-name docstring? & body :as expr]]
  (let [docstring (when (string? docstring?) docstring?)
        expr (if docstring (list* defn fn-name body)
                 expr)
        f (eval-fn ctx interpret expr)
        fn-name (-> f meta :sci/name)
        f (if docstring (vary-meta f assoc :sci/doc docstring)
              f)]
    (swap! (:env ctx) assoc fn-name f)
    f))

;;;; Scratch

(comment
  )
