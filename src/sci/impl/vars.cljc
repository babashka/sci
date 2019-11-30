(ns sci.impl.vars
  {:no-doc true}
  (:refer-clojure :exclude [var? binding
                            push-thread-bindings
                            get-thread-bindings
                            pop-thread-bindings
                            with-redefs
                            with-redefs-fn]))

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

;; adapted from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Var.java
#?(:clj
   (do
     (set! *warn-on-reflection* true)
     (deftype Frame [bindings prev])
     (def top-frame (Frame. {} nil))

     (def ^ThreadLocal dvals (proxy [ThreadLocal] []
                               (initialValue [] top-frame)))

     (defn get-thread-binding-frame ^Frame []
       (.get dvals))

     (defn reset-thread-binding-frame [frame]
       (.set dvals frame))

     (deftype TBox [thread ^:volatile-mutable val]
       IBox
       (setVal [_ v]
         (set! val v))
       (getVal [_] val))

     (defn dynamic-var? [v]
       (:dynamic (meta v)))

     (defn push-thread-bindings [bindings]
       (let [frame (get-thread-binding-frame)
             bmap (.-bindings frame)
             bmap (reduce (fn [acc [var* val*]]
                            (when-not (dynamic-var? var*)
                              (throw (new IllegalStateException
                                          (str "Can't dynamically bind non-dynamic var " var*))))
                            (assoc acc var* (TBox. (Thread/currentThread) val*)))
                          bmap
                          bindings)]
         (reset-thread-binding-frame (Frame. bmap frame))))

     (defn pop-thread-bindings []
       (if-let [f (.-prev (get-thread-binding-frame))]
         (if (identical? top-frame f)
           (.remove dvals)
           (reset-thread-binding-frame f))
         (throw (Exception. "nothing to pop!"))))

     (defn get-thread-bindings []
       (let [f (get-thread-binding-frame)]
         (loop [ret {}
                kvs (seq (.-bindings f))]
           (if kvs
             (let [[var* ^TBox tbox] (first kvs)
                   tbox-val (getVal tbox)]
               (recur (assoc ret var* tbox-val)
                      (next kvs)))
             ret))))

     (defn get-thread-binding ^TBox [sci-var]
       (when-let [^Frame f (.get dvals)]
         (when-let [bindings (.-bindings f)]
           (get bindings sci-var))))))

(defprotocol IVar
  (bindRoot [this v])
  (getRawRoot [this]))

;; adapted from https://github.com/clojure/clojurescript/blob/df1837048d01b157a04bb3dc7fedc58ee349a24a/src/main/cljs/cljs/core.cljs#L1118
(deftype SciVar [#?(:clj ^:volatile-mutable root
                    :cljs ^:mutable root)
                 sym
                 #?(:clj ^:volatile-mutable _meta
                    :cljs ^:mutable _meta)]
  IVar
  (bindRoot [this v]
    (set! (.-root this) v))
  (getRawRoot [this]
    root)
  IBox
  (setVal [this v]
    #?(:cljs (set! (.-root this) v)
       :clj
       (let [b (get-thread-binding this)]
         (if (some? b)
           (let [t (.-thread b)]
             (if (not (identical? t (Thread/currentThread)))
               (throw (new IllegalStateException
                           (str "Can't change/establish root binding of " this " with set")))
               (setVal b v)))
           (throw (new IllegalStateException
                       (str "Can't change/establish root binding of " this " with set")))))))
  (getVal [this] root)
  Object
  ;; #?(:cljs
  ;;    (isMacro [_]
  ;;             (. (val) -cljs$lang$macro)))
  (toString [_]
    (str "#'" sym))
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  #?(:clj (deref [this] (or (when-let [tbox (get-thread-binding this)]
                              (getVal tbox))
                            root))
     :cljs (-deref [_] root))
  #?(:clj clojure.lang.IMeta :cljs IMeta)
  #?(:clj (meta [_] _meta) :cljs (-meta [_] _meta))
  #?(:clj clojure.lang.IObj :cljs IWithMeta)
  #?(:clj
     (withMeta [this new-meta]
               (set! _meta new-meta)
               this)
     :cljs
     (-with-meta [this new-meta]
                 (set! _meta new-meta)
                 this))
  ;; #?(:clj Comparable :cljs IEquiv)
  ;; (-equiv [this other]
  ;;   (if (instance? Var other)
  ;;     (= (.-sym this) (.-sym other))
  ;;     false))
  ;; #?(:clj clojure.lang.IHashEq :cljs IHash)
  ;; (-hash [_]
  ;;   (hash-symbol sym))
  #?@(:clj [clojure.lang.IFn] :cljs [IFn])
  (#?(:clj invoke :cljs -invoke) [_]
    (root))
  (#?(:clj invoke :cljs -invoke) [_ a]
    (root a))
  (#?(:clj invoke :cljs -invoke) [_ a b]
    (root a b))
  (#?(:clj invoke :cljs -invoke) [_ a b c]
    (root a b c))
  (#?(:clj invoke :cljs -invoke) [_ a b c d]
    (root a b c d))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e]
    (root a b c d e))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f]
    (root a b c d e f))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g]
    (root a b c d e f g))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h]
    (root a b c d e f g h))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i]
    (root a b c d e f g h i))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j]
    (root a b c d e f g h i j))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k]
    (root a b c d e f g h i j k))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l]
    (root a b c d e f g h i j k l))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m]
    (root a b c d e f g h i j k l m))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n]
    (root a b c d e f g h i j k l m n))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o]
    (root a b c d e f g h i j k l m n o))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p]
    (root a b c d e f g h i j k l m n o p))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q]
    (root a b c d e f g h i j k l m n o p q))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r]
    (root a b c d e f g h i j k l m n o p q r))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s]
    (root a b c d e f g h i j k l m n o p q r s))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t]
    (root a b c d e f g h i j k l m n o p q r s t))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t rest]
    (apply root a b c d e f g h i j k l m n o p q r s t rest)))

(defn var? [x]
  ;; (prn "X" x (instance? sci.impl.vars.SciVar x))
  (instance? sci.impl.vars.SciVar x))

(defn binding
  [_ _ bindings & body]
  #_(assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  #?(:clj (let [var-ize (fn [var-vals]
                          (loop [ret [] vvs (seq var-vals)]
                            (if vvs
                              (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                                      (next (next vvs)))
                              (seq ret))))]
            `(let []
               (clojure.core/push-thread-bindings (hash-map ~@(var-ize bindings)))
               (try
                 ~@body
                 (finally
                   (clojure.core/pop-thread-bindings)))))
     :cljs  (let [names (take-nth 2 bindings)]
              ;; TODO: confirm bindings
              `(clojure.core/with-redefs ~bindings ~@body))))

(defn with-redefs-fn
  [binding-map func]
  (let [root-bind (fn [m]
                    (doseq [[a-var a-val] m]
                      (sci.impl.vars/bindRoot a-var a-val)))
        old-vals (zipmap (keys binding-map)
                         (map #(sci.impl.vars/getRawRoot %) (keys binding-map)))]
    (try
      (root-bind binding-map)
      (func)
      (finally
        (root-bind old-vals)))))

(defn with-redefs
  [_ _ bindings & body]
  `(clojure.core/with-redefs-fn ~(zipmap (map #(list `var %) (take-nth 2 bindings))
                                         (take-nth 2 (next bindings)))
     (fn [] ~@body)))

(comment
  (def v1 (SciVar. (fn [] 0) 'foo nil))
  @v1 ;; 0
  (push-thread-bindings {v1 2})
  (get-thread-binding v1) ;; 2
  (push-thread-bindings {v1 3})
  (get-thread-binding v1) ;; 3
  (pop-thread-bindings)
  (get-thread-binding v1) ;; 2
  (pop-thread-bindings)
  (get-thread-binding v1) ;; nil
  @v1 ;; 0
  (pop-thread-bindings) ;; exception
  )
