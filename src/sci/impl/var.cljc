(ns sci.impl.var
  {:no-doc true})

(defprotocol ISettable
  (setVal [_ _]))

;; adapted from https://github.com/clojure/clojurescript/blob/df1837048d01b157a04bb3dc7fedc58ee349a24a/src/main/cljs/cljs/core.cljs#L1118
(deftype SciVar [#?(:clj ^:volatile-mutable val
                    :cljs ^:mutable val)
                 sym
                 #?(:clj ^:volatile-mutable _meta
                    :cljs ^:mutable _meta)]
  ISettable
  (setVal [_ v]
    (set! val (fn [] v)))
  Object
  #?(:cljs
     (isMacro [_]
              (. (val) -cljs$lang$macro)))
  (toString [_]
    (str "#'" sym))
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  #?(:clj (deref [_] (val)) :cljs (-deref [_] (val)))
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
  #?@(:clj [clojure.lang.IFn] :cljs [Fn
                                     IFn])
  (#?(:clj invoke :cljs -invoke) [_]
    ((val)))
  (#?(:clj invoke :cljs -invoke) [_ a]
    ((val) a))
  (#?(:clj invoke :cljs -invoke) [_ a b]
    ((val) a b))
  (#?(:clj invoke :cljs -invoke) [_ a b c]
    ((val) a b c))
  (#?(:clj invoke :cljs -invoke) [_ a b c d]
    ((val) a b c d))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e]
    ((val) a b c d e))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f]
    ((val) a b c d e f))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g]
    ((val) a b c d e f g))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h]
    ((val) a b c d e f g h))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i]
    ((val) a b c d e f g h i))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j]
    ((val) a b c d e f g h i j))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k]
    ((val) a b c d e f g h i j k))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l]
    ((val) a b c d e f g h i j k l))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m]
    ((val) a b c d e f g h i j k l m))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n]
    ((val) a b c d e f g h i j k l m n))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o]
    ((val) a b c d e f g h i j k l m n o))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p]
    ((val) a b c d e f g h i j k l m n o p))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q]
    ((val) a b c d e f g h i j k l m n o p q))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r]
    ((val) a b c d e f g h i j k l m n o p q r))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s]
    ((val) a b c d e f g h i j k l m n o p q r s))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t]
    ((val) a b c d e f g h i j k l m n o p q r s t))
  (#?(:clj invoke :cljs -invoke) [_ a b c d e f g h i j k l m n o p q r s t rest]
    (apply (val) a b c d e f g h i j k l m n o p q r s t rest)))
