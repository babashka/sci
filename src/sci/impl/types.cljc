(ns sci.impl.types
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  #?(:clj (:require [sci.impl.macros :as macros]))
  #?(:cljs (:require-macros [sci.impl.macros :as macros]
                            [sci.impl.types :refer [->Node]]))
  #?(:clj (:import [sci.impl.types ICustomType])))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

#?(:cljs
   (defprotocol ICustomType
     (getInterfaces [_])
     (getMethods [_])
     (getProtocols [_])
     (getFields [_])))

#?(:clj
   (do (defn getMethods [obj]
         (.getMethods ^ICustomType obj))
       (defn getInterfaces [obj]
         (.getInterfaces ^ICustomType obj))
       (defn getProtocols [obj]
         (.getProtocols ^ICustomType obj))
       (defn getFields [obj]
         (.getFields ^ICustomType obj))))

#?(:cljs (declare sci-pr-writer))
#?(:cljs (declare sci-invoke))

(deftype Reified [interfaces meths protocols]
  ICustomType
  (getInterfaces [_] interfaces)
  (getMethods [_] meths)
  (getProtocols [_] protocols)
  (getFields [_] nil)
  #?@(:cljs [IPrintWithWriter
             (-pr-writer [this w opts]
                         (sci-pr-writer this w opts))
             IFn
             (-invoke [this]
                      ((get meths '-invoke) this))
             (-invoke [this a]
                      ((get meths '-invoke) this a))
             (-invoke [this a b]
                      ((get meths '-invoke) this a b))
             (-invoke [this a b c]
                      ((get meths '-invoke) this a b c))
             (-invoke [this a b c d]
                      ((get meths '-invoke) this a b c d))
             (-invoke [this a b c d e]
                      ((get meths '-invoke) this a b c d e))
             (-invoke [this a b c d e f]
                      ((get meths '-invoke) this a b c d e f))
             (-invoke [this a b c d e f g]
                      ((get meths '-invoke) this a b c d e f g))
             (-invoke [this a b c d e f g h]
                      ((get meths '-invoke) this a b c d e f g h))
             (-invoke [this a b c d e f g h i]
                      ((get meths '-invoke) this a b c d e f g h i))
             (-invoke [this a b c d e f g h i j]
                      ((get meths '-invoke) this a b c d e f g h i j))
             (-invoke [this a b c d e f g h i j k]
                      ((get meths '-invoke) this a b c d e f g h i j k))
             (-invoke [this a b c d e f g h i j k l]
                      ((get meths '-invoke) this a b c d e f g h i j k l))
             (-invoke [this a b c d e f g h i j k l m]
                      ((get meths '-invoke) this a b c d e f g h i j k l m))
             (-invoke [this a b c d e f g h i j k l m n]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n))
             (-invoke [this a b c d e f g h i j k l m n o]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n o))
             (-invoke [this a b c d e f g h i j k l m n o p]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n o p))
             (-invoke [this a b c d e f g h i j k l m n o p q]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n o p q))
             (-invoke [this a b c d e f g h i j k l m n o p q r]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n o p q r))
             (-invoke [this a b c d e f g h i j k l m n o p q r s]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n o p q r s))
             (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                      ((get meths '-invoke) this a b c d e f g h i j k l m n o p q r s t))]))

(defprotocol SciTypeInstance
  (-get-type [_])
  (-mutate [_ k v]))

(defn type-impl
  "Must be varargs because used in multimethods
  Only for internal use!"
  [x & _]
  (or (when (#?(:clj instance?
                :cljs cljs.core/implements?) sci.impl.types.SciTypeInstance x)
        (-get-type x))
      (when #?(:clj (instance? sci.impl.types.ICustomType x)
               :cljs (cljs.core/implements? sci.impl.types.ICustomType x))
        :sci.impl.protocols/reified)
      (some-> x meta :type)
      #?(:clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

#?(:cljs (defmulti sci-pr-writer (fn [x & _] (type-impl x))))
#?(:cljs (defmulti sci-invoke (fn [x & _] (type-impl x))))

(defn type-impl2
  "Externally available type implementation."
  [x]
  (or (some-> x meta :type)
      (when (#?(:clj instance?
                :cljs cljs.core/implements?) sci.impl.types.SciTypeInstance x)
        (-get-type x))
      #?(:clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

;; returned from analyzer when macroexpansion needs interleaved eval
(deftype EvalForm [form]
  IBox
  (getVal [_this] form))

(defprotocol Stack
  (stack [this]))

(extend-protocol Stack
  #?(:clj Object :cljs default) (stack [_this] nil))

#?(:clj (defprotocol Eval
          (eval [expr ctx ^objects bindings])))

#?(:cljs
   (defrecord NodeR [f stack]
     Stack (stack [_] stack)))

(deftype BindingNode [#?(:clj ^int idx :cljs idx)
                      _meta]
  #?@(:clj [Eval (eval [_ _ bindings]
                   (aget ^objects bindings idx))
            Stack (stack [_] nil)
            clojure.lang.IObj
            (withMeta [_ m] (BindingNode. idx m))
            clojure.lang.IMeta
            (meta [_] _meta)]
      :cljs [IMeta
             (-meta [_] _meta)
             IWithMeta
             (-with-meta [_ m] (BindingNode. idx m))]))

(defn eval-node? [x]
  #?(:clj (instance? sci.impl.types.Eval x)
     :cljs (or (instance? NodeR x)
               (instance? BindingNode x))))

#?(:cljs
   ;; For performance reasons on CLJS we do not use eval as a protcol method but
   ;; as a separate function which does an instance check on a concrete type.
   (defn eval [expr ctx bindings]
     (if (instance? NodeR expr)
       ((.-f expr) expr ctx bindings)
       (if (instance? BindingNode expr)
         (aget ^objects bindings (.-idx expr))
         expr))))

(macros/deftime
  (defmacro ->Node
    [body stack]
    (macros/?
     :clj `(reify
             sci.impl.types/Eval
             (~'eval [~'this ~'ctx ~'bindings]
              ~body)
             sci.impl.types/Stack
             (~'stack [_#] ~stack))
     :cljs `(->NodeR
             (fn [~'this ~'ctx ~'bindings]
               ~body)
             ~stack))))

#?(:clj
   (deftype ConstantNode [x]
     Eval (eval [_expr _bindings _ctx]
            x)
     Stack (stack [_] nil)))

(defn ->constant [x]
  #?(:clj (->ConstantNode x)
     :cljs x))

(defprotocol HasName ;; INamed was already taken by CLJS
  (getName [_]))
