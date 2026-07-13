(ns sci.impl.types
  {:no-doc true}
  (:refer-clojure :exclude [eval])
  #?@(:cljd [(:require [sci.impl.multimethods :as mm])]
      :clj [(:require [sci.impl.macros :as macros])])
  #?(:cljs (:require-macros [sci.impl.macros :as macros]
                            [sci.impl.types :refer [->Node attach-ast]]))
  #?@(:cljd [] :clj [(:import [sci.impl.types ICustomType])]))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

(defprotocol IBox
  (setVal [_this _v])
  (getVal [_this]))

#?(:cljd
   (defprotocol ICustomType
     (getInterfaces [_])
     (getMethods [_])
     (getProtocols [_])
     (getFields [_]))
   :clj
   (do (defn getMethods [obj]
         (.getMethods ^ICustomType obj))
       (defn getInterfaces [obj]
         (.getInterfaces ^ICustomType obj))
       (defn getProtocols [obj]
         (.getProtocols ^ICustomType obj))
       (defn getFields [obj]
         (.getFields ^ICustomType obj)))
   :cljs
   (defprotocol ICustomType
     (getInterfaces [_])
     (getMethods [_])
     (getProtocols [_])
     (getFields [_])))

#?(:cljs (declare sci-pr-writer))
(declare sci-invoke)

(deftype Reified [interfaces meths protocols]
  ICustomType
  (getInterfaces [_] interfaces)
  (getMethods [_] meths)
  (getProtocols [_] protocols)
  (getFields [_] nil)
  #?@(:cljd [IFn
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
             (-invoke-more [this a b c d e f g h i rest*]
                           (apply (get meths '-invoke) this a b c d e f g h i rest*))
             (-apply [this args]
                     (apply (get meths '-invoke) this args))])
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
  (or (when #?(:cljd (satisfies? SciTypeInstance x)
               :clj (instance? sci.impl.types.SciTypeInstance x)
               :cljs (cljs.core/implements? sci.impl.types.SciTypeInstance x))
        (-get-type x))
      (when #?(:cljd (satisfies? ICustomType x)
               :clj (instance? sci.impl.types.ICustomType x)
               :cljs (cljs.core/implements? sci.impl.types.ICustomType x))
        :sci.impl.protocols/reified)
      (some-> x meta :type)
      #?(:cljd (when (some? x) (.-runtimeType x))
         :clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

#?(:cljs (defmulti sci-pr-writer (fn [x & _] (type-impl x))))
#?(:cljd (def sci-invoke (mm/->SciMultiFn 'sci-invoke (fn [x & _] (type-impl x)) :default (atom {})))
   :default (defmulti sci-invoke (fn [x & _] (type-impl x))))
#?(:cljd nil :clj (defmulti sci-apply-to (fn [x & _] (type-impl x))))
#?(:cljd nil :clj (defmethod sci-apply-to :default [x args]
                    (apply sci-invoke x args)))

(defn type-impl2
  "Externally available type implementation."
  [x]
  (or (some-> x meta :type)
      (when #?(:cljd (satisfies? SciTypeInstance x)
               :clj (instance? sci.impl.types.SciTypeInstance x)
               :cljs (cljs.core/implements? sci.impl.types.SciTypeInstance x))
        (-get-type x))
      #?(:cljd (when (some? x) (.-runtimeType x))
         :clj (class x) ;; no need to check for metadata anymore
         :cljs (type x))))

;; returned from analyzer when macroexpansion needs interleaved eval
(deftype EvalForm [form]
  IBox
  (getVal [_this] form))

(defprotocol Stack
  (stack [this]))

(extend-protocol Stack
  #?(:cljd fallback :clj Object :cljs default) (stack [_this] nil))

#?(:cljd nil
   :clj (defprotocol Eval
          (eval [expr ctx ^objects bindings])))

;; The jit flag lives here (dependency root) so the analyzer can gate ast
;; attachment on it without a cycle. Probed eagerly: eval availability
;; (CSP) is fixed per JS realm, and this makes analysis pay zero when
;; blocked. On a CSP page the probe logs one console warning at load.
;; test-build kill switch (closure-define), so CI can run the suite with
;; the jit off; not a user-facing flag
#?(:cljs (goog-define jit-force-off false))

#?(:cljs
   (def jit-enabled
     (volatile! (if ^boolean jit-force-off
                  false
                  (try ((js/Function. "return 1")) true
                       (catch :default _ false))))))

;; ast: walkable mini-AST for the JS codegen tier (jit), a field rather
;; than an extmap key so attaching it costs one ctor call, not a map build
#?(:cljd
   (defrecord NodeR [f stack ast]
     Stack (stack [_] stack))
   :cljs
   (defrecord NodeR [f stack ast]
     Stack (stack [_] stack)))

(deftype BindingNode [#?(:cljd idx :clj ^int idx :cljs idx)
                      _meta]
  #?@(:cljd [IMeta
             (-meta [_] _meta)
             IWithMeta
             (-with-meta [_ m] (BindingNode. idx m))]
      :clj [Eval (eval [_ _ bindings]
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
  #?(:cljd (or (instance? NodeR x)
               (instance? BindingNode x))
     :clj (instance? sci.impl.types.Eval x)
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

#?(:cljd
   (defn eval [expr ctx bindings]
     (if (instance? NodeR expr)
       ((.-f ^NodeR expr) expr ctx bindings)
       (if (instance? BindingNode expr)
         (aget bindings (.-idx ^BindingNode expr))
         expr))))

;; The optional ast argument is the jit's walkable mini-AST. It only
;; exists on CLJS: the :clj reify and :cljd branches DISCARD the form at
;; expansion time, so passing it costs other platforms nothing.
#?(:cljd
   (defmacro ->Node
     ([body stack]
      `(->NodeR
        (fn [~'this ~'ctx ~'bindings]
          ~body)
        ~stack
        nil))
     ([body stack _ast]
      `(->NodeR
        (fn [~'this ~'ctx ~'bindings]
          ~body)
        ~stack
        nil)))
   :default
   (macros/deftime
     (defmacro ->Node
       ([body stack] `(sci.impl.types/->Node ~body ~stack nil))
       ([body stack ast]
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
                 ~stack
                 (when ^boolean @sci.impl.types/jit-enabled ~ast)))))
     ;; attach an ast to an already-built node; the form is discarded on
     ;; :clj and only evaluated on :cljs when the jit is enabled
     (defmacro attach-ast
       [node ast]
       (macros/?
        :clj node
        :cljs `(let [n# ~node]
                 (if ^boolean @sci.impl.types/jit-enabled
                   (cljs.core/assoc n# :ast ~ast)
                   n#))))))

;; the jit and its ast only exist on cljs
#?(:cljd (defmacro attach-ast [node _ast] node))

#?(:cljd nil
   :clj
   (deftype ConstantNode [x]
     Eval (eval [_expr _bindings _ctx]
            x)
     Stack (stack [_] nil)))

(defn ->constant [x]
  #?(:cljd x
     :clj (->ConstantNode x)
     :cljs x))

(defprotocol HasName ;; INamed was already taken by CLJS
  (getName [_]))

;; cljd has no IReference, reset-meta! only works on atoms there
#?(:cljd
   (defprotocol IResetMeta
     (-reset-meta! [x m])))
