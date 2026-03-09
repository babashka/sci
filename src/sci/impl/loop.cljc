(ns sci.impl.loop
  "Bytecode VM for tight loop/recur bodies. Replaces the tree-walking
  interpreter for simple loop bodies, eliminating protocol dispatch
  overhead on native-image."
  {:no-doc true}
  (:require [sci.impl.resolve :as resolve]
            #?@(:cljs [[sci.lang :refer [Var]]]))
  #?(:clj (:import [sci.lang Var])))

#?(:clj (set! *warn-on-reflection* true))

;; Opcodes — each instruction is 2 words: [opcode operand]
;; Using ^:const for use in the compiler; the interpreter uses literal ints in case.
(def ^:const OP_LOAD_BINDING  0)
(def ^:const OP_LOAD_CONST    1)
(def ^:const OP_CALL_1        2)
(def ^:const OP_CALL_2        3)
(def ^:const OP_BRANCH_FALSE  4)
(def ^:const OP_GOTO          5)
(def ^:const OP_STORE_BINDING 6)
(def ^:const OP_RETURN        7)

;; 1-arg function IDs
(def ^:const F1_INC   0)
(def ^:const F1_DEC   1)
(def ^:const F1_ZERO  2)
(def ^:const F1_POS   3)
(def ^:const F1_NEG   4)
(def ^:const F1_NOT   5)
(def ^:const F1_NIL   6)
(def ^:const F1_UINC  7)
(def ^:const F1_UDEC  8)

;; 2-arg function IDs
(def ^:const F2_ADD   0)
(def ^:const F2_SUB   1)
(def ^:const F2_MUL   2)
(def ^:const F2_LT    3)
(def ^:const F2_GT    4)
(def ^:const F2_LTE   5)
(def ^:const F2_GTE   6)
(def ^:const F2_EQ    7)
(def ^:const F2_UADD  8)
(def ^:const F2_USUB  9)
(def ^:const F2_UMUL  10)
(def ^:const F2_REM   11)

;; Known function → [arity fn-id]
(def known-fns-1
  #?(:clj
     {clojure.core/inc        F1_INC
      clojure.core/dec        F1_DEC
      clojure.core/zero?      F1_ZERO
      clojure.core/pos?       F1_POS
      clojure.core/neg?       F1_NEG
      clojure.core/not        F1_NOT
      clojure.core/nil?       F1_NIL
      clojure.core/unchecked-inc F1_UINC
      clojure.core/unchecked-dec F1_UDEC}
     :cljs
     {cljs.core/inc        F1_INC
      cljs.core/dec        F1_DEC
      cljs.core/zero?      F1_ZERO
      cljs.core/pos?       F1_POS
      cljs.core/neg?       F1_NEG
      cljs.core/not        F1_NOT
      cljs.core/nil?       F1_NIL
      cljs.core/unchecked-inc F1_UINC
      cljs.core/unchecked-dec F1_UDEC}))

(def known-fns-2
  #?(:clj
     {clojure.core/+          F2_ADD
      clojure.core/-          F2_SUB
      clojure.core/*          F2_MUL
      clojure.core/<          F2_LT
      clojure.core/>          F2_GT
      clojure.core/<=         F2_LTE
      clojure.core/>=         F2_GTE
      clojure.core/==         F2_EQ
      clojure.core/unchecked-add       F2_UADD
      clojure.core/unchecked-subtract  F2_USUB
      clojure.core/unchecked-multiply  F2_UMUL
      clojure.core/rem        F2_REM}
     :cljs
     {cljs.core/+          F2_ADD
      cljs.core/-          F2_SUB
      cljs.core/*          F2_MUL
      cljs.core/<          F2_LT
      cljs.core/>          F2_GT
      cljs.core/<=         F2_LTE
      cljs.core/>=         F2_GTE
      cljs.core/==         F2_EQ
      cljs.core/unchecked-add       F2_UADD
      cljs.core/unchecked-subtract  F2_USUB
      cljs.core/unchecked-multiply  F2_UMUL
      cljs.core/mod         F2_REM}))

;;; Compiler — walks raw loop body forms, emits bytecodes

(defn- resolve-fn
  "Resolve a symbol in the SCI context and return the function value, or nil."
  [ctx sym]
  (try
    (let [v (resolve/resolve-symbol ctx sym true)]
      (if (instance? Var v) @v v))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn try-compile
  "Try to compile a loop body to bytecodes. Returns {:opcodes int-array, :constants object-array}
  or nil if the body contains unsupported forms."
  [ctx binding-syms body-forms]
  (when (and (seq binding-syms)
             (= 1 (count body-forms)))
    (let [binding-map (zipmap binding-syms (range))
          ops #?(:clj (java.util.ArrayList.) :cljs (array))
          consts #?(:clj (java.util.ArrayList.) :cljs (array))
          emit! (fn [op operand]
                  #?(:clj (do (.add ^java.util.ArrayList ops (int op))
                              (.add ^java.util.ArrayList ops (int operand)))
                     :cljs (do (.push ops op)
                               (.push consts operand))))
          ops-size (fn [] #?(:clj (.size ^java.util.ArrayList ops)
                             :cljs (.-length ops)))
          ops-set! (fn [idx val]
                     #?(:clj (.set ^java.util.ArrayList ops (int idx) (int val))
                        :cljs (aset ops idx val)))
          add-const! (fn [v]
                       (let [idx #?(:clj (.size ^java.util.ArrayList consts)
                                    :cljs (.-length consts))]
                         #?(:clj (.add ^java.util.ArrayList consts v)
                            :cljs (.push consts v))
                         idx))]
      (letfn [(compile-expr [form]
                (cond
                  ;; Loop binding reference
                  (and (symbol? form) (contains? binding-map form))
                  (do (emit! OP_LOAD_BINDING (get binding-map form)) true)

                  ;; Constant value
                  (or (number? form) (boolean? form) (nil? form)
                      (string? form) (keyword? form) (char? form))
                  (do (emit! OP_LOAD_CONST (add-const! form)) true)

                  ;; Function call
                  (and (seq? form) (symbol? (first form)))
                  (let [sym (first form)
                        args (rest form)
                        n (count args)]
                    (when-let [f (resolve-fn ctx sym)]
                      (cond
                        ;; 1-arg known function
                        (and (= n 1) (get known-fns-1 f))
                        (when (compile-expr (first args))
                          (emit! OP_CALL_1 (get known-fns-1 f))
                          true)

                        ;; 2-arg known function
                        (and (= n 2) (get known-fns-2 f))
                        (when (compile-expr (first args))
                          (when (compile-expr (second args))
                            (emit! OP_CALL_2 (get known-fns-2 f))
                            true))

                        :else nil)))

                  :else nil))

              (compile-tail [form]
                (cond
                  ;; if
                  (and (seq? form) (= 'if (first form)))
                  (let [[_ test then else] form]
                    (when (compile-expr test)
                      (emit! OP_BRANCH_FALSE 0) ;; placeholder target
                      (let [patch-idx (dec (ops-size))]
                        (when (compile-tail then)
                          (let [else-pc (ops-size)]
                            (ops-set! patch-idx else-pc)
                            (if else
                              (compile-tail else)
                              (do (emit! OP_LOAD_CONST (add-const! nil))
                                  (emit! OP_RETURN 0)
                                  true)))))))

                  ;; recur
                  (and (seq? form) (= 'recur (first form)))
                  (let [args (rest form)
                        n (count args)]
                    (when (= n (count binding-syms))
                      (when (loop [remaining (seq args)]
                              (if remaining
                                (when (compile-expr (first remaining))
                                  (recur (next remaining)))
                                true))
                        ;; Store in reverse order (stack is LIFO)
                        (doseq [i (reverse (range n))]
                          (emit! OP_STORE_BINDING i))
                        (emit! OP_GOTO 0)
                        true)))

                  ;; Value expression in tail position
                  :else
                  (when (compile-expr form)
                    (emit! OP_RETURN 0)
                    true)))]

        (when (compile-tail (first body-forms))
          #?(:clj
             (let [ops-arr (int-array (ops-size))]
               (dotimes [i (ops-size)]
                 (aset ops-arr i (.intValue ^Integer (.get ^java.util.ArrayList ops i))))
               {:opcodes ops-arr
                :constants (.toArray ^java.util.ArrayList consts)})
             :cljs
             {:opcodes ops
              :constants consts}))))))

;;; Interpreter — bytecode eval loop

#?(:clj
   (defn run-loop
     "Execute a bytecode loop. Returns the loop result."
     ^Object [^ints opcodes ^objects constants ^objects bindings]
     (let [stack (object-array 8)]
       (loop [pc (int 0)
              sp (int 0)]
         (case (aget opcodes pc)
           0 ;; LOAD_BINDING
           (let [idx (aget opcodes (unchecked-inc-int pc))]
             (aset stack sp (aget bindings idx))
             (recur (unchecked-add-int pc 2) (unchecked-inc-int sp)))

           1 ;; LOAD_CONST
           (let [idx (aget opcodes (unchecked-inc-int pc))]
             (aset stack sp (aget constants idx))
             (recur (unchecked-add-int pc 2) (unchecked-inc-int sp)))

           2 ;; CALL_1
           (let [fid (aget opcodes (unchecked-inc-int pc))
                 i (unchecked-dec-int sp)
                 a (aget stack i)
                 r (case fid
                     0 (clojure.lang.Numbers/inc a)
                     1 (clojure.lang.Numbers/dec a)
                     2 (clojure.lang.Numbers/isZero a)
                     3 (clojure.lang.Numbers/isPos a)
                     4 (clojure.lang.Numbers/isNeg a)
                     5 (clojure.core/not a)
                     6 (clojure.core/nil? a)
                     7 (clojure.lang.Numbers/unchecked_inc a)
                     8 (clojure.lang.Numbers/unchecked_dec a))]
             (aset stack i r)
             (recur (unchecked-add-int pc 2) sp))

           3 ;; CALL_2
           (let [fid (aget opcodes (unchecked-inc-int pc))
                 j (unchecked-dec-int sp)
                 i (unchecked-dec-int j)
                 a (aget stack i)
                 b (aget stack j)
                 r (case fid
                     0 (clojure.lang.Numbers/add a b)
                     1 (clojure.lang.Numbers/minus a b)
                     2 (clojure.lang.Numbers/multiply a b)
                     3 (clojure.lang.Numbers/lt a b)
                     4 (clojure.lang.Numbers/gt a b)
                     5 (clojure.lang.Numbers/lte a b)
                     6 (clojure.lang.Numbers/gte a b)
                     7 (clojure.lang.Numbers/equiv a b)
                     8 (clojure.lang.Numbers/unchecked_add a b)
                     9 (clojure.lang.Numbers/unchecked_minus a b)
                     10 (clojure.lang.Numbers/unchecked_multiply a b)
                     11 (clojure.lang.Numbers/remainder a b))]
             (aset stack i r)
             (recur (unchecked-add-int pc 2) (unchecked-dec-int sp)))

           4 ;; BRANCH_FALSE
           (let [i (unchecked-dec-int sp)
                 v (aget stack i)]
             (if v
               (recur (unchecked-add-int pc 2) i)
               (recur (aget opcodes (unchecked-inc-int pc)) i)))

           5 ;; GOTO
           (recur (aget opcodes (unchecked-inc-int pc)) sp)

           6 ;; STORE_BINDING
           (let [i (unchecked-dec-int sp)
                 idx (aget opcodes (unchecked-inc-int pc))]
             (aset bindings idx (aget stack i))
             (recur (unchecked-add-int pc 2) i))

           7 ;; RETURN
           (aget stack (unchecked-dec-int sp))))))

   :cljs
   (defn run-loop
     "Execute a bytecode loop. Returns the loop result."
     [opcodes constants bindings]
     (let [stack (make-array 8)]
       (loop [pc 0
              sp 0]
         (case (aget opcodes pc)
           0 ;; LOAD_BINDING
           (do (aset stack sp (aget bindings (aget opcodes (inc pc))))
               (recur (+ pc 2) (inc sp)))

           1 ;; LOAD_CONST
           (do (aset stack sp (aget constants (aget opcodes (inc pc))))
               (recur (+ pc 2) (inc sp)))

           2 ;; CALL_1
           (let [fid (aget opcodes (inc pc))
                 i (dec sp)
                 a (aget stack i)
                 r (case fid
                     0 (inc a)
                     1 (dec a)
                     2 (zero? a)
                     3 (pos? a)
                     4 (neg? a)
                     5 (not a)
                     6 (nil? a)
                     7 (unchecked-inc a)
                     8 (unchecked-dec a))]
             (aset stack i r)
             (recur (+ pc 2) sp))

           3 ;; CALL_2
           (let [fid (aget opcodes (inc pc))
                 j (dec sp)
                 i (dec j)
                 a (aget stack i)
                 b (aget stack j)
                 r (case fid
                     0 (+ a b)
                     1 (- a b)
                     2 (* a b)
                     3 (< a b)
                     4 (> a b)
                     5 (<= a b)
                     6 (>= a b)
                     7 (== a b)
                     8 (unchecked-add a b)
                     9 (unchecked-subtract a b)
                     10 (unchecked-multiply a b)
                     11 (mod a b))]
             (aset stack i r)
             (recur (+ pc 2) (dec sp)))

           4 ;; BRANCH_FALSE
           (let [i (dec sp)
                 v (aget stack i)]
             (if v
               (recur (+ pc 2) i)
               (recur (aget opcodes (inc pc)) i)))

           5 ;; GOTO
           (recur (aget opcodes (inc pc)) sp)

           6 ;; STORE_BINDING
           (let [i (dec sp)
                 idx (aget opcodes (inc pc))]
             (aset bindings idx (aget stack i))
             (recur (+ pc 2) i))

           7 ;; RETURN
           (aget stack (dec sp)))))))
