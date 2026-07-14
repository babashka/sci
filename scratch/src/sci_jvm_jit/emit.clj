(ns sci-jvm-jit.emit
  "Spike: emit JVM bytecode for two sci-shaped fn bodies via clojure.asm
  (Clojure's bundled shaded ASM), define at runtime, benchmark against the
  sci interpreter. Emission mirrors what a sci jit tier would produce from
  the analyzed AST: boxed clojure.lang.Numbers ops (sci semantics), no
  primitive inference."
  (:require [sci.core :as sci])
  (:import [clojure.asm ClassWriter Opcodes MethodVisitor Label]
           [clojure.lang DynamicClassLoader IFn Numbers]))

(set! *warn-on-reflection* true)

(def ^:private object-desc "Ljava/lang/Object;")

(defn- long-const
  "Push Long.valueOf(v) on the stack."
  [^MethodVisitor mv ^long v]
  (.visitLdcInsn mv (Long/valueOf v))
  (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Long" "valueOf"
                    "(J)Ljava/lang/Long;" false))

(defn- numbers-op
  "invokestatic clojure.lang.Numbers.<name> with the given descriptor."
  [^MethodVisitor mv ^String name ^String desc]
  (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/Numbers" name desc false))

(defn- begin-class ^ClassWriter [^String internal-name]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)]
    (.visit cw Opcodes/V1_8 (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL)
            internal-name nil "clojure/lang/AFunction" nil)
    (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL "clojure/lang/AFunction" "<init>" "()V" false)
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    cw))

(def ^:dynamic *dump-dir* nil)

(defn- define-fn ^IFn [^String class-name ^ClassWriter cw]
  (.visitEnd cw)
  (let [bytes (.toByteArray cw)]
    (when *dump-dir*
      (let [f (java.io.File. (str *dump-dir* "/" (.replace class-name "." "/") ".class"))]
        (.mkdirs (.getParentFile f))
        (with-open [o (java.io.FileOutputStream. f)]
          (.write o ^bytes bytes))
        (println "dumped" (.getPath f))))
    (let [cl (DynamicClassLoader.)
          cls (.defineClass cl class-name bytes nil)]
      (.newInstance ^Class cls))))

(defn emit-fib
  "(fn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
  Boxed: Numbers.lt/minus/add on Objects, self-call via invokevirtual."
  []
  (let [in "sci_jit/Fib"
        cw (begin-class in)
        mv (.visitMethod cw Opcodes/ACC_PUBLIC "invoke"
                         (str "(" object-desc ")" object-desc) nil nil)
        else (Label.)]
    (.visitCode mv)
    ;; if (Numbers.lt(n, 2L))
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitLdcInsn mv (Long/valueOf 2))
    (numbers-op mv "lt" "(Ljava/lang/Object;J)Z")
    (.visitJumpInsn mv Opcodes/IFEQ else)
    ;; then: return n
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitInsn mv Opcodes/ARETURN)
    ;; else: Numbers.add(this.invoke(Numbers.minus(n,1)), this.invoke(Numbers.minus(n,2)))
    (.visitLabel mv else)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitLdcInsn mv (Long/valueOf 1))
    (numbers-op mv "minus" "(Ljava/lang/Object;J)Ljava/lang/Number;")
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL in "invoke"
                      (str "(" object-desc ")" object-desc) false)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitLdcInsn mv (Long/valueOf 2))
    (numbers-op mv "minus" "(Ljava/lang/Object;J)Ljava/lang/Number;")
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL in "invoke"
                      (str "(" object-desc ")" object-desc) false)
    (numbers-op mv "add" "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Number;")
    (.visitInsn mv Opcodes/ARETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)
    (define-fn "sci_jit.Fib" cw)))

(defn emit-loop
  "(fn [] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j)))))
  Boxed: locals-mode registers, Numbers.isZero/inc/dec."
  []
  (let [in "sci_jit/TightLoop"
        cw (begin-class in)
        mv (.visitMethod cw Opcodes/ACC_PUBLIC "invoke"
                         (str "()" object-desc) nil nil)
        top (Label.)
        done (Label.)]
    (.visitCode mv)
    (long-const mv 0)        (.visitVarInsn mv Opcodes/ASTORE 1) ; i
    (long-const mv 10000000) (.visitVarInsn mv Opcodes/ASTORE 2) ; j
    (.visitLabel mv top)
    (.visitVarInsn mv Opcodes/ALOAD 2)
    (numbers-op mv "isZero" "(Ljava/lang/Object;)Z")
    (.visitJumpInsn mv Opcodes/IFNE done)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (numbers-op mv "inc" "(Ljava/lang/Object;)Ljava/lang/Number;")
    (.visitVarInsn mv Opcodes/ASTORE 1)
    (.visitVarInsn mv Opcodes/ALOAD 2)
    (numbers-op mv "dec" "(Ljava/lang/Object;)Ljava/lang/Number;")
    (.visitVarInsn mv Opcodes/ASTORE 2)
    (.visitJumpInsn mv Opcodes/GOTO top)
    (.visitLabel mv done)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitInsn mv Opcodes/ARETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)
    (define-fn "sci_jit.TightLoop" cw)))

;; --- benchmark ---

(defn- bench [label f]
  (dotimes [_ 3] (f))
  (let [times (mapv (fn [_]
                      (let [t0 (System/nanoTime)]
                        (f)
                        (/ (- (System/nanoTime) t0) 1e6)))
                    (range 7))]
    (println (format "%-28s %8.2f ms (min of 7)" label (apply min times)))))

(defn -main [& [mode dir]]
  (when (= mode "dump")
    (binding [*dump-dir* (or dir "classes")]
      (emit-fib)
      (emit-loop))
    (System/exit 0))
  (let [fib-src "(fn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"
        loop-src "(fn [] (loop [i 0 j 10000000] (if (zero? j) i (recur (inc i) (dec j)))))"
        ctx (sci/init {})
        sci-fib (sci/eval-string* ctx fib-src)
        sci-loop (sci/eval-string* ctx loop-src)
        jit-fib ^IFn (emit-fib)
        jit-loop ^IFn (emit-loop)
        clj-fib (eval (read-string fib-src))
        clj-loop (eval (read-string loop-src))]
    (assert (= (sci-fib 20) (.invoke jit-fib 20) (clj-fib 20)))
    (assert (= (sci-loop) (.invoke jit-loop) (clj-loop)))
    (println "fib 27:")
    (bench "  sci interpreter" #(sci-fib 27))
    (bench "  emitted bytecode (boxed)" #(.invoke jit-fib 27))
    (bench "  Clojure compiler" #(clj-fib 27))
    (println "10M loop:")
    (bench "  sci interpreter" #(sci-loop))
    (bench "  emitted bytecode (boxed)" #(.invoke jit-loop))
    (bench "  Clojure compiler" #(clj-loop))))
