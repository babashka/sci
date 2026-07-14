(ns sci.impl.jvm-type-emit
  "Emits a real JVM class for a deftype that implements Java interfaces.
  SciType is final, so the emitted class wraps a SciType delegate: it
  implements all of SciType's interfaces by delegation (so the sci type
  machinery - fields via ext-map, mutation, printing, protocol dispatch -
  keeps working) plus the user's Java interfaces via stubs that call
  sci-interpreted fns from a per-class static impl table. Needs runtime
  class definition: any JVM, or a native image built with
  -H:+RuntimeClassLoading (Crema)."
  {:no-doc true}
  (:import [clojure.asm ClassWriter Opcodes MethodVisitor Label Type]
           [clojure.lang DynamicClassLoader IFn]
           [java.lang.reflect Constructor Method Modifier]))

(set! *warn-on-reflection* true)

(definterface IJvmSciType
  (^Object sci_delegate []))

(def ^:private super-name "sci.impl.deftype.SciType")
(def ^:private super-internal "sci/impl/deftype/SciType")
(def ^:private super-desc (str "L" super-internal ";"))
(def ^:private self-iface-internal "sci/impl/jvm_type_emit/IJvmSciType")
(def ^:private ctor-desc
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V")
(def ^:private ifn-desc "Lclojure/lang/IFn;")

(defn- box-prim [^MethodVisitor mv ^Class c]
  (cond
    (= Long/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" false)
    (= Integer/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" false)
    (= Double/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" false)
    (= Float/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Float" "valueOf" "(F)Ljava/lang/Float;" false)
    (= Boolean/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Boolean" "valueOf" "(Z)Ljava/lang/Boolean;" false)
    (= Character/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" false)
    (= Byte/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Byte" "valueOf" "(B)Ljava/lang/Byte;" false)
    (= Short/TYPE c) (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Short" "valueOf" "(S)Ljava/lang/Short;" false)
    :else nil))

(defn- return-adapted [^MethodVisitor mv ^Class rt]
  (cond
    (= Void/TYPE rt) (do (.visitInsn mv Opcodes/POP) (.visitInsn mv Opcodes/RETURN))
    (= rt Object) (.visitInsn mv Opcodes/ARETURN)
    (= Long/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "longCast" "(Ljava/lang/Object;)J" false)
                         (.visitInsn mv Opcodes/LRETURN))
    (= Integer/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "intCast" "(Ljava/lang/Object;)I" false)
                            (.visitInsn mv Opcodes/IRETURN))
    (= Double/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "doubleCast" "(Ljava/lang/Object;)D" false)
                           (.visitInsn mv Opcodes/DRETURN))
    (= Float/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "floatCast" "(Ljava/lang/Object;)F" false)
                          (.visitInsn mv Opcodes/FRETURN))
    (= Boolean/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "booleanCast" "(Ljava/lang/Object;)Z" false)
                            (.visitInsn mv Opcodes/IRETURN))
    (= Short/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "shortCast" "(Ljava/lang/Object;)S" false)
                          (.visitInsn mv Opcodes/IRETURN))
    (= Byte/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "byteCast" "(Ljava/lang/Object;)B" false)
                         (.visitInsn mv Opcodes/IRETURN))
    (= Character/TYPE rt) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "charCast" "(Ljava/lang/Object;)C" false)
                              (.visitInsn mv Opcodes/IRETURN))
    :else (do (.visitTypeInsn mv Opcodes/CHECKCAST (.getInternalName (Type/getType rt)))
              (.visitInsn mv Opcodes/ARETURN))))

(defn- load-args
  "Load method arguments (slots after this), no conversion."
  [^MethodVisitor mv ^Method m]
  (let [ptypes (.getParameterTypes m)]
    (loop [i 0 slot 1]
      (when (< i (alength ptypes))
        (let [t (Type/getType ^Class (aget ptypes i))]
          (.visitVarInsn mv (.getOpcode t Opcodes/ILOAD) slot)
          (recur (inc i) (+ slot (.getSize t))))))))

(defn- emit-impl-stub
  "User interface method: adapt(sci_impls[idx].invoke(this, box(arg)...))"
  [^ClassWriter cw ^String internal-name ^Method m ^long idx]
  (let [ptypes (.getParameterTypes m)
        n (alength ptypes)
        mv (.visitMethod cw Opcodes/ACC_PUBLIC (.getName m)
                         (Type/getMethodDescriptor m) nil nil)]
    (.visitCode mv)
    (.visitFieldInsn mv Opcodes/GETSTATIC internal-name "sci_impls" (str "[" ifn-desc))
    (.visitLdcInsn mv (int idx))
    (.visitInsn mv Opcodes/AALOAD)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (loop [i 0 slot 1]
      (when (< i n)
        (let [^Class pt (aget ptypes i)
              t (Type/getType pt)]
          (.visitVarInsn mv (.getOpcode t Opcodes/ILOAD) slot)
          (box-prim mv pt)
          (recur (inc i) (+ slot (.getSize t))))))
    (.visitMethodInsn mv Opcodes/INVOKEINTERFACE "clojure/lang/IFn" "invoke"
                      (str "(" (apply str (repeat (inc n) "Ljava/lang/Object;"))
                           ")Ljava/lang/Object;")
                      true)
    (return-adapted mv (.getReturnType m))
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-delegating-stub
  "SciType machinery method: sci_delegate.<m>(args), signatures unchanged."
  [^ClassWriter cw ^String internal-name ^Method m]
  (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC (.getName m)
                         (Type/getMethodDescriptor m) nil nil)
        rt (Type/getReturnType m)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitFieldInsn mv Opcodes/GETFIELD internal-name "sci_delegate" super-desc)
    (load-args mv m)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL super-internal (.getName m)
                      (Type/getMethodDescriptor m) false)
    (.visitInsn mv (.getOpcode rt Opcodes/IRETURN))
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- emit-object-methods
  "toString/hashCode delegate; equals unwraps another emitted instance
  before delegating so value equality between two emitted instances holds."
  [^ClassWriter cw ^String internal-name]
  (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "toString" "()Ljava/lang/String;" nil nil)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitFieldInsn mv Opcodes/GETFIELD internal-name "sci_delegate" super-desc)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL super-internal "toString" "()Ljava/lang/String;" false)
    (.visitInsn mv Opcodes/ARETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv))
  (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "hashCode" "()I" nil nil)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitFieldInsn mv Opcodes/GETFIELD internal-name "sci_delegate" super-desc)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL super-internal "hashCode" "()I" false)
    (.visitInsn mv Opcodes/IRETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv))
  (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "equals" "(Ljava/lang/Object;)Z" nil nil)
        not-wrapped (Label.)]
    (.visitCode mv)
    (.visitVarInsn mv Opcodes/ALOAD 0)
    (.visitFieldInsn mv Opcodes/GETFIELD internal-name "sci_delegate" super-desc)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitTypeInsn mv Opcodes/INSTANCEOF self-iface-internal)
    (.visitJumpInsn mv Opcodes/IFEQ not-wrapped)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitTypeInsn mv Opcodes/CHECKCAST self-iface-internal)
    (.visitMethodInsn mv Opcodes/INVOKEINTERFACE self-iface-internal "sci_delegate" "()Ljava/lang/Object;" true)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z" false)
    (.visitInsn mv Opcodes/IRETURN)
    (.visitLabel mv not-wrapped)
    (.visitVarInsn mv Opcodes/ALOAD 1)
    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "java/lang/Object" "equals" "(Ljava/lang/Object;)Z" false)
    (.visitInsn mv Opcodes/IRETURN)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn- super-class ^Class []
  (Class/forName super-name))

(defn- emit-wrapper-bytes ^bytes [^String internal-name user-ifaces user-methods]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        machinery-ifaces (.getInterfaces (super-class))
        all-ifaces (into [self-iface-internal]
                         (comp (map (fn [^Class i] (.getInternalName (Type/getType i))))
                               (distinct))
                         (concat machinery-ifaces user-ifaces))
        user-sigs (into #{} (map (fn [^Method m] [(.getName m) (Type/getMethodDescriptor m)]))
                        user-methods)]
    (.visit cw Opcodes/V1_8 Opcodes/ACC_PUBLIC internal-name nil "java/lang/Object"
            (into-array String all-ifaces))
    (.visitField cw (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "sci_impls" (str "[" ifn-desc) nil nil)
    (.visitField cw (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL) "sci_delegate" super-desc nil nil)
    ;; ctor: same shape as SciType's (rec-name type type-meta ext-map)
    (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" ctor-desc nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V" false)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitTypeInsn mv Opcodes/NEW super-internal)
      (.visitInsn mv Opcodes/DUP)
      (dotimes [i 4] (.visitVarInsn mv Opcodes/ALOAD (inc i)))
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL super-internal "<init>" ctor-desc false)
      (.visitFieldInsn mv Opcodes/PUTFIELD internal-name "sci_delegate" super-desc)
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    ;; IJvmSciType accessor
    (let [mv (.visitMethod cw Opcodes/ACC_PUBLIC "sci_delegate" "()Ljava/lang/Object;" nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitFieldInsn mv Opcodes/GETFIELD internal-name "sci_delegate" super-desc)
      (.visitInsn mv Opcodes/ARETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    (emit-object-methods cw internal-name)
    ;; machinery delegation (user impls win on signature collision)
    (let [seen (volatile! user-sigs)]
      (doseq [^Class iface machinery-ifaces
              ^Method m (.getMethods iface)
              :when (not (Modifier/isStatic (.getModifiers m)))
              :let [sig [(.getName m) (Type/getMethodDescriptor m)]]
              :when (not (contains? @seen sig))]
        (vswap! seen conj sig)
        (emit-delegating-stub cw internal-name m)))
    ;; user interface stubs
    (doall (map-indexed (fn [i ^Method m] (emit-impl-stub cw internal-name m i)) user-methods))
    (.visitEnd cw)
    (.toByteArray cw)))

(def class-defining-available?
  "True when the host can define classes at runtime: any JVM, or a native
  image built with -H:+RuntimeClassLoading."
  (delay (try (let [nm "sci.impl.jvm_type_emit.Probe"
                    bytes (emit-wrapper-bytes (.replace nm "." "/") [] [])]
                (.defineClass (DynamicClassLoader.) nm bytes nil)
                true)
              (catch Throwable _ false))))

;; rec-type sym -> {:class Class :ctor Constructor}
(defonce ^:private registry (atom {}))

(defn define-type-class!
  "Emit and define the wrapper class for `rec-type` implementing `ifaces`,
  delegating to the IFns in `impls` ({method-name-str IFn}). Re-evaluation
  defines a fresh class in a fresh loader."
  [rec-type ifaces impls]
  (let [class-name (str rec-type)
        methods (->> ifaces
                     (mapcat (fn [^Class i] (.getMethods i)))
                     (filter (fn [^Method m] (contains? impls (.getName m))))
                     (remove (fn [^Method m] (Modifier/isStatic (.getModifiers m))))
                     (reduce (fn [acc ^Method m]
                               (let [k [(.getName m) (Type/getMethodDescriptor m)]]
                                 (if (contains? (:seen acc) k)
                                   acc
                                   (-> acc (update :seen conj k) (update :ms conj m)))))
                             {:seen #{} :ms []})
                     :ms)
        bytes (emit-wrapper-bytes (.replace class-name "." "/") ifaces methods)
        cls (.defineClass (DynamicClassLoader.) class-name bytes nil)
        impl-arr (into-array IFn (map (fn [^Method m] (get impls (.getName m))) methods))]
    (-> (.getField ^Class cls "sci_impls") (.set nil impl-arr))
    (swap! registry assoc rec-type
           {:class cls
            :ctor (.getConstructor ^Class cls (into-array Class (repeat 4 Object)))})
    cls))

(defn construct
  "Positional construction for an emitted type: same arguments as
  sci.impl.deftype/->type-impl."
  [rec-type rec-name type type-meta m]
  (let [{:keys [^Constructor ctor]} (get @registry rec-type)]
    (.newInstance ctor (object-array [rec-name type type-meta m]))))
