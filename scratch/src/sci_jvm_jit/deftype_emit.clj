(ns sci-jvm-jit.deftype-emit
  "Spike: emit a real JVM class for sci deftype - public mutable Object
  fields, arbitrary Java interfaces, every implemented method a thin stub
  delegating to a sci-interpreted IFn from a static impl table. This is the
  class shape a Crema-enabled bb would define at runtime; on plain JVM
  defineClass always works, so the mechanism is testable without a native
  build."
  (:import [clojure.asm ClassWriter Opcodes MethodVisitor Label Type]
           [clojure.lang DynamicClassLoader IFn]
           [java.lang.reflect Method Modifier]))

(set! *warn-on-reflection* true)

(def ^:private ifn-desc "Lclojure/lang/IFn;")

(defn- box-prim
  "Box a primitive value on the stack, or leave references alone."
  [^MethodVisitor mv ^Class c]
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

(defn- return-adapted
  "Adapt the IFn.invoke Object result to the method's declared return type."
  [^MethodVisitor mv ^Class rt]
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
    :else (do (.visitTypeInsn mv Opcodes/CHECKCAST (.getInternalName (Type/getType rt)))
              (.visitInsn mv Opcodes/ARETURN))))

(defn- emit-method-stub
  "public <ret> name(args) { return adapt(impls[idx].invoke(this, box(arg)...)); }"
  [^ClassWriter cw ^String internal-name ^Method m ^long idx]
  (let [ptypes (.getParameterTypes m)
        n (alength ptypes)
        mv (.visitMethod cw Opcodes/ACC_PUBLIC (.getName m)
                         (Type/getMethodDescriptor m) nil nil)]
    (.visitCode mv)
    (.visitFieldInsn mv Opcodes/GETSTATIC internal-name "sci_impls" (str "[" ifn-desc))
    (.visitLdcInsn mv (int idx))
    (.visitInsn mv Opcodes/AALOAD)
    (.visitVarInsn mv Opcodes/ALOAD 0) ;; this
    (loop [i 0 slot 1]
      (when (< i n)
        (let [^Class pt (aget ptypes i)
              t (Type/getType pt)]
          (.visitVarInsn mv (.getOpcode t Opcodes/ILOAD) slot)
          (box-prim mv pt)
          (recur (inc i) (+ slot (.getSize t))))))
    (let [invoke-desc (str "(" (apply str (repeat (inc n) "Ljava/lang/Object;"))
                           ")Ljava/lang/Object;")]
      (.visitMethodInsn mv Opcodes/INVOKEINTERFACE "clojure/lang/IFn" "invoke" invoke-desc true))
    (return-adapted mv (.getReturnType m))
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn emit-type-class
  "Emit and define a class: public mutable Object fields, the given
  interfaces, methods from `impls` ({\"name\" ifn}) as delegating stubs.
  Returns the Class. The impl table is installed via the static sci_impls
  field."
  ^Class [^String class-name fields ifaces impls]
  (let [internal-name (.replace class-name "." "/")
        cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        iface-internal (into-array String (map (fn [^Class i] (.getInternalName (Type/getType i))) ifaces))
        ;; reflected methods we implement: name present in impls, dedup by name+descriptor
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
                     :ms)]
    (.visit cw Opcodes/V1_8 Opcodes/ACC_PUBLIC internal-name nil "java/lang/Object" iface-internal)
    ;; static IFn[] sci_impls
    (.visitField cw (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "sci_impls" (str "[" ifn-desc) nil nil)
    ;; public mutable Object fields
    (doseq [f fields]
      (.visitField cw Opcodes/ACC_PUBLIC (name f) "Ljava/lang/Object;" nil nil))
    ;; ctor: (Object f0, Object f1, ...) storing fields
    (let [ctor-desc (str "(" (apply str (repeat (count fields) "Ljava/lang/Object;")) ")V")
          mv (.visitMethod cw Opcodes/ACC_PUBLIC "<init>" ctor-desc nil nil)]
      (.visitCode mv)
      (.visitVarInsn mv Opcodes/ALOAD 0)
      (.visitMethodInsn mv Opcodes/INVOKESPECIAL "java/lang/Object" "<init>" "()V" false)
      (doall
       (map-indexed (fn [i f]
                      (.visitVarInsn mv Opcodes/ALOAD 0)
                      (.visitVarInsn mv Opcodes/ALOAD (inc i))
                      (.visitFieldInsn mv Opcodes/PUTFIELD internal-name (name f) "Ljava/lang/Object;"))
                    fields))
      (.visitInsn mv Opcodes/RETURN)
      (.visitMaxs mv 0 0)
      (.visitEnd mv))
    ;; delegating stubs
    (doall (map-indexed (fn [i ^Method m] (emit-method-stub cw internal-name m i)) methods))
    (.visitEnd cw)
    (let [bytes (.toByteArray cw)
          cl (DynamicClassLoader.)
          cls (.defineClass cl class-name bytes nil)
          impl-arr (into-array IFn (map (fn [^Method m] (get impls (.getName m))) methods))]
      (-> (.getField cls "sci_impls") (.set nil impl-arr))
      cls)))

;; --- tests ---

(defn -main [& _]
  ;; java.util.function.Function: Object in/out
  (let [cls (emit-type-class "sci_type.FnType" '[state]
                             [java.util.function.Function]
                             {"apply" (fn [this x] [(.get ^java.lang.reflect.Field (.getField ^Class (class this) "state") this) (inc x)])})
        ctor (.getConstructor ^Class cls (into-array Class [Object]))
        inst (.newInstance ctor (object-array [:hello]))]
    (println "Function/apply:" (.apply ^java.util.function.Function inst 41)))
  ;; java.util.Comparator: primitive int return, called by Java (sort)
  (let [cls (emit-type-class "sci_type.CmpType" '[]
                             [java.util.Comparator]
                             {"compare" (fn [_this a b] (compare b a))})
        ^java.util.Comparator cmp (.newInstance ^Class cls)
        l (java.util.ArrayList. [3 1 2])]
    (java.util.Collections/sort l cmp)
    (println "Comparator via Collections/sort (desc):" (vec l)))
  ;; java.lang.Runnable: void return
  (let [p (promise)
        cls (emit-type-class "sci_type.RunType" '[]
                             [Runnable]
                             {"run" (fn [_this] (deliver p :ran))})
        ^Runnable r (.newInstance ^Class cls)
        t (Thread. r)]
    (.start t) (.join t)
    (println "Runnable on real Thread:" @p))
  ;; multiple interfaces + ctor fields + overloads (ILookup valAt 1&2)
  (let [cls (emit-type-class "sci_type.MultiType" '[m]
                             [clojure.lang.ILookup java.util.function.Supplier]
                             (let [fld (fn [this] (-> (.getField (class this) "m") (.get this)))]
                               {"valAt" (fn
                                          ([this k] (get (fld this) k))
                                          ([this k d] (get (fld this) k d)))
                                "get" (fn [_this] :supplied)}))
        ctor (.getConstructor ^Class cls (into-array Class [Object]))
        inst (.newInstance ctor (object-array [{:a 1}]))]
    (println "ILookup 2-arity:" (:a inst) "| 3-arity default:" (:b inst :dflt)
             "| Supplier:" (.get ^java.util.function.Supplier inst))))
