;;TODO thoroughly review this code and compare with original Java code. it was AI generated.
(ns sci.impl.reflector
  "Minimal reflection support for SCI.

  JVM notes:
  - mostly based on clojure.java.Reflector (transliterated by Claude Sonnet 4.5)
    - made invokeMatchingMethod public via invoke-matching-method
      - added arg-types to allow type hints to steer reflection resolution
  - FISupport - extracted from Compiler to support functional interface adaptation"
  {:no-doc true}
  #?(:clj
     (:import [java.lang.reflect Method Modifier Proxy]
              [clojure.lang Reflector Compiler IFn RT])))

#?(:clj (set! *warn-on-reflection* :warn-on-boxed))

;; FISupport

#?(:clj
   (do
     ;; AFn already implements these functional interfaces, so we don't need to adapt them
     (def ^:private afn-fis
       #{java.util.concurrent.Callable
         java.lang.Runnable
         java.util.Comparator})

     (def ^:private object-methods
       #{"equals" "toString" "hashCode"})

     (defn- maybe-fi-method
       "Return FI method if:
        1) Target is a functional interface and not already implemented by AFn
        2) Target method matches one of our fn invoker methods (0 <= arity <= 10)"
       [^Class target]
       (when (and target
                  (.isAnnotationPresent target java.lang.FunctionalInterface)
                  (not (contains? afn-fis target)))
         (let [methods (.getMethods target)]
           (some (fn [^Method method]
                   (when (and (>= (.getParameterCount method) 0)
                              (<= (.getParameterCount method) 10)
                              (Modifier/isAbstract (.getModifiers method))
                              (not (contains? object-methods (.getName method))))
                     method))
                 methods))))))

;; Reflector functions

#?(:clj
   (defn get-methods
     "Get methods matching the given class, arity, method name and static flag.
      Delegates directly to clojure.lang.Reflector."
     [^Class c arity ^String method-name static?]
     (Reflector/getMethods c arity method-name static?)))

#?(:clj
   (defn get-static-field
     "Get a static field value from a class.
      Delegates directly to clojure.lang.Reflector."
     [^Class c ^String field-name]
     (Reflector/getStaticField c field-name)))

#?(:clj
   (defn invoke-constructor
     "Invoke a constructor on a class with the given arguments.
      Delegates directly to clojure.lang.Reflector."
     [^Class c args]
     (Reflector/invokeConstructor c args)))

#?(:clj
   (defn invoke-static-method
     "Invoke a static method on a class with the given arguments.
      Delegates directly to clojure.lang.Reflector."
     [^Class c ^String method-name ^objects args]
     (Reflector/invokeStaticMethod c method-name args)))

#?(:clj
   (defn- coerce-adapter-return
     "Return type coercions match coercions in FnInvokers for compiled invokers"
     [ret ^Class target-type]
     (if (.isPrimitive target-type)
       (case (.getName target-type)
         "boolean" (RT/booleanCast ret)
         "long"    (RT/longCast ret)
         "double"  (RT/doubleCast ret)
         "int"     (RT/intCast ret)
         "short"   (RT/shortCast ret)
         "byte"    (RT/byteCast ret)
         "float"   (RT/floatCast ret)
         ret)
       ret)))

#?(:clj
   (defn- box-arg
     "Box an argument to match the parameter type.
      Handles IFn -> Functional Interface adaptation."
     [^Class param-type ^Object arg]
     (if (and (instance? IFn arg)
              (when-let [_fi-method (maybe-fi-method param-type)]
                (not (.isInstance param-type arg))))
       ;; Adapt IFn obj to targetType using dynamic proxy
       (Proxy/newProxyInstance
        (RT/baseLoader)
        (into-array Class [param-type])
        (reify java.lang.reflect.InvocationHandler
          (invoke [_ _proxy method method-args]
            (let [ret (.applyTo ^IFn arg (RT/seq method-args))]
              (coerce-adapter-return ret (.getReturnType ^Method method))))))
       ;; Standard boxing
       (cond
         (not (.isPrimitive param-type))
         (.cast param-type arg)

         (identical? param-type Boolean/TYPE)
         (.cast Boolean arg)

         (identical? param-type Character/TYPE)
         (.cast Character arg)

         (instance? Number arg)
         (let [^Number n arg]
           (cond
             (identical? param-type Integer/TYPE) (.intValue n)
             (identical? param-type Float/TYPE) (.floatValue n)
             (identical? param-type Double/TYPE) (.doubleValue n)
             (identical? param-type Long/TYPE) (.longValue n)
             (identical? param-type Short/TYPE) (.shortValue n)
             (identical? param-type Byte/TYPE) (.byteValue n)
             :else (throw (IllegalArgumentException.
                           (str "Unexpected param type, expected: " param-type
                                ", given: " (.getName (.getClass arg)))))))

         :else
         (throw (IllegalArgumentException.
                 (str "Unexpected param type, expected: " param-type
                      ", given: " (.getName (.getClass arg)))))))))

#?(:clj
   (defn- box-args
     "Box all arguments to match parameter types."
     [^objects params ^objects args]
     (if (zero? (alength params))
       nil
       (let [ret (object-array (alength params))]
         (dotimes [i (alength params)]
           (aset ret i (box-arg (aget params i) (aget args i))))
         ret))))

#?(:clj
   (defn- param-arg-type-match?
     "Check if a parameter type matches an argument type.
      Includes support for functional interface adaptation."
     [^Class param-type ^Class arg-type]
     (cond
       (nil? arg-type)
       (not (.isPrimitive param-type))

       (or (identical? param-type arg-type) (.isAssignableFrom param-type arg-type))
       true

       (and (maybe-fi-method param-type) (.isAssignableFrom IFn arg-type))
       true

       (identical? param-type Integer/TYPE)
       (or (identical? arg-type Integer)
           (identical? arg-type Long/TYPE)
           (identical? arg-type Long)
           (identical? arg-type Short/TYPE)
           (identical? arg-type Byte/TYPE))

       (identical? param-type Float/TYPE)
       (or (identical? arg-type Float)
           (identical? arg-type Double/TYPE))

       (identical? param-type Double/TYPE)
       (or (identical? arg-type Double)
           (identical? arg-type Float/TYPE))

       (identical? param-type Long/TYPE)
       (or (identical? arg-type Long)
           (identical? arg-type Integer/TYPE)
           (identical? arg-type Short/TYPE)
           (identical? arg-type Byte/TYPE))

       (identical? param-type Character/TYPE)
       (identical? arg-type Character)

       (identical? param-type Short/TYPE)
       (identical? arg-type Short)

       (identical? param-type Byte/TYPE)
       (identical? arg-type Byte)

       (identical? param-type Boolean/TYPE)
       (identical? arg-type Boolean)

       :else
       false)))

#?(:clj
   (defn- is-congruent?
     "Check if parameters are congruent with arguments and arg-types."
     [^objects params ^objects args ^objects arg-types]
     (if (nil? args)
       (zero? (alength params))
       (and (== (alength params) (alength args))
            (loop [i 0]
              (if (< i (alength params))
                (let [arg (aget args i)
                      arg-type (if arg-types
                                 (let [t (aget arg-types i)]
                                   (if (and (nil? t) (some? arg))
                                     (.getClass ^Object arg)
                                     t))
                                 (when arg (.getClass ^Object arg)))
                      param-type (aget params i)]
                  (if (param-arg-type-match? param-type arg-type)
                    (recur (inc i))
                    false))
                true))))))

#?(:clj
   (defn- match-method
     "Find the best matching method from a list of methods given args and optional arg-types."
     [^java.util.List methods args arg-types]
     (let [size (.size methods)]
       (loop [i 0
              found-m nil]
         (if (< i size)
           (if-let [^Method m (.get methods i)]
             (let [params (.getParameterTypes m)]
               (if (and (is-congruent? params args arg-types)
                        (or (nil? found-m)
                            (Compiler/subsumes params (.getParameterTypes ^Method found-m))))
                 (recur (inc i) m)
                 (recur (inc i) found-m)))
             found-m)
           found-m)))))

#?(:clj
   (defn- widen-boxed-args!
     "Widen boxed numeric arguments (e.g., Integer -> Long, Float -> Double).
      Mutates args"
     [^objects args]
     (let [widened args]
       (dotimes [i (alength args)]
         (let [arg (aget args i)]
           (when (some? arg)
             (let [val-class (.getClass ^Object arg)]
               (aset widened i
                     (cond
                       (or (identical? val-class Integer) (identical? val-class Short) (identical? val-class Byte))
                       (.longValue ^Number arg)

                       (identical? val-class Float)
                       (.doubleValue ^Number arg)

                       :else
                       arg))))))
       widened)))

#?(:clj
   (defn invoke-matching-method
     "Invoke a method matching the given name from a list of methods.
      This is the core SCI-specific method that supports type hints via arg-types.
      Parameters:
      - method-name: String name of the method
      - methods: java.util.List of Method objects (retured by getMethods)
      - context-class: Class for error messages (can be nil for static methods)
      - target: Object to invoke on (nil for static methods)
      - args: Object array of arguments
      - arg-types: Optional array of Class objects for type hints"
     ([method-name methods ^Object target args]
      (invoke-matching-method method-name methods
                              (when target (.getClass target))
                              target args nil))
     ([method-name methods context-class target args]
      (invoke-matching-method method-name methods context-class target args nil))
     ([method-name ^java.util.List methods context-class target ^objects args arg-types]
      (if (.isEmpty methods)
        (throw (IllegalArgumentException.
                (str "No matching method " method-name " found taking "
                     (alength args) " args"
                     (when context-class (str " for " context-class)))))
        (let [^Method m (if (== 1 (.size methods))
                          (.get methods 0)
                          (or (match-method methods args arg-types)
                              ;; widen boxed args and re-try
                              (match-method methods (widen-boxed-args! args) arg-types)))]
          (if (nil? m)
            (throw (IllegalArgumentException.
                    (str "No matching method " method-name " found taking "
                         (alength args) " args"
                         (when context-class (str " for " context-class)))))
            ;; Use Reflector's helper to find accessible version of method
            (let [^Method
                  accessible-m (if (or (not (Modifier/isPublic
                                             (.getModifiers (.getDeclaringClass m))))
                                       (and target
                                            (not (.canAccess m target))))
                                 (clojure.lang.Reflector/getAsMethodOfAccessibleBase (or context-class (.getDeclaringClass m))
                                                                                     m
                                                                                     target)
                                 m)]
              (when (nil? accessible-m)
                (throw (IllegalArgumentException.
                        (str "Can't call public method of non-public class: " m))))
              (try
                (let [ret (.invoke accessible-m target (box-args (.getParameterTypes accessible-m) args))]
                  (Reflector/prepRet (.getReturnType accessible-m) ret))
                (catch Exception e
                  (throw (clojure.lang.Util/sneakyThrow
                          (or (.getCause e) e))))))))))))
