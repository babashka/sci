(ns sci.impl.interop
  {:no-doc true}
  (:require [sci.impl.types :as types]
            [sci.impl.utils :as utils]
            #?@(:cljs []
                :default [[sci.impl.reflector :as reflector]]))
  #?(:clj (:import
           [java.lang.reflect Modifier])))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:cljs nil :default (set! *warn-on-reflection* true))

(defn invoke-instance-field
  #?@(:cljs [[obj _target-class field-name]
             ;; gobject/get didn't work here
             (aget obj field-name)]
      :clj
      [[obj ^Class target-class method]
       (let [field (.getField target-class method)
             mod (.getModifiers field)]
         (if (and (not (Modifier/isStatic mod))
                  (Modifier/isPublic mod))
           (.get field obj)
           (throw (ex-info (str "Not found or accessible instance field: " method) {}))))]
      :cljr
      [[obj ^Type target-class method]
       (throw (ex-info (str "TODO CLR support: " `invoke-instance-field) {}))]))

#?(:cljs nil :default
   (defn meth-cache [ctx class meth-name len fetch-fn k]
     (let [cname #?(:clj (.getName ^Class class)
                    :cljr (.FullName ^Type class))
           env (:env ctx)
           meths (-> (deref env) k (get cname) (get meth-name) (get len))]
       (or meths
           (let [meths (fetch-fn)]
             (swap! env assoc-in [k cname meth-name len] meths)
             meths)))))

(defn invoke-instance-method
  #?@(:cljs [[ctx bindings obj _target-class method-name args _arg-count _arg-types]
             ;; gobject/get didn't work here
             (if-some [method (aget obj method-name)]
               ;; use Reflect rather than (.apply method ...), see https://github.com/babashka/nbb/issues/118
               (let [args (.map args #(sci.impl.types/eval % ctx bindings))]
                 (js/Reflect.apply method obj args))
               (throw (js/Error. (str "Could not find instance method: " method-name))))]
      :default
      [[ctx bindings obj target-class method ^objects args arg-count arg-types]
       (let [methods (meth-cache ctx target-class method arg-count #(reflector/get-methods target-class arg-count method false) :instance-methods)
             zero-args? (zero? arg-count)]
         (if (and zero-args?
                  #?(:clj (.isEmpty ^java.util.List methods)
                     :default (empty? methods)))
           (invoke-instance-field obj target-class method)
           (let [args-array (object-array arg-count)]
             (areduce args idx _ret nil
                      (aset args-array idx (types/eval (aget args idx) ctx bindings)))
             ;; Note: I also tried caching the method that invokeMatchingMethod looks up, but retrieving it from the cache was actually more expensive than just doing the invocation!
             ;; See getMatchingMethod in Reflector
             (reflector/invoke-matching-method method methods target-class obj args-array arg-types))))]))

(defn get-static-field [class field-name-sym]
  #?(:cljs (unchecked-get class field-name-sym)
     :default (reflector/get-static-field class (str field-name-sym))))

#?(:cljs
   (def fn-eval-allowed?
     (try (do (js/Function. "return 1")
              true)
          (catch :default _ false))))

#?(:cljs
   (do
     (defn get-static-fields [cur ^js parts]
       (loop [cur cur
              i 0]
         (if (< i (.-length parts))
           (recur (unchecked-get cur (aget parts i)) (inc i)) cur)))
     (defn accessor-fn
       "TODO: use this to look up at runtime, rather than look up at compile time, for correct behavior wrt. issue 877"
       [^js parts]
       (if fn-eval-allowed?
         (js/Function. "obj" (str "return obj." (.join parts ".")))
         (fn [obj]
           (get-static-fields obj parts))))))

#?(:cljs
   (defn invoke-js-constructor* [ctx bindings constructor args]
     (js/Reflect.construct constructor (.map args #(sci.impl.types/eval % ctx bindings)))))

#?(:cljs nil :default
   (defn invoke-constructor [class args]
     (reflector/invoke-constructor class (object-array args))))

(defn invoke-static-method #?(:cljs [ctx bindings class method args]
                              :default [ctx bindings class method-name ^objects args len])
  #?(:cljs (js/Reflect.apply method class (.map args #(sci.impl.types/eval % ctx bindings)))
     :default
     (let [args-array (object-array len)]
       ;; [a idx ret init expr]
       (areduce args idx _ret nil
                (aset args-array idx (sci.impl.types/eval (aget args idx) ctx bindings)))
       ;; List methods = getMethods(c, args.length, methodName, true);
       ;; invokeMatchingMethod(methodName, methods, null, args)
       (let [meths (meth-cache ctx class method-name len #(reflector/get-methods class len method-name true) :static-methods)]
         ;; Note: I also tried caching the method that invokeMatchingMethod looks up, but retrieving it from the cache was actually more expensive than just doing the invocation!
         ;; See getMatchingMethod in Reflector
         (reflector/invoke-matching-method method-name meths nil args-array)))))

(defn fully-qualify-class [ctx sym]
  (let [env @(:env ctx)
        class->opts (:class->opts env)]
    (or #?(:cljs (if-let [ns* (namespace sym)]
                   (when (identical? "js" ns*)
                     (when (contains? class->opts (symbol (name sym)))
                       sym))
                   (when (contains? class->opts sym)
                     sym))
           :default (when (contains? class->opts sym) sym))
        (let [cnn (utils/current-ns-name)
              imports (get-in env [:namespaces cnn :imports])]
          (if-let [[_ v] (find imports sym)]
            ;; finding a nil v means the object was unmapped
            v
            (get-in env [:imports sym]))))))

(defn resolve-class-opts [ctx sym]
  ;; note, we can't re-use fully-qualify class in this function, although it's
  ;; almost the same, since `js/Foo` stays fully qualified
  (let [env @(:env ctx)
        class->opts (:class->opts env)
        class-opts (or #?(:cljs (if-let [ns* (namespace sym)]
                                  (when (identical? "js" ns*)
                                    (get class->opts (symbol (name sym))))
                                  (get class->opts sym))
                          :default (get class->opts sym))
                       (let [cnn (utils/current-ns-name)
                             imports (get-in env [:namespaces cnn :imports])]
                         (if-let [[_ v] (find imports sym)]
                           ;; finding a nil v means the object was unmapped
                           (get class->opts v)
                           (when-let [v (get-in env [:imports sym])]
                             (get class->opts v)))))]
    class-opts))

(defn resolve-class [ctx sym]
  (:class (resolve-class-opts ctx sym)))

#?(:clj
   (def prim->class
     {'int Integer/TYPE
      'ints (Class/forName "[I")
      'long Long/TYPE
      'longs (Class/forName "[J")
      'float Float/TYPE
      'floats (Class/forName "[F")
      'double Double/TYPE
      'doubles (Class/forName "[D")
      'void Void/TYPE
      'short Short/TYPE
      'shorts (Class/forName "[S")
      'boolean Boolean/TYPE
      'booleans (Class/forName "[Z")
      'byte Byte/TYPE
      'bytes (Class/forName "[B")
      'char Character/TYPE
      'chars (Class/forName "[C")})
   :cljr
   (def prim->class
     {'int Int32
      'ints (Type/GetType "System.Int32[]")
      'long Int64
      'longs (Type/GetType "System.Int64[]")
      'float Single
      'floats (Type/GetType "System.Single[]")
      'double Double
      'doubles (Type/GetType "System.Double[]")
      'void  System.Void
      'short Int16 
      'shorts (Type/GetType "System.Int16[]")
      'boolean Boolean
      'booleans (Type/GetType "System.Boolean[]")
      'byte Byte
      'bytes (Type/GetType "System.Byte[]")
      'sbyte SByte
      'sbytes (Type/GetType "System.SByte[]")
      'ushort UInt16
      'ushorts (Type/GetType "System.UInt16[]")
      'uint  UInt32
      'uints (Type/GetType "System.UInt32[]")
      'ulong UInt64
      'ulongs (Type/GetType "System.UInt64[]")
      'char Char
      'chars (Type/GetType "System.Char[]")}))

#?(:cljs nil :default
   (defn resolve-type-hint [ctx sym]
     (if (string? sym) (#?(:clj Class/forName :cljr Type/GetType) sym)
         (or (get prim->class sym)
             (:class (resolve-class-opts ctx sym))))))

#?(:cljs nil :default
   (def ->array-class
     (memoize (fn [clazz dim]
                (class (apply make-array clazz (vec (repeat dim 0))))))))

#?(:cljs nil :default
   (defn resolve-array-class
     #?(:clj ^Class [ctx sym-ns ^String sym-name-str]
        :cljr ^Type [ctx sym-ns ^String sym-name-str])
     (when-let [clazz (or (resolve-class ctx sym-ns)
                          (get prim->class sym-ns))]
       (let [dim (- (int (#?(:clj .charAt :cljr .get_Chars) sym-name-str 0)) 48)]
         (->array-class clazz dim)))))
