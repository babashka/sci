(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import
           [java.lang.reflect Field Modifier]
           [sci.impl Reflector]))
  (:require [sci.impl.types]
            [sci.impl.utils :as utils]))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:clj (set! *warn-on-reflection* true))

(defn invoke-instance-field
  #?@(:cljs [[obj _target-class field-name]
             ;; gobject/get didn't work here
             (aget obj field-name)]
      :clj
      [[obj ^Class target-class method]
       (let [^Field field (.getField target-class method)
             mod (.getModifiers field)]
         (if (and (not (Modifier/isStatic mod))
                  (Modifier/isPublic mod))
           (.get field obj)
           (throw (ex-info (str "Not found or accessible instance field: " method) {}))))]))

#?(:clj
   (defn meth-cache [ctx ^Class class meth-name len fetch-fn k]
     (let [cname (.getName class)
           env (:env ctx)
           meths (-> (deref env) k (get cname) (get meth-name) (get len))]
       (or meths
           (let [meths (fetch-fn)]
             (swap! env assoc-in [k cname meth-name len] meths)
             meths)))))

(defn invoke-instance-method
  #?@(:cljs [[ctx bindings obj _target-class method-name args _arg-count]
             ;; gobject/get didn't work here
             (if-some [method (aget obj method-name)]
               ;; use Reflect rather than (.apply method ...), see https://github.com/babashka/nbb/issues/118
               (let [args (.map args #(sci.impl.types/eval % ctx bindings))]
                 (js/Reflect.apply method obj args))
               (throw (js/Error. (str "Could not find instance method: " method-name))))]
      :clj
      [[ctx bindings obj ^Class target-class method ^objects args arg-count]
       (let [^"[Ljava.util.List;" methods
             (meth-cache ctx target-class method arg-count #(Reflector/getMethods target-class arg-count method false) :instance-methods)]
         (if (and (zero? arg-count) (.isEmpty ^java.util.List methods))
           (invoke-instance-field obj target-class method)
           (do (let [args-array (object-array arg-count)
                     ^"[Ljava.lang.Class;" types-array (when (> (count methods) 1)
                                                         (make-array Class arg-count))]
                 (areduce args idx _ret nil
                          (do (aset args-array idx (sci.impl.types/eval (aget args idx) ctx bindings))
                              (when types-array
                                (when-let [t (:tag-class (meta (aget args idx)))]
                                  (when (class? t)
                                    (aset types-array idx t))))))
                 ;; Note: I also tried caching the method that invokeMatchingMethod looks up, but retrieving it from the cache was actually more expensive than just doing the invocation!
                 ;; See getMatchingMethod in Reflector
                 (Reflector/invokeMatchingMethod method methods target-class obj args-array types-array)))))]))

(defn get-static-field [^Class class field-name-sym]
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (unchecked-get class field-name-sym)))

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

#?(:clj
   (defn invoke-constructor #?(:clj [^Class class args]
                               :cljs [constructor args])
     (Reflector/invokeConstructor class (object-array args))))

(defn invoke-static-method #?(:clj [ctx bindings ^Class class ^String method-name ^objects args len]
                              :cljs [ctx bindings class method args])
  #?(:clj
     (let [args-array (object-array len)]
       ;; [a idx ret init expr]
       (areduce args idx _ret nil
                (aset args-array idx (sci.impl.types/eval (aget args idx) ctx bindings)))
       ;; List methods = getMethods(c, args.length, methodName, true);
       ;; invokeMatchingMethod(methodName, methods, null, args)
       (let [meths (meth-cache ctx class method-name len #(sci.impl.Reflector/getMethods class len method-name true) :static-methods)]
         ;; Note: I also tried caching the method that invokeMatchingMethod looks up, but retrieving it from the cache was actually more expensive than just doing the invocation!
         ;; See getMatchingMethod in Reflector
         (sci.impl.Reflector/invokeMatchingMethod method-name meths nil args-array)))
     :cljs (js/Reflect.apply method class (.map args #(sci.impl.types/eval % ctx bindings)))))

(defn fully-qualify-class [ctx sym]
  (let [env @(:env ctx)
        class->opts (:class->opts env)]
    (or #?(:clj (when (contains? class->opts sym) sym)
           :cljs (if-let [ns* (namespace sym)]
                   (when (identical? "js" ns*)
                     (when (contains? class->opts (symbol (name sym)))
                       sym))
                   (when (contains? class->opts sym)
                     sym)))
        (get (:imports env) sym)
        (let [cnn (utils/current-ns-name)]
          (get-in env [:namespaces cnn :imports sym])))))

(defn resolve-class-opts [ctx sym]
  (let [env @(:env ctx)
        class->opts (:class->opts env)
        class-opts (or #?(:clj (get class->opts sym)
                          :cljs (if-let [ns* (namespace sym)]
                                  (when (identical? "js" ns*)
                                    (get class->opts (symbol (name sym))))
                                  (get class->opts sym)))
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
      'chars (Class/forName "[C")}))

(defn resolve-type-hint [ctx sym]
  (or (get prim->class sym)
      (:class (resolve-class-opts ctx sym))))

#?(:clj
   (def ->array-class
     (memoize (fn [clazz dim]
                (class (apply make-array clazz (vec (repeat dim 0))))))))

#?(:clj
   (defn resolve-array-class [ctx sym-ns ^String sym-name-str]
     (when-let [clazz (or (resolve-class ctx sym-ns)
                          (get prim->class sym-ns))]
       (let [dim (- (int (.charAt sym-name-str 0)) 48)]
         (->array-class clazz dim)))))
