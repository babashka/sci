(ns sci.impl.interop
  {:no-doc true}
  #?(:clj (:import
           [java.lang.reflect Field Modifier]
           [sci.impl Reflector]))
  (:require #?(:cljs [goog.object :as gobject])
            [sci.impl.types]
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
     (let [env (:env ctx)
           static-meths (k @env)
           class-name (.getName class)
           meths (get-in static-meths [class-name meth-name len])]
       (or meths
           (let [meths (fetch-fn)]
             (swap! env assoc-in [k class-name meth-name len] meths)
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
       (let [methods
             (meth-cache ctx target-class method arg-count #(Reflector/getMethods target-class arg-count method false) :instance-methods)]
         (if (and (zero? arg-count) (.isEmpty ^java.util.List methods))
           (invoke-instance-field obj target-class method)
           (do (let [args-array (object-array arg-count)]
                 (areduce args idx _ret nil
                          (aset args-array idx (sci.impl.types/eval (aget args idx) ctx bindings)))
                 (Reflector/invokeMatchingMethod method methods obj args-array)))))]))

(defn get-static-field [^Class class field-name-sym]
  #?(:clj (Reflector/getStaticField class (str field-name-sym))
     :cljs (gobject/get class field-name-sym)))

#?(:cljs (defn get-static-fields [class path-array idx max-idx]
           (if (nil? idx)
             (recur class path-array 0 (dec (alength path-array)))
             (let [class (gobject/get class (aget path-array idx))]
               (if (== idx max-idx)
                 class
                 (recur class path-array (inc idx) max-idx))))))

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
         (sci.impl.Reflector/invokeMatchingMethod method-name meths nil args-array))
       #_(Reflector/invokeStaticMethod class ^String method-name ^"[Ljava.lang.Object;" args-array))
     :cljs (js/Reflect.apply method class (.map args #(sci.impl.types/eval % ctx bindings)))))

#?(:clj
   (comment
     (time (dotimes [_ 1000000]
             (Reflector/invokeStaticMethod Math "sin" (object-array [1]))))
     (require '[sci.core])
     (let [meths (sci.impl.Reflector/getMethods Math 1 "sin" true)]
       (time (dotimes [_ 1000000]
               (sci.impl.Reflector/invokeMatchingMethod "sin" meths nil (object-array [1])))))
     (time (sci.core/eval-string "(dotimes [i 1000000] (Math/sin 1))" {:classes {'Math Math}}))
     (time (sci.core/eval-string "(dotimes [i 1000000] (.length \"foo\"))" {:classes {'Math Math}}))
     ))

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
        (or (get (:imports env) sym)
            (let [cnn (utils/current-ns-name)]
              (get-in env [:namespaces cnn :imports sym]))))))

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
