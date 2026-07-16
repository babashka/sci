(ns sci.impl.interop
  {:no-doc true}
  #?@(:cljd [] :clj [(:import
                      [java.lang.reflect Field Modifier])])
  (:require [sci.impl.types]
            [sci.impl.utils :as utils]
            #?@(:cljd [] :clj [[sci.impl.reflector :as reflector]])))

;; see https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
;; see invokeStaticMethod, getStaticField, etc.

#?(:cljd nil :clj (set! *warn-on-reflection* true))

(defn invoke-instance-field
  #?@(:cljd [[obj _target-class field-name]
             (throw (ex-info (str "Instance fields not yet supported in cljd: " field-name) {}))]
      :cljs [[obj _target-class field-name]
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

#?(:cljd nil
   :clj
   (defn meth-cache [ctx ^Class class meth-name len fetch-fn k]
     (let [cname (.getName class)
           env (:env ctx)
           meths (-> (deref env) k (get cname) (get meth-name) (get len))]
       (or meths
           (let [meths (fetch-fn)]
             (swap! env assoc-in [k cname meth-name len] meths)
             meths)))))

#?(:clj
   (defn instance-method-list [ctx ^Class target-class method arg-count]
     (meth-cache ctx target-class method arg-count
                 #(reflector/get-methods target-class arg-count method false)
                 :instance-methods)))

#?(:clj
   (defn invoke-instance-method-with-methods
     "Like invoke-instance-method but takes a pre-resolved method list, skipping
     the meth-cache lookup. Used by the per-call-site inline cache."
     [ctx bindings obj ^Class target-class method ^java.util.List methods ^objects args arg-count arg-types]
     (if (and (zero? arg-count) (.isEmpty methods))
       (invoke-instance-field obj target-class method)
       (let [args-array (object-array arg-count)]
         (areduce args idx _ret nil
                  (aset args-array idx (sci.impl.types/eval (aget args idx) ctx bindings)))
         (reflector/invoke-matching-method method methods target-class obj args-array arg-types)))))

(defn invoke-instance-method
  #?@(:cljd [[_ctx _bindings _obj _target-class method-name _args _arg-count _arg-types]
             (throw (ex-info (str "Instance methods not yet supported in cljd: " method-name) {}))]
      :cljs [[ctx bindings obj _target-class method-name args _arg-count _arg-types]
             ;; gobject/get didn't work here
             (if-some [method (aget obj method-name)]
               ;; use Reflect rather than (.apply method ...), see https://github.com/babashka/nbb/issues/118
               (let [args (.map args #(sci.impl.types/eval % ctx bindings))]
                 (js/Reflect.apply method obj args))
               (throw (js/Error. (str "Could not find instance method: " method-name))))]
      :clj
      [[ctx bindings obj ^Class target-class method ^objects args arg-count arg-types]
       (let [methods (meth-cache ctx target-class method arg-count #(reflector/get-methods target-class arg-count method false) :instance-methods)]
         ;; Note: I also tried caching the method that invokeMatchingMethod looks up, but retrieving it from the cache was actually more expensive than just doing the invocation!
         ;; See getMatchingMethod in Reflector
         (invoke-instance-method-with-methods ctx bindings obj target-class method methods args arg-count arg-types))]))

(defn get-static-field [#?(:cljd class :clj ^Class class :cljs class) field-name-sym]
  #?(:cljd (throw (ex-info (str "Static fields not yet supported in cljd: " field-name-sym) {}))
     :clj (reflector/get-static-field class (str field-name-sym))
     :cljs (unchecked-get class field-name-sym)))

#?(:cljs
   (def fn-eval-allowed?
     ;; the shared probe respects SCI_DISABLE_JIT and CSP; when false,
     ;; accessor-fn uses the loop fallback
     sci.impl.types/js-eval-available))

#?(:cljs
   (def ^:private instance-override-names-cache
     ;; [class->opts result], keyed on map identity: any add-class! or init
     ;; produces a new map
     (volatile! nil)))

#?(:cljs
   (defn instance-override-names
     "Returns the set of member name symbols with an :instance-methods or
     :instance-fields override in any :classes entry. Cached on class->opts
     identity."
     [ctx]
     (let [class->opts (:class->opts @(:env ctx))
           cached @instance-override-names-cache]
       (if (and cached (identical? (nth cached 0) class->opts))
         (nth cached 1)
         (let [res (reduce (fn [acc opts]
                             (if (map? opts)
                               (-> acc
                                   (into (keys (:instance-methods opts)))
                                   (into (keys (:instance-fields opts))))
                               acc))
                           #{}
                           (vals class->opts))]
           (vreset! instance-override-names-cache [class->opts res])
           res)))))

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

#?(:cljd nil
   :clj
   (defn invoke-constructor #?(:clj [^Class class args]
                               :cljs [constructor args])
     (reflector/invoke-constructor class (object-array args))))

(defn invoke-static-method #?(:cljd [_ctx _bindings _class method _args]
                              :clj [ctx bindings ^Class class ^String method-name ^objects args len]
                              :cljs [ctx bindings class method args])
  #?(:cljd
     (throw (ex-info (str "Static methods not yet supported in cljd: " method) {}))
     :clj
     (let [args-array (object-array len)]
       ;; [a idx ret init expr]
       (areduce args idx _ret nil
                (aset args-array idx (sci.impl.types/eval (aget args idx) ctx bindings)))
       ;; List methods = getMethods(c, args.length, methodName, true);
       ;; invokeMatchingMethod(methodName, methods, null, args)
       (let [meths (meth-cache ctx class method-name len #(reflector/get-methods class len method-name true) :static-methods)]
         ;; Note: I also tried caching the method that invokeMatchingMethod looks up, but retrieving it from the cache was actually more expensive than just doing the invocation!
         ;; See getMatchingMethod in Reflector
         (reflector/invoke-matching-method method-name meths nil args-array)))
     :cljs (js/Reflect.apply method class (.map args #(sci.impl.types/eval % ctx bindings)))))

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

(defn closed?
  "True when class-opts closes the given section (:instance-methods,
  :static-methods, :instance-fields or :static-fields). Class-level
  `:closed true` closes all sections, a section-level `:closed true`
  closes just that section."
  [class-opts section]
  (or (true? (:closed class-opts))
      (true? (:closed (get class-opts section)))))

(defn member-disposition
  "How a configured member value resolves. `override` is the value looked up in
  a member section (nil, `true`, or a fn). Returns `:override` when it is a fn to
  apply, `:deny` when the member is unlisted and the section is closed, and
  `:reflect` otherwise (unlisted and open, or the `true` sentinel)."
  [override class-opts section]
  (cond
    (and override (not (true? override))) :override
    (and (not override) (closed? class-opts section)) :deny
    :else :reflect))

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

#?(:cljd nil
   :clj
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

#?(:cljd nil
   :clj
   (def ->array-class
     (memoize (fn [clazz dim]
                (class (apply make-array clazz (vec (repeat dim 0))))))))

#?(:cljd nil
   :clj
   (defn resolve-array-class [ctx sym-ns ^String sym-name-str]
     (when-let [clazz (or (resolve-class ctx sym-ns)
                          (get prim->class sym-ns))]
       (let [dim (- (int (.charAt sym-name-str 0)) 48)]
         (->array-class clazz dim)))))

#?(:clj
   (def ^:private descriptor-prim->sym
     {\B 'byte \S 'short \I 'int \J 'long \F 'float \D 'double \C 'char \Z 'boolean}))

#?(:clj
   (defn resolve-array-descriptor [ctx ^String s]
     ;; Normalize a JVM array descriptor to Clojure 1.12 array notation (element + dim)
     ;; and resolve the element through the :classes allowlist, then build the array
     ;; class. "[B" -> byte/1, "[[Ljava.lang.String;" -> java.lang.String/2.
     ;; No Class/forName: an off-allowlist element class is never loaded, and the
     ;; element is gated exactly like the 1.12 Foo/N notation (resolve-array-class).
     (let [len (.length s)
           dim (inc (.lastIndexOf s (int \[)))   ; brackets are contiguous and leading
           c (.charAt s dim)
           elem-class (if (identical? \L c)
                        (when (identical? \; (.charAt s (dec len)))
                          (resolve-class ctx (symbol (subs s (inc dim) (dec len)))))
                        (get prim->class (get descriptor-prim->sym c)))]
       (when elem-class
         (->array-class elem-class dim)))))

#?(:clj
   (defn resolve-type-hint [ctx sym]
     ;; A string tag must go through the :classes allowlist, same as a symbol tag.
     ;; Calling (Class/forName sym) directly let untrusted code load and
     ;; static-initialize any class on the classpath (sandbox bypass). A nil result
     ;; means "not allowed" / "no hint", which the analyzer already handles.
     ;; Array descriptors ("[B", "[Ljava.lang.String;") are normalized and gated
     ;; through the allowlist via the element class, see resolve-array-descriptor.
     (if (string? sym)
       (if (.startsWith ^String sym "[")
         (resolve-array-descriptor ctx sym)
         (:class (resolve-class-opts ctx (symbol sym))))
       (or (get prim->class sym)
           (:class (resolve-class-opts ctx sym))))))
