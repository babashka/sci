(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn pr-str prn-str print print-str println println-str
                            newline flush with-out-str with-in-str read-line
                            printf #?@(:cljs [string-print])
                            #?@(:clj [print-simple])])
  (:require
   #?(:cljs [goog.string])
   [sci.ctx-store :as store]
   [sci.impl.copy-vars :refer [copy-var]]
   [sci.impl.records]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]))

#?(:cljd nil :clj (set! *warn-on-reflection* true))

(defn core-dynamic-var
  "create a dynamic var with clojure.core :ns meta"
  ([name] (core-dynamic-var name nil))
  ([name init-val] (utils/dynamic-var name init-val {:ns utils/clojure-core-ns
                                                     :sci/built-in true}))
  ([name init-val extra-meta] (utils/dynamic-var name init-val
                                                 (assoc extra-meta :ns utils/clojure-core-ns
                                                        :sci/built-in true))))

(def in (store/with-ctx {:unrestricted true}
          (doto (core-dynamic-var '*in*)
            (vars/unbind)
            #?(:clj (alter-meta! assoc
                                 :doc "A java.io.Reader object representing standard input for read operations.")))))

(def out (store/with-ctx {:unrestricted true}
           #?(:cljd (core-dynamic-var '*out* *out*)
              :default
              (doto (core-dynamic-var '*out*)
                (vars/unbind)
                #?(:clj (alter-meta! assoc :doc "A java.io.Writer object representing standard output for print operations."))))))

(def err (store/with-ctx {:unrestricted true}
           #?(:cljd (core-dynamic-var '*err* *err*)
              :default
              (doto (core-dynamic-var '*err*)
                (vars/unbind)
                #?(:clj (alter-meta! assoc :doc " A java.io.Writer object representing standard error for print operations."))))))

#?(:cljs
   (def print-fn
     (store/with-ctx {:unrestricted true}
       (doto (core-dynamic-var '*print-fn*)
         (vars/unbind)))))

#?(:cljs
   (def print-err-fn
     (store/with-ctx {:unrestricted true}
       (doto (core-dynamic-var '*print-err-fn*)
         (vars/unbind)))))

;; TODO: CLJS print-fn-bodies

(def print-meta (copy-var *print-meta* utils/clojure-core-ns {:dynamic true}))
(def print-length (copy-var *print-length* utils/clojure-core-ns {:dynamic true}))
(def print-level (copy-var *print-level* utils/clojure-core-ns {:dynamic true}))
;; no-op on cljd, cljd.core/pr never emits the #:ns{...} form
(def print-namespace-maps #?(:cljd (core-dynamic-var '*print-namespace-maps* true)
                             :default (copy-var *print-namespace-maps* utils/clojure-core-ns {:dynamic true :init true})))
(def flush-on-newline #?(:cljd (core-dynamic-var '*flush-on-newline* true)
                         :default (copy-var *flush-on-newline* utils/clojure-core-ns {:dynamic true})))
(def print-readably (copy-var *print-readably* utils/clojure-core-ns {:dynamic true}))
(def print-dup-var (copy-var *print-dup* utils/clojure-core-ns {:dynamic true}))
#?(:cljs (def print-newline (copy-var *print-newline* utils/clojure-core-ns {:dynamic true})))

#?(:cljs (defn string-print [x]
           (binding [*print-fn* @print-fn]
             (cljs.core/string-print x))) )

#?(:clj (defn pr-on
          {:private true
           :static true}
          [x w]
          (if *print-dup*
            (print-dup x w)
            (print-method x w))
          nil))

#?(:clj
   (defn print-fn-with-host-vars
     "Returns print method f wrapped to run with SCI's print vars bound to
  the values of Clojure's."
     [f]
     (fn [x w]
       (let [length *print-length*
             level *print-level*
             meta? *print-meta*
             ns-maps *print-namespace-maps*
             readably *print-readably*
             dup *print-dup*]
         (if (and (identical? length @print-length)
                  (identical? level @print-level)
                  (identical? meta? @print-meta)
                  (identical? ns-maps @print-namespace-maps)
                  (identical? readably @print-readably)
                  (identical? dup @print-dup-var))
           (f x w)
           (vars/with-bindings {print-length length
                                print-level level
                                print-meta meta?
                                print-namespace-maps ns-maps
                                print-readably readably
                                print-dup-var dup}
             (f x w)))))))

#?(:clj
   (defn print-multi-fn
     "Returns a MultiFn that delegates to host-fn and binds Clojure's print
  vars to the values of SCI's for each call."
     [fn-name ^clojure.lang.MultiFn host-fn]
     (proxy [clojure.lang.MultiFn] [fn-name
                                    (.-dispatchFn host-fn)
                                    (.-defaultDispatchVal host-fn)
                                    (.-hierarchy host-fn)]
       (invoke [x w]
         (let [length @print-length
               level @print-level
               meta? @print-meta
               ns-maps @print-namespace-maps
               readably @print-readably
               dup @print-dup-var]
           (if (and (identical? length *print-length*)
                    (identical? level *print-level*)
                    (identical? meta? *print-meta*)
                    (identical? ns-maps *print-namespace-maps*)
                    (identical? readably *print-readably*)
                    (identical? dup *print-dup*))
             (host-fn x w)
             (binding [*print-length* length
                       *print-level* level
                       *print-meta* meta?
                       *print-namespace-maps* ns-maps
                       *print-readably* readably
                       *print-dup* dup]
               (host-fn x w)))))
       (addMethod [dispatch-val f] (.addMethod host-fn dispatch-val f) this)
       (removeMethod [dispatch-val] (.removeMethod host-fn dispatch-val) this)
       (preferMethod [x y] (.preferMethod host-fn x y) this)
       (reset [] (.reset host-fn) this)
       (getMethod [dispatch-val] (.getMethod host-fn dispatch-val))
       (getMethodTable [] (.getMethodTable host-fn))
       (getPreferTable [] (.getPreferTable host-fn)))))

#?(:clj (defn core-pr-on
          [x w]
          (binding [*print-length* @print-length
                    *print-level* @print-level
                    *print-meta* @print-meta
                    *print-namespace-maps* @print-namespace-maps
                    *print-readably* @print-readably
                    *print-dup* @print-dup-var]
            (pr-on x w))))

#?(:cljd (defn pr
           [& objs]
           (binding [*out* @out
                     *print-length* @print-length
                     *print-level* @print-level
                     *print-meta* @print-meta
                     *print-readably* @print-readably
                     *print-dup* @print-dup-var]
             (apply cljd.core/pr objs)))
   :clj (defn pr
          ([] nil)
          ([x] (core-pr-on x @out))
          ([x & more]
           (pr x)
           (. ^java.io.Writer @out (append \space))
           (if-let [nmore (next more)]
             (recur (first more) nmore)
             (apply pr more))))
   :cljs (defn pr
           [& objs]
           (binding [*print-fn* @print-fn
                     *print-length* @print-length
                     *print-level* @print-level
                     *print-meta* @print-meta
                     *print-namespace-maps* @print-namespace-maps
                     *print-readably* @print-readably
                     *print-newline* @print-newline
                     *print-dup* @print-dup-var]
             (apply cljs.core/pr objs))))

#?(:cljd (defn flush [] ;stub
           nil)
   :clj
   (defn flush
     []
     (. ^java.io.Writer @out (flush))
     nil)
   :cljs (defn flush [] ;stub
           nil))

#?(:cljs (declare println))

#?(:cljd (defn newline
           []
           (binding [*out* @out]
             (cljd.core/newline)))
   :clj (defn newline
          []
          (. ^java.io.Writer @out (append ^String @#'clojure.core/system-newline))
          nil)
   :cljs (defn newline
           []
           (binding [*print-fn* @print-fn]
             (cljs.core/newline))))

#?(:cljd
   (defn pr-str
     [& xs]
     (let [sw (StringBuffer.)]
       (vars/with-bindings {out sw}
         (apply pr xs))
       (str sw)))
   :clj
   (defn pr-str
     "pr to a string, returning it"
     [& xs]
     (let [sw (java.io.StringWriter.)]
       (vars/with-bindings {out sw}
         (apply pr xs))
       (str sw)))
   :cljs
   (defn pr-str
     "pr to a string, returning it"
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta
               *print-namespace-maps* @print-namespace-maps
               *print-readably* @print-readably
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/pr-str objs))))

#?(:cljd
   (defn prn
     [& more]
     (apply pr more)
     (newline)
     (when @flush-on-newline
       (flush)))
   :clj
   (defn prn
     [& more]
     (apply pr more)
     (newline)
     (when @flush-on-newline
       (flush)))
   :cljs
   (defn prn
     [& objs]
     (binding [*print-fn* @print-fn
               *print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta
               *print-namespace-maps* @print-namespace-maps
               *print-readably* @print-readably
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/prn objs))))

#?(:cljd
   (defn prn-str
     [& xs]
     (let [sw (StringBuffer.)]
       (vars/with-bindings {out sw}
         (apply prn xs))
       (str sw)))
   :clj
   (defn prn-str
     "prn to a string, returning it"
     [& xs]
     (let [sw (java.io.StringWriter.)]
       (vars/with-bindings {out sw}
         (apply prn xs))
       (str sw)))
   :cljs
   (defn prn-str
     "prn to a string, returning it"
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta
               *print-namespace-maps* @print-namespace-maps
               *print-readably* @print-readably
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/prn-str objs))))

#?(:cljd
   (defn print
     [& more]
     (vars/with-bindings {print-readably nil}
       (apply pr more)))
   :clj
   (defn print
     [& more]
     (vars/with-bindings {print-readably nil}
       (apply pr more)))
   :cljs
   (defn print
     [& objs]
     (binding [*print-fn* @print-fn
               *print-length* @print-length
               *print-level* @print-level
               *print-namespace-maps* @print-namespace-maps
               *print-readably* nil
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/print objs))))

#?(:cljd
   (defn print-str
     [& xs]
     (let [sw (StringBuffer.)]
       (vars/with-bindings {out sw}
         (apply print xs))
       (str sw)))
   :clj
   (defn print-str
     "print to a string, returning it"
     [& xs]
     (let [sw (java.io.StringWriter.)]
       (vars/with-bindings {out sw}
         (apply print xs))
       (str sw)))
   :cljs
   (defn print-str
     "print to a string, returning it"
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta
               *print-namespace-maps* @print-namespace-maps
               *print-readably* @print-readably
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/print-str objs))))

#?(:cljd
   (defn println
     [& more]
     (vars/with-bindings {print-readably nil}
       (apply prn more)))
   :clj
   (defn println
     [& more]
     (vars/with-bindings {print-readably nil}
       (apply prn more)))
   :cljs
   (defn println
     [& objs]
     (binding [*print-fn* @print-fn
               *print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta
               *print-namespace-maps* @print-namespace-maps
               *print-readably* @print-readably
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/println objs))))

#?(:cljd
   (defn println-str
     [& xs]
     (let [sw (StringBuffer.)]
       (vars/with-bindings {out sw}
         (apply println xs))
       (str sw)))
   :clj
   (defn println-str
     "println to a string, returning it"
     [& xs]
     (let [sw (java.io.StringWriter.)]
       (vars/with-bindings {out sw}
         (apply println xs))
       (str sw)))
   :cljs
   (defn println-str
     "println to a string, returning it"
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta
               *print-namespace-maps* @print-namespace-maps
               *print-readably* @print-readably
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/println-str objs))))

#?(:clj
   (defn printf
     [fmt & args]
     (print (apply format fmt args))))

(defn with-out-str
  [_ _ & body]
  `(let [s# (new #?(:cljd ~'StringBuffer
                    :clj java.io.StringWriter
                    :cljs goog.string.StringBuffer))]
     #?(:cljd
        (binding [*out* s#]
          ~@body
          (str s#))
        :clj
        (binding [*out* s#]
          ~@body
          (str s#))
        :cljs
        (binding [*print-newline* true
                  *print-fn* (fn [x#]
                               (. s# ~utils/allowed-append x#))]
          ~@body
          (str s#)))))

#?(:clj
   (defn with-in-str
     [_ _ s & body]
     `(with-open [s# (-> (java.io.StringReader. ~s) clojure.lang.LineNumberingPushbackReader.)]
        (binding [*in* s#]
          ~@body))))

#?(:clj
   (defn read-line
     []
     (if (instance? clojure.lang.LineNumberingPushbackReader @in)
       (.readLine ^clojure.lang.LineNumberingPushbackReader @in)
       (.readLine ^java.io.BufferedReader @in))))

#?(:clj
   (defn print-simple [o w]
     (binding [*print-dup* @print-dup-var
               *print-meta* @print-meta
               *print-readably* @print-readably]
       (clojure.core/print-simple o w))))
