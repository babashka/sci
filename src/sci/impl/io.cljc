(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn pr-str prn-str print print-str println
                            newline flush with-out-str with-in-str read-line
                            printf #?@(:cljs [string-print])])
  (:require #?(:cljs [goog.string])
            [sci.impl.unrestrict :refer [*unrestricted*]]
            #?(:cljs [sci.impl.utils :as utils])
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(defn core-dynamic-var
  "create a dynamic var with clojure.core :ns meta"
  ([name] (core-dynamic-var name nil))
  ([name init-val] (vars/dynamic-var name init-val {:ns vars/clojure-core-ns})))

(def in (binding [*unrestricted* true]
          (doto (core-dynamic-var '*in*)
            (vars/unbind))))

(def out (binding [*unrestricted* true]
           (doto (core-dynamic-var '*out*)
             (vars/unbind))))

(def err (binding [*unrestricted* true]
           (doto (core-dynamic-var '*err*)
             (vars/unbind))))

#?(:cljs
   (def print-fn
     (binding [*unrestricted* true]
       (doto (core-dynamic-var '*print-fn*)
         (vars/unbind)))))

;; TODO: CLJS print-err-fn
;; TODO: CLJS print-fn-bodies

(def print-meta
  (core-dynamic-var '*print-meta* false))

(def print-length (core-dynamic-var '*print-length*))
(def print-level (core-dynamic-var '*print-level*))
(def print-namespace-maps (core-dynamic-var '*print-namespace-maps* true))
(def flush-on-newline (core-dynamic-var '*flush-on-newline* *flush-on-newline*))
(def print-readably (core-dynamic-var '*print-readably* *print-readably*))
#?(:cljs (def print-newline (core-dynamic-var '*print-newline* *print-newline*)))

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

#?(:clj (defn pr
          ([] nil)
          ([x]
           (binding [*print-length* @print-length
                     *print-level* @print-level
                     *print-meta* @print-meta
                     *print-namespace-maps* @print-namespace-maps
                     *print-readably* @print-readably]
             (pr-on x @out)))
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
                     *print-newline* @print-newline]
             (apply cljs.core/pr objs))))

#?(:clj
   (defn flush
     []
     (. ^java.io.Writer @out (flush))
     nil)
   :cljs (defn flush [] ;stub
           nil))

#?(:cljs (declare println))

#?(:clj (defn newline
          []
          (. ^java.io.Writer @out (append ^String @#'clojure.core/system-newline))
          nil)
   :cljs (defn newline
           []
           (binding [*print-fn* @print-fn]
             (cljs.core/newline))))

#?(:clj
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
               *print-newline* @print-newline]
       (apply cljs.core/pr-str objs))))

#?(:clj
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
               *print-newline* @print-newline]
       (apply cljs.core/prn objs))))

#?(:clj
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
               *print-newline* @print-newline]
       (apply cljs.core/prn-str objs))))

#?(:clj
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
               *print-newline* @print-newline]
       (apply cljs.core/print objs))))

#?(:clj
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
               *print-newline* @print-newline]
       (apply cljs.core/print-str objs))))

#?(:clj
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
               *print-newline* @print-newline]
       (apply cljs.core/println objs))))

#?(:clj
   (defn printf
     [fmt & args]
     (print (apply format fmt args))))

(defn with-out-str
  [_ _ & body]
  `(let [s# (new #?(:clj java.io.StringWriter
                   :cljs goog.string.StringBuffer))]
     #?(:clj
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
