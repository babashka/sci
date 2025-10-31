(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn pr-str prn-str print print-str println println-str
                            newline flush with-out-str with-in-str read-line
                            printf #?@(:cljs [string-print])
                            #?@(:cljs [] :default [print-simple])])
  (:require
   #?(:cljs [goog.string])
   [sci.impl.copy-vars :refer [copy-var]]
   [sci.impl.records]
   [sci.impl.unrestrict :refer [*unrestricted*]]
   [sci.impl.utils :as utils]
   [sci.impl.vars :as vars]))

#?(:cljs nil :default (set! *warn-on-reflection* true))

(defn core-dynamic-var
  "create a dynamic var with clojure.core :ns meta"
  ([name] (core-dynamic-var name nil))
  ([name init-val] (utils/dynamic-var name init-val {:ns utils/clojure-core-ns
                                                     :sci/built-in true}))
  ([name init-val extra-meta] (utils/dynamic-var name init-val
                                                 (assoc extra-meta :ns utils/clojure-core-ns
                                                        :sci/built-in true))))

(def in (binding [*unrestricted* true]
          (doto (core-dynamic-var '*in*)
            (vars/unbind)
            #?(:clj (alter-meta! assoc
                                 :doc "A java.io.Reader object representing standard input for read operations.")
               :cljr (alter-meta! assoc
                                  :doc "A System.IO.TextReader object representing standard input for read operations.")))))

(def out (binding [*unrestricted* true]
           (doto (core-dynamic-var '*out*)
             (vars/unbind)
             #?(:clj (alter-meta! assoc :doc "A java.io.Writer object representing standard output for print operations.")
                :cljr (alter-meta! assoc :doc "A System.IO.TextWriter object representing standard output for print operations.")))))

(def err (binding [*unrestricted* true]
           (doto (core-dynamic-var '*err*)
             (vars/unbind)
             #?(:clj (alter-meta! assoc :doc " A java.io.Writer object representing standard error for print operations.")
                :cljr (alter-meta! assoc :doc " A System.IO.TextWriter object representing standard error for print operations.")))))

#?(:cljs
   (def print-fn
     (binding [*unrestricted* true]
       (doto (core-dynamic-var '*print-fn*)
         (vars/unbind)))))

#?(:cljs
   (def print-err-fn
     (binding [*unrestricted* true]
       (doto (core-dynamic-var '*print-err-fn*)
         (vars/unbind)))))

;; TODO: CLJS print-fn-bodies

(def print-meta (copy-var *print-meta* utils/clojure-core-ns {:dynamic true}))
(def print-length (copy-var *print-length* utils/clojure-core-ns {:dynamic true}))
(def print-level (copy-var *print-level* utils/clojure-core-ns {:dynamic true}))
(def print-namespace-maps (copy-var *print-namespace-maps* utils/clojure-core-ns {:dynamic true :init true}))
(def flush-on-newline (copy-var *flush-on-newline* utils/clojure-core-ns {:dynamic true}))
(def print-readably (copy-var *print-readably* utils/clojure-core-ns {:dynamic true}))
(def print-dup-var (copy-var *print-dup* utils/clojure-core-ns {:dynamic true}))
#?(:cljs (def print-newline (copy-var *print-newline* utils/clojure-core-ns {:dynamic true})))

#?(:cljs (defn string-print [x]
           (binding [*print-fn* @print-fn]
             (cljs.core/string-print x))) )

#?(:cljs nil :default
   (defn pr-on
     {:private true
      :static true}
     [x w]
     (if *print-dup*
       (print-dup x w)
       (print-method x w))
     nil))

#?(:cljs (defn pr
           [& objs]
           (binding [*print-fn* @print-fn
                     *print-length* @print-length
                     *print-level* @print-level
                     *print-meta* @print-meta
                     *print-namespace-maps* @print-namespace-maps
                     *print-readably* @print-readably
                     *print-newline* @print-newline
                     *print-dup* @print-dup-var]
             (apply cljs.core/pr objs)))
   :default (defn pr
              ([] nil)
              ([x]
               (binding [*print-length* @print-length
                         *print-level* @print-level
                         *print-meta* @print-meta
                         *print-namespace-maps* @print-namespace-maps
                         *print-readably* @print-readably
                         *print-dup* @print-dup-var]
                 (pr-on x @out)))
              ([x & more]
               (pr x)
               (. #?(:clj ^java.io.Writer @out
                     :cljr ^System.IO.TextWriter @out)
                  (append \space))
               (if-let [nmore (next more)]
                 (recur (first more) nmore)
                 (apply pr more)))))

#?(:cljs (defn flush [] ;stub
           nil)
   :default
   (defn flush
     []
     (. #?(:clj ^java.io.Writer @out
           :cljr ^System.IO.TextWriter @out)
        (flush))
     nil))

#?(:cljs (declare println))

#?(:cljs (defn newline
           []
           (binding [*print-fn* @print-fn]
             (cljs.core/newline)))
   :default (defn newline
              []
              (. #?(:clj ^java.io.Writer @out
                    :cljr ^System.IO.TextWriter @out)
                 (append ^String @#'clojure.core/system-newline))
              nil))

#?(:cljs
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
       (apply cljs.core/pr-str objs)))
   :default
   (defn pr-str
     "pr to a string, returning it"
     [& xs]
     (let [sw (#?(:clj java.io.StringWriter. :cljr System.IO.StringWriter.))]
       (vars/with-bindings {out sw}
         (apply pr xs))
       (str sw))))

#?(:cljs
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
       (apply cljs.core/prn objs)))
   :default
   (defn prn
     [& more]
     (apply pr more)
     (newline)
     (when @flush-on-newline
       (flush))))

#?(:cljs
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
       (apply cljs.core/prn-str objs)))
   :default
   (defn prn-str
     "prn to a string, returning it"
     [& xs]
     (let [sw (#?(:clj java.io.StringWriter. :cljr System.IO.StringWriter.))]
       (vars/with-bindings {out sw}
         (apply prn xs))
       (str sw))))

#?(:cljs
   (defn print
     [& objs]
     (binding [*print-fn* @print-fn
               *print-length* @print-length
               *print-level* @print-level
               *print-namespace-maps* @print-namespace-maps
               *print-readably* nil
               *print-newline* @print-newline
               *print-dup* @print-dup-var]
       (apply cljs.core/print objs)))
   :default
   (defn print
     [& more]
     (vars/with-bindings {print-readably nil}
       (apply pr more))))

#?(:cljs
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
       (apply cljs.core/print-str objs)))
   :clj
   (defn print-str
     "print to a string, returning it"
     [& xs]
     (let [sw (#?(:clj java.io.StringWriter. :cljr System.IO.StringWriter.))]
       (vars/with-bindings {out sw}
         (apply print xs))
       (str sw))))

#?(:cljs
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
       (apply cljs.core/println objs)))
   :default
   (defn println
     [& more]
     (vars/with-bindings {print-readably nil}
       (apply prn more))))

#?(:cljs
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
       (apply cljs.core/println-str objs)))
   :default
   (defn println-str
     "println to a string, returning it"
     [& xs]
     (let [sw (#?(:clj java.io.StringWriter. :cljr System.IO.StringWriter.))]
       (vars/with-bindings {out sw}
         (apply println xs))
       (str sw))))

#?(:cljs nil :default
   (defn printf
     [fmt & args]
     (print (apply format fmt args))))

(defn with-out-str
  [_ _ & body]
  `(let [s# (new #?(:clj java.io.StringWriter
                    :cljs goog.string.StringBuffer
                    :cljr System.IO.StringWriter))]
     #?(:cljs
        (binding [*print-newline* true
                  *print-fn* (fn [x#]
                               (. s# ~utils/allowed-append x#))]
          ~@body
          (str s#))
        :default
        (binding [*out* s#]
          ~@body
          (str s#)))))

#?(:cljs nil :default
   (defn with-in-str
     [_ _ s & body]
     `(with-open [s# #?(:clj (-> (java.io.StringReader. ~s) clojure.lang.LineNumberingPushbackReader.)
                        :cljr (System.IO.StringReader. ~s))]
        (binding [*in* s#]
          ~@body))))

#?(:clj
   (defn read-line
     []
     (if (instance? clojure.lang.LineNumberingPushbackReader @in)
       (.readLine ^clojure.lang.LineNumberingPushbackReader @in)
       (.readLine ^java.io.BufferedReader @in)))
   :cljr
   (defn read-line []
     (System.IO.TextReader/.ReadLine @in)))

#?(:cljs nil :default
   (defn print-simple [o w]
     (binding [*print-dup* @print-dup-var
               *print-meta* @print-meta
               *print-readably* @print-readably]
       (clojure.core/print-simple o w))))
