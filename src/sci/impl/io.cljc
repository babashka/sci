(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn pr-str prn-str print print-str println
                            newline flush with-out-str with-in-str read-line
                            printf #?@(:cljs [string-print])])
  (:require #?(:clj [sci.impl.io :as sio])
            #?(:cljs [goog.string])
            [sci.impl.unrestrict :refer [*unrestricted*]]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(def in (binding [*unrestricted* true]
          (doto (vars/dynamic-var '*in*)
                                        (vars/unbind))))

(def out (binding [*unrestricted* true]
           (doto (vars/dynamic-var '*out*)
             (vars/unbind))))

(def err (binding [*unrestricted* true]
           (doto (vars/dynamic-var '*err*)
             (vars/unbind))))

(def print-meta
  (vars/dynamic-var '*print-meta* false))

(def print-length (vars/dynamic-var '*print-length* nil))

(def print-level (vars/dynamic-var '*print-level* nil))

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
                     *print-meta* @print-meta]
             (pr-on x @out)))
          ([x & more]
           (pr x)
           (. ^java.io.Writer @out (append \space))
           (if-let [nmore (next more)]
             (recur (first more) nmore)
             (apply pr more))))
   :cljs (defn pr
           [& objs]
           (binding [*print-length* @print-length
                     *print-level* @print-level
                     *print-meta* @print-meta]
             (.append @out (apply cljs.core/pr-str objs)))))

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
           (println)))

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
               *print-meta* @print-meta]
       (apply cljs.core/pr-str objs))))

#?(:clj
   (defn prn
     [& more]
     (apply pr more)
     (newline)
     (when *flush-on-newline*
       (flush)))
   :cljs
   (defn prn
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta]
       (.append @out (apply cljs.core/prn-str objs)))))

#?(:clj
   (defn prn-str
     "pr to a string, returning it"
     [& xs]
     (let [sw (java.io.StringWriter.)]
       (vars/with-bindings {out sw}
         (apply prn xs))
       (str sw)))
   :cljs
   (defn prn-str
     "pr to a string, returning it"
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta]
       (apply cljs.core/prn-str objs))))

#?(:clj
   (defn print
     [& more]
     (binding [*print-readably* nil]
       (apply pr more)))
   :cljs
   (defn print
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level]
       (.append @out (apply cljs.core/print-str objs)))))

#?(:clj
   (defn print-str
     "pr to a string, returning it"
     [& xs]
     (let [sw (java.io.StringWriter.)]
       (vars/with-bindings {out sw}
         (apply print xs))
       (str sw)))
   :cljs
   (defn print-str
     "pr to a string, returning it"
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta]
       (apply cljs.core/print-str objs))))

#?(:clj
   (defn println
     [& more]
     (binding [*print-readably* nil]
       (apply prn more)))
   :cljs
   (defn println
     [& objs]
     (binding [*print-length* @print-length
               *print-level* @print-level
               *print-meta* @print-meta]
       (.append @out (apply println-str objs)))))

#?(:clj
   (defn printf
     [fmt & args]
     (print (apply format fmt args))))

(defn with-out-str
  [_ _ & body]
  `(let [s# (new #?(:clj java.io.StringWriter
                    :cljs goog.string.StringBuffer))]
     (binding [*out* s#]
       ~@body
       (str s#))))

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
