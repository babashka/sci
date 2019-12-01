(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn print println newline flush
                            with-out-str with-in-str read-line
                            #?@(:cljs [string-print])])
  (:require [sci.impl.vars :as vars]
            #?(:clj [sci.impl.io :as sio])
            #?(:cljs [goog.string])))

#?(:clj (set! *warn-on-reflection* true))

(def in (vars/dynamic-var '*in* #?(:clj (-> (java.io.StringReader. "")
                                            clojure.lang.LineNumberingPushbackReader.)
                                   )))

(def out (vars/dynamic-var '*out* #?(:clj (java.io.StringWriter.)
                                     :cljs (goog.string/StringBuffer.))))

(def err (vars/dynamic-var '*err* #?(:clj (java.io.StringWriter.))))

#?(:cljs
   (defn string-print [x]
     (when (nil? *print-fn*)
       (throw (js/Error. "No *print-fn* fn set for evaluation environment")))
     (*print-fn* x)
     nil))

#?(:clj (defn pr-on
          {:private true
           :static true}
          [x w]
          (if *print-dup*
            (print-dup x w)
            (print-method x w))
          nil)
   :cljs (defn- pr-with-opts
           "Prints a sequence of objects using string-print, observing all
  the options given in opts"
           [objs opts]
           (string-print (pr-str-with-opts objs opts))))

#?(:clj (defn pr
          "Prints the object(s) to the output stream that is the current value
  of *out*.  Prints the object(s), separated by spaces if there is
  more than one.  By default, pr and prn print in a way that objects
  can be read by the reader"
          ([] nil)
          ([x]
           (pr-on x @out))
          ([x & more]
           (pr x)
           (. ^java.io.Writer @out (append \space))
           (if-let [nmore (next more)]
             (recur (first more) nmore)
             (apply pr more))))
   :cljs (defn pr
           "Prints the object(s) using string-print.  Prints the
  object(s), separated by spaces if there is more than one.
  By default, pr and prn print in a way that objects can be
  read by the reader"
           [& objs]
           (pr-with-opts objs (pr-opts))))

#?(:clj
   (defn flush
     "Flushes the output stream that is the current value of
  *out*"
     {:added "1.0"
      :static true}
     []
     (. ^java.io.Writer @out (flush))
     nil)
   :cljs (defn flush [] ;stub
           nil))

#?(:clj (defn newline
          "Writes a platform-specific newline to *out*"
          {:added "1.0"
           :static true}
          []
          (. ^java.io.Writer @out (append ^String @#'clojure.core/system-newline))
          nil)
   :cljs (defn newline
           "Prints a newline using *print-fn*"
           ([] (newline nil))
           ([opts]
            (string-print "\n")
            (when (get opts :flush-on-newline)
              (flush)))))

#?(:clj
   (defn prn
     "Same as pr followed by (newline). Observes *flush-on-newline*"
     {:added "1.0"
      :static true}
     [& more]
     ;; (clojure.core/prn "more" more)
     (apply pr more)
     (newline)
     (when *flush-on-newline*
       (flush))))
#?(:clj
   (defn print
     "Prints the object(s) to the output stream that is the current value
  of *out*.  print and println produce output for human consumption."
     {:added "1.0"
      :static true}
     [& more]
     (binding [*print-readably* nil]
       (apply pr more))))

#?(:clj
   (defn println
     "Same as print followed by (newline)"
     {:added "1.0"
      :static true}
     [& more]
     (binding [*print-readably* nil]
       (apply prn more)))
   :cljs
   (defn println
     "Same as print followed by (newline)"
     [& objs]
     (.append @out (apply println-str objs))))

#?(:clj
   (defn with-out-str
     [_ _ & body]
     `(let [s# (new java.io.StringWriter)]
        (binding [*out* s#]
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
     "Reads the next line from stream that is the current value of *in* ."
     {:added "1.0"
      :static true}
     []
     (if (instance? clojure.lang.LineNumberingPushbackReader @in)
       (.readLine ^clojure.lang.LineNumberingPushbackReader @in)
       (.readLine ^java.io.BufferedReader @in))))

#?(:clj
   (defmacro with-sci-out-str
     "For external use. Useful for testing sci programs."
     [& body]
     `(let [sw# (java.io.StringWriter.)
            _# (try (vars/push-thread-bindings {sio/out sw#})
                    (do ~@body)
                    (finally (vars/pop-thread-bindings)))
            out# (str sw#)]
        out#)))

#?(:clj
   (defmacro with-sci-in-str
     "For external use. Useful for testing sci programs."
     [s & body]
     `(with-open [s# (-> (java.io.StringReader. ~s) clojure.lang.LineNumberingPushbackReader.)]
        (try (vars/push-thread-bindings {sio/in s#})
             (do ~@body)
             (finally (vars/pop-thread-bindings))))))
