(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn print println newline flush
                            with-out-str with-in-str read-line])
  (:require [sci.impl.vars :as vars]
            #?(:clj [sci.impl.io :as sio])))

#?(:clj (set! *warn-on-reflection* true))

(def in (vars/dynamic-var '*in* #?(:clj *in*)))

(def out (vars/dynamic-var '*out* *out*))

(def err (vars/dynamic-var '*err* #?(:clj *err*)))

#?(:clj (defn pr-on
          {:private true
           :static true}
          [x w]
          (if *print-dup*
            (print-dup x w)
            (print-method x w))
          nil))

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
             (apply pr more)))))

#?(:clj (defn newline
          "Writes a platform-specific newline to *out*"
          {:added "1.0"
           :static true}
          []
          (. ^java.io.Writer @out (append ^String @#'clojure.core/system-newline))
          nil))
#?(:clj
   (defn flush
     "Flushes the output stream that is the current value of
  *out*"
     {:added "1.0"
      :static true}
     []
     (. ^java.io.Writer @out (flush))
     nil))

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
       (apply prn more))))

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
