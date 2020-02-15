(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn print println newline flush with-out-str
                            with-in-str read-line printf
                            #?@(:cljs [string-print])])
  (:require #?(:clj [sci.impl.io :as sio])
            #?(:cljs [goog.string])
            [sci.impl.unrestrict :refer [*unrestricted*]]
            [sci.impl.vars :as vars]))

#?(:clj (set! *warn-on-reflection* true))

(def init-in (fn []
               #?(:clj (-> (java.io.StringReader. "")
                           clojure.lang.LineNumberingPushbackReader.))))

(def init-out (fn [] (new #?(:clj java.io.StringWriter
                             :cljs goog.string/StringBuffer))))

(def init-err (fn [] #?(:clj (java.io.StringWriter.))))

(def in (binding [*unrestricted* true]
          (doto (vars/dynamic-var '*in*)
                                        (vars/unbind))))

(def out (binding [*unrestricted* true]
           (doto (vars/dynamic-var '*out*)
             (vars/unbind))))

(def err (binding [*unrestricted* true]
           (doto (vars/dynamic-var '*err*)
             (vars/unbind))))

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
           (pr-on x @out))
          ([x & more]
           (pr x)
           (. ^java.io.Writer @out (append \space))
           (if-let [nmore (next more)]
             (recur (first more) nmore)
             (apply pr more))))
   :cljs (defn pr
           [& objs]
           (.append @out (apply pr-str objs))))

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
   (defn prn
     [& more]
     (apply pr more)
     (newline)
     (when *flush-on-newline*
       (flush)))
   :cljs
   (defn prn
     [& objs]
     (.append @out (apply prn-str objs))))

#?(:clj
   (defn print
     [& more]
     (binding [*print-readably* nil]
       (apply pr more)))
   :cljs
   (defn print
     [& objs]
     (.append @out (apply print-str objs))))

#?(:clj
   (defn println
     [& more]
     (binding [*print-readably* nil]
       (apply prn more)))
   :cljs
   (defn println
     [& objs]
     (.append @out (apply println-str objs))))

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
