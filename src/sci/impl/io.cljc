(ns sci.impl.io
  {:no-doc true}
  (:refer-clojure :exclude [pr prn print println newline flush])
  (:require [sci.impl.vars :as vars]))

(def in (vars/dynamic-var '*in* #?(:clj *in*)))

(def out (vars/dynamic-var '*in* *out*))

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
           (. @out (append \space))
           (if-let [nmore (next more)]
             (recur (first more) nmore)
             (apply pr more)))))

#?(:clj (defn newline
          "Writes a platform-specific newline to *out*"
          {:added "1.0"
           :static true}
          []
          (. @out (append @#'clojure.core/system-newline))
          nil))
#?(:clj
   (defn flush
     "Flushes the output stream that is the current value of
  *out*"
     {:added "1.0"
      :static true}
     []
     (. @out (flush))
     nil))

#?(:clj
   (defn prn
     "Same as pr followed by (newline). Observes *flush-on-newline*"
     {:added "1.0"
      :static true}
     [& more]
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
