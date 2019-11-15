(ns sci.impl.exceptions
  {:no-doc true})

#?(:clj
   (defn create-jvm-exception
     ([] (Exception.))
     ([msg-or-cause] (if (string? msg-or-cause)
                       (Exception. ^String msg-or-cause)
                       (Exception. ^Throwable msg-or-cause)))
     ([^String msg ^Throwable cause] (Exception. msg cause))))

#?(:clj
   (def java-exception-bindings
     {;; 'Exception Exception
      ;; 'Exception. create-jvm-exception
      ;; 'java.lang.Exception Exception
      'java.lang.Exception. create-jvm-exception
      'ArithmeticException ArithmeticException
      'java.lang.ArithMeticException ArithmeticException}))

#?(:cljs
   (defn create-js-error [msg]
     (js/Error. msg)))

#?(:cljs
   (def js-exception-bindings
     {'js/Error js/Error
      'js/Error. js/Error.}))

(def exception-bindings
  #?(:clj java-exception-bindings
     :cljs js-exception-bindings))
