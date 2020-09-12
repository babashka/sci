(ns sci.examples.repl
  (:require [sci.core :as sci]))

(let [;; we are going to read Clojure expressions from stdin
      reader (sci/reader *in*)
      ctx (sci/init {})]
  ;; establish a thread-local binding for sci/ns to allow namespace switches
  (sci/with-bindings {sci/ns @sci/ns}
    (loop []
      ;; fetch the current namespace name for printig a prompt
      (let [ns-name (sci/eval-string* ctx "(ns-name *ns*)")]
        (print (str ns-name "> "))
        (flush))
      (let [;; read the next form from stdin
            next-form (sci/parse-next ctx reader)]
        ;; if we did not reach end of file (the user pressed ctrl-d)
        (when-not (= ::sci/eof next-form)
          ;; eval the form and print the result
          (prn (sci/eval-form ctx next-form))
          ;; repeat!
          (recur))))))

;; user> (+ 1 2 3)
;; 6
;; user> (ns foo)
;; nil
;; foo> (defn my-fn [x] (inc x))
;; #'foo/my-fn
;; foo> (my-fn 10)
;; 11
;; foo> ^D

