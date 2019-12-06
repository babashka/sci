(ns sci.addons
  (:refer-clojure :exclude [future]))

#?(:clj
   (def future* ^:sci/macro
     (fn [_ _ & body]
       `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
          (~'future-call f#)))))

#?(:clj
   (def future
     {:namespaces
      {'clojure.core
       {'future future*
        'future-call future-call}}}))
