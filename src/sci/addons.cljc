(ns sci.addons
  {:no-doc true}
  (:refer-clojure :exclude [future]))

#?(:clj
   (def future* ^:sci/macro
     (fn [_ _ & body]
       `(let [f# (~'binding-conveyor-fn (fn [] ~@body))]
          (~'future-call f#)))))

#?(:clj
   (defn future [opts]
     (update-in opts [:namespaces 'clojure.core]
                assoc
                'future future*
                'future-call future-call
                'future-cancel future-cancel
                'future-cancelled? future-cancelled?
                'future-done? future-done?
                'future? future?)))
