(require '[sci.core :as sci])
(sci/eval-string "(defprotocol IFoo) nil (prn @#'defrecord) (defrecord Dude [] IFoo)"
                 {:bindings {'prn prn}})

