(ns sci.protocol-lib
  "Test fixture: a user namespace defining protocols, copied with copy-ns.")

(defprotocol IShout
  (shout [this])
  (shout2 [this n]))

(defprotocol IMarker)

(defn shout-loudly [x]
  (shout x))
