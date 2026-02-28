Answer the question: why do we need to support symbol on custom types in SCI? I don't remember why we added the tests. In Clojure:

user=> (deftype Dude [])
user.Dude
user=> (symbol Dude)
Execution error (IllegalArgumentException) at user/eval143 (REPL:1).
no conversion to symbol


