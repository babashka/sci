# Instance and static member control

SCI allows you to control instance and static methods and field overrides.
You can use this for sandboxing reasons or to fix e.g. performance problems since the default interop code in SCI uses reflection.
The `:classes` config allows you to set a list of definitions to `:closed` such that you can't access anything else on a specific class. Without `:closed`, the configs act as overrides.

## A complete example

The following config shows all the available options to control interop.
One class has the `:closed` option which means that the config options replace all the available interop on the class.

```clojure
{:classes
 {;; A closed class: only the members listed here are accessible, any other
  ;; method or field access throws "not allowed"
  'java.io.File
  {:class java.io.File
   :closed true
   :instance-methods {'getName true                    ;; allowed, via reflection
                      'getPath (fn [file] :redacted)}   ;; overridden
   :static-methods   {'createTempFile true}
   :static-fields    {'separator true
                      'pathSeparator (fn [_class] ":")}}

  ;; Without `:closed`, listed members are overridden, everything
  ;; else falls back to normal (reflective) interop
  'java.awt.Point
  {:class java.awt.Point
   :instance-methods {'toString (fn [point] :custom-tostring)}
   :instance-fields  {'x (fn [point] :hidden-x)}}}}     ;; overrides the public field x
```

In the above config you can see that overrides can be either functions or a `true` value. The latter means that the class is allowed but treated via the built-in interop code that SCI itself has.

Note that `:closed` may appear on the class config globally or on an individual member section.

```clojure
{:classes
 {'java.lang.Integer
  {:class java.lang.Integer
   ;; :closed on a member section: only specific static methods are allowed
   :static-methods   {:closed true
                      'parseInt true}
   ;; other sections without :closed remain overrides
   :instance-methods {'toString (fn [i] :custom)}}}}
```

## Interaction with `:allow :all`

A per-class member config takes precedence ove `:allow :all` when it is `:closed`.

```clojure
(sci/eval-string
 "(.getPath (java.io.File. \"x\"))"
 {:classes {:allow :all
            'java.io.File
            {:class java.io.File
             :closed true
             :instance-methods {'getName true}}}})
;;=> throws: Method getPath on class java.io.File not allowed!
```

## Config changes on a live context

For performance, member analysis is done at analysis time as much as possible. If you tighten class configurations later with `sci/merge-opts`, functions already produced will bypass the new restrictions. Only code analyzed after the `merge-opts` sees the tighter config. Set the full config at `sci/init` before evaluating untrusted code and the normal sandbox flow is unaffected.

```clojure
(def ctx (sci/init {:classes {:allow :all 'java.lang.String {:class String}}}))
(sci/eval-string* ctx "(defn f [] (.length \"abc\"))")   ;; analyzed while String open
(sci/eval-string* ctx "(f)")                              ;; => 3
(sci/merge-opts ctx {:classes {'java.lang.String {:class String :closed true}}})
(sci/eval-string* ctx "(f)")                              ;; still 3: f's node was analyzed before :closed
(sci/eval-string* ctx "(.length \"abc\")")               ;; throws: newly analyzed, sees :closed
```
