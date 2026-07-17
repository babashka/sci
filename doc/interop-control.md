# Instance and static member control

SCI allows you to control instance and static methods and field overrides.
You can use this for sandboxing reasons or to fix e.g. performance problems since the default interop code in SCI uses reflection.
The `:classes` config allows you to set a list of definitions to `:closed` such that you can't access anything else on a specific class. Without `:closed`, the configs act as overrides.

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

## ClojureScript

On CLJS the member configs are only partially honored:

- In a sandboxed context, `:instance-methods`, `:instance-fields` and
  `:closed` work as on the JVM.
- With `:unrestricted true`, instance interop skips the `:classes` config
  entirely, including overrides and `:closed`.
- `:static-methods` and `:static-fields` overrides are never consulted on
  CLJS.

To intercept or replace a member on CLJS, register a patched object in
`:classes` instead:

``` clojure
(sci/eval-string "(Math/abs -1)"
  {:classes {'Math #js {:abs (fn [x] :intercepted)
                        :floor js/Math.floor}}})
;;=> :intercepted
```

This also works in unrestricted contexts. The member is resolved when the
code is analyzed, so register the patched object before evaluating code
that uses it.
