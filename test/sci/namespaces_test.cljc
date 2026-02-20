(ns sci.namespaces-test
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is testing]]
   [sci.core :as sci]
   [sci.test-utils :as tu]))

(defn eval*
  ([form] (eval* nil form))
  ([binding form]
   (tu/eval* form {:aliases {'str 'clojure.string
                             'set 'clojure.set}
                   :bindings {'*in* binding}})))

(deftest require-test
  (is (= "1-2-3" (eval* "(str/join \"-\" [1 2 3])")))
  (is (= "1-2-3" (eval* "(require '[clojure.string :as string]) (string/join \"-\" [1 2 3])")))
  (is (= "1-2-3" (eval* "(require '[clojure.string :refer [join]]) (join \"-\" [1 2 3])")))
  (is (= "1-2-3" (eval* "(require '[clojure.string :refer :all]) (join \"-\" [1 2 3])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"must be a sequential"
                        (eval* "(require '[clojure.string :refer 1]) (join \"-\" [1 2 3])")))
  (is (= #{1 4 6 3 2 5} (eval* "(set/union #{1 2 3} #{4 5 6})")))
  (is (= #{1 4 6 3 2 5} (eval* "(require '[clojure.set :as s]) (s/union #{1 2 3} #{4 5 6})")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"clojure.foo"
                        (eval* "(require '[clojure.foo :as s])")))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"quux does not exist"
                        (eval* "(require '[clojure.set :refer [quux]])")))
  (is (= [1 2 3] (eval* "(ns foo) (def x 1) (ns bar) (def x 2) (in-ns 'baz) (def x 3) (require 'foo 'bar) [foo/x bar/x x]")))
  (testing
      "Require evaluates arguments"
    (is (= [1 2 3] (eval* "
(ns foo)
(def x 1)

(ns bar)
(def x 2)

(in-ns 'baz)
(def x 3)
(require (symbol \"foo\") (symbol \"bar\"))
[foo/x bar/x x]"))))
  (testing "require as function"
    (is (= 1 (eval* "(ns foo) (defn foo [] 1) (ns bar) (apply require ['[foo :as f]]) (f/foo)"))))
  (testing "rename"
    (is (= #{1 2} (eval* "(require '[clojure.set :refer [union] :rename {union union2}]) (union2 #{1} #{2})")))
    (is (= 16 (eval* "(ns foo (:refer-clojure :rename {bit-shift-left <<})) (<< 8 1)")))
    (is (thrown-with-msg?
         #?(:clj Exception :cljs js/Error)
         #"Unable to resolve.*bit-shift-left"
         (eval* "(ns foo (:refer-clojure :rename {bit-shift-left <<})) (bit-shift-left 8 1)"))))
  (when-not tu/native?
    (testing "load-fn + requiring-resolve"
      (is (= :success
             (tu/eval* "(deref (requiring-resolve 'foo.bar/x))"
                       {:load-fn (fn [{:keys [:namespace]}]
                                   (when (= 'foo.bar namespace)
                                     {:source "(ns foo.bar) (def x :success)"
                                      :file "foo/bar.clj"}))})))))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"already refers to"
       (eval* "
(ns foo (:require [clojure.string :refer [split]]))
(declare split)"))))

(deftest use-test
  (is (= #{1 2} (eval* "(ns foo (:use clojure.set)) (union #{1} #{2})")))
  (is (= #{1 2} (eval* "(use 'clojure.set) (union #{1} #{2})")))
  (is (= #{1 2} (eval* "(use '[clojure.set :only [union]]) (union #{1} #{2})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"Unable to resolve.*union"
       (eval* "(use '[clojure.set :exclude [union]]) (union #{1} #{2})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"Unable to resolve.*union"
       (eval* "(use '[clojure.set :only [difference]]) (union #{1} #{2})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"already refers to"
       (eval* "
(ns foo (:use clojure.string))
(declare split)"))))

(deftest misc-namespace-test
  (is (= 1 (eval* "(alias (symbol \"c\") (symbol \"clojure.core\")) (c/and true 1)")))
  (is (= #{1 3 2} (eval* "(mapv alias ['set1 'set2] ['clojure.set 'clojure.set]) (set2/difference
(set1/union #{1 2 3} #{4 5 6}) #{4 5 6})")))
  (is (= 'clojure.set (eval* "(ns-name (find-ns 'clojure.set))")))
  (is (= 'clojure.set (eval* "(ns-name (the-ns (the-ns 'clojure.set)))")))
  (is (= 'clojure.core (eval* "(alias 'c 'clojure.core) (ns-name (get (ns-aliases *ns*) 'c))")))
  (is (contains? (set (eval* "(clojure.repl/dir-fn 'clojure.string)"))
                 'last-index-of))
  (is (true? (eval* "(def foo-ns (create-ns 'foo)) (def another-foo-ns (create-ns 'foo))
                     (and (identical? foo-ns another-foo-ns)
                     (= 'foo (ns-name foo-ns)))"))))

(deftest autoresolve-test
  (is (= :user/foo (eval* "::foo")))
  (is (= :bar/foo (eval* "(in-ns 'bar) ::foo")))
  (is (= :clojure.string/foo (eval* "::str/foo")))
  (is (= :clojure.set/foo (eval* "(require '[clojure.set :as str]) ::str/foo")))
  (is (= :clojure.string/foo (eval* "(in-ns 'foo) (require '[clojure.string :as str]) ::str/foo")))
  (is (= :clojure.string/foo (eval* "(ns foo (:require [clojure.string :as s])) ::s/foo"))))

(deftest in-ns-test
  (is (= :user/foo (eval* "::foo")))
  (is (= :bar/foo (eval* "(in-ns 'bar) ::foo")))
  (is (= :bar/foo (eval* "(in-ns 'bar) (def just-one-ns ::foo) (in-ns 'bar) just-one-ns"))))

(deftest vars-partitioned-by-namespace-test
  (is (= 10 (eval* "(in-ns 'foo) (def x 10) (in-ns 'bar) (def x 11) (in-ns 'foo) x"))))

(deftest ns-form-test
  (is (= #{1} (eval* "(ns foo (:require [clojure.set :as x])) (x/difference #{1 2 3} #{2 3 4})")))
  (is (= #{1} (eval* "(ns foo
                        \"docstring\"
                        {:metadata 1}
                        (:require [clojure.set :refer [difference]])) (difference #{1 2 3} #{2 3 4})")))
  #?(:clj
     (is (= :foo/foo
            (eval* "(ns foo (:import [clojure.lang ExceptionInfo]))
                    (try ::foo (catch ExceptionInfo e nil))")))))

(deftest ns-name-test
  (is (= 'foo (eval* "(ns foo) (ns-name *ns*)"))))

(deftest no-crash-test
  (is (= :foo/foo (eval* "(ns foo \"docstring\") ::foo"))))

(deftest ns-metadata-test
  (is (= {:a 1, :b 1}
         (eval* "(ns ^{:a 1} foo {:b 1}) (meta *ns*)")))
  (is (= {:a 1, :b 1}
         (eval* "(ns ^{:a 1} foo {:b 1}) (meta *ns*) (ns bar) (meta (the-ns 'foo))"))))

(deftest recycle-namespace-objects
  (when-not tu/native?
    (is (empty? (set/difference (eval* "(set (all-ns))") (eval* "(set (all-ns))"))))))

(deftest namespace-doc
  (is (= "foobar" (:doc (eval* "(ns foo \"foobar\") (meta (find-ns 'foo))")))))

(deftest ns-imports-test
  #?@(:clj
      [(is (eval* "(some? (get (ns-imports *ns*) 'String))"))
       (is (eval* "
(import clojure.lang.ExceptionInfo) (some? (get (ns-imports *ns*) 'ExceptionInfo))"))]))

(deftest refer-clojure-exclude
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (eval* "(ns foo (:refer-clojure :exclude [get])) (some? get)")))
  (is (true? (eval* "(ns foo (:refer-clojure :exclude [get])) (defn get []) (some? get)"))))

(deftest refer-test
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (eval* "(refer 'clojure.string :only [join]) includes?")))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (eval* "(refer 'clojure.string :exclude [join]) join")))
  (is (eval* "(refer 'clojure.string :only '[join]) (some? join)"))
  (is (eval* "(refer 'clojure.string) (some? join)"))
  (is (eval* "(defn join []) (refer 'clojure.string) (= 'clojure.string (ns-name (:ns (meta #'join))))"))
  (is (eval* "(defn join []) (refer 'clojure.string) (= 'clojure.string/join `join)")))

(deftest ns-publics-test
  (is (str/includes? (eval* "(defn foo []) (str (ns-publics *ns*))")
                     "foo #'user/foo"))
  (testing "See issue 519, 520, 523"
    (is (eval* "(require '[clojure.string :refer [includes?]]) (nil? (get (ns-publics *ns*) :refer))"))))

(deftest ns-refers-test
  (is (eval* "(some? (get (ns-refers *ns*) 'inc))"))
  (is (eval* "(def x 1) (nil? (get (ns-refers *ns*) 'x))"))
  (is (eval* "(require '[clojure.string :refer [includes?]]) (some? (get (ns-refers *ns*) 'includes?))"))
  (testing "private vars are not referred"
    (is (eval* "(every? (fn [[_ v]] (not (:private (meta v)))) (ns-refers *ns*))"))))

(deftest ns-map-test
  (is (eval* "(some? (get (ns-map *ns*) 'inc))"))
  #?(:clj (is (eval* "(some? (get (ns-map *ns*) 'String))")))
  (is (eval* "(defn- foo []) (some? (get (ns-map *ns*) 'foo))"))
  (testing "ns-map reflects interned vars shadowing refers"
    (is (= :foo (eval* "(defn inc [x] :foo) ((get (ns-map *ns*) 'inc) 1)")))))

(deftest ns-unmap-test
  (is (eval* "(def foo 1) (ns-unmap *ns* 'foo) (nil? (resolve 'foo))"))
  (is (eval* "(defn bar []) (ns-unmap *ns* 'bar) (nil? (resolve 'bar))"))
  (is (eval* "(defn- baz []) (ns-unmap *ns* 'baz) (nil? (resolve 'baz))"))
  (is (eval* "(require '[clojure.string :refer [join]]) (ns-unmap *ns* 'join) (defn join [])"))
  #?(:clj (is (= [false true] (eval* "
(ns-unmap *ns* 'Object)
(def o1 (resolve 'Object))
(import '[java.lang Object])
(def o2 (resolve 'Object))
[(some? o1) (some? o2)]"))))
  (testing "issue 637: config already contain name of referred var in config"
    (let [remote {'cake "cake"}
          opts {:namespaces {'remote remote
                             'user {'cake "init"}}}]
      (is (= [nil "bar"]
             (sci/binding [sci/out *out*]
               (sci/eval-form
                (sci/init opts)
                '(do (require '[remote :refer [cake]])
                     (ns-unmap *ns* 'cake)
                     (def resolved (resolve 'cake))
                     (intern *ns* 'cake "bar")
                     [resolved cake])))))))
  (testing "unmapping Class and then fully qualifying it"
    (is (= 'user/String (eval* "(ns-unmap *ns* 'String) `String ;;=> user/String")))))

(deftest find-var-test
  (is (eval* "(= #'clojure.core/map (find-var 'clojure.core/map))"))
  (is (eval* "(nil? (find-var 'clojure.core/no-such-symbol))"))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                        #"No such namespace: no.such.namespace"
                        (eval* "(find-var 'no.such.namespace/var)")))
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                        #"Not a qualified symbol: no-namespace"
                        (eval* "(find-var 'no-namespace)"))))

(deftest find-ns-test
  (is (true? (eval* "(ns foo) (some? (find-ns 'foo))")))
  (is (nil? (eval* "(find-ns 'foo)"))))

(deftest remove-ns-test
  (is (nil? (eval* "(ns foo) (ns bar) (remove-ns 'foo) (find-ns 'foo)"))))

(deftest ns-unalias-test
  (testing "Removing an alias in an unknown namespace throws"
    (is (thrown? #?(:clj Exception :cljs js/Error) (eval* "(ns-unalias (find-ns 'unknown) 'core)"))))

  (testing "Removing an undefined alias is a no-op"
    (is (nil? (eval* "(ns-unalias *ns* 'core)"))))

  (testing "Removing a defined alias -- ns symbol"
    (is (nil? (eval* "(alias 'core 'clojure.core) (ns-unalias 'user 'core) (get (ns-aliases *ns*) 'core)"))))

  (testing "Removing a defined alias -- ns object"
    (is (nil? (eval* "(alias 'core 'clojure.core) (ns-unalias *ns* 'core) (get (ns-aliases *ns*) 'core)")))))

(deftest ns-syntax-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs :default)
       #"symbol"
       (eval* "(ns 1)"))))

(deftest nested-libspecs-test
  (is (= #{1 2 3 4} (eval* "(require '[clojure [set :refer [union]]]) (union #{1 2 3} #{2 3 4})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"lib names inside prefix lists must not contain periods"
       (eval* "(ns clojure.core.protocols) (ns foo) (require '[clojure [core.protocols]])")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"Unsupported option\(s\) supplied: :foo"
       (eval* "(ns foo (:require [clojure.core] [dude] :foo))")))
  (testing "error message contains location"
    (is (thrown-with-data?
         {:line 1 :column 9}
         (eval* "(ns foo (:require [clojure.core] [dude] :foo))")))))

(deftest cyclic-load-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"Cyclic load dependency: \[ foo \]->bar->\[ foo \]"
       (sci/eval-string "(require 'foo)"
                        {:load-fn (fn [{:keys [:namespace]}]
                                    (case namespace
                                      foo {:source "(ns foo (:require bar)) bar/x"}
                                      bar {:source "(ns bar (:require foo)) (def x)"}))})))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error) #"Cyclic load dependency: \[ bar \]->foo->\[ bar \]"
       (sci/eval-string "(require 'bar)"
                        {:load-fn (fn [{:keys [:namespace]}]
                                    (case namespace
                                      foo {:source "(ns foo (:require bar)) bar/x"}
                                      bar {:source "(ns bar (:require foo)) (def x)"}))})))
  (is (= 1 (sci/eval-string "(require 'foo) foo/foo"
                            {:load-fn (fn [{:keys [:namespace]}]
                                        (case namespace
                                          foo {:source "
(ns foo)
(def foo 1)
;; foo already loaded, should be ok to have cyclic dep on foo from bar now
(require 'bar)
bar/bar"}
                                          bar {:source "
(ns bar (:require foo))
(def bar foo/foo)"}))}))))

(deftest as-alias-test
  (is (= :dude/bar (sci/eval-string "(ns my-ns (:require [dude :as-alias foo])) ::foo/bar")))
  (is (= :dude/bar (sci/eval-string "(require '[dude :as-alias foo]) ::foo/bar"))))

(deftest exposed-vals-test
  (let [success? (atom false)
        the-ctx (sci/init {:load-fn (fn [{:keys [libname ctx ns opts]}]
                                      (is (= libname 'bar))
                                      (is ctx)
                                      (is (= 'foo ns))
                                      (is (= 'b (:as opts)))
                                      (reset! success? true)
                                      {:handled true})})]
    (sci/eval-string* the-ctx "(ns foo) (require '[bar :as b])")
    (is @success?)))

#?(:clj
   (defn clojure-meta [name]
     (-> (resolve (symbol "clojure.core" (str name)))
         meta)))

(deftest docstrings-test
  (testing "macro"
    (is (true? (eval* "(string? (:doc (meta #'when)))"))))
  (testing "core function that needs ctx"
    (doseq [v '[macroexpand
                find-ns
                #?(:clj get-thread-bindings)
                satisfies?
                ns-unmap
                #?(:clj remove-ns)
                find-ns
                #?(:clj find-var)
                ns-publics
                #?(:clj ns-unalias)
                isa?
                eval
                #?(:clj refer)]]
      (is (true? (eval* (str/replace "(string? (:doc (meta #'{{v}})))" "{{v}}" (str v)))) v)))
  (testing "dynvars"
    (doseq [v '[*print-namespace-maps*
                *print-dup*
                *print-readably*
                *1 *2 *3 *e
                *file* *ns*]]
      (is (true? (eval* (str/replace "(string? (:doc (meta #'{{v}})))" "{{v}}" (str v)))) v)))
  (testing "regular vars (with possibly alternative impls)"
    (doseq [v `[inc
                newline
                pr
                #?(:clj push-thread-bindings)
                ~(when (:doc (meta (resolve 'ex-message)))
                   `ex-message)
                #?(:clj swap-vals!)
                #?(:clj reset-vals!)
                alength
                #?(:clj var-get)
                #?(:clj var-set)
                compare-and-set!]
            :when v]
      (is (true? (eval* (str/replace "(string? (:doc (meta #'{{v}})))" "{{v}}" (str v)))) v)))
  (testing "All documented public vars in Clojure should also have a docstring if present in SCI")
  #?(:clj
     (is (empty? (for [[name var] (-> (sci/init {})
                                      :env
                                      deref
                                      :namespaces
                                      ('clojure.core))
                       :let [sci-meta (meta var)]
                       :when (not (:doc sci-meta))
                       :let [m (clojure-meta name)]
                       :when (:doc m)]
                   name)))))

#?(:clj
   (deftest no-cljs-var-resolve-in-clj-test
     (is (nil? (sci/eval-string "(resolve 'cljs.core/inc)")))))

#?(:cljs
   (deftest test-munge-demunge
     (is (= 'cljs.core/first?
            (demunge (munge 'cljs.core/first?))))))

(deftest macroexpand-eval-test
  (is (= 'clojure.string/x (eval* "(eval (macroexpand '(ns foo (:require [clojure.string :as str])))) `str/x"))))

(deftest loaded-libs-test
  (when-not tu/native?
    (is (= [true true]
           (tu/eval* "(requiring-resolve 'foo.bar/x) [(contains? (loaded-libs) 'foo.bar) (contains? @*loaded-libs* 'foo.bar)]"
                     {:load-fn (fn [{:keys [:namespace]}]
                                 (when (= 'foo.bar namespace)
                                   {:source "(ns foo.bar) (def x :success)"
                                    :file "foo/bar.clj"}))})))))

(deftest issue-1011
  (is (= {:b 1} (sci/eval-string "(ns foo {:a 1}) (ns foo {:b 1}) (in-ns 'foo) (meta *ns*)"))))
