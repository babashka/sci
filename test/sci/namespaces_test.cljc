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
   (tu/eval* form {:bindings {'*in* binding}})))

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
         #"not.*resolve.*bit-shift-left"
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
       #"not.*resolve.*union"
       (eval* "(use '[clojure.set :exclude [union]]) (union #{1} #{2})")))
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"not.*resolve.*union"
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
  (is (= {:line 1, :column 5, :a 1, :b 1}
         (eval* "(ns ^{:a 1} foo {:b 1}) (meta *ns*)")))
  (is (= {:line 1, :column 5, :a 1, :b 1}
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
  (is (eval* "(require '[clojure.string :refer [includes?]]) (some? (get (ns-refers *ns*) 'includes?))")))

(deftest ns-map-test
  (is (eval* "(some? (get (ns-map *ns*) 'inc))"))
  #?(:clj (is (eval* "(some? (get (ns-map *ns*) 'String))")))
  (is (eval* "(defn- foo []) (some? (get (ns-map *ns*) 'foo))")))

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
                     [resolved cake]))))))))

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

(deftest ns-syntax-test
  (is (thrown-with-msg?
       #?(:clj Exception :cljs js/Error)
       #"symbol"
       (eval* "(ns)"))))

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
