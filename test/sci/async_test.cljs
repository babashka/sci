(ns sci.async-test
  (:require [clojure.string :as str]
            [clojure.test :as test :refer [deftest is testing async]]
            [promesa.core :as p]
            [sci.async :as scia]
            [sci.core :as sci]))

(deftest async-eval-string-js-lib-test
  (async done
         (p/let [ctx (sci/init {:async-load-fn
                                (fn [{:keys [libname opts ctx ns]}]
                                  (case libname
                                    "some_js_lib"
                                    (-> (js/Promise.resolve #js {:libfn (fn [] "yes")})
                                        (.then (fn [mod]
                                                 (sci/add-class! ctx (:as opts) mod)
                                                 (sci/add-import! ctx ns (:as opts) (:as opts))
                                                 {:handled true})))))})
                 code (str/join
                       "\n"
                       (map pr-str '[(ns dude (:require ["some_js_lib" :as my-lib]))
                                     (my-lib/libfn)]))
                 res (scia/eval-string* ctx code)]
           (is (= "yes" res))
           (done))))

(deftest async-eval-string-cljs-source-lib-test
  (async done
         (p/let [ctx (sci/init {:async-load-fn
                                (fn [{:keys [libname]}]
                                  (case libname
                                    foobar
                                    {:source "(ns foobar) (defn hello [] :hello)"}
                                    foobar2
                                    {:source "(ns foobar2) (defn hello [] :bye)"}))
                                :namespaces {'clojure.core {'require scia/require}}})
                 res (scia/eval-string* ctx "(str *ns*)")
                 _ (is (= "user" res))
                 code (str/join
                       "\n"
                       (map pr-str '[(ns dude (:require [foobar :as my-lib]))
                                     [(my-lib/hello)
                                      (foobar/hello)
                                      (str *ns*)]]))
                 res (scia/eval-string* ctx code)
                 _ (is (= [:hello :hello "dude"] res))
                 res (scia/eval-string* ctx "(str *ns*)")
                 _ (is (= "user" res))
                 code "(require '[foobar2 :as foo]) (foo/hello)"
                 res (scia/eval-string* ctx code)
                 _ (is (= :bye res))]
           (done))))

(deftest async-eval-string-cljs-source-lib-error-test
  (async done
         (-> (p/let [ctx (sci/init {:async-load-fn
                                    (fn [_]
                                      (js/Promise.reject (js/Error. "Not found")))})
                     code (str/join
                           "\n"
                           (map pr-str '[(ns dude (:require [foobar :as my-lib]))
                                         (my-lib/hello)]))
                     _res (scia/eval-string* ctx code)
                     _ (is false "Should not reach here")])
             (p/catch (fn [err]
                        (is (= "Not found" (.-message err)))
                        (done))))))

(deftest async-eval-string-reader-order-dependency-test
  (async done
         (p/let [ctx (sci/init {})
                 code "(ns foo) (def x) (defn foo [] `x) (foo)"
                 res (scia/eval-string* ctx code)]
           (is (= 'foo/x res))
           (done))))

(defn my-lazy-loaded-fn []
  :hello)

(deftest async-eval-load-clojure-ns-test
  (async done
         (p/let [sci-ns (sci/create-ns 'my.lazy-ns)
                 lazy-ns {'my-lazy-fn (sci/copy-var my-lazy-loaded-fn sci-ns)}
                 ctx (sci/init {:async-load-fn
                                (fn [{:keys [libname ctx]}]
                                  (js/Promise.resolve
                                   (sci/add-namespace! ctx libname lazy-ns)
                                   {}))})
                 code "(ns foo (:require [my.lazy-ns :refer [my-lazy-fn]])) (my-lazy-fn)"
                 res (scia/eval-string* ctx code)
                 _ (is (= :hello res))
                 code "

(ns foo1 (:require [my.lazy-ns]))
(my.lazy-ns/my-lazy-fn)

(ns foo2 (:require [my.lazy-ns :refer [my-lazy-fn]]))
(my-lazy-fn)
"
                 res (scia/eval-string* ctx code)
                 _ (is (= :hello res))]
           (done))))

(deftest no-flatten-promises
  (async done
         (p/let [ctx (sci/init {:async-load-fn
                                (fn [_]
                                  {})
                                :namespaces {'promesa.core {'delay p/delay}}
                                :classes {:allow :all 'js goog/global}})
                 code "(require '[promesa.core :as p])
                       (def a (atom :init))
                       (.then (p/delay 10 :x) (fn [] (reset! a :yolo)))
                       a"
                 res (scia/eval-string* ctx code)
                 _ (is (= :init @res))
                 _ (p/delay 11)
                 _ (is (= :yolo @res))]
           (done))))

(deftest multiple-async-evals
  (async done
         (let [ctx (sci/init {:namespaces  {'clojure.core {'require scia/require}}
                              :async-load-fn (fn [{:keys [libname ns]}]
                                               (cond (= 'acme.foo libname)
                                                     (is (= 'test (symbol ns)))
                                                     (= 'acme.bar libname)
                                                     (is (= 'user (symbol ns)))
                                                     (= 'acme.baz libname)
                                                     (is (= 'test2 (symbol ns)))
                                                     :else (throw (ex-info "Should not reach here" {})))
                                               (p/resolved
                                                (case libname
                                                  acme.foo {:source "(ns acme.foo) (defn the-fn [] :foo)"}
                                                  acme.bar {:source "(ns acme.bar) (defn the-fn [] :bar)"}
                                                  acme.baz {:source "(ns acme.baz) (defn the-fn [] :baz)"})))})]
           (-> (p/let [x (p/all [(scia/eval-string* ctx "(ns test (:require [acme.foo :as foo])) (foo/the-fn)")
                                 (scia/eval-string* ctx "(require '[acme.bar :as bar]) (bar/the-fn)")
                                 (scia/eval-string* ctx "(ns test2) (require '[acme.baz :as baz]) (baz/the-fn)")])]
                 (is (= [:foo :bar :baz] x)))
               (p/finally done)))))

(deftest eval-string+-test
  (async done
         (p/let [ctx (sci/init {})
                 {:keys [_ ns] :as ret} (scia/eval-string+ ctx "(ns foo)")
                 _ (is (= "foo" (str ns)))
                 {:keys [val ns]} (scia/eval-string+ ctx "(defn foo [] :hello) (foo/foo)" ret)
                 _ (is (= :hello val))
                 _ (is (= "foo" (str ns)))
                 {:keys [val ns]} (scia/eval-string+ ctx "(defn bar []) (symbol #'bar)")
                 _ (is (= 'user/bar val))
                 _ (is (= "user" (str ns)))]
           (done))))
