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
                                (fn [{:keys [libname opts ctx ns]}]
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
