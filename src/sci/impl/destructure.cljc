(ns sci.impl.destructure
  "Destructure function, adapted from Clojure and ClojureScript."
  {:no-doc true}
  (:refer-clojure :exclude [destructure])
  (:require [clojure.string :as str]))

;; destvec* and destmap* track clojure/clojure core.clj at dd006fb9 (2026-07-24),
;; the commit that added the :all directive. Diff against that when syncing.

(defn- destructure-error [msg]
  (throw #?(:cljs (new js/Error msg)
            :default (new Exception msg))))

;; Emitted as symbols rather than as embedded values. clojure.core/req! and
;; clojure.core/some-vals only exist in sci's own clojure.core, not necessarily
;; in the host, and sci resolves a clojure.core/ prefix on every dialect.
(def ^:private req!-sym 'clojure.core/req!)
(def ^:private some-vals-sym 'clojure.core/some-vals)
(def ^:private merge-sym 'clojure.core/merge)
(def ^:private select-keys-sym 'clojure.core/select-keys)
(def ^:private when-let-sym 'clojure.core/when-let)

(defn- destvec*
  [pb bvec b val loc]
  (let [gvec (gensym "vec__")
        gseq (gensym "seq__")
        gfirst (gensym "first__")
        has-rest (some #{'&} b)]
    (loop [ret (let [ret (conj bvec gvec val)]
                 (if has-rest
                   (conj ret gseq (list seq gvec))
                   ret))
           n 0
           bs b
           seen-rest? false]
      (if (seq bs)
        (let [firstb (first bs)]
          (cond
            (= firstb '&) (recur (pb ret (second bs) gseq)
                                 n
                                 (nnext bs)
                                 true)
            (= firstb :as) (pb ret (second bs) gvec)
            :else (if seen-rest?
                    (destructure-error "Unsupported binding form, only :as can follow & parameter")
                    (recur (pb (if has-rest
                                 (conj ret
                                       gfirst `(~first ~gseq)
                                       gseq `(~next ~gseq))
                                 ret)
                               firstb
                               (if has-rest
                                 gfirst
                                 (cond-> (list nth gvec n nil)
                                   loc (with-meta loc))))
                           (inc n)
                           (next bs)
                           seen-rest?))))
        ret))))

(defn- destmap*
  [pb bvec b v]
  (let [gmap (gensym "map__")
        gignore (gensym "ignore__")
        defaults (:or b)
        defaults-as (:defaults b)
        _ (when (and defaults-as (not defaults))
            (destructure-error "Can't specify :defaults without :or"))
        b (dissoc b :defaults)
        gdefaults (when defaults (zipmap (keys defaults) (repeatedly #(gensym "default__"))))
        select (:select b)
        all (:all b)
        xf (fn [mk]
             (let [mkns (namespace mk)
                   mkn (name mk)]
               (cond (str/starts-with? mkn "keys") #(keyword (or mkns (namespace %)) (name %))
                     (str/starts-with? mkn "syms") #(list 'quote (symbol (or mkns (namespace %)) (name %)))
                     (str/starts-with? mkn "strs") str
                     :else (destructure-error (str "Unsupported map directive: " mk)))))
        ret (reduce (fn [ret e]
                      (conj ret (val e) (defaults (key e))))
                    bvec gdefaults)
        ret (-> ret (conj gmap) (conj v)
                (conj gmap)
                (conj (list 'if (list seq? gmap)
                            `(clojure.core/seq-to-map-for-destructuring ~gmap)
                            gmap))
                ((fn [ret]
                   (if (:as b)
                     (conj ret (:as b) gmap)
                     ret))))
        bes (dissoc b :as :or :select :all)
        localize (fn [bb] (if #?(:cljd (satisfies? INamed bb)
                                 :clj  (instance? clojure.lang.Named bb)
                                 :cljs (implements? INamed bb))
                            (with-meta (symbol nil (name bb)) (meta bb))
                            bb))
        push1 (fn [ret bb bk req?]
                (let [getter (if req? req!-sym `get)
                      local (localize bb)
                      local-default? (contains? defaults local)
                      key-default? (contains? defaults bk)
                      bv (if (or local-default? key-default?)
                           (if (and local-default? key-default?)
                             (destructure-error
                              (str "Multiple :or defaults for same key: " bk " '" local "'"))
                             (if req?
                               (destructure-error
                                (str "Can't supply default value for required key: " bk))
                               (list `get gmap bk (if local-default?
                                                    (gdefaults local)
                                                    (gdefaults bk)))))
                           (list getter gmap bk))]
                  (if (or (keyword? bb) (symbol? bb)) ;(ident? bb)
                    (-> ret (conj local bv))
                    (pb ret bb bv))))
        retsel
        (loop [ret ret, sel #{}, bes bes, b->k {}, subs nil, suba nil]
          (if (seq bes)
            (let [be (first bes), bb (key be), bk (val be)]
              (if (keyword? bb)
                (let [dir bb
                      tr (xf bb)
                      req? (str/ends-with? (name bb) "!")
                      retsel
                      (loop [ret ret, sel sel, bbs (seq bk), preamp? true, b->k b->k]
                        (if (seq bbs)
                          (let [bb (first bbs)]
                            (if (= bb '&)
                              (if preamp?
                                (recur ret sel (next bbs) false b->k)
                                (destructure-error (str "& can only appear once in " dir)))
                              (let [_ (when (and (not preamp?) (symbol? bb))
                                        (destructure-error
                                         (str "'" bb "' - binding symbols can only appear before '&', use keys after")))
                                    bk (if preamp? (tr bb) bb)]
                                (recur (if (or preamp? req?)
                                         (push1 ret (if preamp? bb gignore) bk req?)
                                         ret)
                                       (conj sel bk)
                                       (next bbs) preamp?
                                       (if preamp? (assoc b->k (localize bb) bk) b->k)))))
                          {:ret ret, :sel sel, :b->k b->k}))]
                  (recur (:ret retsel) (:sel retsel) (next bes) (:b->k retsel) subs suba))
                (let [subsel? (and select (map? bb))
                      bb (if (or (not subsel?) (:select bb))
                           bb
                           (assoc bb :select (gensym "select__")))
                      subs (if subsel? (assoc subs bk (:select bb)) subs)
                      suball? (and all (map? bb))
                      bb (if (or (not suball?) (:all bb))
                           bb
                           (assoc bb :all (gensym "all__")))
                      suba (if suball? (assoc suba bk (:all bb)) suba)
                      b->k (if (symbol? bb) (assoc b->k bb bk) b->k)]
                  (recur (push1 ret bb bk false) (conj sel bk) (next bes) b->k subs suba))))
            {:ret ret, :sel sel, :b->k b->k, :subs subs, :suba suba}))
        ret (:ret retsel), sel (:sel retsel), b->k (:b->k retsel)
        new-or-code (and defaults (or defaults-as select all))
        bk #(if (symbol? %)
              (let [bk (b->k %)]
                (when (and new-or-code (not bk))
                  (destructure-error (str "symbol " % " in :or does not refer to a binding")))
                bk)
              %)
        dm (when defaults (dissoc (zipmap (map bk (keys gdefaults)) (vals gdefaults)) nil))
        _ (and new-or-code (not= (count (select-keys dm sel)) (count defaults))
               (destructure-error (str "keys "
                                       (apply disj (set (keys dm)) sel)
                                       " appear only in :or")))
        merged (fn [subs]
                 (list merge-sym (list some-vals-sym dm) gmap (list some-vals-sym subs)))
        ret (if select
              (let [mm (gensym "mm__")]
                (conj ret select
                      (list when-let-sym [mm (merged (:subs retsel))]
                            (list select-keys-sym mm sel))))
              ret)
        ret (if all
              (conj ret all (merged (:suba retsel)))
              ret)
        ret (if defaults-as (conj ret defaults-as dm) ret)]
    ret))

(defn destructure* [bindings loc]
  (let [bents (partition 2 bindings)
        pb (fn pb [bvec b v]
             (cond
               (symbol? b) (-> bvec (conj (if (namespace b)
                                            (symbol (name b)) b)) (conj v))
               (keyword? b) (-> bvec (conj (symbol (name b))) (conj v))
               (vector? b) (destvec* pb bvec b v loc)
               (map? b) (destmap* pb bvec b v)
               :else (destructure-error (str "Unsupported binding form: " b))))
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? symbol? (map first bents))
      bindings
      (if-let [kwbs (seq (filter #(keyword? (first %)) bents))]
        (destructure-error (str "Unsupported binding key: " (ffirst kwbs)))
        (reduce process-entry [] bents)))))

(defn destructure
  ([b] (destructure b nil))
  ([b loc]
   (destructure* b loc)))
