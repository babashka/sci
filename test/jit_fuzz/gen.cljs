(ns jit-fuzz.gen
  "Seeded random Clojure program generator for differential testing.
  Programs are terminating by construction: loops count down to a fixed
  bound, self-recursion decrements to a base case. No host interop, no
  nondeterminism inside generated code.")

;; --- deterministic PRNG (LCG, 32-bit) ---

(deftype Rnd [^:mutable s])

(defn rnd [seed] (Rnd. (js-mod (+ 2463534242 seed) 4294967296)))

(defn- next! [^Rnd r]
  (let [s' (js-mod (+ (* (.-s r) 1664525) 1013904223) 4294967296)]
    (set! (.-s r) s')
    s'))

(defn rint
  "Random int in [0, n)."
  [r n]
  (js-mod (Math/floor (/ (next! r) 65536)) n))

(defn- pick [r v] (nth v (rint r (count v))))

(defn- chance [r pct] (< (rint r 100) pct))

;; --- literal generators ---

(defn- glit-num [r]
  (pick r [0 1 2 3 5 7 10 -1 -3 42 100 0.5 -2.5 1000000]))

(defn- glit [r]
  (case (rint r 10)
    (0 1 2 3 4) (glit-num r)
    5 (pick r ["" "a" "hello" "x-y"])
    6 (pick r [:a :b :k/q :status])
    7 (pick r [true false])
    8 nil
    9 (pick r ['[1 2 3] '[] '{:a 1 :b 2} '{} '#{1 2}])))

;; --- expression generator ---
;; env: vector of in-scope local symbols
;; fns: vector of [name arity] for defined top-level fns
;; size: recursion budget

(declare gexpr)

(defn- gatom [r env]
  (if (and (seq env) (chance r 55))
    (pick r env)
    (glit r)))

(defn- gnum-expr [r env fns size]
  (if (or (zero? size) (chance r 40))
    (if (and (seq env) (chance r 40)) (pick r env) (glit-num r))
    (gexpr r env fns (dec size))))

(def ^:private num-ops-2 '[+ - * min max quot rem mod / < > <= >= = == not=])
(def ^:private num-ops-1 '[inc dec zero? pos? neg? - abs])

(def ^:private coll-exprs
  ;; templates: %1 %2 %3 replaced by generated exprs
  '[(conj C X) (assoc M :extra X) (get M K) (get M K X) (first C) (rest C)
    (count C) (contains? M K) (vec C) (seq C) (nth V I) (keys M) (vals M)
    (into [] C) (last C) (empty? C) (reverse C) (take I C) (drop I C)
    (str X X2) (name K) (keyword X3) (subs X3 0) (not X) (nil? X)
    (boolean X) (number? X) (string? X) (keyword? X) (vector? X) (map? X)])

(defn- gcoll [r env fns size]
  (let [tmpl (pick r coll-exprs)
        sub (fn sub [form]
              (cond
                (= 'C form) (if (chance r 50) (pick r '[[1 2 3] [:a :b] []])
                                (gexpr r env fns (dec size)))
                (= 'V form) (pick r '[[10 20 30] [1 2 3 4 5]])
                (= 'M form) (if (chance r 60) (pick r '[{:a 1 :b 2} {} {:k [1 2]}])
                                (gexpr r env fns (dec size)))
                (= 'K form) (pick r [:a :b :k :missing])
                (= 'I form) (rint r 4)
                (= 'X form) (gatom r env)
                (= 'X2 form) (gatom r env)
                (= 'X3 form) (pick r ["abc" "k" "hello"])
                (seq? form) (map sub form)
                :else form))]
    (sub tmpl)))

(defn- gsym [r prefix] (symbol (str prefix (rint r 1000))))

(defn- glet [r env fns size]
  (let [n (inc (rint r 2))
        [env' binds] (loop [i 0 env' env binds []]
                       (if (< i n)
                         (let [s (gsym r "l")]
                           (recur (inc i) (conj env' s)
                                  (conj binds s (gexpr r env fns (dec size)))))
                         [env' binds]))]
    (list 'let binds (gexpr r env' fns (dec size)))))

(defn- gdestructure [r env fns size]
  (if (chance r 50)
    (let [a (gsym r "d") b (gsym r "d")]
      (list 'let [{:keys [a b] :as (gsym r "m")} {:a (gatom r env) :b (gatom r env)}
                  'ignore_ 0]
            (gexpr r (conj env a b) fns (dec size))))
    (let [x (gsym r "d") y (gsym r "d")]
      (list 'let [[x y] [(gatom r env) (gatom r env)]]
            (gexpr r (conj env x y) fns (dec size))))))

(defn- gif [r env fns size]
  (list 'if (gexpr r env fns (dec size))
        (gexpr r env fns (dec size))
        (gexpr r env fns (dec size))))

(defn- gandor [r env fns size]
  (list* (pick r '[and or])
         (repeatedly (+ 2 (rint r 2)) #(gexpr r env fns (dec size)))))

(defn- gdo [r env fns size]
  (list* 'do (repeatedly (inc (rint r 2)) #(gexpr r env fns (dec size)))))

(defn- gloop [r env fns size]
  (let [i (gsym r "i") acc (gsym r "acc")
        bound (+ 2 (rint r 8))]
    (list 'loop [i 0 acc (gatom r env)]
          (list 'if (list '< i bound)
                (list 'recur (list 'inc i)
                      (gexpr r (conj env i acc) fns (dec size)))
                acc))))

(defn- gfn-call [r env fns size]
  (if (and (seq fns) (chance r 60))
    ;; call a defined top-level fn
    (let [[name arity] (pick r fns)]
      (list* name (repeatedly arity #(gexpr r env fns (dec size)))))
    ;; immediately-invoked anonymous fn
    (case (rint r 4)
      0 (let [p (gsym r "p")]
          (list (list 'fn [p] (gexpr r (conj env p) fns (dec size)))
                (gatom r env)))
      1 (let [p (gsym r "p") q (gsym r "q")]
          (list (list 'fn [p q] (gexpr r (conj env p q) fns (dec size)))
                (gatom r env) (gatom r env)))
      ;; varargs
      2 (let [xs (gsym r "xs")]
          (list (list 'fn ['& xs] (list 'count xs))
                (gatom r env) (gatom r env) (gatom r env)))
      ;; multi-arity
      3 (let [p (gsym r "p") q (gsym r "q")]
          (list (list 'fn
                      (list [p] (gexpr r (conj env p) fns (dec size)))
                      (list [p q] (gexpr r (conj env p q) fns (dec size))))
                (gatom r env))))))

(defn- ghof [r env fns size]
  (let [p (gsym r "h")]
    (case (rint r 3)
      0 (list 'reduce (list 'fn [p (gsym r "x")] (list 'inc p)) 0
              (pick r '[[1 2 3] [5 6] []]))
      1 (list 'mapv (list 'fn [p] (gexpr r (conj env p) fns (dec size)))
              (pick r '[[1 2 3] [0]]))
      2 (list 'filterv (list 'fn [p] (list 'number? p))
              (pick r '[[1 :a 2 "x"] [nil 3]])))))

(defn- gcase [r env fns size]
  (list 'case (list 'mod (gnum-expr r env fns 0) 3)
        0 (gexpr r env fns (dec size))
        1 (gexpr r env fns (dec size))
        (gexpr r env fns (dec size))))

(defn- gtry [r env fns size]
  (let [e (gsym r "e")
        body (if (chance r 40)
               (list 'throw (list 'ex-info "boom" {:v (glit-num r)}))
               (gexpr r env fns (dec size)))
        handler (if (chance r 50)
                  (list 'ex-message e)
                  (gexpr r env fns (dec size)))
        fin (when (chance r 25)
              (list 'finally (gexpr r env fns 0)))]
    (concat
     (case (rint r 4)
       ;; :default catch
       0 (list 'try body (list 'catch :default e handler))
       ;; ^:sci/error catch (wrap-at-call-site path, callstack preserved)
       1 (list 'try body
               (list 'catch (with-meta 'js/Error {:sci/error true}) e handler))
       ;; class catch before ^:sci/error catch (unwrap dispatch)
       2 (list 'try body
               (list 'catch 'js/RangeError e (list 'do handler :range))
               (list 'catch (with-meta 'js/Error {:sci/error true}) e handler))
       ;; try/finally only, or plain class catch
       3 (if (chance r 50)
           (list 'try body)
           (list 'try body (list 'catch 'js/Error e handler))))
     (when fin [fin]))))

(def ^:private interop-exprs
  '[(.toUpperCase X3) (.indexOf X3 "b") (.charAt X3 I) (.-length X3)
    (.toFixed N 2) (.nope X) (.-nope X) (.concat X3 X3)
    (js/Math.abs N) (js/Math.max N I) (js/parseInt X3) (js/Math.nope N)
    (vec (js/Array. I N))])

(defn- ginterop [r env fns size]
  (let [tmpl (pick r interop-exprs)
        sub (fn sub [form]
              (cond
                (= 'X3 form) (if (chance r 60)
                               (pick r ["abc" "k" "hello"])
                               (gexpr r env fns (dec size)))
                (= 'N form) (glit-num r)
                (= 'I form) (rint r 4)
                (= 'X form) (gatom r env)
                (seq? form) (map sub form)
                :else form))]
    (sub tmpl)))

(defn gexpr [r env fns size]
  (if (zero? size)
    (gatom r env)
    (case (rint r 17)
      0 (gatom r env)
      1 (list (pick r num-ops-1) (gnum-expr r env fns size))
      (2 3) (list (pick r num-ops-2)
                  (gnum-expr r env fns size)
                  (gnum-expr r env fns size))
      4 (gcoll r env fns size)
      5 (glet r env fns size)
      6 (gif r env fns size)
      7 (gandor r env fns size)
      8 (gdo r env fns size)
      9 (gloop r env fns size)
      (10 11) (gfn-call r env fns size)
      12 (ghof r env fns size)
      13 (gcase r env fns size)
      14 (gtry r env fns size)
      15 (gdestructure r env fns size)
      16 (ginterop r env fns size))))

;; --- top-level program ---

(defn- gdefn [r idx _fns size]
  (let [name (symbol (str "f" idx))
        arity (inc (rint r 2))
        params (vec (repeatedly arity #(gsym r "a")))
        ;; occasionally self-recursive with decreasing guard
        body (if (chance r 25)
               (let [n (first params)]
                 (list 'if (list 'pos? n)
                       (list* name (list 'dec n) (rest params))
                       (gexpr r params [] size)))
               (gexpr r params [] size))]
    [(list 'defn name params body) [name arity]]))

(defn gen-program
  "Returns program as a string of top-level forms."
  [seed]
  (let [r (rnd seed)
        size (+ 2 (rint r 3))
        nfns (inc (rint r 3))
        [defs fns] (loop [i 0 defs [] fns []]
                     (if (< i nfns)
                       (let [[d f] (gdefn r i fns size)]
                         (recur (inc i) (conj defs d) (conj fns f)))
                       [defs fns]))
        ;; sometimes redefine f0 to test redefinition through call sites
        redef (when (and (chance r 30) (seq fns))
                (let [[name arity] (first fns)]
                  (list 'defn name (vec (repeatedly arity #(gsym r "b")))
                        (glit-num r))))
        ;; dynamic var + binding occasionally; strings because pr-str
        ;; drops the ^:dynamic meta
        dyn (when (chance r 20)
              ["(def ^:dynamic *dyn* 10)"
               "(defn use-dyn [] *dyn*)"
               "(def dyn-res (binding [*dyn* 42] (use-dyn)))"
               "(def dyn-res2 [(use-dyn) dyn-res])"])
        ;; var-mutation stress for jit deref caches: alter-var-root /
        ;; with-redefs interleaved with calls through the same var
        var-mut (when (and (chance r 25) (seq fns))
                  (let [[name arity] (pick r fns)
                        call (list* name (repeat arity 1))
                        newf (list 'fn (vec (repeatedly arity #(gsym r "c")))
                                   (glit-num r))]
                    (case (rint r 2)
                      0 [(list 'def 'vm-before call)
                         (list 'alter-var-root (list 'var name) (list 'constantly newf))
                         (list 'def 'vm-after call)
                         '(def vm-res [vm-before vm-after])]
                      1 [(list 'def 'vm-before call)
                         (list 'def 'vm-during
                               (list 'with-redefs [name newf] call))
                         (list 'def 'vm-after call)
                         '(def vm-res [vm-before vm-during vm-after])])))
        calls (vec (repeatedly (+ 2 (rint r 3))
                               #(gexpr r [] fns 3)))
        calls (cond-> calls
                redef (conj 'before-redef)
                dyn (conj 'dyn-res2)
                var-mut (conj 'vm-res))
        forms (concat defs
                      (when redef
                        [(list 'def 'before-redef (gexpr r [] fns 2))
                         redef])
                      (when dyn dyn)
                      var-mut
                      [calls])]
    (->> forms
         ;; *print-meta*: the ^:sci/error catch metadata must survive
         ;; stringification; generated forms carry no other metadata
         (map #(if (string? %) % (binding [*print-meta* true] (pr-str %))))
         (interpose "\n")
         (apply str))))
