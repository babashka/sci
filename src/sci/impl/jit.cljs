(ns sci.impl.jit
  "Experimental JS codegen for analyzed fn bodies (CLJS only).

  The analyzer attaches a small walkable AST (the ast field of NodeR,
  CLJS-only) at supported sites. compile-template turns a fn body into a JS
  function via js/Function once per analyzed body; closure creation just
  instantiates it. Unsupported subtrees compile to an interpreter escape
  (H.ev), sharing the invocation array, so semantics are preserved.

  On by default. The first compilation probes whether eval is available
  (CSP can forbid it, e.g. browser extensions on locked-down pages) and
  everything falls back to the interpreter when blocked. enable!/disable!
  exist as internal overrides for tests and benchmarks."
  {:no-doc true}
  (:require [sci.impl.evaluator :as eval]
            [sci.impl.types :as t]
            [sci.impl.vars :as vars]
            [sci.impl.utils :as utils]))

;; nil = not probed yet
(def ^:private enabled (volatile! nil))

(def collect-srcs?
  "Debug: when true, compiled template sources accumulate in last-srcs.
  Never enable in production - unbounded growth."
  (volatile! false))

(def last-srcs
  "Debug: JS sources of compiled templates, only when collect-srcs? is on."
  (volatile! []))

(def strict-compile?
  "When true, compile-template rethrows an emitter exception instead of
  falling back to the interpreter. Off in production (a compile failure
  must never break evaluation); ON in tests, so an emitter bug surfaces as
  a failure instead of hiding behind a silent, still-correct fallback."
  (volatile! false))

(defn enable! []
  (vreset! enabled t/js-eval-available)
  @enabled)

(defn disable! []
  (vreset! enabled false))

(defn enabled? []
  (and ^boolean @t/jit-enabled
       (if-some [e @enabled]
         e
         (vreset! enabled t/js-eval-available))))

;; Helpers passed to generated code, which accesses them as literal
;; property names (H.d, H.ev, ...): string keys via js-obj so advanced
;; compilation can't rename them.
;; d: var deref, ev: interpreter escape, s: recur sentinel,
;; re: rethrow with the active call site's stack map (throw path only;
;; the throwaway NodeR just carries the map to the Stack protocol).
(def ^:private helpers
  (js-obj "d" deref
          "ev" (fn [node ctx b] (t/eval node ctx b))
          "s" utils/recur
          ;; stack nil/undefined (s=-1): no enclosing sited call in this
          ;; template, stay transparent like an interpreter node without a
          ;; catch (calls with nil stack maps never own a site, see
          ;; intern-stack!).
          ;; g: the var-mutation epoch; call-var sites cache derefs on it
          "g" vars/var-epoch
          ;; cs: case dispatch, the same structural map lookup as
          ;; eval-case; returns the branch index, -1 = default
          "cs" (fn [idx-map v] (get idx-map v -1))
          ;; git/sit: read/write utils/*in-try*, the compiled try's
          ;; equivalent of the interpreter's binding around the try body
          "git" (fn [] utils/*in-try*)
          "sit" (fn [v] (set! utils/*in-try* v))
          ;; tc: catch dispatch for compiled try, the interpreter's own
          ;; eval-catches (exception path only)
          "tc" (fn [ctx b body catches sci-error e]
                 (eval/eval-catches ctx b body catches sci-error e))
          ;; tw: the wrap the innermost open call site would have applied
          ;; had the exception unwound through the template catch — a
          ;; compiled try's catch intercepts first, so it must wrap before
          ;; dispatching (matters when *in-try* is :sci/error, where the
          ;; interpreter's call nodes wrap inside the try body). Returns
          ;; the (possibly) wrapped error instead of throwing.
          "tw" (fn [ctx e stack]
                 (if (nil? stack)
                   e
                   (try (utils/rethrow-with-location-of-node
                         ctx e (t/->NodeR nil (when stack stack) nil))
                        (catch :default e2 e2))))
          ;; re: the template's single catch calls this with the stack map
          ;; of the innermost open call (the stacks const indexed by s). nil
          ;; stack (s=-1): rethrow raw, transparent. otherwise rethrow
          ;; through the interpreter's location machinery, so frames and
          ;; error locations match the interpreter exactly. the throwaway
          ;; NodeR only carries the map to the Stack protocol.
          "re" (fn [ctx e stack]
                 (if (nil? stack)
                   (throw e)
                   (utils/rethrow-with-location-of-node
                    ctx e (t/->NodeR nil (when stack stack) nil))))))

(def ^:private max-call-arity 8)

(defn- call-ast? [op]
  (case op (:call-direct :call-var :call-bind :call-node) true false))

(defn ->ast
  "Resolve a child (node or constant) to a walkable AST vector."
  [x]
  (cond
    (instance? t/BindingNode x) [:binding (.-idx x)]
    (instance? t/NodeR x)
    (let [a (.-ast ^js x)]
      (if (and a
               (if (call-ast? (nth a 0))
                 (<= (count (nth a 2)) max-call-arity)
                 true))
        a
        [:escape x]))
    :else [:const x]))

(defn escape-free? [x]
  (let [a (->ast x)]
    (case (nth a 0)
      (:const :binding) true
      :escape false
      :if (and (escape-free? (nth a 1))
               (escape-free? (nth a 2))
               (escape-free? (nth a 3)))
      :do (every? escape-free? (nth a 1))
      :let (and (every? escape-free? (nth a 2))
                (escape-free? (nth a 3)))
      :recur (every? escape-free? (nth a 1))
      (:or :and) (every? escape-free? (nth a 1))
      (:call-direct :call-var :call-bind) (every? escape-free? (nth a 2))
      :call-node (and (escape-free? (nth a 1))
                      (every? escape-free? (nth a 2)))
      :iget (escape-free? (nth a 1))
      :imeth (and (escape-free? (nth a 1))
                  (every? escape-free? (nth a 3)))
      :case (and (escape-free? (nth a 1))
                 (every? escape-free? (nth a 3))
                 (escape-free? (nth a 4)))
      :jsctor (every? escape-free? (nth a 2))
      :jsstatic (every? escape-free? (nth a 3))
      ;; closure creation reads captures straight from slots, needs no B
      :mkfn true
      :vderef true
      false)))

;; --- emitter ---
;; emit-expr pushes statements onto lines and returns a JS expression string.
;; emit-tail pushes statements ending in return/continue.
;; deftype (not #js literal): field access must survive advanced renaming.

;; lines: the JS statements emitted so far (a mutable JS array), joined with
;;   newlines into the function body at the end
;; consts: values the generated code references as C[i] (fn objects, stack
;;   maps, per-site deref caches) — user values reach the code only this way
;; stacks: interned call stack maps; stored in consts, indexed by s at the
;;   template catch (C[i][s])
;; tmp: counter for fresh temp variable names (t0, t1, ...)
;; stack-idx: what the s register holds at the current emission point
;;   (nil = unknown, e.g. after a control-flow merge)
;; locals: locals mode (params/bindings are real JS locals) vs array mode
;; tbl-ref: the C[i] expression of the stacks table (interned as the first
;;   const so compiled try catches can reference it; the array is mutable,
;;   later pushes are visible)
(deftype EmitterState [lines consts stacks ^:mutable tmp ^:mutable stack-idx locals ^:mutable tbl-ref])

(defn- line! [^EmitterState st s]
  (.push (.-lines st) s)
  nil)

(defn- tmp! [^EmitterState st]
  (let [n (.-tmp st)]
    (set! (.-tmp st) (inc n))
    (str "t" n)))

(defn- const! [^EmitterState st v]
  (let [c (.-consts st)]
    (.push c v)
    (str "C[" (dec (.-length c)) "]")))

(defn- slot [^EmitterState st idx]
  (if ^boolean (.-locals st)
    (str "b" idx)
    (str "B[" idx "]")))

;; The s register mirrors the interpreter's try/catch nesting: only call
;; nodes catch, and a call's try covers callee and argument evaluation and
;; closes when the call returns. The emitter keeps the invariant that at
;; runtime s always equals the stack index of the innermost OPEN call
;; (-1 = none, the template catch rethrows raw, like interpreter nodes
;; without a catch). Emission tracks the current s statically and
;; re-asserts the ambient stack after statements that may have moved it;
;; control-flow merges invalidate the tracking so the next assert always
;; emits.
(defn- intern-stack!
  "Intern a call's stack map into the template's stack table, returning
  its index. Only stacks that are guaranteed to WRAP are interned (line
  and column present): rethrow-with-location-of-node passes through
  otherwise, and the interpreter's next enclosing catch wraps instead —
  which in the flat one-catch-per-template model is the ambient site, so
  a passthrough stack returns the ambient index rather than shadowing it.
  With this invariant the flat model reproduces the interpreter's catch
  nesting exactly: the innermost open SITED call is always the one whose
  location wins."
  [^EmitterState st stack amb]
  (if (and (some? stack) (:line stack) (:column stack))
    (let [stacks (.-stacks st)]
      (.push stacks stack)
      (dec (.-length stacks)))
    amb))

(defn- assert-stack! [^EmitterState st idx]
  (when-not (= idx (.-stack-idx st))
    (set! (.-stack-idx st) idx)
    (line! st (str "s=" idx ";"))))

(defn- invalidate-stack! [^EmitterState st]
  (set! (.-stack-idx st) nil))

(defn- truthy [e]
  (str "(" e "!=null&&" e "!==false)"))

(declare emit-expr emit-tail)

(defn- spill
  "Capture e in a temp unless it is immutable (const ref, temp, literal).
  Used where evaluation order relative to later statements matters."
  [st e]
  (if (re-matches #"C\[\d+\]|t\d+|null|true|false|\(-?[0-9.e+-]+\)" e)
    e
    (let [t (tmp! st)]
      (line! st (str "var " t "=" e ";"))
      t)))

(defn- emit-arg
  "Emit into a spill under ambient stack index amb, restoring s afterwards
  (a nested call moves it; its try closes when the spill statement ends)."
  [st amb x]
  (let [e (spill st (emit-expr st amb x))]
    (assert-stack! st amb)
    e))

(defn- join-args [args]
  (.join (to-array args) ","))

(defn- arity-impl
  "Arity-specific invoke fn of a multi-arity CLJS fn, when present.
  Property access in CLJS source, so it survives advanced renaming."
  [f n]
  (case n
    0 (.-cljs$core$IFn$_invoke$arity$0 ^js f)
    1 (.-cljs$core$IFn$_invoke$arity$1 ^js f)
    2 (.-cljs$core$IFn$_invoke$arity$2 ^js f)
    3 (.-cljs$core$IFn$_invoke$arity$3 ^js f)
    4 (.-cljs$core$IFn$_invoke$arity$4 ^js f)
    5 (.-cljs$core$IFn$_invoke$arity$5 ^js f)
    6 (.-cljs$core$IFn$_invoke$arity$6 ^js f)
    7 (.-cljs$core$IFn$_invoke$arity$7 ^js f)
    8 (.-cljs$core$IFn$_invoke$arity$8 ^js f)
    nil))

;; Inlined core fns whose compiled bodies are bare JS operators (verified
;; against cljs.core 1.11.132 source): emitting the operator is identical to
;; calling the fn, including on non-numbers ((inc "a") => "a1" either way).
;; rem is NOT eligible (quot-based body, not JS %); = is deep equality.
;; Identity-keyed js/Map: the :sci.impl/inlined fn IS the cljs.core fn.
(def ^:private op-gens-1
  (doto (js/Map.)
    (.set inc (fn [a] (str "(" (nth a 0) "+1)")))
    (.set dec (fn [a] (str "(" (nth a 0) "-1)")))
    (.set unchecked-inc (fn [a] (str "(" (nth a 0) "+1)")))
    (.set unchecked-dec (fn [a] (str "(" (nth a 0) "-1)")))
    (.set zero? (fn [a] (str "(" (nth a 0) "===0)")))
    (.set pos? (fn [a] (str "(" (nth a 0) ">0)")))
    (.set neg? (fn [a] (str "(" (nth a 0) "<0)")))
    (.set nil? (fn [a] (str "(" (nth a 0) "==null)")))
    ;; args are spilled temps/literals, safe to reference twice
    (.set not (fn [a] (str "(" (nth a 0) "==null||" (nth a 0) "===false)")))))

(defn- bin-op [op]
  (fn [a] (str "(" (nth a 0) op (nth a 1) ")")))

(def ^:private op-gens-2
  (doto (js/Map.)
    (.set + (bin-op "+"))
    (.set - (bin-op "-"))
    (.set * (bin-op "*"))
    (.set unchecked-add (bin-op "+"))
    (.set unchecked-subtract (bin-op "-"))
    (.set unchecked-multiply (bin-op "*"))
    (.set < (bin-op "<"))
    (.set > (bin-op ">"))
    (.set <= (bin-op "<="))
    (.set >= (bin-op ">="))))

;; == is path-dependent in the interpreter TODAY: the fused/bc
;; specialization (all args bindings/constants) compiles the == MACRO
;; (===), while the general path calls the == FN (-equiv, structural).
;; Mirror that exactly: === only for the shapes the interpreter fuses.
(def ^:private eq-eq ==)
(def ^:private eq-eq-gen (bin-op "==="))

(defn- tri-op [op]
  ;; + - * fold left at arity 3, like the cljs.core macros
  (fn [a] (str "(" (nth a 0) op (nth a 1) op (nth a 2) ")")))

(def ^:private op-gens-3
  (doto (js/Map.)
    (.set + (tri-op "+"))
    (.set - (tri-op "-"))
    (.set * (tri-op "*"))))

(defn- op-gen [f n]
  (case n
    1 (.get op-gens-1 f)
    2 (.get op-gens-2 f)
    3 (.get op-gens-3 f)
    nil))

(defn- emit-call [st amb a]
  (let [op (nth a 0)
        children (nth a 2)
        n (count children)
        idx (intern-stack! st (nth a 3 nil) amb)]
    ;; the interpreter's call-node try wraps callee and argument
    ;; evaluation too, so the call's stack activates before both; each
    ;; argument restores it (a nested call moves it, then its try closes)
    (assert-stack! st idx)
    (case op
      ;; :call-direct is always a real fn: plain call is safe; prefer an
      ;; operator template (which can still throw: JS coercion of a lazy
      ;; operand forces it), then the arity impl to skip variadic dispatch
      :call-direct
      (let [f (nth a 1)
            gen (if (identical? f eq-eq)
                  (when (and (= 2 n)
                             (every? #(case (nth (->ast %) 0)
                                        (:binding :const) true
                                        false)
                                     children))
                    eq-eq-gen)
                  (op-gen f n))
            head (when-not gen (const! st (or (arity-impl f n) f)))
            args (mapv #(emit-arg st idx %) children)]
        (if gen
          (gen args)
          (str head "(" (join-args args) ")")))
      ;; callee evaluates before args, matching the interpreter;
      ;; .call(null, ...) is what the CLJS compiler emits for unknown
      ;; callees: works for fns, keywords, MetaFn (IFn types get .call)
      (let [head (case op
                   ;; per-call-site deref cache [val epoch], invalidated by
                   ;; the global var-mutation epoch: hot path is two array
                   ;; reads + compare instead of the full deref
                   :call-var (let [cache (const! st #js [nil -1])
                                   vref (const! st (nth a 1))
                                   c (tmp! st)]
                               (line! st (str "var " c ";"))
                               (line! st (str "if(" cache "[1]===H.g[0]){" c "=" cache "[0];}else{"
                                              c "=H.d(" vref ");"
                                              cache "[0]=" c ";"
                                              cache "[1]=H.g[0];}"))
                               c)
                   :call-bind (spill st (slot st (nth a 1)))
                   ;; computed callee ((add 1) 2): a full expression, itself
                   ;; recursively compiled (or escaping on its own)
                   :call-node (emit-arg st idx (nth a 1)))
            args (mapv #(emit-arg st idx %) children)]
        (str head ".call(" (join-args (cons "null" args)) ")")))))

(defn- emit-chain
  "Short-circuit chain for or/and into temp t via nested ifs."
  [st amb t children and?]
  (line! st (str t "=" (emit-expr st amb (first children)) ";"))
  (assert-stack! st amb)
  (when-let [more (next children)]
    (line! st (str "if(" (when-not and? "!") (truthy t) "){"))
    (emit-chain st amb t more and?)
    (line! st "}")
    (invalidate-stack! st)
    (assert-stack! st amb)))

(defn- emit-let-bindings [st amb a]
  (let [idxs (nth a 1)
        inits (nth a 2)]
    (dotimes [i (count idxs)]
      (line! st (str (slot st (nth idxs i)) "=" (emit-expr st amb (nth inits i)) ";"))
      (assert-stack! st amb))))

(defn- emit-expr [st amb x]
  (let [a (->ast x)]
    (case (nth a 0)
      :const (let [v (nth a 1)]
               (cond (nil? v) "null"
                     (true? v) "true"
                     (false? v) "false"
                     (and (number? v) (js/isFinite v)) (str "(" v ")")
                     :else (const! st v)))
      :binding (slot st (nth a 1))
      ;; interpreter nodes locate their own errors (or defer to the
      ;; enclosing call, which the active s already reflects)
      :escape (str "H.ev(" (const! st (nth a 1)) ",CTX,B)")
      :if (let [c (emit-arg st amb (nth a 1))
                t (tmp! st)]
            (line! st (str "var " t ";"))
            (line! st (str "if" (truthy c) "{"))
            (line! st (str t "=" (emit-expr st amb (nth a 2)) ";"))
            (line! st "}else{")
            (invalidate-stack! st)
            (line! st (str t "=" (emit-expr st amb (nth a 3)) ";"))
            (line! st "}")
            (invalidate-stack! st)
            (assert-stack! st amb)
            t)
      :do (let [children (nth a 1)]
            (if (empty? children)
              "null"
              (do (doseq [c (butlast children)]
                    (line! st (str (emit-expr st amb c) ";"))
                    (assert-stack! st amb))
                  (emit-expr st amb (last children)))))
      :let (do (emit-let-bindings st amb a)
               (emit-expr st amb (nth a 3)))
      (:or :and) (let [t (tmp! st)]
                   (line! st (str "var " t ";"))
                   (emit-chain st amb t (nth a 1) (case (nth a 0) :and true :or false))
                   t)
      ;; instance interop, only attached under ctx :unrestricted: no own
      ;; stack, matching the interpreter where dot nodes don't catch;
      ;; errors defer to the enclosing call (the ambient s). Semantics match
      ;; interop/invoke-instance-field / invoke-instance-method.
      :iget (str (emit-arg st amb (nth a 1))
                 "[" (js/JSON.stringify (nth a 2)) "]")
      :imeth (let [o (emit-arg st amb (nth a 1))
                   nm (nth a 2)
                   m (tmp! st)
                   k (str "[" (js/JSON.stringify nm) "]")]
               (line! st (str "var " m "=" o k ";"))
               ;; method missing throws before args evaluate, like the interpreter
               (line! st (str "if(" m "==null)throw new Error("
                              (js/JSON.stringify (str "Could not find instance method: " nm)) ");"))
               (let [args (mapv #(emit-arg st amb %) (nth a 3))]
                 ;; exactly invoke-instance-method: the property is read
                 ;; ONCE (an accessor must not fire twice) and the call is
                 ;; Reflect.apply on that value with this=o (nbb#118 rules
                 ;; out .call/.apply on the method itself)
                 (str "Reflect.apply(" m "," o ",[" (join-args args) "])")))
      ;; hybrid case: interpreter-identical dispatch (structural map
      ;; lookup via H.cs), compiled arms behind a JS switch on the index
      :case (let [v (emit-arg st amb (nth a 1))
                  im (const! st (nth a 2))
                  branches (nth a 3)
                  d (tmp! st)
                  t (tmp! st)]
              (line! st (str "var " t ";"))
              (line! st (str "var " d "=H.cs(" im "," v ");"))
              (line! st (str "switch(" d "){"))
              (dotimes [i (count branches)]
                (invalidate-stack! st)
                (line! st (str "case " i ":"))
                (line! st (str t "=" (emit-expr st amb (nth branches i)) ";"))
                (line! st "break;"))
              (invalidate-stack! st)
              (line! st "default:")
              (line! st (str t "=" (emit-expr st amb (nth a 4)) ";"))
              (line! st "}")
              (invalidate-stack! st)
              (assert-stack! st amb)
              t)
      ;; hybrid try: the body and finally compile, catch dispatch is the
      ;; interpreter's eval-catches (exception path only, needs B — a try
      ;; therefore keeps its enclosing body in array mode). *in-try* is
      ;; set around the body exactly like the interpreter's binding:
      ;; :sci/error when a ^:sci/error catch exists, else true when any
      ;; catch exists, untouched for try/finally-only. On the exception
      ;; path s is restored to the ambient site before dispatch, so a
      ;; no-match rethrow wraps like the interpreter's transparent try
      ;; node. JS finally-throw masking matches eval-try-plain (host
      ;; semantics); interrupt-fn ctxs never get this ast.
      :try (let [body (nth a 1)
                 catches (nth a 2)
                 fin (nth a 3)
                 sci-error (nth a 4)
                 in-try-val (cond sci-error (const! st :sci/error)
                                  (seq catches) "true"
                                  :else nil)
                 t (tmp! st)
                 oit (when in-try-val (tmp! st))
                 e (tmp! st)]
             (line! st (str "var " t ";"))
             (when in-try-val
               (line! st (str "var " oit "=H.git();H.sit(" in-try-val ");")))
             (line! st "try{")
             (line! st (str t "=" (emit-expr st amb body) ";"))
             (line! st (str "}catch(" e "){"))
             (invalidate-stack! st)
             ;; when the throw crossed a call opened INSIDE the body
             ;; (s moved off the try's ambient), apply the wrap that call
             ;; site would have applied — BEFORE restoring *in-try*, since
             ;; the wrap consults it (:sci/error re-enables wrapping). A
             ;; site opened OUTSIDE the try must not wrap here: the
             ;; interpreter's try catch runs first and its no-match
             ;; rethrow supplies the body location.
             (line! st (str "if(s!==" amb "){" e "=H.tw(CTX," e ","
                            (.-tbl-ref st) "[s]);}"))
             (when in-try-val
               (line! st (str "H.sit(" oit ");")))
             (line! st (str "s=" amb ";"))
             (line! st (str t "=H.tc(CTX,B," (const! st body) "," (const! st catches) ","
                            (if sci-error "true" "false") "," e ");"))
             (line! st "}finally{")
             (invalidate-stack! st)
             (when in-try-val
               (line! st (str "H.sit(" oit ");")))
             (when fin
               (line! st (str (emit-expr st amb fin) ";")))
             (assert-stack! st amb)
             (line! st "}")
             ;; the finally guarantees s=amb on every path
             (set! (.-stack-idx st) amb)
             t)
      ;; js/ static interop, attached only under ctx :unrestricted; class
      ;; and method were resolved at analysis time and the RESOLVED method
      ;; identity must be called (mutating the class property after
      ;; analysis is not observed — same as the interpreter node, which
      ;; closed over the method). When the member is a real function it is
      ;; bound to the class ONCE at template compile, so the call site is
      ;; a plain fn call; otherwise keep Reflect.apply so the
      ;; not-a-function error matches invoke-static-method exactly. The
      ;; interpreter's static node catches, so this call has its own
      ;; stack entry.
      :jsstatic (let [idx (intern-stack! st (nth a 4 nil) amb)]
                  (assert-stack! st idx)
                  (let [method (nth a 1)
                        args (mapv #(emit-arg st idx %) (nth a 3))]
                    (if (fn? method)
                      (str (const! st (.bind method (nth a 2))) "("
                           (join-args args) ")")
                      (str "Reflect.apply(" (const! st method) ","
                           (const! st (nth a 2)) ",[" (join-args args) "])"))))
      ;; value-position var read: plain deref, never cached (unlike
      ;; :call-var) so plain data in vars is not retained
      :vderef (str "H.d(" (const! st (nth a 1)) ")")
      ;; closure creation: build the enclosed array from this template's
      ;; own slots (static capture pairs) and hand it to mk, which reuses
      ;; make-fn — stubs, laziness and self-reference patching included.
      ;; cnt nil = zero captures, mk gets null like (constantly nil) would.
      ;; No own stack: the interpreter's fn node has none.
      :mkfn (let [mk (const! st (nth a 1))
                  pairs (nth a 2)
                  cnt (nth a 3)]
              (if (nil? cnt)
                (str mk "(CTX,null)")
                (let [t (tmp! st)]
                  (line! st (str "var " t "=new Array(" cnt ");"))
                  (doseq [[bidx eidx] pairs]
                    (line! st (str t "[" eidx "]=" (slot st bidx) ";")))
                  (str mk "(CTX," t ")"))))
      ;; interp ctor node has no catch: no own stack. direct `new C(args)`
      ;; when the ctor is a real function (V8 optimizes it, unlike
      ;; Reflect.construct); keep Reflect.construct otherwise so the
      ;; not-a-constructor error matches invoke-js-constructor*.
      :jsctor (let [ctor (nth a 1)
                    args (mapv #(emit-arg st amb %) (nth a 2))]
                (if (fn? ctor)
                  (str "new " (const! st ctor) "(" (join-args args) ")")
                  (str "Reflect.construct(" (const! st ctor) ",[" (join-args args) "])")))
      (:call-direct :call-var :call-bind :call-node) (emit-call st amb a))))

(defn- emit-tail [st amb x]
  (let [a (->ast x)]
    (case (nth a 0)
      :if (let [c (emit-arg st amb (nth a 1))]
            (line! st (str "if" (truthy c) "{"))
            (emit-tail st amb (nth a 2))
            (line! st "}else{")
            (invalidate-stack! st)
            (emit-tail st amb (nth a 3))
            (line! st "}")
            (invalidate-stack! st))
      :do (let [children (nth a 1)]
            (if (empty? children)
              (line! st "return null;")
              (do (doseq [c (butlast children)]
                    (line! st (str (emit-expr st amb c) ";"))
                    (assert-stack! st amb))
                  (emit-tail st amb (last children)))))
      :let (do (emit-let-bindings st amb a)
               (emit-tail st amb (nth a 3)))
      ;; non-last children: early return on short-circuit; last child is a
      ;; real tail position (recur allowed)
      (:or :and) (let [children (nth a 1)
                       and? (case (nth a 0) :and true :or false)
                       t (tmp! st)]
                   (line! st (str "var " t ";"))
                   (doseq [c (butlast children)]
                     (line! st (str t "=" (emit-expr st amb c) ";"))
                     (assert-stack! st amb)
                     (line! st (str "if(" (when and? "!") (truthy t) ")return " t ";")))
                   (emit-tail st amb (last children)))
      ;; recur args evaluate against the old bindings: spill all, then
      ;; assign; s stays ambient across the backedge (recur is only legal
      ;; where amb is the fn body's -1)
      :recur (let [args (nth a 1)
                   ts (mapv #(emit-arg st amb %) args)]
               (dotimes [i (count ts)]
                 (line! st (str (slot st i) "=" (nth ts i) ";")))
               (line! st "continue r;"))
      ;; arms are real tail positions: recur compiles to continue, which
      ;; jumps from inside the switch to the labeled fn loop
      :case (let [v (emit-arg st amb (nth a 1))
                  im (const! st (nth a 2))
                  branches (nth a 3)
                  d (tmp! st)]
              (line! st (str "var " d "=H.cs(" im "," v ");"))
              (line! st (str "switch(" d "){"))
              (dotimes [i (count branches)]
                (invalidate-stack! st)
                (line! st (str "case " i ":"))
                (emit-tail st amb (nth branches i)))
              (invalidate-stack! st)
              (line! st "default:")
              (emit-tail st amb (nth a 4))
              (line! st "}")
              (invalidate-stack! st))
      ;; a tail escape may produce the recur sentinel (e.g. recur inside
      ;; an interpreted case/try-less special form)
      :escape (let [node (nth a 1)
                    t (tmp! st)]
                (assert-stack! st amb)
                (line! st (str "var " t "=H.ev(" (const! st node) ",CTX,B);"))
                (line! st (str "if(" t "===H.s)continue r;"))
                (line! st (str "return " t ";")))
      (line! st (str "return " (emit-expr st amb x) ";")))))

(defn- make-stub
  "Per-arity stub that compiles the template at FIRST INVOCATION rather
  than closure creation, so loaded-but-never-called fns cost nothing.
  The stub stays the closure's identity forever: one array read and nil
  check per call after the first (V8 inlines through it once impl is
  monomorphic)."
  [d ctx enclosed-array fallback arity]
  (let [state #js [nil]
        ensure! (fn []
                  (or (aget state 0)
                      (aset state 0 (if-let [tpl @d]
                                      (tpl ctx enclosed-array)
                                      (fallback)))))]
    (case arity
      0 (fn [] (let [i (aget state 0)] (if (nil? i) ((ensure!)) (i))))
      1 (fn [a] (let [i (aget state 0)] (if (nil? i) ((ensure!) a) (i a))))
      2 (fn [a b] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b) (i a b))))
      3 (fn [a b c] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c) (i a b c))))
      4 (fn [a b c d] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c d) (i a b c d))))
      5 (fn [a b c d e] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c d e) (i a b c d e))))
      6 (fn [a b c d e f] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c d e f) (i a b c d e f))))
      7 (fn [a b c d e f g] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c d e f g) (i a b c d e f g))))
      8 (fn [a b c d e f g h] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c d e f g h) (i a b c d e f g h))))
      9 (fn [a b c d e f g h i*] (let [i (aget state 0)] (if (nil? i) ((ensure!) a b c d e f g h i*) (i a b c d e f g h i*))))
      ;; exotic arities: compile eagerly, correct but not lazy
      (ensure!))))

(defn make-fn
  "The closure for fn-body: a realized template instance, a lazy stub, or
  the interpreter fallback (varargs/this-as bodies, jit off)."
  [fn-body ctx enclosed-array fallback]
  (let [d (:jit-template fn-body)]
    (cond
      (nil? d) (fallback)
      (some? (:vararg-idx fn-body)) (fallback)
      (some? (:this-as-idx fn-body)) (fallback)
      (realized? d) (if-let [tpl @d]
                      (tpl ctx enclosed-array)
                      (fallback))
      :else (make-stub d ctx enclosed-array fallback (:fixed-arity fn-body)))))

(defn compile-template
  "Compile a fn body to a template (fn [ctx enclosed-array] -> JS fn),
  or nil when the body can't/shouldn't be compiled."
  [fn-body]
  (when (and (enabled?)
             (nil? (:vararg-idx fn-body))
             (nil? (:this-as-idx fn-body)))
    (try
      (let [arity (:fixed-arity fn-body)
            body (:body fn-body)
            invoc-size (:invoc-size fn-body)
            e2i (:copy-enclosed->invocation fn-body)
            e2i-idxs (:enclosed->invocation-idxs fn-body)
            locals? (escape-free? body)
            st (EmitterState. #js [] #js [] #js [] 0 -1 locals? nil)
            _ (set! (.-tbl-ref st) (const! st (.-stacks st)))
            params (if locals?
                     (mapv #(str "b" %) (range arity))
                     (mapv #(str "p" %) (range arity)))]
        (if locals?
          (do (when (> invoc-size arity)
                (line! st (str "var "
                               (join-args (map #(str "b" %) (range arity invoc-size)))
                               ";")))
              (when e2i-idxs
                (dotimes [i (alength e2i-idxs)]
                  (let [pair (aget e2i-idxs i)]
                    (line! st (str "b" (aget pair 1) "=E[" (aget pair 0) "];"))))))
          (do (line! st (str "var B=new Array(" invoc-size ");"))
              (when e2i
                (line! st (str (const! st e2i) "(E,B);")))
              (dotimes [i arity]
                (line! st (str "B[" i "]=p" i ";")))))
        (line! st "var s=-1;")
        (line! st "try{")
        (line! st "r: for(;;){")
        (line! st "if(INT!==null)INT();")
        (emit-tail st -1 body)
        (line! st "}")
        ;; the stack table is the first const (see tbl-ref); H.re rethrows
        ;; with the active call's stack, matching interpreter frames
        (line! st (str "}catch(e){H.re(CTX,e," (.-tbl-ref st) "[s]);throw e;}"))
        (let [src (str "return function(CTX,E,INT){return function("
                       (join-args params)
                       "){" (.join (.-lines st) "\n") "}}")
              _ (when @collect-srcs? (vswap! last-srcs conj src))
              factory ((js/Function. "C" "H" src) (.-consts st) helpers)]
          (fn [ctx enclosed-array]
            (factory ctx enclosed-array (:interrupt-fn ctx)))))
      (catch :default e
        (when @strict-compile? (throw e))
        nil))))
