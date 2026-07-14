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
            [sci.impl.utils :as utils]
            [sci.impl.vars :as vars]))

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
                         ctx e (t/->NodeR nil stack nil))
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
                    ctx e (t/->NodeR nil stack nil))))))

;; The members of the generated-code helpers object H, as the emitter
;; interpolates them (short at runtime, named here so the emitter reads
;; meaningfully). Implementations in `helpers` above.
(def ^:private interpret "H.ev")      ; interpreter escape (node, CTX, B)
(def ^:private deref-var "H.d")        ; var deref
(def ^:private recur-sentinel "H.s")
(def ^:private mutation-epoch "H.g")        ; var-mutation epoch array
(def ^:private case-branch-index "H.cs")
(def ^:private rethrow-located "H.re")     ; template catch rethrow-with-location
(def ^:private read-in-try "H.git")
(def ^:private write-in-try "H.sit")
(def ^:private catch-dispatch "H.tc") ; eval-catches dispatch
(def ^:private wrap-at-site "H.tw")    ; non-throwing wrap at intercepted site

(def ^:private max-call-arity 8)

;; AST shapes, attached by the analyzer (children are nodes or constants,
;; resolved through ->ast):
;;   [:const value]                          synthesized by ->ast
;;   [:binding idx]                          synthesized by ->ast
;;   [:escape node]                          synthesized by ->ast (no ast)
;;   [:if test then else]
;;   [:do [child ...]]
;;   [:let [idx ...] [init ...] body]
;;   [:recur [arg ...]]
;;   [:or [child ...]] / [:and [child ...]]
;;   [:call-direct f [arg ...] stack]        f = known fn value
;;   [:call-var var [arg ...] stack]         deref cached on the epoch
;;   [:call-bind idx [arg ...] stack]        callee = binding slot
;;   [:call-node callee [arg ...] stack]     callee = expression
;;   [:vderef var]                           value-position var read
;;   [:iget obj name stack]                  instance field
;;   [:iset obj name val]                    instance field write
;;   [:imeth obj name [arg ...] stack]       instance method
;;   [:case dispatch idx-map [branch ...] default]
;;   [:jsstatic method class [arg ...] stack]  resolved at analysis
;;   [:jsctor ctor [arg ...]]                  resolved at analysis
;;   [:mkfn mk capture-pairs enclosed-cnt]     closure creation
;;   [:try body catches finally sci-error]
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

(defn escape-free?
  "True when compiling x needs no B array: every subtree compiles (no
  interpreter escape) and none of it hands bindings to the runtime."
  [x]
  (let [[op x1 x2 x3 x4] (->ast x)]
    (case op
      (:const :binding) true
      :escape false
      :if (and (escape-free? x1) (escape-free? x2) (escape-free? x3))
      :do (every? escape-free? x1)
      :let (and (every? escape-free? x2) (escape-free? x3))
      :recur (every? escape-free? x1)
      (:or :and) (every? escape-free? x1)
      (:call-direct :call-var :call-bind) (every? escape-free? x2)
      :call-node (and (escape-free? x1) (every? escape-free? x2))
      :iget (escape-free? x1)
      :iset (and (escape-free? x1) (escape-free? x3))
      :imeth (and (escape-free? x1) (every? escape-free? x3))
      :case (and (escape-free? x1)
                 (every? escape-free? x3)
                 (escape-free? x4))
      :jsctor (every? escape-free? x2)
      :jsstatic (every? escape-free? x3)
      ;; closure creation reads captures straight from slots, needs no B
      :mkfn true
      :vderef true
      ;; :try catch dispatch hands B to eval-catches
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

(defn- stmt!
  "Emit one JS statement from string fragments."
  [^EmitterState st & parts]
  (line! st (apply str parts)))

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
    (stmt! st "s=" idx ";")))

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
      (stmt! st "var " t "=" e ";")
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

(defn- js-call
  "A JS call expression f(arg1,arg2,...); f and args are JS fragments."
  [f & args]
  (str f "(" (join-args args) ")"))

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

(defn- emit-call [st amb [op callee children stack]]
  (let [n (count children)
        idx (intern-stack! st stack amb)]
    ;; the interpreter's call-node try wraps callee and argument
    ;; evaluation too, so the call's stack activates before both; each
    ;; argument restores it (a nested call moves it, then its try closes)
    (assert-stack! st idx)
    (case op
      ;; :call-direct is always a real fn: plain call is safe; prefer an
      ;; operator template (which can still throw: JS coercion of a lazy
      ;; operand forces it), then the arity impl to skip variadic dispatch
      :call-direct
      (let [gen (if (identical? callee eq-eq)
                  (when (and (= 2 n)
                             (every? #(case (nth (->ast %) 0)
                                        (:binding :const) true
                                        false)
                                     children))
                    eq-eq-gen)
                  (op-gen callee n))
            head (when-not gen (const! st (or (arity-impl callee n) callee)))
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
                                   var-ref (const! st callee)
                                   deref-tmp (tmp! st)]
                               (stmt! st "var " deref-tmp ";")
                               (stmt! st "if(" cache "[1]===" mutation-epoch "[0]){" deref-tmp "=" cache "[0];}else{"
                                              deref-tmp "=" deref-var "(" var-ref ");"
                                              cache "[0]=" deref-tmp ";"
                                              cache "[1]=" mutation-epoch "[0];}")
                               deref-tmp)
                   ;; callee = binding slot index
                   :call-bind (spill st (slot st callee))
                   ;; computed callee ((add 1) 2): a full expression, itself
                   ;; recursively compiled (or escaping on its own)
                   :call-node (emit-arg st idx callee))
            args (mapv #(emit-arg st idx %) children)]
        (str head ".call(" (join-args (cons "null" args)) ")")))))

(defn- emit-chain
  "Short-circuit chain for or/and into temp t via nested ifs."
  [st amb t children and?]
  (stmt! st t "=" (emit-expr st amb (first children)) ";")
  (assert-stack! st amb)
  (when-let [more (next children)]
    (stmt! st "if(" (when-not and? "!") (truthy t) "){")
    (emit-chain st amb t more and?)
    (line! st "}")
    (invalidate-stack! st)
    (assert-stack! st amb)))

(defn- emit-let-bindings [st amb [_ idxs inits _body]]
  (dotimes [i (count idxs)]
    (stmt! st (slot st (nth idxs i)) "=" (emit-expr st amb (nth inits i)) ";")
    (assert-stack! st amb)))

(defn- emit-expr [st amb x]
  (let [[op :as a] (->ast x)]
    (case op
      :const (let [[_ v] a]
               (cond (nil? v) "null"
                     (true? v) "true"
                     (false? v) "false"
                     (and (number? v) (js/isFinite v)) (str "(" v ")")
                     :else (const! st v)))
      :binding (let [[_ idx] a] (slot st idx))
      ;; interpreter nodes locate their own errors (or defer to the
      ;; enclosing call, which the active s already reflects)
      :escape (let [[_ node] a]
                (js-call interpret (const! st node) "CTX" "B"))
      :if (let [[_ test then else] a
                test-expr (emit-arg st amb test)
                res (tmp! st)]
            (stmt! st "var " res ";")
            (stmt! st "if" (truthy test-expr) "{")
            (stmt! st res "=" (emit-expr st amb then) ";")
            (line! st "}else{")
            (invalidate-stack! st)
            (stmt! st res "=" (emit-expr st amb else) ";")
            (line! st "}")
            (invalidate-stack! st)
            (assert-stack! st amb)
            res)
      :do (let [[_ children] a]
            (if (empty? children)
              "null"
              (do (doseq [child (butlast children)]
                    (stmt! st (emit-expr st amb child) ";")
                    (assert-stack! st amb))
                  (emit-expr st amb (last children)))))
      :let (let [[_ _ _ body] a]
             (emit-let-bindings st amb a)
             (emit-expr st amb body))
      (:or :and) (let [[_ children] a
                       res (tmp! st)]
                   (stmt! st "var " res ";")
                   (emit-chain st amb res children (keyword-identical? :and op))
                   res)
      ;; instance interop, only attached under ctx :unrestricted: no own
      ;; stack, matching the interpreter where dot nodes don't catch;
      ;; errors defer to the enclosing call (the ambient s). Semantics match
      ;; interop/invoke-instance-field / invoke-instance-method.
      :iget (let [[_ obj field-name] a]
              (str (emit-arg st amb obj)
                   "[" (js/JSON.stringify field-name) "]"))
      ;; a JS assignment yields the assigned value, matching set! in CLJS
      :iset (let [[_ obj field-name val] a
                  obj-expr (emit-arg st amb obj)
                  val-expr (emit-arg st amb val)]
              (str "(" obj-expr "[" (js/JSON.stringify field-name) "]="
                   val-expr ")"))
      :imeth (let [[_ obj meth-name arg-children] a
                   obj-expr (emit-arg st amb obj)
                   meth (tmp! st)
                   key-lit (str "[" (js/JSON.stringify meth-name) "]")]
               (stmt! st "var " meth "=" obj-expr key-lit ";")
               ;; method missing throws before args evaluate, like the interpreter
               (stmt! st "if(" meth "==null)throw new Error("
                              (js/JSON.stringify (str "Could not find instance method: " meth-name)) ");")
               (let [args (mapv #(emit-arg st amb %) arg-children)]
                 ;; exactly invoke-instance-method: the property is read
                 ;; ONCE (an accessor must not fire twice) and the call is
                 ;; Reflect.apply on that value with this=obj (nbb#118
                 ;; rules out .call/.apply on the method itself)
                 (str "Reflect.apply(" meth "," obj-expr ",[" (join-args args) "])")))
      ;; hybrid case: interpreter-identical dispatch (structural map
      ;; lookup via H.cs), compiled arms behind a JS switch on the index
      :case (let [[_ dispatch idx-map branches default] a
                  dispatch-expr (emit-arg st amb dispatch)
                  idx-map-ref (const! st idx-map)
                  branch-idx (tmp! st)
                  res (tmp! st)]
              (stmt! st "var " res ";")
              (stmt! st "var " branch-idx "=" (js-call case-branch-index idx-map-ref dispatch-expr) ";")
              (stmt! st "switch(" branch-idx "){")
              (dotimes [i (count branches)]
                (invalidate-stack! st)
                (stmt! st "case " i ":")
                (stmt! st res "=" (emit-expr st amb (nth branches i)) ";")
                (line! st "break;"))
              (invalidate-stack! st)
              (line! st "default:")
              (stmt! st res "=" (emit-expr st amb default) ";")
              (line! st "}")
              (invalidate-stack! st)
              (assert-stack! st amb)
              res)
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
      :try (let [[_ body catches fin sci-error] a
                 in-try-val (cond sci-error (const! st :sci/error)
                                  (seq catches) "true"
                                  :else nil)
                 res (tmp! st)
                 old-in-try (when in-try-val (tmp! st))
                 err (tmp! st)]
             (stmt! st "var " res ";")
             (when in-try-val
               (stmt! st "var " old-in-try "=" read-in-try "();" write-in-try "(" in-try-val ");"))
             (line! st "try{")
             (stmt! st res "=" (emit-expr st amb body) ";")
             (stmt! st "}catch(" err "){")
             (invalidate-stack! st)
             ;; when the throw crossed a call opened INSIDE the body
             ;; (s moved off the try's ambient), apply the wrap that call
             ;; site would have applied — BEFORE restoring *in-try*, since
             ;; the wrap consults it (:sci/error re-enables wrapping). A
             ;; site opened OUTSIDE the try must not wrap here: the
             ;; interpreter's try catch runs first and its no-match
             ;; rethrow supplies the body location.
             (stmt! st "if(s!==" amb "){" err "=" wrap-at-site "(CTX," err ","
                            (.-tbl-ref st) "[s]);}")
             (when in-try-val
               (stmt! st write-in-try "(" old-in-try ");"))
             (stmt! st "s=" amb ";")
             (stmt! st res "=" catch-dispatch "(CTX,B," (const! st body) "," (const! st catches) ","
                            (if sci-error "true" "false") "," err ");")
             (line! st "}finally{")
             (invalidate-stack! st)
             (when in-try-val
               (stmt! st write-in-try "(" old-in-try ");"))
             (when fin
               (stmt! st (emit-expr st amb fin) ";"))
             (assert-stack! st amb)
             (line! st "}")
             ;; the finally guarantees s=amb on every path
             (set! (.-stack-idx st) amb)
             res)
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
      :jsstatic (let [[_ method class arg-children stack] a
                      idx (intern-stack! st stack amb)]
                  (assert-stack! st idx)
                  (let [args (mapv #(emit-arg st idx %) arg-children)]
                    (if (fn? method)
                      (str (const! st (.bind method class)) "("
                           (join-args args) ")")
                      (str "Reflect.apply(" (const! st method) ","
                           (const! st class) ",[" (join-args args) "])"))))
      ;; value-position var read: plain deref, never cached (unlike
      ;; :call-var) so plain data in vars is not retained
      :vderef (let [[_ v] a]
                (js-call deref-var (const! st v)))
      ;; closure creation: build the enclosed array from this template's
      ;; own slots (static capture pairs) and hand it to mk, which reuses
      ;; make-fn — stubs, laziness and self-reference patching included.
      ;; enclosed-cnt nil = zero captures, mk gets null like
      ;; (constantly nil) would. No own stack: the interpreter's fn node
      ;; has none.
      :mkfn (let [[_ mk capture-pairs enclosed-cnt] a
                  mk-ref (const! st mk)]
              (if (nil? enclosed-cnt)
                (str mk-ref "(CTX,null)")
                (let [enclosed (tmp! st)]
                  (stmt! st "var " enclosed "=new Array(" enclosed-cnt ");")
                  (doseq [[binding-idx enclosed-idx] capture-pairs]
                    (stmt! st enclosed "[" enclosed-idx "]=" (slot st binding-idx) ";"))
                  (str mk-ref "(CTX," enclosed ")"))))
      ;; interp ctor node has no catch: no own stack. direct `new C(args)`
      ;; when the ctor is a real function (V8 optimizes it, unlike
      ;; Reflect.construct); keep Reflect.construct otherwise so the
      ;; not-a-constructor error matches invoke-js-constructor*.
      :jsctor (let [[_ ctor arg-children] a
                    args (mapv #(emit-arg st amb %) arg-children)]
                (if (fn? ctor)
                  (str "new " (const! st ctor) "(" (join-args args) ")")
                  (str "Reflect.construct(" (const! st ctor) ",[" (join-args args) "])")))
      (:call-direct :call-var :call-bind :call-node) (emit-call st amb a))))

(defn- emit-tail [st amb x]
  (let [[op :as a] (->ast x)]
    (case op
      :if (let [[_ test then else] a
                test-expr (emit-arg st amb test)]
            (stmt! st "if" (truthy test-expr) "{")
            (emit-tail st amb then)
            (line! st "}else{")
            (invalidate-stack! st)
            (emit-tail st amb else)
            (line! st "}")
            (invalidate-stack! st))
      :do (let [[_ children] a]
            (if (empty? children)
              (line! st "return null;")
              (do (doseq [child (butlast children)]
                    (stmt! st (emit-expr st amb child) ";")
                    (assert-stack! st amb))
                  (emit-tail st amb (last children)))))
      :let (let [[_ _ _ body] a]
             (emit-let-bindings st amb a)
             (emit-tail st amb body))
      ;; non-last children: early return on short-circuit; last child is a
      ;; real tail position (recur allowed)
      (:or :and) (let [[_ children] a
                       and? (keyword-identical? :and op)
                       res (tmp! st)]
                   (stmt! st "var " res ";")
                   (doseq [child (butlast children)]
                     (stmt! st res "=" (emit-expr st amb child) ";")
                     (assert-stack! st amb)
                     (stmt! st "if(" (when and? "!") (truthy res) ")return " res ";"))
                   (emit-tail st amb (last children)))
      ;; recur args evaluate against the old bindings: spill all, then
      ;; assign; s stays ambient across the backedge (recur is only legal
      ;; where amb is the fn body's -1)
      :recur (let [[_ args] a
                   spilled (mapv #(emit-arg st amb %) args)]
               (dotimes [i (count spilled)]
                 (stmt! st (slot st i) "=" (nth spilled i) ";"))
               (line! st "continue r;"))
      ;; arms are real tail positions: recur compiles to continue, which
      ;; jumps from inside the switch to the labeled fn loop
      :case (let [[_ dispatch idx-map branches default] a
                  dispatch-expr (emit-arg st amb dispatch)
                  idx-map-ref (const! st idx-map)
                  branch-idx (tmp! st)]
              (stmt! st "var " branch-idx "=" (js-call case-branch-index idx-map-ref dispatch-expr) ";")
              (stmt! st "switch(" branch-idx "){")
              (dotimes [i (count branches)]
                (invalidate-stack! st)
                (stmt! st "case " i ":")
                (emit-tail st amb (nth branches i)))
              (invalidate-stack! st)
              (line! st "default:")
              (emit-tail st amb default)
              (line! st "}")
              (invalidate-stack! st))
      ;; a tail escape may produce the recur sentinel (e.g. recur inside
      ;; an interpreted special form)
      :escape (let [[_ node] a
                    res (tmp! st)]
                (assert-stack! st amb)
                (stmt! st "var " res "=" (js-call interpret (const! st node) "CTX" "B") ";")
                (stmt! st "if(" res "===" recur-sentinel ")continue r;")
                (stmt! st "return " res ";"))
      (stmt! st "return " (emit-expr st amb x) ";"))))

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
                (stmt! st "var "
                               (join-args (map #(str "b" %) (range arity invoc-size)))
                               ";"))
              (when e2i-idxs
                (dotimes [i (alength e2i-idxs)]
                  (let [pair (aget e2i-idxs i)]
                    (stmt! st "b" (aget pair 1) "=E[" (aget pair 0) "];")))))
          (do (stmt! st "var B=new Array(" invoc-size ");")
              (when e2i
                (stmt! st (const! st e2i) "(E,B);"))
              (dotimes [i arity]
                (stmt! st "B[" i "]=p" i ";"))))
        (line! st "var s=-1;")
        (line! st "try{")
        (line! st "r: for(;;){")
        (line! st "if(INT!==null)INT();")
        (emit-tail st -1 body)
        (line! st "}")
        ;; the stack table is the first const (see tbl-ref); H.re rethrows
        ;; with the active call's stack, matching interpreter frames
        (stmt! st "}catch(e){" rethrow-located "(CTX,e," (.-tbl-ref st) "[s]);throw e;}")
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
