## sci.impl.opts
### `->ctx`
<code>[bindings env features readers check-permissions?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L128-L134)
### `->ctx`
<code>[bindings env features readers check-permissions?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L128-L134)
### `default-classes`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L59-L82)
### `default-classes`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L59-L82)
### `default-imports`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L84-L93)
### `default-imports`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L84-L93)
### `default-reify-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L108-L121)
### `default-reify-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L108-L121)
### `init`
<code>[{:keys [:bindings :env :allow :deny :aliases :namespaces :classes :imports :features :load-fn :readers :reify-fn :proxy-fn]}]</code><br>

Initializes options

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L136-L163)
### `init`
<code>[{:keys [:bindings :env :allow :deny :aliases :namespaces :classes :imports :features :load-fn :readers :reify-fn :proxy-fn :async-load-fn]}]</code><br>

Initializes options

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L136-L163)
### `init-env!`
<code>[env bindings aliases namespaces classes raw-classes imports load-fn]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L15-L54)
### `init-env!`
<code>[env bindings aliases namespaces classes raw-classes imports load-fn async-load-fn]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L15-L54)
### `merge-opts`
<code>[ctx opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L165-L186)
### `merge-opts`
<code>[ctx opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L165-L186)
### `normalize-classes`
<code>[classes]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L95-L106)
### `normalize-classes`
<code>[classes]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L95-L106)
### `process-permissions`
<code>[prev-perms & permissions]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L56-L57)
### `process-permissions`
<code>[prev-perms & permissions]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/opts.cljc#L56-L57)
## sci.impl.macros
### `?`
<code>[& {:keys [cljs clj]}]</code><br>

Macro.


Private. case macro from https://github.com/cgrand/macrovich

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/macros.cljc#L15-L22)
### `?`
<code>[& {:keys [cljs clj]}]</code><br>

Macro.


Private. case macro from https://github.com/cgrand/macrovich

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/macros.cljc#L15-L22)
### `deftime`
<code>[& body]</code><br>

Macro.


Private. deftime macro from https://github.com/cgrand/macrovich

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/macros.cljc#L6-L12)
### `deftime`
<code>[& body]</code><br>

Macro.


Private. deftime macro from https://github.com/cgrand/macrovich

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/macros.cljc#L6-L12)
## sci.impl.proxy
### `proxy`
<code>[form _ _ctx classes _args & methods]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/proxy.clj#L5-L19)
### `proxy*`
<code>[ctx _form abstract-class interfaces methods]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/proxy.clj#L21-L29)
## sci.impl.main
### `-main`
<code>[& args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L53-L55)
### `-main`
<code>[& args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L53-L55)
### `main`
<code>[& [form ctx n]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L37-L50)
### `main`
<code>[& [form ctx n]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L37-L50)
### `opts`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L27-L35)
### `opts`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L27-L35)
### `time*`
<code>[_ _ expr]</code><br>

Evaluates expr and prints the time it took.  Returns the value of
  expr.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/main.cljc#L18-L25)
## sci.impl.types
### `->ConstantNode`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L84-L87)
### `->EvalForm`
<code>[form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L43-L45)
### `->EvalForm`
<code>[form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L43-L45)
### `->Node`
<code>[body stack]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L69-L81)
### `->Node`
<code>[body stack]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L69-L81)
### `->NodeR`
<code>[f stack]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L57-L58)
### `->Reified`
<code>[interfaces meths protocols]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L29-L33)
### `->Reified`
<code>[interfaces meths protocols]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L29-L33)
### `->constant`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L89-L91)
### `->constant`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L89-L91)
### `ConstantNode`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L84-L87)
### `Eval`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L53-L54)
### `EvalForm`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L43-L45)
### `EvalForm`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L43-L45)
### `IBox`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L11-L13)
### `IBox`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L11-L13)
### `IReified`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L16-L19)
### `NodeR`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L57-L58)
### `Reified`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L29-L33)
### `Reified`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L29-L33)
### `Stack`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L47-L48)
### `Stack`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L47-L48)
### `eval`
<code>[expr ctx bindings]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L53-L54)
### `eval`
<code>[expr ctx bindings]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L63-L66)
### `getInterfaces`
<code>[obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L24-L25)
### `getInterfaces`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L16-L19)
### `getMethods`
<code>[obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L22-L23)
### `getMethods`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L16-L19)
### `getProtocols`
<code>[obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L26-L27)
### `getProtocols`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L16-L19)
### `getVal`
<code>[_this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L11-L13)
### `getVal`
<code>[_this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L11-L13)
### `map->NodeR`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L57-L58)
### `setVal`
<code>[_this _v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L11-L13)
### `setVal`
<code>[_this _v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L11-L13)
### `stack`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L47-L48)
### `stack`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L47-L48)
### `type-impl`
<code>[x & _xs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L35-L40)
### `type-impl`
<code>[x & _xs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/types.cljc#L35-L40)
## sci.impl.core-protocols
### `-deref`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L10-L10)
### `-reset!`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L66-L66)
### `-swap!`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L65-L65)
### `clj-lang-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L29-L29)
### `cljs-core-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L31-L31)
### `compare-and-set!*`
<code>[ref old new]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L151-L152)
### `compareAndSet`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L67-L67)
### `defaults`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L200-L200)
### `defaults`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L200-L200)
### `deref`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L10-L10)
### `deref*`
<code>[x]</code><br>
<code>[x & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L20-L26)
### `deref*`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L20-L26)
### `deref-protocol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L33-L47)
### `deref-protocol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L33-L47)
### `iatom-defaults`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L118-L136)
### `iatom-defaults`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L118-L136)
### `iatom2-protocol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L190-L196)
### `ideref-default`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L16-L18)
### `ideref-default`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L16-L18)
### `reset`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L66-L66)
### `reset!*`
<code>[ref v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L147-L148)
### `reset!*`
<code>[ref v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L147-L148)
### `reset-protocol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L181-L187)
### `reset-vals!*`
<code>[ref v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L159-L160)
### `resetVals`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L69-L69)
### `swap`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L65-L65)
### `swap!*`
<code>[ref f & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L140-L145)
### `swap!*`
<code>[ref f & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L140-L145)
### `swap-protocol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L164-L178)
### `swap-protocol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L164-L178)
### `swap-vals!*`
<code>[ref f & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L155-L156)
### `swapVals`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/core_protocols.cljc#L68-L68)
## sci.impl.evaluator
### `def-fn-call`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L286-L306)
### `def-fn-call`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L286-L306)
### `eval-and`
<code>[ctx bindings args]</code><br>

The and macro from clojure.core. Note: and is unrolled in the analyzer, this is a fallback.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L26-L38)
### `eval-and`
<code>[ctx bindings args]</code><br>

The and macro from clojure.core. Note: and is unrolled in the analyzer, this is a fallback.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L26-L38)
### `eval-case`
<code>[ctx bindings case-map case-val]</code><br>
<code>[ctx bindings case-map case-val case-default]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L108-L119)
### `eval-case`
<code>[ctx bindings case-map case-val]</code><br>
<code>[ctx bindings case-map case-val case-default]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L108-L119)
### `eval-def`
<code>[ctx bindings var-name init m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L76-L100)
### `eval-def`
<code>[ctx bindings var-name init m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L76-L100)
### `eval-do`
<code>[ctx bindings exprs]</code><br>

Note: various arities of do have already been unrolled in the analyzer.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L259-L268)
### `eval-do`
<code>[ctx bindings exprs]</code><br>

Note: various arities of do have already been unrolled in the analyzer.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L259-L268)
### `eval-form`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L198-L198)
### `eval-form`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L198-L198)
### `eval-import`
<code>[ctx & import-symbols-or-lists]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L218-L253)
### `eval-import`
<code>[ctx & import-symbols-or-lists]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L218-L253)
### `eval-instance-method-invocation`
<code>[ctx bindings instance-expr method-str field-access args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L160-L192)
### `eval-instance-method-invocation`
<code>[ctx bindings instance-expr method-str field-access args allowed]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L160-L192)
### `eval-let`
<code>[ctx bindings let-bindings exprs idxs]</code><br>

The let macro from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L53-L74)
### `eval-let`
<code>[ctx bindings let-bindings exprs idxs]</code><br>

The let macro from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L53-L74)
### `eval-or`
<code>[ctx bindings args]</code><br>

The or macro from clojure.core. Note: or is unrolled in the analyzer, this is a fallback.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L40-L51)
### `eval-or`
<code>[ctx bindings args]</code><br>

The or macro from clojure.core. Note: or is unrolled in the analyzer, this is a fallback.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L40-L51)
### `eval-resolve`
<code>[ctx bindings sym]</code><br>
<code>[ctx bindings env sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L200-L210)
### `eval-resolve`
<code>[ctx bindings sym]</code><br>
<code>[ctx bindings env sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L200-L210)
### `eval-static-method-invocation`
<code>[ctx bindings expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L150-L153)
### `eval-static-method-invocation`
<code>[ctx bindings expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L150-L153)
### `eval-string`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L257-L257)
### `eval-string`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L257-L257)
### `eval-string*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L106-L106)
### `eval-string*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L106-L106)
### `eval-try`
<code>[ctx bindings body catches finally]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L121-L146)
### `eval-try`
<code>[ctx bindings body catches finally]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L121-L146)
### `fn-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L16-L16)
### `fn-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L16-L16)
### `macros`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L20-L22)
### `macros`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L20-L22)
### `resolve-symbol`
<code>[bindings sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L102-L104)
### `resolve-symbol`
<code>[bindings sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L102-L104)
### `super-symbols`
<code>[clazz]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/evaluator.cljc#L156-L158)
## sci.impl.parser
### `auto-resolve`
<code>[ctx opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L89-L97)
### `auto-resolve`
<code>[ctx opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L89-L97)
### `data-readers`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L19-L22)
### `data-readers`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L19-L22)
### `default-data-reader-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L24-L27)
### `default-data-reader-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L24-L27)
### `default-opts`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L34-L41)
### `default-opts`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L34-L41)
### `eof`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L13-L13)
### `eof`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L13-L13)
### `fully-qualify`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L50-L83)
### `fully-qualify`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L50-L83)
### `get-column-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L102-L103)
### `get-column-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L102-L103)
### `get-line-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L99-L100)
### `get-line-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L99-L100)
### `parse-next`
<code>[ctx r]</code><br>
<code>[ctx r opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L105-L150)
### `parse-next`
<code>[ctx r]</code><br>
<code>[ctx r opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L105-L150)
### `parse-string`
<code>[ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L161-L165)
### `parse-string`
<code>[ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L161-L165)
### `read-eval`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L15-L17)
### `read-eval`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L15-L17)
### `reader`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L152-L159)
### `reader`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L152-L159)
### `reader-resolver`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L29-L32)
### `reader-resolver`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L29-L32)
### `throw-eval-read`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L85-L87)
### `throw-eval-read`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L85-L87)
### `var->sym`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L43-L48)
### `var->sym`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/parser.cljc#L43-L48)
## sci.impl.for-macro
### `assert-args`
<code>[expr seq-exprs _body-expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/for_macro.cljc#L8-L18)
### `assert-args`
<code>[expr seq-exprs _body-expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/for_macro.cljc#L8-L18)
### `expand-for`
<code>[expr _ seq-exprs body-expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/for_macro.cljc#L21-L96)
### `expand-for`
<code>[expr _ seq-exprs body-expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/for_macro.cljc#L21-L96)
## sci.addons
### `future`
<code>[opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/addons.cljc#L8-L9)
## sci.impl.analyzer
### `->FnBody`
<code>[params body fixed-arity var-arg-name self-ref-idx iden->invoke-idx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L249-L249)
### `FnBody`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L249-L249)
### `analyze`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `analyze`
<code>[ctx expr]</code><br>
<code>[ctx expr top-level?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1516-L1553)
### `analyze`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `analyze`
<code>[ctx expr]</code><br>
<code>[ctx expr top-level?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1516-L1553)
### `analyze-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `analyze-call`
<code>[ctx expr m top-level?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1209-L1414)
### `analyze-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `analyze-call`
<code>[ctx expr m top-level?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1209-L1414)
### `analyze-case`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L673-L710)
### `analyze-case`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L673-L710)
### `analyze-children`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `analyze-children`
<code>[ctx children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L246-L247)
### `analyze-children`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `analyze-children`
<code>[ctx children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L246-L247)
### `analyze-children-tail`
<code>[ctx children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L99-L104)
### `analyze-children-tail`
<code>[ctx children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L99-L104)
### `analyze-def`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L542-L571)
### `analyze-def`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L542-L571)
### `analyze-defn`
<code>[ctx [op fn-name & body :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L573-L615)
### `analyze-defn`
<code>[ctx [op fn-name & body :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L573-L615)
### `analyze-dot`
<code>[ctx [_dot instance-expr method-expr & args :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L782-L858)
### `analyze-dot`
<code>[ctx [_dot instance-expr method-expr & args :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L782-L858)
### `analyze-fn`
<code>[ctx fn-expr macro?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L456-L459)
### `analyze-fn`
<code>[ctx fn-expr macro?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L456-L459)
### `analyze-fn*`
<code>[ctx [_fn name? & body :as fn-expr] macro?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L311-L436)
### `analyze-fn*`
<code>[ctx [_fn name? & body :as fn-expr] macro?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L311-L436)
### `analyze-import`
<code>[_ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1198-L1207)
### `analyze-import`
<code>[_ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1198-L1207)
### `analyze-in-ns`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1190-L1196)
### `analyze-in-ns`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1190-L1196)
### `analyze-js-obj`
<code>[ctx js-val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1498-L1514)
### `analyze-lazy-seq`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L634-L641)
### `analyze-lazy-seq`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L634-L641)
### `analyze-let`
<code>[ctx [_let let-bindings & exprs :as expr]]</code><br>

The let macro from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L508-L512)
### `analyze-let`
<code>[ctx [_let let-bindings & exprs :as expr]]</code><br>

The let macro from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L508-L512)
### `analyze-let*`
<code>[ctx expr destructured-let-bindings exprs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L475-L506)
### `analyze-let*`
<code>[ctx expr destructured-let-bindings exprs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L475-L506)
### `analyze-loop`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L617-L632)
### `analyze-loop`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L617-L632)
### `analyze-map`
<code>[ctx expr m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1436-L1462)
### `analyze-map`
<code>[ctx expr m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1436-L1462)
### `analyze-new`
<code>[ctx [_new class-sym & args :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L876-L963)
### `analyze-new`
<code>[ctx [_new class-sym & args :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L876-L963)
### `analyze-ns-form`
<code>[ctx [_ns ns-name & exprs :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L988-L1032)
### `analyze-ns-form`
<code>[ctx [_ns ns-name & exprs :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L988-L1032)
### `analyze-quote`
<code>[_ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1184-L1188)
### `analyze-quote`
<code>[_ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1184-L1188)
### `analyze-set!`
<code>[ctx [_ obj v :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1042-L1063)
### `analyze-set!`
<code>[ctx [_ obj v :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1042-L1063)
### `analyze-throw`
<code>[ctx [_throw ex :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L764-L778)
### `analyze-throw`
<code>[ctx [_throw ex :as expr]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L764-L778)
### `analyze-try`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L712-L762)
### `analyze-try`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L712-L762)
### `analyze-var`
<code>[ctx [_ var-name]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1039-L1040)
### `analyze-var`
<code>[ctx [_ var-name]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1039-L1040)
### `analyze-vec-or-set`
<code>[ctx f1 f2 expr m]</code><br>

Returns analyzed vector or set

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1464-L1495)
### `analyze-vec-or-set`
<code>[ctx f1 f2 expr m]</code><br>

Returns analyzed vector or set

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1464-L1495)
### `analyzed-fn-meta`
<code>[ctx m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L303-L309)
### `analyzed-fn-meta`
<code>[ctx m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L303-L309)
### `constant-node?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1425-L1427)
### `constant-node?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1425-L1427)
### `expand-constructor`
<code>[ctx [constructor-sym & args]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L965-L971)
### `expand-constructor`
<code>[ctx [constructor-sym & args]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L965-L971)
### `expand-dot*`
<code>[ctx [method-name obj & args :as expr]]</code><br>

Expands (.foo x)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L868-L874)
### `expand-dot*`
<code>[ctx [method-name obj & args :as expr]]</code><br>

Expands (.foo x)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L868-L874)
### `expand-dot**`
<code>[ctx expr]</code><br>

Expands (. x method)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L860-L866)
### `expand-dot**`
<code>[ctx expr]</code><br>

Expands (. x method)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L860-L866)
### `expand-fn-args+body`
<code>[{:keys [:fn-expr] :as ctx} [binding-vector & body-exprs] macro? fn-name fn-id]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L253-L301)
### `expand-fn-args+body`
<code>[{:keys [:fn-expr] :as ctx} [binding-vector & body-exprs] macro? fn-name fn-id]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L253-L301)
### `fn-ctx-fn`
<code>[_ctx struct fn-meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L438-L454)
### `fn-ctx-fn`
<code>[_ctx struct fn-meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L438-L454)
### `gen-return-and`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L170-L197)
### `gen-return-and`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L170-L197)
### `gen-return-binding-call`
<code>[]</code><br>

Macro.


Creates returning-binding-call function, optimizes calling a local
  binding as function.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1068-L1097)
### `gen-return-binding-call`
<code>[]</code><br>

Macro.


Creates returning-binding-call function, optimizes calling a local
  binding as function.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1068-L1097)
### `gen-return-call`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1139-L1179)
### `gen-return-call`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1139-L1179)
### `gen-return-do`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L106-L130)
### `gen-return-do`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L106-L130)
### `gen-return-needs-ctx-call`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1103-L1129)
### `gen-return-needs-ctx-call`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1103-L1129)
### `gen-return-or`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L138-L165)
### `gen-return-or`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L138-L165)
### `gen-return-recur`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L203-L237)
### `gen-return-recur`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L203-L237)
### `init-var!`
<code>[ctx name expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L514-L540)
### `init-var!`
<code>[ctx name expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L514-L540)
### `macroexpand`
<code>[ctx form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L87-L92)
### `macroexpand`
<code>[ctx form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L87-L92)
### `macroexpand-1`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L60-L85)
### `macroexpand-1`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L60-L85)
### `map->FnBody`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L249-L249)
### `map-fn`
<code>[children-count]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1416-L1418)
### `map-fn`
<code>[children-count]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1416-L1418)
### `recur-target`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L35-L36)
### `recur-target`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L35-L36)
### `recur-target?`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L44-L45)
### `recur-target?`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L44-L45)
### `return-and`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L199-L199)
### `return-and`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L199-L199)
### `return-binding-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1099-L1099)
### `return-binding-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1099-L1099)
### `return-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `return-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1181-L1181)
### `return-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `return-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1181-L1181)
### `return-do`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L135-L135)
### `return-do`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L135-L135)
### `return-if`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L643-L671)
### `return-if`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L643-L671)
### `return-map`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `return-map`
<code>[ctx the-map analyzed-children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1420-L1423)
### `return-map`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L56-L56)
### `return-map`
<code>[ctx the-map analyzed-children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1420-L1423)
### `return-needs-ctx-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1131-L1131)
### `return-needs-ctx-call`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1131-L1131)
### `return-ns-op`
<code>[_ctx f expr analyzed-args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L977-L986)
### `return-ns-op`
<code>[_ctx f expr analyzed-args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L977-L986)
### `return-or`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L167-L167)
### `return-or`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L167-L167)
### `return-recur`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L243-L243)
### `return-recur`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L243-L243)
### `special-syms`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L51-L51)
### `special-syms`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L51-L51)
### `unwrap-children`
<code>[children]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L1429-L1434)
### `update-parents`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L251-L251)
### `update-parents`
<code>[ctx closure-bindings ob]</code><br>

:syms = closed over values

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L461-L473)
### `update-parents`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L251-L251)
### `update-parents`
<code>[ctx closure-bindings ob]</code><br>

:syms = closed over values

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L461-L473)
### `with-recur-target`
<code>[ctx v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L38-L39)
### `with-recur-target`
<code>[ctx v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L38-L39)
### `without-recur-target`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L41-L42)
### `without-recur-target`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/analyzer.cljc#L41-L42)
## sci.impl.load
### `add-loaded-lib`
<code>[env lib]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L62-L67)
### `add-loaded-lib`
<code>[env lib]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L62-L67)
### `eval-refer`
<code>[ctx ns-sym & filters]</code><br>

The function equivalent of :refer is handled differently than what we
  did before (this is more like what Clojure itself does.) For
  referring clojure.core we still use the old code.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L278-L286)
### `eval-refer`
<code>[ctx ns-sym & filters]</code><br>

The function equivalent of :refer is handled differently than what we
  did before (this is more like what Clojure itself does.) For
  referring clojure.core we still use the old code.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L278-L286)
### `eval-refer*`
<code>[env ns-sym filters]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L240-L276)
### `eval-refer*`
<code>[env ns-sym filters]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L240-L276)
### `eval-refer-clojure`
<code>[ctx exprs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L202-L238)
### `eval-refer-clojure`
<code>[ctx exprs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L202-L238)
### `eval-require`
<code>[ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L190-L192)
### `eval-require`
<code>[ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L190-L192)
### `eval-use`
<code>[ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L196-L198)
### `eval-use`
<code>[ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L196-L198)
### `handle-refer-all`
<code>[the-current-ns the-loaded-ns include-sym? rename-sym only]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L9-L20)
### `handle-refer-all`
<code>[the-current-ns the-loaded-ns include-sym? rename-sym only]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L9-L20)
### `handle-require-libspec`
<code>[ctx lib opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L69-L132)
### `handle-require-libspec`
<code>[ctx lib opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L69-L132)
### `handle-require-libspec-env`
<code>[_ctx env current-ns the-loaded-ns lib-name {:keys [:as :refer :rename :exclude :only :use] :as _parsed-libspec}]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L22-L60)
### `handle-require-libspec-env`
<code>[_ctx env current-ns the-loaded-ns lib-name {:keys [:as :refer :rename :exclude :only :use] :as _parsed-libspec}]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L22-L60)
### `load-lib`
<code>[ctx prefix lib & options]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L134-L142)
### `load-lib`
<code>[ctx prefix lib & options]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/load.cljc#L134-L142)
## sci.impl.interpreter
### `eval-form`
<code>[ctx form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interpreter.cljc#L15-L52)
### `eval-form`
<code>[ctx form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interpreter.cljc#L15-L52)
### `eval-string`
<code>[s]</code><br>
<code>[s opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interpreter.cljc#L70-L75)
### `eval-string`
<code>[s]</code><br>
<code>[s opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interpreter.cljc#L70-L75)
### `eval-string*`
<code>[ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interpreter.cljc#L56-L64)
### `eval-string*`
<code>[ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interpreter.cljc#L56-L64)
## sci.impl.io
### `core-dynamic-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>

create a dynamic var with clojure.core :ns meta

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L29-L32)
### `core-dynamic-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>

create a dynamic var with clojure.core :ns meta

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L29-L32)
### `err`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L42-L44)
### `err`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L42-L44)
### `flush`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L113-L116)
### `flush`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L117-L118)
### `flush-on-newline`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L66-L66)
### `flush-on-newline`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L66-L66)
### `in`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L34-L36)
### `in`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L34-L36)
### `newline`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L122-L125)
### `newline`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L126-L129)
### `out`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L38-L40)
### `out`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L38-L40)
### `pr`
<code>[]</code><br>
<code>[x]</code><br>
<code>[x & more]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L84-L99)
### `pr`
<code>[& objs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L100-L110)
### `pr-str`
<code>[& xs]</code><br>

pr to a string, returning it

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L132-L138)
### `pr-str`
<code>[& objs]</code><br>

pr to a string, returning it

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L140-L150)
### `print`
<code>[& more]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L194-L197)
### `print`
<code>[& objs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L199-L208)
### `print-dup-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L68-L68)
### `print-dup-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L68-L68)
### `print-err-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L53-L56)
### `print-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L47-L50)
### `print-length`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L63-L63)
### `print-length`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L63-L63)
### `print-level`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L64-L64)
### `print-level`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L64-L64)
### `print-meta`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L60-L61)
### `print-meta`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L60-L61)
### `print-method`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L14-L20)
### `print-namespace-maps`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L65-L65)
### `print-namespace-maps`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L65-L65)
### `print-newline`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L69-L69)
### `print-readably`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L67-L67)
### `print-readably`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L67-L67)
### `print-str`
<code>[& xs]</code><br>

print to a string, returning it

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L211-L217)
### `print-str`
<code>[& objs]</code><br>

print to a string, returning it

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L219-L229)
### `printf`
<code>[fmt & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L250-L252)
### `println`
<code>[& more]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L232-L235)
### `println`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L120-L120)
### `println`
<code>[& objs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L237-L247)
### `prn`
<code>[& more]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L153-L158)
### `prn`
<code>[& objs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L160-L170)
### `prn-str`
<code>[& xs]</code><br>

prn to a string, returning it

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L173-L179)
### `prn-str`
<code>[& objs]</code><br>

prn to a string, returning it

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L181-L191)
### `read-line`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L277-L281)
### `string-print`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L71-L73)
### `with-in-str`
<code>[_ _ s & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L270-L274)
### `with-out-str`
<code>[_ _ & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L254-L267)
### `with-out-str`
<code>[_ _ & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/io.cljc#L254-L267)
## sci.impl.hierarchies
### `ancestors*`
<code>[ctx tag]</code><br>
<code>[_ctx h tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L35-L40)
### `ancestors*`
<code>[ctx tag]</code><br>
<code>[_ctx h tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L35-L40)
### `derive*`
<code>[ctx tag parent]</code><br>
<code>[_ctx h tag parent]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L10-L17)
### `derive*`
<code>[ctx tag parent]</code><br>
<code>[_ctx h tag parent]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L10-L17)
### `descendants*`
<code>[ctx tag]</code><br>
<code>[_ctx h tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L42-L47)
### `descendants*`
<code>[ctx tag]</code><br>
<code>[_ctx h tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L42-L47)
### `global-hierarchy`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L7-L8)
### `global-hierarchy`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L7-L8)
### `isa?*`
<code>[ctx child parent]</code><br>
<code>[_ctx h child parent]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L28-L33)
### `isa?*`
<code>[ctx child parent]</code><br>
<code>[_ctx h child parent]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L28-L33)
### `parents*`
<code>[ctx tag]</code><br>
<code>[_ctx h tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L49-L54)
### `parents*`
<code>[ctx tag]</code><br>
<code>[_ctx h tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L49-L54)
### `underive*`
<code>[ctx tag parent]</code><br>
<code>[_ctx h tag parent]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L19-L26)
### `underive*`
<code>[ctx tag parent]</code><br>
<code>[_ctx h tag parent]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/hierarchies.cljc#L19-L26)
## sci.impl.doseq-macro
### `assert-args`
<code>[seq-exprs _body-exprs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/doseq_macro.cljc#L7-L13)
### `assert-args`
<code>[seq-exprs _body-exprs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/doseq_macro.cljc#L7-L13)
### `expand-doseq`
<code>[expr _ seq-exprs & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/doseq_macro.cljc#L15-L66)
### `expand-doseq`
<code>[expr _ seq-exprs & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/doseq_macro.cljc#L15-L66)
## sci.impl.faster
### `assoc-3`
<code>[m k v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L12-L16)
### `assoc-3`
<code>[m k v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L12-L16)
### `deref-1`
<code>[ref]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L24-L29)
### `deref-1`
<code>[ref]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L24-L29)
### `get-2`
<code>[m k]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L18-L22)
### `get-2`
<code>[m k]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L18-L22)
### `nth-2`
<code>[c i]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L6-L10)
### `nth-2`
<code>[c i]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/faster.cljc#L6-L10)
## sci.impl.records
### `->SciRecord`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L31-L34)
### `->record-impl`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L46-L47)
### `->record-impl`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L46-L47)
### `SciRecord`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L31-L34)
### `assert-no-jvm-interface`
<code>[protocol protocol-name expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L12-L17)
### `clojure-str`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L26-L29)
### `clojure-str`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L26-L29)
### `defrecord`
<code>[form _ ctx record-name fields & raw-protocol-impls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L49-L126)
### `defrecord`
<code>[form _ ctx record-name fields & raw-protocol-impls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L49-L126)
### `map->SciRecord`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L31-L34)
### `resolve-record-class`
<code>[ctx class-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L155-L158)
### `resolve-record-class`
<code>[ctx class-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L155-L158)
### `resolve-record-or-protocol-class`
<code>[ctx sym]</code><br>
<code>[ctx package class]</code><br>

A record class is represented by a symbol with metadata (currently). This is only an implementation detail.
   A protocol is represented by a map with :ns, :methods and optionally :class. This is also an implementation detail.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L134-L153)
### `resolve-record-or-protocol-class`
<code>[ctx sym]</code><br>
<code>[ctx package class]</code><br>

A record class is represented by a symbol with metadata (currently). This is only an implementation detail.
   A protocol is represented by a map with :ns, :methods and optionally :class. This is also an implementation detail.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L134-L153)
### `sci-record?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L128-L132)
### `sci-record?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L128-L132)
### `to-string`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L19-L19)
### `to-string`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/records.cljc#L19-L19)
## sci.impl.protocols
### `->sigs`
<code>[signatures]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L20-L27)
### `->sigs`
<code>[signatures]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L20-L27)
### `default?`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L12-L18)
### `default?`
<code>[_ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L12-L18)
### `defprotocol`
<code>[_ _ _ctx protocol-name & signatures]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L29-L98)
### `defprotocol`
<code>[_ _ _ctx protocol-name & signatures]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L29-L98)
### `extend`
<code>[ctx atype & proto+mmaps]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L101-L123)
### `extend`
<code>[ctx atype & proto+mmaps]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L101-L123)
### `extend-protocol`
<code>[_ _ ctx protocol-name & impls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L183-L195)
### `extend-protocol`
<code>[_ _ ctx protocol-name & impls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L183-L195)
### `extend-type`
<code>[_ _ ctx atype & proto+meths]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L197-L207)
### `extend-type`
<code>[_ _ ctx atype & proto+meths]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L197-L207)
### `extends?`
<code>[protocol atype]</code><br>

Returns true if atype extends protocol

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L261-L264)
### `extends?`
<code>[protocol atype]</code><br>

Returns true if atype extends protocol

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L261-L264)
### `find-matching-non-default-method`
<code>[protocol obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L211-L216)
### `find-matching-non-default-method`
<code>[protocol obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L211-L216)
### `instance-impl`
<code>[clazz x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L242-L259)
### `instance-impl`
<code>[clazz x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L242-L259)
### `process-methods`
<code>[ctx type meths protocol-ns extend-via-metadata]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L160-L181)
### `process-methods`
<code>[ctx type meths protocol-ns extend-via-metadata]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L160-L181)
### `process-single`
<code>[fq [args & body]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L151-L158)
### `process-single`
<code>[fq [args & body]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L151-L158)
### `process-single-extend-meta`
<code>[fq [args & body] default-method?]</code><br>

Processes single args+body pair for extending via metadata

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L125-L149)
### `process-single-extend-meta`
<code>[fq [args & body] default-method?]</code><br>

Processes single args+body pair for extending via metadata

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L125-L149)
### `satisfies?`
<code>[protocol obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L218-L240)
### `satisfies?`
<code>[protocol obj]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/protocols.cljc#L218-L240)
## sci.impl.interop
### `fully-qualify-class`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L77-L89)
### `fully-qualify-class`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L77-L89)
### `get-static-field`
<code>[[class field-name-sym]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L44-L49)
### `get-static-field`
<code>[[class field-name-sym]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L44-L49)
### `invoke-constructor`
<code>[class args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L55-L58)
### `invoke-constructor`
<code>[constructor args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L55-L58)
### `invoke-instance-field`
<code>[obj target-class method]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L14-L25)
### `invoke-instance-field`
<code>[obj _target-class field-name]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L14-L25)
### `invoke-instance-method`
<code>[obj target-class method args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L27-L42)
### `invoke-instance-method`
<code>[obj _target-class method-name args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L27-L42)
### `invoke-js-constructor`
<code>[constructor args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L52-L53)
### `invoke-static-method`
<code>[[class method-name] args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L60-L75)
### `invoke-static-method`
<code>[[class method-name] args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L60-L75)
### `resolve-class`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L108-L109)
### `resolve-class`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L108-L109)
### `resolve-class-opts`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L91-L106)
### `resolve-class-opts`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/interop.cljc#L91-L106)
## sci.impl.callstack
### `clean-ns`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L26-L29)
### `clean-ns`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L26-L29)
### `expr->data`
<code>[expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L16-L24)
### `expr->data`
<code>[expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L16-L24)
### `format-stacktrace`
<code>[st]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L59-L82)
### `format-stacktrace`
<code>[st]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L59-L82)
### `right-pad`
<code>[s n]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L55-L57)
### `right-pad`
<code>[s n]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L55-L57)
### `sci-ns-name`
<code>[ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L8-L9)
### `sci-ns-name`
<code>[ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L8-L9)
### `select`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L11-L14)
### `select`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L11-L14)
### `stacktrace`
<code>[callstack]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L31-L53)
### `stacktrace`
<code>[callstack]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/callstack.cljc#L31-L53)
## sci.impl.js
### `evalString`
<code>[s]</code><br>
<code>[s opts]</code><br>

Evaluates string `s` as a Clojure form using the Small Clojure Interpreter.

  The object `opts` may contain the following:

  - `"bindings"`: an object with bindings that are used to resolve symbols
  in the Clojure form, e.g. `{'x 1}`.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/js.cljs#L50-L59)
### `map-keys`
<code>[f m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/js.cljs#L11-L12)
### `map-vals`
<code>[f m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/js.cljs#L8-L9)
### `obj->clj`
<code>[x k-fn]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/js.cljs#L14-L17)
### `toJS`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/js.cljs#L44-L48)
## sci.async
### `eval-string*`

[Source](https://github.com/babashka/process/blob/main/src/sci/async.cljs#L8-L8)
### `eval-string*`
<code>[ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/async.cljs#L77-L92)
### `handle-libspecs`
<code>[ctx ns-obj libspecs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/async.cljs#L12-L58)
### `last-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/async.cljs#L10-L10)
## sci.impl.read
### `load-string`
<code>[sci-ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L67-L74)
### `load-string`
<code>[sci-ctx s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L67-L74)
### `read`
<code>[sci-ctx]</code><br>
<code>[sci-ctx stream]</code><br>
<code>[sci-ctx stream eof-error? eof-value]</code><br>
<code>[sci-ctx stream _eof-error? eof-value _recursive?]</code><br>
<code>[sci-ctx opts stream]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L36-L57)
### `read`
<code>[sci-ctx]</code><br>
<code>[sci-ctx stream]</code><br>
<code>[sci-ctx stream eof-error? eof-value]</code><br>
<code>[sci-ctx stream _eof-error? eof-value _recursive?]</code><br>
<code>[sci-ctx opts stream]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L36-L57)
### `read-string`
<code>[sci-ctx s]</code><br>
<code>[sci-ctx opts s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L59-L65)
### `read-string`
<code>[sci-ctx s]</code><br>
<code>[sci-ctx opts s]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L59-L65)
### `source-logging-reader`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L77-L85)
### `source-logging-reader`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L77-L85)
### `with-resolver`
<code>[opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L24-L34)
### `with-resolver`
<code>[opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/read.cljc#L24-L34)
## sci.impl.vars
### `->Frame`
<code>[bindings prev]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L59-L59)
### `->Frame`
<code>[bindings prev]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L59-L59)
### `->SciNamespace`
<code>[name meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L38-L53)
### `->SciNamespace`
<code>[name meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L38-L53)
### `->SciUnbound`
<code>[the-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L172-L223)
### `->SciUnbound`
<code>[the-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L172-L223)
### `->SciVar`
<code>[root sym meta thread-bound]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L243-L373)
### `->SciVar`
<code>[root sym meta thread-bound]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L243-L373)
### `->TBox`
<code>[thread val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L73-L78)
### `->TBox`
<code>[thread val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L73-L78)
### `Frame`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L59-L59)
### `Frame`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L59-L59)
### `HasName`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L24-L25)
### `HasName`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L24-L25)
### `IVar`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `IVar`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `SciNamespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L38-L53)
### `SciNamespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L38-L53)
### `SciUnbound`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L172-L223)
### `SciUnbound`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L172-L223)
### `SciVar`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L243-L373)
### `SciVar`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L243-L373)
### `TBox`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L73-L78)
### `TBox`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L73-L78)
### `alter-var-root`
<code>[v f]</code><br>
<code>[v f & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L432-L440)
### `alter-var-root`
<code>[v f]</code><br>
<code>[v f & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L432-L440)
### `bindRoot`
<code>[this v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `bindRoot`
<code>[this v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `binding-conveyor-fn`
<code>[f]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L148-L166)
### `binding-conveyor-fn`
<code>[f]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L148-L166)
### `built-in-var?`
<code>[var-meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L227-L228)
### `built-in-var?`
<code>[var-meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L227-L228)
### `clojure-core-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L410-L410)
### `clojure-core-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L410-L410)
### `clone-thread-binding-frame`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L80-L83)
### `clone-thread-binding-frame`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L80-L83)
### `current-file`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L413-L413)
### `current-file`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L413-L413)
### `current-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L415-L415)
### `current-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L415-L415)
### `current-ns-name`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L417-L418)
### `current-ns-name`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L417-L418)
### `dvals`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L64-L65)
### `dvals`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L67-L67)
### `dynamic-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L398-L405)
### `dynamic-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L398-L405)
### `dynamic-var?`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L91-L94)
### `dynamic-var?`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L91-L94)
### `get-thread-binding`
<code>[sci-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L141-L146)
### `get-thread-binding`
<code>[sci-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L141-L146)
### `get-thread-binding-frame`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L69-L71)
### `get-thread-binding-frame`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L69-L71)
### `get-thread-bindings`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L129-L139)
### `get-thread-bindings`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L129-L139)
### `getName`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L24-L25)
### `getName`
<code>[_]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L24-L25)
### `getRawRoot`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `getRawRoot`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `hasRoot`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `hasRoot`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `isMacro`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `isMacro`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `namespace?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L55-L57)
### `namespace?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L55-L57)
### `new-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Returns a new sci var.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L442-L447)
### `new-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Returns a new sci var.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L442-L447)
### `pop-thread-bindings`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L120-L127)
### `pop-thread-bindings`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L120-L127)
### `push-thread-bindings`
<code>[bindings]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L105-L118)
### `push-thread-bindings`
<code>[bindings]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L105-L118)
### `reset-thread-binding-frame`
<code>[frame]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L85-L87)
### `reset-thread-binding-frame`
<code>[frame]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L85-L87)
### `setThreadBound`
<code>[this v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `setThreadBound`
<code>[this v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `throw-unbound-call-exception`
<code>[the-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L168-L170)
### `throw-unbound-call-exception`
<code>[the-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L168-L170)
### `toSymbol`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `toSymbol`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `top-frame`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L61-L61)
### `top-frame`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L61-L61)
### `unbind`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `unbind`
<code>[this]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L96-L103)
### `unqualify-symbol`
<code>[sym]</code><br>

If sym is namespace-qualified, remove the namespace, else return sym

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L391-L396)
### `unqualify-symbol`
<code>[sym]</code><br>

If sym is namespace-qualified, remove the namespace, else return sym

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L391-L396)
### `user-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L408-L408)
### `user-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L408-L408)
### `v1`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L450-L450)
### `v1`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L450-L450)
### `var-get`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L381-L382)
### `var-get`
<code>[v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L381-L382)
### `var-set`
<code>[v val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L384-L385)
### `var-set`
<code>[v val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L384-L385)
### `var?`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L89-L89)
### `var?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L387-L389)
### `var?`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L89-L89)
### `var?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L387-L389)
### `with-bindings`
<code>[bindings & body]</code><br>

Macro.


Macro for binding sci vars for internal use.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L421-L430)
### `with-bindings`
<code>[bindings & body]</code><br>

Macro.


Macro for binding sci vars for internal use.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L421-L430)
### `with-writeable-namespace`
<code>[the-ns-object ns-meta & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L28-L36)
### `with-writeable-namespace`
<code>[the-ns-object ns-meta & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L28-L36)
### `with-writeable-var`
<code>[the-var var-meta & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L231-L241)
### `with-writeable-var`
<code>[the-var var-meta & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/vars.cljc#L231-L241)
## sci.impl.test
### `Foo`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/test.cljc#L31-L32)
### `Foo`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/test.cljc#L31-L32)
### `foo`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/test.cljc#L28-L29)
### `foo`
<code>[]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/test.cljc#L28-L29)
## sci.impl.utils
### `*in-try*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L41-L41)
### `*in-try*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L41-L41)
### `allowed-append`

used for allowing interop in with-out-str

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L51-L52)
### `allowed-loop`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L148-L148)
### `allowed-loop`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L148-L148)
### `allowed-recur`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L149-L149)
### `allowed-recur`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L149-L149)
### `ana-macros`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L220-L224)
### `ana-macros`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L220-L224)
### `constant?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L15-L24)
### `constant?`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L15-L24)
### `eval`
<code>[sci-ctx form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L206-L207)
### `eval`
<code>[sci-ctx form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L206-L207)
### `eval-do*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L201-L201)
### `eval-do*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L201-L201)
### `eval-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L202-L202)
### `eval-fn`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L202-L202)
### `eval-form-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L194-L194)
### `eval-form-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L194-L194)
### `eval-refer-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L198-L198)
### `eval-refer-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L198-L198)
### `eval-require-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L195-L195)
### `eval-require-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L195-L195)
### `eval-resolve-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L197-L197)
### `eval-resolve-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L197-L197)
### `eval-string*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L203-L203)
### `eval-string*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L203-L203)
### `eval-use-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L196-L196)
### `eval-use-state`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L196-L196)
### `kw-identical?`
<code>[k v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L26-L29)
### `kw-identical?`
<code>[k v]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L26-L29)
### `log`
<code>[& xs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L254-L256)
### `log`
<code>[& xs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L254-L256)
### `lookup`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L204-L204)
### `lookup`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L204-L204)
### `macro?`
<code>[f]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L43-L46)
### `macro?`
<code>[f]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L43-L46)
### `macroexpand*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L199-L199)
### `macroexpand*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L199-L199)
### `macroexpand-1*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L200-L200)
### `macroexpand-1*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L200-L200)
### `make-stack`
<code>[expr-meta]</code><br>
<code>[expr-meta special?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L246-L252)
### `make-stack`
<code>[expr-meta]</code><br>
<code>[expr-meta special?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L246-L252)
### `maybe-destructured`
<code>[params body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L226-L242)
### `maybe-destructured`
<code>[params body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L226-L242)
### `namespace-object`
<code>[env ns-sym create? attr-map]</code><br>

Fetches namespaces from env if it exists. Else, if `create?`,
  produces one regardless of the existince of the namespace in env and
  adds it to env before returning it.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L175-L186)
### `namespace-object`
<code>[env ns-sym create? attr-map]</code><br>

Fetches namespaces from env if it exists. Else, if `create?`,
  produces one regardless of the existince of the namespace in env and
  adds it to env before returning it.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L175-L186)
### `needs-ctx`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L48-L48)
### `needs-ctx`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L48-L48)
### `prewalk`
<code>[f form]</code><br>

Prewalk with metadata preservation. Does not prewalk :sci.impl/op nodes.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L170-L173)
### `prewalk`
<code>[f form]</code><br>

Prewalk with metadata preservation. Does not prewalk :sci.impl/op nodes.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L170-L173)
### `rethrow-with-location-of-node`
<code>[ctx e raw-node]</code><br>
<code>[ctx _bindings e raw-node]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L86-L129)
### `rethrow-with-location-of-node`
<code>[ctx e raw-node]</code><br>
<code>[ctx _bindings e raw-node]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L86-L129)
### `rewrite-ex-msg`
<code>[ex-msg env fm]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L55-L84)
### `set-namespace!`
<code>[ctx ns-sym attr-map]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L188-L192)
### `set-namespace!`
<code>[ctx ns-sym attr-map]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L188-L192)
### `split-when`
<code>[pred coll]</code><br>

Like partition-by but splits collection only when `pred` returns
  a truthy value. E.g. `(split-when odd? [1 2 3 4 5]) => ((1 2) (3 4) (5))`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L209-L218)
### `split-when`
<code>[pred coll]</code><br>

Like partition-by but splits collection only when `pred` returns
  a truthy value. E.g. `(split-when odd? [1 2 3 4 5]) => ((1 2) (3 4) (5))`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L209-L218)
### `strip-core-ns`
<code>[sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L143-L146)
### `strip-core-ns`
<code>[sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L143-L146)
### `throw-error-with-location`
<code>[msg iobj]</code><br>
<code>[msg iobj data]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L31-L39)
### `throw-error-with-location`
<code>[msg iobj]</code><br>
<code>[msg iobj data]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L31-L39)
### `unqualify-symbol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L244-L244)
### `unqualify-symbol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L244-L244)
### `var-unbound`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L150-L151)
### `var-unbound`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L150-L151)
### `vary-meta*`
<code>[obj f & args]</code><br>

Only adds metadata to obj if d is not nil and if obj already has meta

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L136-L141)
### `vary-meta*`
<code>[obj f & args]</code><br>

Only adds metadata to obj if d is not nil and if obj already has meta

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L136-L141)
### `walk*`
<code>[inner form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L153-L168)
### `walk*`
<code>[inner form]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/utils.cljc#L153-L168)
## sci.impl.resolve
### `check-permission!`
<code>[ctx sym [check-sym v]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L24-L36)
### `check-permission!`
<code>[ctx sym [check-sym v]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L24-L36)
### `lookup`
<code>[ctx sym call?]</code><br>
<code>[ctx sym call? tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L126-L151)
### `lookup`
<code>[ctx sym call?]</code><br>
<code>[ctx sym call? _tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L126-L151)
### `lookup*`
<code>[ctx sym call?]</code><br>
<code>[ctx sym call? only-var?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L38-L92)
### `lookup*`
<code>[ctx sym call?]</code><br>
<code>[ctx sym call? only-var?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L38-L92)
### `mark-resolve-sym`
<code>[sym idx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L15-L22)
### `mark-resolve-sym`
<code>[sym idx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L15-L22)
### `resolve-dotted-access`
<code>[ctx sym call? tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L158-L179)
### `resolve-symbol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L156-L156)
### `resolve-symbol`
<code>[ctx sym]</code><br>
<code>[ctx sym call?]</code><br>
<code>[ctx sym call? tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L181-L204)
### `resolve-symbol`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L156-L156)
### `resolve-symbol`
<code>[ctx sym]</code><br>
<code>[ctx sym call?]</code><br>
<code>[ctx sym call? tag]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L181-L204)
### `throw-error-with-location`
<code>[msg node]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L12-L13)
### `throw-error-with-location`
<code>[msg node]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L12-L13)
### `update-parents`
<code>[ctx closure-bindings ob]</code><br>

:syms = closed over -> idx

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L94-L124)
### `update-parents`
<code>[ctx closure-bindings ob]</code><br>

:syms = closed over -> idx

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/resolve.cljc#L94-L124)
## sci.impl.fns
### `eval-fn`
<code>[ctx bindings fn-name fn-bodies macro? single-arity self-ref? bindings-fn]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L144-L168)
### `eval-fn`
<code>[ctx bindings fn-name fn-bodies macro? single-arity self-ref? bindings-fn]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L144-L168)
### `fn-arity-map`
<code>[ctx enclosed-array bindings fn-name macro? fn-bodies]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L132-L142)
### `fn-arity-map`
<code>[ctx enclosed-array bindings fn-name macro? fn-bodies]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L132-L142)
### `fun`
<code>[ctx enclosed-array bindings fn-body fn-name macro?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L63-L126)
### `fun`
<code>[ctx enclosed-array bindings fn-body fn-name macro?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L63-L126)
### `gen-fn`
<code>[n]</code><br>
<code>[n disable-arity-checks]</code><br>
<code>[n _disable-arity-checks varargs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L13-L57)
### `gen-fn`
<code>[n]</code><br>
<code>[n disable-arity-checks]</code><br>
<code>[n _disable-arity-checks varargs]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L13-L57)
### `lookup-by-arity`
<code>[arities arity]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L128-L130)
### `lookup-by-arity`
<code>[arities arity]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/fns.cljc#L128-L130)
## sci.impl.unrestrict
### `*unrestricted*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/unrestrict.cljc#L4-L4)
### `*unrestricted*`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/unrestrict.cljc#L4-L4)
## sci.impl.destructure
### `destructure`
<code>[b]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/destructure.cljc#L115-L116)
### `destructure`
<code>[b]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/destructure.cljc#L115-L116)
### `destructure*`
<code>[bindings]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/destructure.cljc#L6-L113)
### `destructure*`
<code>[bindings]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/destructure.cljc#L6-L113)
## sci.impl.namespaces
### `*1`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L766-L766)
### `*1`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L766-L766)
### `*2`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L768-L768)
### `*2`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L768-L768)
### `*3`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L770-L770)
### `*3`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L770-L770)
### `*e`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L772-L772)
### `*e`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L772-L772)
### `->*`
<code>[_ _ x & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L102-L111)
### `->*`
<code>[_ _ x & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L102-L111)
### `->>*`
<code>[_ _ x & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L113-L122)
### `->>*`
<code>[_ _ x & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L113-L122)
### `amap*`
<code>[_ _ a idx ret expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L316-L324)
### `amap*`
<code>[_ _ a idx ret expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L316-L324)
### `apply-template`
<code>[argv expr values]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1637-L1641)
### `apply-template`
<code>[argv expr values]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1637-L1641)
### `apropos`
<code>[ctx str-or-pattern]</code><br>

Given a regular expression or stringable thing, return a seq of all
  public definitions in all currently-loaded namespaces that match the
  str-or-pattern.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1480-L1492)
### `apropos`
<code>[ctx str-or-pattern]</code><br>

Given a regular expression or stringable thing, return a seq of all
  public definitions in all currently-loaded namespaces that match the
  str-or-pattern.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1480-L1492)
### `areduce*`
<code>[_ _ a idx ret init expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L309-L314)
### `areduce*`
<code>[_ _ a idx ret init expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L309-L314)
### `as->*`
<code>[_ _ expr name & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L124-L130)
### `as->*`
<code>[_ _ expr name & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L124-L130)
### `assert*`
<code>[_&form _ x]</code><br>
<code>[_&form _ x message]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L299-L307)
### `assert*`
<code>[_&form _ x]</code><br>
<code>[_&form _ x message]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L299-L307)
### `assert-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L297-L297)
### `assert-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L297-L297)
### `bound-fn*`
<code>[f]</code><br>

Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L669-L677)
### `bound-fn*`
<code>[f]</code><br>

Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L669-L677)
### `clean-ns`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L462-L463)
### `clean-ns`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L462-L463)
### `clojure-core`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L838-L1428)
### `clojure-core`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L838-L1428)
### `clojure-core-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L38-L38)
### `clojure-core-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L38-L38)
### `clojure-edn-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1659-L1659)
### `clojure-edn-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1659-L1659)
### `clojure-lang`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L741-L755)
### `clojure-repl`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1623-L1635)
### `clojure-repl`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1623-L1635)
### `clojure-repl-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1618-L1618)
### `clojure-repl-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1618-L1618)
### `clojure-set-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1657-L1657)
### `clojure-set-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1657-L1657)
### `clojure-string-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1656-L1656)
### `clojure-string-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1656-L1656)
### `clojure-template`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1651-L1654)
### `clojure-template`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1651-L1654)
### `clojure-template-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1649-L1649)
### `clojure-template-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1649-L1649)
### `clojure-version`
<code>[]</code><br>

Returns clojure version as a printable string.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L820-L833)
### `clojure-version-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L812-L818)
### `clojure-walk-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1658-L1658)
### `clojure-walk-namespace`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1658-L1658)
### `clojure-walk-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1675-L1686)
### `clojure-walk-ns`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1675-L1686)
### `comment*`
<code>[_ _ & _body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L132-L133)
### `comment*`
<code>[_ _ & _body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L132-L133)
### `cond*`
<code>[_ _ & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L176-L185)
### `cond*`
<code>[_ _ & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L176-L185)
### `cond->*`
<code>[_&form _&env expr & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L187-L197)
### `cond->*`
<code>[_&form _&env expr & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L187-L197)
### `cond->>*`
<code>[_&form _&env expr & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L199-L209)
### `cond->>*`
<code>[_&form _&env expr & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L199-L209)
### `condp*`
<code>[_ _ pred expr & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L381-L402)
### `condp*`
<code>[_ _ pred expr & clauses]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L381-L402)
### `copy-core-var`
<code>[sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L63-L63)
### `copy-core-var`
<code>[sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L82-L84)
### `copy-core-var`
<code>[sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L63-L63)
### `copy-core-var`
<code>[sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L82-L84)
### `copy-var`
<code>[sym _ns]</code><br>
<code>[sym _ns _opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L60-L62)
### `copy-var`
<code>[sym ns & [opts]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L65-L81)
### `copy-var`
<code>[sym _ns]</code><br>
<code>[sym _ns _opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L60-L62)
### `copy-var`
<code>[sym ns & [opts]]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L65-L81)
### `core-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L835-L836)
### `core-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L835-L836)
### `declare*`
<code>[_ _ & names]</code><br>

defs the supplied var names with no bindings, useful for making forward declarations.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L277-L279)
### `declare*`
<code>[_ _ & names]</code><br>

defs the supplied var names with no bindings, useful for making forward declarations.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L277-L279)
### `defn-*`
<code>[_ _ name & decls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L377-L379)
### `defn-*`
<code>[_ _ name & decls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L377-L379)
### `defonce*`
<code>[_ _ name expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L404-L408)
### `defonce*`
<code>[_ _ name expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L404-L408)
### `delay*`
<code>[_ _ & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L372-L375)
### `delay*`
<code>[_ _ & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L372-L375)
### `demunge`
<code>[fn-name]</code><br>

Given a string representation of a fn class,
  as in a stack trace element, returns a readable version.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1568-L1573)
### `dir`
<code>[_ _ nsname]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1437-L1440)
### `dir`
<code>[_ _ nsname]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1437-L1440)
### `dir-fn`
<code>[ctx ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1430-L1435)
### `dir-fn`
<code>[ctx ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1430-L1435)
### `do-template`
<code>[_ _ argv expr & values]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1643-L1647)
### `do-template`
<code>[_ _ argv expr & values]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1643-L1647)
### `doc`
<code>[_ _ sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1455-L1462)
### `doc`
<code>[_ _ sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1455-L1462)
### `dotimes*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L135-L145)
### `dotimes*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L135-L145)
### `doto*`
<code>[_&form _&env x & forms]</code><br>

doto from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L162-L174)
### `doto*`
<code>[_&form _&env x & forms]</code><br>

doto from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L162-L174)
### `double-dot`
<code>[_ _ x form]</code><br>
<code>[_ _ x form & more]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L417-L419)
### `double-dot`
<code>[_ _ x form]</code><br>
<code>[_ _ x form & more]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L417-L419)
### `elide-vars`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L46-L46)
### `elide-vars`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L48-L48)
### `ex-cause`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L289-L295)
### `ex-cause`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L289-L295)
### `ex-message`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L281-L287)
### `ex-message`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L281-L287)
### `find-doc`
<code>[ctx re-string-or-pattern]</code><br>

Prints documentation for any var whose documentation or name
  contains a match for re-string-or-pattern

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1464-L1478)
### `find-doc`
<code>[ctx re-string-or-pattern]</code><br>

Prints documentation for any var whose documentation or name
  contains a match for re-string-or-pattern

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1464-L1478)
### `has-root-impl`
<code>[sci-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L424-L425)
### `has-root-impl`
<code>[sci-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L424-L425)
### `if-let*`
<code>[&form &env bindings then]</code><br>
<code>[_&form _&env bindings then else & _oldform]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L211-L220)
### `if-let*`
<code>[&form &env bindings then]</code><br>
<code>[_&form _&env bindings then else & _oldform]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L211-L220)
### `if-not*`
<code>[&form &env test then]</code><br>
<code>[_&form _&env test then else]</code><br>

if-not from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L147-L151)
### `if-not*`
<code>[&form &env test then]</code><br>
<code>[_&form _&env test then else]</code><br>

if-not from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L147-L151)
### `if-some*`
<code>[&form &env bindings then]</code><br>
<code>[_&form _&env bindings then else & _oldform]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L222-L231)
### `if-some*`
<code>[&form &env bindings then]</code><br>
<code>[_&form _&env bindings then else & _oldform]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L222-L231)
### `if-vars-elided`
<code>[then else]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L51-L53)
### `if-vars-elided`
<code>[then else]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L51-L53)
### `inlined-vars`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L43-L44)
### `inlined-vars`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L43-L44)
### `lazy-cat*`
<code>[_ _ & colls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L421-L422)
### `lazy-cat*`
<code>[_ _ & colls]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L421-L422)
### `letfn*`
<code>[_ _ fnspecs & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L339-L348)
### `letfn*`
<code>[_ _ fnspecs & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L339-L348)
### `macroexpand*`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L731-L732)
### `macroexpand*`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L731-L732)
### `macroexpand-1*`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L734-L735)
### `macroexpand-1*`
<code>[ctx expr]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L734-L735)
### `macroexpand-all`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1661-L1673)
### `macroexpand-all`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1661-L1673)
### `macrofy`
<code>[f]</code><br>
<code>[sym f]</code><br>
<code>[sym f ns]</code><br>
<code>[sym f ns ctx?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L86-L93)
### `macrofy`
<code>[f]</code><br>
<code>[sym f]</code><br>
<code>[sym f ns]</code><br>
<code>[sym f ns ctx?]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L86-L93)
### `namespaces`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1688-L1732)
### `namespaces`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1688-L1732)
### `ns-new-var`
<code>[ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L95-L100)
### `ns-new-var`
<code>[ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L95-L100)
### `print-doc`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1442-L1453)
### `print-doc`
<code>[m]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1442-L1453)
### `pst`
<code>[ctx]</code><br>
<code>[ctx e-or-depth]</code><br>
<code>[_ctx e depth]</code><br>

Prints a stack trace of the exception, to the depth requested. If none supplied, uses the root cause of the
  most recent repl exception (*e), and a depth of 12.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1590-L1616)
### `repl-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1620-L1621)
### `repl-var`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1620-L1621)
### `require`
<code>[sci-ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L577-L578)
### `require`
<code>[sci-ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L577-L578)
### `root-cause`
<code>[t]</code><br>

Returns the initial cause of an exception or error by peeling off all of
  its wrappers

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1554-L1565)
### `sci-alias`
<code>[ctx alias-sym ns-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L432-L437)
### `sci-alias`
<code>[ctx alias-sym ns-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L432-L437)
### `sci-all-ns`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L530-L535)
### `sci-all-ns`
<code>[ctx]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L530-L535)
### `sci-binding`
<code>[form _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L647-L667)
### `sci-binding`
<code>[form _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L647-L667)
### `sci-bound-fn`
<code>[_ _ & fntail]</code><br>

Returns a function defined by the given fntail, which will install the
  same bindings in effect as in the thread at the time bound-fn was called.
  This may be used to define a helper function which runs on a different
  thread, but needs the same bindings in place.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L679-L685)
### `sci-bound-fn`
<code>[_ _ & fntail]</code><br>

Returns a function defined by the given fntail, which will install the
  same bindings in effect as in the thread at the time bound-fn was called.
  This may be used to define a helper function which runs on a different
  thread, but needs the same bindings in place.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L679-L685)
### `sci-bound?`
<code>[sci-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L566-L571)
### `sci-bound?`
<code>[sci-var]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L566-L571)
### `sci-create-ns`
<code>[ctx ns-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L439-L440)
### `sci-create-ns`
<code>[ctx ns-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L439-L440)
### `sci-find-ns`
<code>[ctx ns-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L442-L444)
### `sci-find-ns`
<code>[ctx ns-sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L442-L444)
### `sci-find-var`
<code>[sci-ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L614-L625)
### `sci-find-var`
<code>[sci-ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L614-L625)
### `sci-impl-records`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L759-L762)
### `sci-impl-records`

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L759-L762)
### `sci-intern`
<code>[ctx ns var-sym]</code><br>
<code>[ctx ns var-sym val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L542-L564)
### `sci-intern`
<code>[ctx ns var-sym]</code><br>
<code>[ctx ns var-sym val]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L542-L564)
### `sci-ns-aliases`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L453-L460)
### `sci-ns-aliases`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L453-L460)
### `sci-ns-imports`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L482-L491)
### `sci-ns-imports`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L482-L491)
### `sci-ns-interns`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L465-L470)
### `sci-ns-interns`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L465-L470)
### `sci-ns-map`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L502-L505)
### `sci-ns-map`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L502-L505)
### `sci-ns-name`
<code>[ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L429-L430)
### `sci-ns-name`
<code>[ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L429-L430)
### `sci-ns-publics`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L472-L480)
### `sci-ns-publics`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L472-L480)
### `sci-ns-refers`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L493-L500)
### `sci-ns-refers`
<code>[ctx sci-ns]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L493-L500)
### `sci-ns-resolve`
<code>[sci-ctx ns sym]</code><br>
<code>[sci-ctx ns env sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L595-L601)
### `sci-ns-resolve`
<code>[sci-ctx ns sym]</code><br>
<code>[sci-ctx ns env sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L595-L601)
### `sci-ns-unmap`
<code>[ctx sci-ns sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L507-L528)
### `sci-ns-unmap`
<code>[ctx sci-ns sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L507-L528)
### `sci-refer`
<code>[sci-ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L589-L590)
### `sci-refer`
<code>[sci-ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L589-L590)
### `sci-refer-clojure`
<code>[_ _ & filters]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L592-L593)
### `sci-refer-clojure`
<code>[_ _ & filters]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L592-L593)
### `sci-remove-ns`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L537-L540)
### `sci-remove-ns`
<code>[ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L537-L540)
### `sci-requiring-resolve`
<code>[sci-ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L603-L612)
### `sci-requiring-resolve`
<code>[sci-ctx sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L603-L612)
### `sci-resolve`
<code>[sci-ctx sym]</code><br>
<code>[sci-ctx env sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L583-L587)
### `sci-resolve`
<code>[sci-ctx sym]</code><br>
<code>[sci-ctx env sym]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L583-L587)
### `sci-the-ns`
<code>[ctx x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L446-L451)
### `sci-the-ns`
<code>[ctx x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L446-L451)
### `sci-thread-bound?`
<code>[& vars]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L687-L688)
### `sci-thread-bound?`
<code>[& vars]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L687-L688)
### `sci-with-bindings`
<code>[_ _ binding-map & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L643-L645)
### `sci-with-bindings`
<code>[_ _ binding-map & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L643-L645)
### `sci-with-redefs`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L703-L708)
### `sci-with-redefs`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L703-L708)
### `sci-with-redefs-fn`
<code>[binding-map func]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L690-L701)
### `sci-with-redefs-fn`
<code>[binding-map func]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L690-L701)
### `seq-to-map-for-destructuring`
<code>[s]</code><br>

Builds a map from a seq as described in
  https://clojure.org/reference/special_forms#keyword-arguments

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L793-L800)
### `some->*`
<code>[_&form _&env expr & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L255-L264)
### `some->*`
<code>[_&form _&env expr & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L255-L264)
### `some->>*`
<code>[_ _ expr & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L266-L275)
### `some->>*`
<code>[_ _ expr & forms]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L266-L275)
### `source`
<code>[_ _ n]</code><br>

Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1544-L1551)
### `source`
<code>[_ _ n]</code><br>

Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1544-L1551)
### `source-fn`
<code>[ctx x]</code><br>

Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (source-fn 'filter)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1520-L1542)
### `source-fn`
<code>[ctx x]</code><br>

Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (source-fn 'filter)

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1520-L1542)
### `stack-element-str`
<code>[el]</code><br>

Returns a (possibly unmunged) string representation of a StackTraceElement

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L1576-L1587)
### `symbol*`
<code>[name]</code><br>
<code>[ns name]</code><br>

Returns a Symbol with the given namespace and name. Arity-1 works
  on strings, keywords, and vars.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L714-L725)
### `symbol*`
<code>[name]</code><br>
<code>[ns name]</code><br>

Returns a Symbol with the given namespace and name. Arity-1 works
  on strings, keywords, and vars.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L714-L725)
### `type`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L778-L780)
### `use`
<code>[sci-ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L580-L581)
### `use`
<code>[sci-ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L580-L581)
### `vswap!`
<code>[_ _ vol f & args]</code><br>

Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L364-L370)
### `vswap!`
<code>[_ _ vol f & args]</code><br>

Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L364-L370)
### `when*`
<code>[_ _ test & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L153-L155)
### `when*`
<code>[_ _ test & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L153-L155)
### `when-<-clojure-1.11.0`
<code>[& body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L784-L789)
### `when-first*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L241-L245)
### `when-first*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L241-L245)
### `when-let*`
<code>[_&form _&env bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L233-L239)
### `when-let*`
<code>[_&form _&env bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L233-L239)
### `when-not*`
<code>[_&form _&env test & body]</code><br>

when-not from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L157-L160)
### `when-not*`
<code>[_&form _&env test & body]</code><br>

when-not from clojure.core

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L157-L160)
### `when-some*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L247-L253)
### `when-some*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L247-L253)
### `while*`
<code>[_ _ test & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L410-L415)
### `while*`
<code>[_ _ test & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L410-L415)
### `with-bindings*`
<code>[binding-map f & args]</code><br>

Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then calls f with the supplied arguments.
  Pops the installed bindings after f returned. Returns whatever f returns.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L631-L641)
### `with-bindings*`
<code>[binding-map f & args]</code><br>

Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then calls f with the supplied arguments.
  Pops the installed bindings after f returned. Returns whatever f returns.

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L631-L641)
### `with-local-vars*`
<code>[form _ name-vals-vec & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L350-L362)
### `with-local-vars*`
<code>[form _ name-vals-vec & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L350-L362)
### `with-open*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L326-L337)
### `with-open*`
<code>[_ _ bindings & body]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/namespaces.cljc#L326-L337)
## sci.impl.reify
### `reify`
<code>[form _ _ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/reify.cljc#L6-L15)
### `reify`
<code>[form _ _ctx & args]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/reify.cljc#L6-L15)
### `reify*`
<code>[ctx form classes methods]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/reify.cljc#L17-L31)
### `reify*`
<code>[_ctx _form classes methods]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/reify.cljc#L17-L31)
## sci.addons.future
### `future*`

[Source](https://github.com/babashka/process/blob/main/src/sci/addons/future.clj#L7-L10)
### `future**`
<code>[& body]</code><br>

Macro.


Like clojure.core/future but also conveys sci bindings to the thread.

[Source](https://github.com/babashka/process/blob/main/src/sci/addons/future.clj#L12-L17)
### `install`
<code>[opts]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/addons/future.clj#L38-L47)
### `pmap`
<code>[f coll]</code><br>
<code>[f coll & colls]</code><br>

Like clojure.core/pmap but also conveys sci bindings to the threads.

[Source](https://github.com/babashka/process/blob/main/src/sci/addons/future.clj#L19-L36)
## sci.core
### `*1`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L113-L113)
### `*1`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L113-L113)
### `*2`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L114-L114)
### `*2`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L114-L114)
### `*3`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L115-L115)
### `*3`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L115-L115)
### `*e`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L116-L116)
### `*e`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L116-L116)
### `add-class!`
<code>[ctx class-name class]</code><br>

Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L440-L450)
### `add-class!`
<code>[ctx class-name class]</code><br>

Adds class (JVM class or JS object) to `ctx` as `class-name` (a
  symbol). Returns mutated context.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L440-L450)
### `add-import!`
<code>[ctx ns-name class-name alias]</code><br>

Adds import of class named by `class-name` (a symbol) to namespace named by `ns-name` (a symbol) under alias `alias` (a symbol). Returns mutated context.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L452-L457)
### `add-import!`
<code>[ctx ns-name class-name alias]</code><br>

Adds import of class named by `class-name` (a symbol) to namespace named by `ns-name` (a symbol) under alias `alias` (a symbol). Returns mutated context.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L452-L457)
### `all-ns`
<code>[ctx]</code><br>

Returns all SCI ns objects in the `ctx`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L464-L467)
### `all-ns`
<code>[ctx]</code><br>

Returns all SCI ns objects in the `ctx`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L464-L467)
### `alter-var-root`
<code>[v f]</code><br>
<code>[v f & args]</code><br>

Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L177-L183)
### `alter-var-root`
<code>[v f]</code><br>
<code>[v f & args]</code><br>

Atomically alters the root binding of sci var v by applying f to its
  current value plus any args.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L177-L183)
### `assert`

SCI var that represents SCI's clojure.core/*assert*

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L111-L111)
### `assert`

SCI var that represents SCI's clojure.core/*assert*

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L111-L111)
### `binding`
<code>[bindings & body]</code><br>

Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L87-L94)
### `binding`
<code>[bindings & body]</code><br>

Macro.


Macro for binding sci vars. Must be called with a vector of sci
  dynamic vars to values.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L87-L94)
### `cljs-ns-publics`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L341-L341)
### `cljs-ns-publics`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L350-L350)
### `copy-ns`
<code>[ns-sym sci-ns]</code><br>
<code>[ns-sym sci-ns opts]</code><br>

Macro.


Returns map of names to SCI vars as a result of copying public
  Clojure vars from ns-sym (a symbol). Attaches sci-ns (result of
  sci/create-ns) to meta. Copies :name, :macro :doc, :no-doc
  and :argslists metadata.

  Options:

  - :exclude: a seqable of names to exclude from the
  namespace. Defaults to none.

  - :copy-meta: a seqable of keywords to copy from the original var
  meta.  Use :all instead of a seqable to copy all. Defaults
  to [:doc :arglists :macro].

  - :exclude-when-meta: seqable of keywords; vars with meta matching
  these keys are excluded.  Defaults to [:no-doc :skip-wiki]

  The selection of vars is done at compile time which is mostly
  important for ClojureScript to not pull in vars into the compiled
  JS. Any additional vars can be added after the fact with sci/copy-var
  manually.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L358-L438)
### `copy-ns`
<code>[ns-sym sci-ns]</code><br>
<code>[ns-sym sci-ns opts]</code><br>

Macro.


Returns map of names to SCI vars as a result of copying public
  Clojure vars from ns-sym (a symbol). Attaches sci-ns (result of
  sci/create-ns) to meta. Copies :name, :macro :doc, :no-doc
  and :argslists metadata.

  Options:

  - :exclude: a seqable of names to exclude from the
  namespace. Defaults to none.

  - :copy-meta: a seqable of keywords to copy from the original var
  meta.  Use :all instead of a seqable to copy all. Defaults
  to [:doc :arglists :macro].

  - :exclude-when-meta: seqable of keywords; vars with meta matching
  these keys are excluded.  Defaults to [:no-doc :skip-wiki]

  The selection of vars is done at compile time which is mostly
  important for ClojureScript to not pull in vars into the compiled
  JS. Any additional vars can be added after the fact with sci/copy-var
  manually.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L358-L438)
### `copy-var`
<code>[sym ns]</code><br>
<code>[sym ns opts]</code><br>

Macro.


Copies contents from var `sym` to a new sci var. The value `ns` is an
  object created with `sci.core/create-ns`. If new-name is supplied, the 
  copied var will be named new-name.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L52-L73)
### `copy-var`
<code>[sym ns]</code><br>
<code>[sym ns opts]</code><br>

Macro.


Copies contents from var `sym` to a new sci var. The value `ns` is an
  object created with `sci.core/create-ns`. If new-name is supplied, the 
  copied var will be named new-name.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L52-L73)
### `create-ns`
<code>[sym]</code><br>
<code>[sym meta]</code><br>

Creates namespace object. Can be used in var metadata.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L249-L253)
### `create-ns`
<code>[sym]</code><br>
<code>[sym meta]</code><br>

Creates namespace object. Can be used in var metadata.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L249-L253)
### `err`

SCI var that represents SCI's `clojure.core/*err*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L99-L99)
### `err`

SCI var that represents SCI's `clojure.core/*err*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L99-L99)
### `eval-form`
<code>[ctx form]</code><br>

Evaluates form (as produced by `parse-string` or `parse-next`) in the
  context of `ctx` (as produced with `init`). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L283-L290)
### `eval-form`
<code>[ctx form]</code><br>

Evaluates form (as produced by `parse-string` or `parse-next`) in the
  context of `ctx` (as produced with `init`). To allow namespace
  switches, establish root binding of `sci/ns` with `sci/binding` or
  `sci/with-bindings.`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L283-L290)
### `eval-string`
<code>[s]</code><br>
<code>[s opts]</code><br>

Evaluates string `s` as one or multiple Clojure expressions using the Small Clojure Interpreter.

  The map `opts` may contain the following:

  - `:namespaces`: a map of symbols to namespaces, where a namespace
  is a map with symbols to values, e.g.: `{'foo.bar {'x 1}}`. These
  namespaces can be used with `require`.

  - `:bindings`: `:bindings x` is the same as `:namespaces {'user x}`.

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:bindings` or `:namespaces` have to be explicitly
  enumerated.

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb}).

  - `:env`: an atom with a map in which state from the
  evaluation (defined namespaced and vars) will be persisted for
  re-use over multiple calls.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L196-L221)
### `eval-string`
<code>[s]</code><br>
<code>[s opts]</code><br>

Evaluates string `s` as one or multiple Clojure expressions using the Small Clojure Interpreter.

  The map `opts` may contain the following:

  - `:namespaces`: a map of symbols to namespaces, where a namespace
  is a map with symbols to values, e.g.: `{'foo.bar {'x 1}}`. These
  namespaces can be used with `require`.

  - `:bindings`: `:bindings x` is the same as `:namespaces {'user x}`.

  - `:allow`: a seqable of allowed symbols. All symbols, even those
  brought in via `:bindings` or `:namespaces` have to be explicitly
  enumerated.

  - `:deny`: a seqable of disallowed symbols, e.g.: `[loop quote
  recur]`.

  - `:features`: when provided a non-empty set of keywords, sci will process reader conditionals using these features (e.g. #{:bb}).

  - `:env`: an atom with a map in which state from the
  evaluation (defined namespaced and vars) will be persisted for
  re-use over multiple calls.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L196-L221)
### `eval-string*`
<code>[ctx s]</code><br>

Evaluates string `s` in the context of `ctx` (as produced with
  `init`).

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L243-L247)
### `eval-string*`
<code>[ctx s]</code><br>

Evaluates string `s` in the context of `ctx` (as produced with
  `init`).

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L243-L247)
### `file`

SCI var that represents SCI's `clojure.core/*file*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L101-L101)
### `file`

SCI var that represents SCI's `clojure.core/*file*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L101-L101)
### `find-ns`
<code>[ctx ns-sym]</code><br>

Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L459-L462)
### `find-ns`
<code>[ctx ns-sym]</code><br>

Returns SCI ns object as created with `sci/create-ns` from `ctx` found by `ns-sym`.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L459-L462)
### `fork`
<code>[ctx]</code><br>

Forks a context (as produced with `init`) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L236-L241)
### `fork`
<code>[ctx]</code><br>

Forks a context (as produced with `init`) into a new context. Any new
  vars created in the new context won't be visible in the original
  context.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L236-L241)
### `format-stacktrace`
<code>[stacktrace]</code><br>

Returns a list of formatted stack trace elements as strings from stacktrace.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L297-L300)
### `format-stacktrace`
<code>[stacktrace]</code><br>

Returns a list of formatted stack trace elements as strings from stacktrace.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L297-L300)
### `future`
<code>[& body]</code><br>

Macro.


Like clojure.core/future but also conveys sci bindings to the thread.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L151-L156)
### `future`
<code>[& body]</code><br>

Macro.


Like clojure.core/future but also conveys sci bindings to the thread.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L151-L156)
### `get-column-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L270-L271)
### `get-column-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L270-L271)
### `get-line-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L267-L268)
### `get-line-number`
<code>[reader]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L267-L268)
### `in`

SCI var that represents SCI's `clojure.core/*in*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L97-L97)
### `in`

SCI var that represents SCI's `clojure.core/*in*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L97-L97)
### `init`
<code>[opts]</code><br>

Creates an initial sci context from given options `opts`. The context
  can be used with `eval-string*`. See `eval-string` for available
  options. The internal organization of the context is implementation
  detail and may change in the future.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L223-L229)
### `init`
<code>[opts]</code><br>

Creates an initial sci context from given options `opts`. The context
  can be used with `eval-string*`. See `eval-string` for available
  options. The internal organization of the context is implementation
  detail and may change in the future.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L223-L229)
### `intern`
<code>[ctx sci-ns name]</code><br>
<code>[ctx sci-ns name val]</code><br>

Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L185-L194)
### `intern`
<code>[ctx sci-ns name]</code><br>
<code>[ctx sci-ns name val]</code><br>

Finds or creates a sci var named by the symbol name in the namespace
  ns (which can be a symbol or a sci namespace), setting its root
  binding to val if supplied. The namespace must exist in the ctx. The
  sci var will adopt any metadata from the name symbol.  Returns the
  sci var.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L185-L194)
### `merge-opts`
<code>[ctx opts]</code><br>

Updates a context with opts merged in and returns it.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L231-L234)
### `merge-opts`
<code>[ctx opts]</code><br>

Updates a context with opts merged in and returns it.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L231-L234)
### `new-dynamic-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Same as new-var but adds :dynamic true to meta.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L31-L36)
### `new-dynamic-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Same as new-var but adds :dynamic true to meta.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L31-L36)
### `new-macro-var`
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L43-L50)
### `new-macro-var`
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Same as new-var but adds :macro true to meta as well
  as :sci/macro true to meta of the fn itself.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L43-L50)
### `new-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Returns a new sci var.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L24-L29)
### `new-var`
<code>[name]</code><br>
<code>[name init-val]</code><br>
<code>[name init-val meta]</code><br>

Returns a new sci var.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L24-L29)
### `ns`

SCI var that represents SCI's `clojure.core/*ns*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L100-L100)
### `ns`

SCI var that represents SCI's `clojure.core/*ns*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L100-L100)
### `ns-name`
<code>[sci-ns]</code><br>

Returns name of SCI ns as symbol.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L302-L305)
### `ns-name`
<code>[sci-ns]</code><br>

Returns name of SCI ns as symbol.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L302-L305)
### `out`

SCI var that represents SCI's `clojure.core/*out*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L98-L98)
### `out`

SCI var that represents SCI's `clojure.core/*out*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L98-L98)
### `parse-next`
<code>[ctx reader]</code><br>
<code>[ctx reader opts]</code><br>

Parses next form from reader

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L273-L281)
### `parse-next`
<code>[ctx reader]</code><br>
<code>[ctx reader opts]</code><br>

Parses next form from reader

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L273-L281)
### `parse-string`
<code>[ctx s]</code><br>

Parses string `s` in the context of `ctx` (as produced with
  `init`).

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L255-L259)
### `parse-string`
<code>[ctx s]</code><br>

Parses string `s` in the context of `ctx` (as produced with
  `init`).

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L255-L259)
### `pmap`
<code>[f coll]</code><br>
<code>[f coll & colls]</code><br>

Like clojure.core/pmap but also conveys sci bindings to the threads.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L158-L175)
### `print-dup`

SCI var that represents SCI's `clojure.core/*print-dup*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L107-L107)
### `print-dup`

SCI var that represents SCI's `clojure.core/*print-dup*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L107-L107)
### `print-err-fn`

SCI var that represents SCI's `cljs.core/*print-err-fn*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L109-L109)
### `print-fn`

SCI var that represents SCI's `cljs.core/*print-fn*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L108-L108)
### `print-length`

SCI var that represents SCI's `clojure.core/*print-length*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L103-L103)
### `print-length`

SCI var that represents SCI's `clojure.core/*print-length*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L103-L103)
### `print-level`

SCI var that represents SCI's `clojure.core/*print-level*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L104-L104)
### `print-level`

SCI var that represents SCI's `clojure.core/*print-level*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L104-L104)
### `print-meta`

SCI var that represents SCI's `clojure.core/*print-meta*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L105-L105)
### `print-meta`

SCI var that represents SCI's `clojure.core/*print-meta*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L105-L105)
### `print-newline`

SCI var that represents SCI's `cljs.core/*print-newline*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L110-L110)
### `print-readably`

SCI var that represents SCI's `clojure.core/*print-readably*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L106-L106)
### `print-readably`

SCI var that represents SCI's `clojure.core/*print-readably*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L106-L106)
### `read-eval`

SCI var that represents SCI's `clojure.core/*read-eval*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L102-L102)
### `read-eval`

SCI var that represents SCI's `clojure.core/*read-eval*`

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L102-L102)
### `reader`
<code>[x]</code><br>

Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L261-L265)
### `reader`
<code>[x]</code><br>

Coerces x into indexing pushback-reader to be used with
  parse-next. Accepts: string or java.io.Reader.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L261-L265)
### `set!`
<code>[dynamic-var v]</code><br>

Establish thread local binding of dynamic var

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L38-L41)
### `set!`
<code>[dynamic-var v]</code><br>

Establish thread local binding of dynamic var

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L38-L41)
### `stacktrace`
<code>[ex]</code><br>

Returns list of stacktrace element maps from exception, if available.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L292-L295)
### `stacktrace`
<code>[ex]</code><br>

Returns list of stacktrace element maps from exception, if available.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L292-L295)
### `with-bindings`
<code>[bindings-map & body]</code><br>

Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L76-L85)
### `with-bindings`
<code>[bindings-map & body]</code><br>

Macro.


Macro for binding sci vars. Must be called with map of sci dynamic
  vars to values. Used in babashka.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L76-L85)
### `with-in-str`
<code>[s & body]</code><br>

Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L122-L129)
### `with-in-str`
<code>[s & body]</code><br>

Macro.


Evaluates body in a context in which sci's *in* is bound to a fresh
  StringReader initialized with the string s.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L122-L129)
### `with-out-str`
<code>[& body]</code><br>

Macro.


Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L132-L148)
### `with-out-str`
<code>[& body]</code><br>

Macro.


Evaluates exprs in a context in which sci's *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls.

[Source](https://github.com/babashka/process/blob/main/src/sci/core.cljc#L132-L148)
## sci.lang
### `IVar`

[Source](https://github.com/babashka/process/blob/main/src/sci/lang.cljc#L4-L4)
## sci.impl.multimethods
### `defmethod`
<code>[_ _ multifn dispatch-val & fn-tail]</code><br>

Creates and installs a new method of multimethod associated with dispatch-value. 

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L102-L105)
### `defmethod`
<code>[_ _ multifn dispatch-val & fn-tail]</code><br>

Creates and installs a new method of multimethod associated with dispatch-value. 

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L102-L105)
### `defmulti`
<code>[_ _ ctx mm-name & options]</code><br>

Creates a new multimethod with the associated dispatch function.
  The docstring and attr-map are optional.

  Options are key-value pairs and may be one of:

  :default

  The default dispatch value, defaults to :default

  :hierarchy

  The value used for hierarchical dispatch (e.g. ::square is-a ::shape)

  Hierarchies are type-like relationships that do not depend upon type
  inheritance. By default Clojure's multimethods dispatch off of a
  global hierarchy map.  However, a hierarchy relationship can be
  created with the derive function used to augment the root ancestor
  created with make-hierarchy.

  Multimethods expect the value of the hierarchy option to be supplied as
  a reference type e.g. a var (i.e. via the Var-quote dispatch macro #'
  or the var special form).

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L20-L84)
### `defmulti`
<code>[_ _ ctx mm-name & options]</code><br>

Creates a new multimethod with the associated dispatch function.
  The docstring and attr-map are optional.

  Options are key-value pairs and may be one of:

  :default

  The default dispatch value, defaults to :default

  :hierarchy

  The value used for hierarchical dispatch (e.g. ::square is-a ::shape)

  Hierarchies are type-like relationships that do not depend upon type
  inheritance. By default Clojure's multimethods dispatch off of a
  global hierarchy map.  However, a hierarchy relationship can be
  created with the derive function used to augment the root ancestor
  created with make-hierarchy.

  Multimethods expect the value of the hierarchy option to be supplied as
  a reference type e.g. a var (i.e. via the Var-quote dispatch macro #'
  or the var special form).

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L20-L84)
### `multi-fn-add-method-impl`
<code>[multifn dispatch-val f]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L97-L100)
### `multi-fn-add-method-impl`
<code>[multifn dispatch-val f]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L97-L100)
### `multi-fn-impl`
<code>[name dispatch-fn default hierarchy]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L90-L95)
### `multi-fn-impl`
<code>[name dispatch-fn default hierarchy method-table prefer-table method-cache cached-hierarchy]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L90-L95)
### `multi-fn?-impl`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L86-L88)
### `multi-fn?-impl`
<code>[x]</code><br>

[Source](https://github.com/babashka/process/blob/main/src/sci/impl/multimethods.cljc#L86-L88)
