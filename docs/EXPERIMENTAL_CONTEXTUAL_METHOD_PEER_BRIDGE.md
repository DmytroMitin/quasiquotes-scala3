# Experimental contextual-method peer bridge

This is the focused API page for the only foreign-package production surface
inside the broader [Dotty-internal exact backend](DOTTY_INTERNAL_BACKEND.md).

`quasiquotes.definitions.dotty.ContextualMethodPeerBridge` is the single
public-for-JVM-access entry point in the remotely unpublished `dottyInternal`
artifact. It exists for tightly coupled peer integrations such as a
Macro-Paradise-loaded AUXify handler that cannot truthfully claim a
`quasiquotes.*` friend package.

The bridge accepts exactly a Scalameta `Defn.Def` in the already admitted
one-type-parameter, one-`using`-parameter contextual-method shape plus a
validated virtual source name. It returns either a deterministic code/detail
failure or a result containing exactly:

- a positioned `dotty.tools.dotc.ast.untpd.DefDef`;
- the deterministic generated source;
- the effective virtual source name.

The returned tree has complete generated-origin spans and remains `NoSymbol`
before ordinary typer. The bridge delegates to the existing neutral Scalameta
projection, validated public/core definition result, raw contextual-method
lowerer, generated-source planner, and position validator. It does not copy or
expose those internal models.

This API is compiler-internal and exact-Scala-version coupled. A consumer must
use the same full Scala compiler version as the artifact; the foreign-package
lane has been verified only on Scala 3.8.4. The source build keeps
`dottyInternal / publish / skip := true`, and no remote coordinate containing
this bridge exists unless a later release gate explicitly authorizes one.

The bridge is not a `uqr`/`uqq` family, a generic `untpd.Tree` lowerer, a typed
`tpd` API, or a placement service. Companion ownership, insertion, validation,
rollback, diagnostics, and ordinary typing remain with the peer handler and
Macro-Paradise. Existing public `qr`/`qq` imports continue to use the current
Dotty frontend unchanged.
