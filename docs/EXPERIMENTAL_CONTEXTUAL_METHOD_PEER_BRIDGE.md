# Experimental contextual-method peer bridge

This is the focused API page for the contextual-method foreign-package surface
inside the broader [Dotty-internal exact backend](DOTTY_INTERNAL_BACKEND.md).
The separate bounded TypeDef operation has its own
[self abstract-Type-member bridge page](SELF_ABSTRACT_TYPE_MEMBER_PEER_BRIDGE.md).

`quasiquotes.definitions.dotty.ContextualMethodPeerBridge` is a
public-for-JVM-access entry point in the remotely unpublished `dottyInternal`
artifact. It exists for tightly coupled peer integrations such as a
Macro-Paradise-loaded AUXify handler that cannot truthfully claim a
`quasiquotes.*` friend package.

The bridge accepts exactly two Scalameta `Defn.Def` families plus a validated
virtual source name:

- the legacy one-unbounded-Type-parameter, one-`using`-parameter contextual
  method, such as `def apply[A](using inst: Show[A]): Show[A] = inst`;
- the bounded `Add.Out` method with exactly two upper-bounded Type
  parameters, an ordered applied contextual Type, one direct contextual-binder
  selected-Type refinement alias, and the contextual binder as its body:

  ```scala
  def apply[N <: Nat, M <: Nat](using inst: Add[N, M]):
    Add[N, M] { type Out = inst.Out } = inst
  ```

It returns either a deterministic code/detail failure or a result containing
exactly:

- a positioned `dotty.tools.dotc.ast.untpd.DefDef`;
- the deterministic generated source;
- the effective virtual source name.

The returned tree has complete generated-origin spans and remains `NoSymbol`
before ordinary typer. The legacy route delegates through the existing public
neutral projection and validated `DefinitionResultView`. The exact-037 route
uses a package-private neutral projection with project `BinderId` identity and
passes its original validated scoped plan to the package-private exact raw and
generated-origin lowerers. A two-Type-parameter shape never falls back to the
legacy name-only projection after a partial 037 match. Neither internal model
is copied into generated source or exposed publicly.

This API is compiler-internal and exact-Scala-version coupled. A consumer must
use the same full Scala compiler version as the artifact; the foreign-package
product fixture has been verified on Scala 3.3.8, 3.8.4, and final 3.9.0, while
the disposable live AUXify build proof is specifically Scala 3.8.4.
`dottyInternal` is a normally publishable production project, so its selected
3.3.8, 3.8.4, and 3.9.0 candidate coordinates need no special property for
task-owned local staging. No remote coordinate containing this bridge exists
unless a later release gate explicitly authorizes one.

The 037 admission does not include arbitrary Type-parameter arity, bounds,
applications, refinements, stable paths, bodies, clauses, modifiers, or other
Definition kinds. The bridge is not a `uqr`/`uqq` family, a generic
`untpd.Tree` lowerer, a typed `tpd` API, or a placement service. Companion
ownership, insertion, validation, rollback, diagnostics, and ordinary typing
remain with the peer handler and Macro-Paradise. Existing public `qr`/`qq`
imports continue to use the current Dotty frontend unchanged.
