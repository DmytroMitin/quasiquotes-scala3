# Dotty-internal exact backend

`dottyInternal` is an experimental, remotely unpublished, full-crossed
production module for exact Scala compiler operations. It is normally
publishable and locally stageable for the selected Scala 3.3.8/3.8.4/3.9.0
candidate coordinates without a special property; Maven availability is not
API stability. It follows the project's 0.x compatibility policy and is not a
generic public `untpd` or `tpd` toolkit. Consumers must align with the module's
full Scala compiler version and active compiler context.

## Current public-for-JVM-access surface

Four definition-specific production objects in this module are intentionally
exposed to foreign packages. `ContextualMethodPeerBridge` accepts either the
legacy single-unbounded-parameter contextual method or the exact bounded
two-parameter bounded `Add.Out` method. The separate
`SelfAbstractTypeMemberPeerBridge` accepts only the bounded self-Type abstract
member family. `DelegatedForwardingMethodPeerBridge` accepts only the exact
one-type-parameter, ordinary-parameter, final-using-parameter
forwarder whose body calls the contextual instance with the ordinary argument.
`AuxTypeAliasPeerBridge` accepts only the exact three-parameter,
two-target-reference, one-refinement-alias family. All four require a virtual
source name and return a categorized
failure or a positioned tree with deterministic generated source and the
effective virtual source name.

The real consumed path is:

```text
scala.meta.Defn.Def
  -> package-private exact dispatch
       -> legacy ScalametaContextualMethodProjection
          -> validated project/core DefinitionResultView
          -> PublicContextualMethodUntypedBackend
          -> PublicContextualMethodGeneratedOriginAdapter
       -> exact-037 Scalameta scoped projection
          -> original ScopedContextualMethodPlan with BinderId identity
          -> ScopedContextualMethodUntypedLowerer
          -> ScopedContextualMethodGeneratedOriginAdapter
  -> ContextualMethodPeerBridge.Lowered
       -> positioned untpd.DefDef
       -> generated source
       -> virtual source name
```

The bridge is used by the narrow AUXify/Macro-Paradise integration. AUXify
authors and requests the contextual method, while Macro-Paradise owns plugin
lifecycle, companion placement and merge, insertion, rollback, and ordinary
typing. Macro-Paradise itself has no Quasiquotes or Scalameta product
dependency.

The focused API and failure contract remain documented on the
[experimental contextual-method peer bridge page](EXPERIMENTAL_CONTEXTUAL_METHOD_PEER_BRIDGE.md).
The second operation is documented separately on the
[self abstract-Type-member bridge page](SELF_ABSTRACT_TYPE_MEMBER_PEER_BRIDGE.md).
The 043 operation is documented on the
[delegated forwarding-method bridge page](DELEGATED_FORWARDING_METHOD_PEER_BRIDGE.md).
The bounded Type-alias operation is documented on the
[Type-alias bridge page](TYPE_ALIAS_PEER_BRIDGE.md).

## Internal module inventory

Only the documented foreign-package bridges above are intended consumer seams.
All other production owners are package-private or otherwise project-internal
and carry no stable compatibility promise:

- `CoreTermShapeUntypedLowerer` lowers the accepted non-binder Core family:
  canonical Int/String/Boolean literals, recursive ordinary infix and unary
  nodes, tuples, explicit conditionals, direct identifiers, recursive
  selections, and exactly one ordinary positional Apply list. It also lowers
  transparent P0 and binder-free P1 blocks. It validates the bounded literal,
  operator, and ASCII non-keyword name sets, rejects placeholders and a direct
  Apply in function position, and audits no source, no span, `NoSymbol`, and
  no `TypedSplice` recursively.
- `ConstructedTermUntypedBackend` and `CompletedTypeUntypedLowerer` lower the
  admitted compiler-free Term/Type models to source-free raw trees. The richer
  Term backend also accepts binder-free P1 blocks with deterministic
  left-to-right sidecar consumption, one bounded P2 local-val block, and one
  bounded P3 local identity-method block. P2 consumes its completed declared-
  Type sidecar before initializer sidecars, installs the existing BinderId only
  after the initializer, and restores the incoming scope at Block exit. P3
  consumes authoritative completed parameter/result Types, gives the body the
  parameter binder but not the method binder, gives the following result the
  method binder but not the parameter binder, and restores the incoming scope.
  Its generated-origin adapter emits parser-equivalent positioned P2 and P3
  topology; the narrower direct lowerer still rejects both.
- `ExistingUntpdMethodBodyRewriter` and its origin adapter are the distinct
  U-style existing-tree direction. The internal `adapt`, `adaptApply`, and
  `adaptSelectedApply` paths rebuild only bounded method-body/container shapes,
  preserve untouched raw objects by identity, and attribute fresh replacement
  nodes to truthful transformation sites. The selected-member Apply slice is
  limited to a direct `Ident` qualifier, a term-name member, and one to three
  leaf arguments. Its granular path selects one existing leaf argument by exact
  identity and replaces it with either one source-free leaf or one bounded
  direct-`Ident` Apply whose one to three arguments are direct leaves. The
  preserved function and untouched arguments retain exact identity; every
  fresh node in the child-bearing replacement receives the old argument site.
  These paths are not D-style new-syntax lowering and are not public APIs.
- `ConstructedDefinitionUntypedBackend` and
  `PublicContextualMethodUntypedBackend` construct bounded raw definitions.
- `ScopedContextualMethodUntypedLowerer` and its generated-origin adapter own
  only the exact bounded two-binder/refinement raw shape.
- `SelfAbstractTypeMemberUntypedLowerer` and its generated-origin adapter own
  only the exact self-Type-member nine-node `untpd.TypeDef` shape.
- `DelegatedForwardingMethodUntypedLowerer` and its generated-origin adapter
  own only the exact delegated-forwarding 14-node `untpd.DefDef` shape and its complete
  generated-source position layout.
- `AuxTypeAliasUntypedLowerer` and its generated-origin adapter own only the
  exact bounded refined-alias 18-node `untpd.TypeDef` shape. The private
  `AuxTypeAliasPlanUntypedInputAdapter` copies plan BinderIds and spellings into
  the backend input without allocating or inferring identities.
- `ScalametaContextualMethodBackend` is an internal bounded forward/reverse
  adapter for the contextual-method shape.
- the generated-origin adapters, result carriers, fragment planner,
  interpolation encoder, and position validators create deterministic virtual
  source and complete truthful spans for admitted generated trees.
- the error ADTs keep internal failure boundaries deterministic without
  widening the foreign-package bridge.

`ConstructedDefinitionGeneratedOriginAdapter`, its single- and two-parameter
specializations, and the public-contextual-method generated-origin adapter
combine raw shapes with planned source provenance. The large
`GeneratedOriginFragmentSupport` owner performs the reusable term/type fragment
rendering, structural planning, positioning, and validation.

## Test-only exact-compiler evidence

Tests demonstrate bounded capabilities that are not production APIs:

- current `q.reflect.Term` values are implemented by `tpd.Tree`, and
  `tpd.applyOverloaded` can construct the typed addition on the exact compiler
  lines;
- `untpd.TypedSplice` can embed typed leaves in a newly constructed untyped
  shell, after which `Typer.typedExpr` produces a typed result.
- the production neutral projector composes with
  `CoreTermShapeUntypedLowerer` for the bounded non-binder and P0/P1 family,
  and the resulting source-free raw structure agrees with independent parser
  oracles on Scala 3.3.8, 3.8.4, and final 3.9.0.
- ordinary Typer accepts the source-free Identifier/Select/Apply results over
  declared fixture names, including empty, one-argument, and multi-argument
  calls, without manufactured positions.

Direct `Typer.typedExpr` on a span-free `untpd.InfixOp` is not supported by the
tested compiler lines: Dotty's infix desugaring reads the left/operator spans.
The exact viability probe therefore uses a test-only source-free recursive
`Apply(Select(...), ...)` shell before typing and proves `Int` type/value
evidence. Production still returns the parser-equivalent span-free `InfixOp`.

These probes do not authorize manual `ExprImpl` construction, a general
`tpd.Tree -> untpd.Tree` inverse, or a stable bridge between arbitrary compiler
contexts.

## Deliberate exclusions

There is no production public bridge from arbitrary `scala.meta.Term` to
`untpd.Tree`, no public neutral projector from arbitrary `scala.meta.Term`, no
generic raw-tree family, no placement service, and no stable published
coordinate for this module today. A future candidate Maven coordinate does not
widen those API boundaries or stabilize the internal machinery. The
package-private production composition admits the bounded accepted
Int/String/Boolean literal, infix, unary, tuple, conditional, direct
identifier/selection, one-list ordinary Apply, and transparent P0/binder-free
P1 families through core `TermShape`.
Typed Scalameta Term traversal instead belongs to the separate unpublished
`hybridScalametaFrontend` and returns caller-owned `q.reflect.Term`.

The direct-lowerer support is bounded to new source-free D construction from
project-owned `TermShape`; the separate U rewriter above owns limited existing-
tree identity/reconstruction semantics. Neither direction introduces a cross-
surface capability layer. `new`, interpolation, ascription, Lambda1 in the
direct lowerer, P2/P3 statement binders in the direct lowerer, and other raw
Term families remain explicit separate slices. The richer backend's P2/P3
support does not widen the direct lowerer because the latter has no
authoritative completed-Type sidecars. None of the
definition-specific bridges widens that boundary.
