# Dotty-internal exact backend

`dottyInternal` is an experimental, remotely unpublished, full-crossed
production module for exact Scala compiler operations. It is normally
publishable and locally stageable for the selected Scala 3.3.8/3.8.4/3.9.0
candidate coordinates without a special property; Maven availability is not
API stability. It follows the project's 0.x compatibility policy and is not a
generic public `untpd` or `tpd` toolkit. Consumers must align with the module's
full Scala compiler version and active compiler context.

The canonical [semantic models and conversions guide](SEMANTIC_MODELS_AND_CONVERSIONS.md)
defines the three representations before the lane labels: **U-D** means exact
fresh lowering into new `untpd` syntax, while **U-U** means exact existing-tree
transformation with explicit identity/provenance promises. **C** owns the
cross-layer API policy and is not another AST. Neither U direction implies a
public `u*` syntax family.

## Current public exact-version surface

`ScalametaTermUntypedBridge` is the public programmatic Term facade. It accepts
one `scala.meta.Term`, mechanically calls `ScalametaTermProjection.project`,
then passes the projected `TermShape` to the package-private
`CoreTermShapeUntypedLowerer.lower`. It returns either a stable categorized
failure or a fresh source-free raw tree. Its exact admitted intersection,
negative families, provenance contract, and Typer characterization are
documented on the
[bounded Scalameta Term bridge page](SCALAMETA_TERM_UNTYPED_BRIDGE.md).

```text
scala.meta.Term
  -> ScalametaTermProjection.project
  -> core TermShape
  -> CoreTermShapeUntypedLowerer.lower
  -> source-free untpd.Tree
```

The public failure codes distinguish missing input, neutral projection failure,
and exact lowering failure. There is no rendering, reparsing, fallback, richer
backend routing, provenance synthesis, typing, symbol creation, ownership, or
placement service.

`ScalametaTermGeneratedOriginBridge` is the separate insertion-oriented Term
facade. It projects through the same neutral authority, completes the projected
shape with `ConstructedTerm.fromShape`, and routes only that completed value
through the package-private generated-origin adapter. It returns a positioned
`untpd.Tree`, deterministic generated source, and the effective virtual
`SourceFile`.

```text
scala.meta.Term
  -> ScalametaTermProjection.project
  -> ConstructedTerm.fromShape
  -> ConstructedTermGeneratedOriginAdapter.lower
  -> positioned untpd.Tree + generated source + SourceFile
```

This path includes the direct source-free intersection and, when its bounded
Types can be completed, Type ascription, Lambda1, P2 local val, and the P3
local identity-method block. It reports `MISSING_INPUT`,
`NEUTRAL_PROJECTION_FAILED`, `TERM_COMPLETION_FAILED`,
`INVALID_VIRTUAL_SOURCE`, or defensive `GENERATED_ORIGIN_FAILED`. It does not
widen neutral projection or change the source-free facade. See the
[bounded Scalameta Term generated-origin bridge page](SCALAMETA_TERM_GENERATED_ORIGIN_BRIDGE.md).

`ScalametaTypeUntypedBridge` is the context-free public sibling for the
bounded Type intersection. It composes
`ScalametaTypeNormalFormProjection.project` with the package-private
`CompletedTypeUntypedLowerer.lower` and returns a categorized failure or a
fresh source-free raw Type tree. Its recursive Int/String/Boolean,
List/Option/Either, Tuple2/3-syntax, and Function1/2-syntax boundary is
documented on the
[bounded Scalameta Type bridge page](SCALAMETA_TYPE_UNTYPED_BRIDGE.md).
It performs no TupleN/FunctionN name recovery, resolution, fallback, typing,
symbol creation, ownership, or placement.

`ScalametaDefinitionUntypedBridge` is the public exact-version source-free
sibling for the reusable Definition category. It accepts `scala.meta.Defn`,
mechanically composes the common neutral projector with the package-private
Definition-shape lowerer, verifies that the result is an `untpd.MemberDef`, and
returns stable missing-input, projection, or exact-lowering diagnostics. Its
closed family set is one explicitly typed immutable `val`, one true
parameterless explicitly typed `def`, one explicitly typed `def` with one
ordinary parameter, one with exactly two ordinary parameters, and one simple
non-generic unbounded Type alias.

`ScalametaDefinitionGeneratedOriginBridge` is a separate public exact-version
operation for insertion clients. It admits only the four concrete val/def
families, completes their project-owned semantics, and returns an
`untpd.MemberDef`, deterministic generated source, and effective virtual
`SourceFile`. A simple Type alias fails with
`GENERATED_ORIGIN_FAMILY_UNSUPPORTED`; this route does not borrow authority
from the specialized refined-alias bridge. The complete boundary, failure
codes, and placement responsibilities are documented on the
[bounded Scalameta Definition bridge page](SCALAMETA_DEFINITION_BRIDGES.md).

```text
scala.meta.Defn
  -> ScalametaDefinitionProjection.project
  -> DefinitionShape
  -> DefinitionShapeUntypedLowerer.lower
  -> fresh source-free untpd.MemberDef

scala.meta.Defn (four concrete val/def families)
  -> ScalametaDefinitionProjection.project
  -> ConstructedDefinition.fromShape
  -> ConstructedDefinitionGeneratedOriginAdapter.lower
  -> positioned untpd.MemberDef + generated source + SourceFile
```

`ScalametaDefinitionClassMemberAppendBridge` is the public bounded hybrid seam
for consumers that already hold one admitted pre-Typer ordinary class. It first
uses `ScalametaDefinitionGeneratedOriginBridge` unchanged, then passes that
exact positioned `DefDef` or `ValDef` to the package-private
`ExistingUntpdClassMemberAppender`. It returns the rebuilt class, the exact
appended member, and the unchanged generated-source metadata.

```text
existing pre-Typer ordinary class + scala.meta.Defn + virtual source name
  -> ScalametaDefinitionGeneratedOriginBridge.lower
  -> exact positioned DefDef or ValDef
  -> ExistingUntpdClassMemberAppender.append
  -> rebuilt TypeDef + exact appended member + generated source + SourceFile
```

The result intentionally has mixed provenance. Every old direct member remains
the exact original object at its original source, the new member retains its
generated virtual source, and only the changed Template/class shells are fresh
at the original same-site replacement source and span. Public failures are
stage-oriented: `GENERATED_DEFINITION_FAILED` or
`EXISTING_CLASS_APPEND_FAILED`, with the upstream stable code retained in the
detail. The bridge appends exactly one member last, admits a final body size no
greater than 64, and performs no lifecycle, target selection, rollback, typing,
owner repair, multi-member insertion, or arbitrary-index editing. See the
[bounded hybrid append guide](SCALAMETA_DEFINITION_CLASS_MEMBER_APPEND_BRIDGE.md).

All Term and Definition operations other than the context-free Type bridge
require an active Dotty `Context`. None performs target admission, insertion,
rollback, typing, symbol ownership, or reownership.

These current public bridges start from Scalameta syntax. Stable public facades
that start directly from public project semantic values—including
`TermShape`, `TypeNormalForm`, and `SemanticDefinition`—are **planned**, not
current. The implementation lowerers remain package-private and must not be
treated as consumer APIs.

Five definition-specific production objects in this module are intentionally
exposed to foreign packages. `ContextualMethodPeerBridge` accepts either the
legacy single-unbounded-parameter contextual method or the exact bounded
two-parameter bounded `Add.Out` method. The separate
`SelfAbstractTypeMemberPeerBridge` accepts only the bounded self-Type abstract
member family. `DelegatedForwardingMethodPeerBridge` accepts only the exact
one-type-parameter, ordinary-parameter, final-using-parameter
forwarder whose body calls the contextual instance with the ordinary argument.
`AuxTypeAliasPeerBridge` accepts only the exact three-parameter,
two-target-reference, one-refinement-alias family. `InstanceFactoryPeerBridge`
accepts only the complete bounded generic factory with a by-name carrier, a
binary-function carrier, one matching anonymous parent, and two ordered
overrides. All five require a virtual
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

The bounded instance-factory path is separately composed as:

```text
scala.meta.Defn.Def
  -> ScalametaInstanceFactoryProjection
  -> package-private InstanceFactoryPlan
  -> InstanceFactoryPlanUntypedLowerer
  -> InstanceFactoryGeneratedOriginAdapter
  -> InstanceFactoryPeerBridge.Lowered
       -> positioned untpd.DefDef
       -> generated source
       -> virtual source name
```

The projector is the sole authority for the factory grammar and binder roles;
the exact backend does not repeat a spelling-based semantic validator. The
bridge returns only after the complete raw tree and generated-origin gates
pass, so malformed input never yields a partial factory.

The focused API and failure contract remain documented on the
[experimental contextual-method peer bridge page](EXPERIMENTAL_CONTEXTUAL_METHOD_PEER_BRIDGE.md).
The second operation is documented separately on the
[self abstract-Type-member bridge page](SELF_ABSTRACT_TYPE_MEMBER_PEER_BRIDGE.md).
The 043 operation is documented on the
[delegated forwarding-method bridge page](DELEGATED_FORWARDING_METHOD_PEER_BRIDGE.md).
The bounded Type-alias operation is documented on the
[Type-alias bridge page](TYPE_ALIAS_PEER_BRIDGE.md).
The bounded instance-factory operation is documented on the
[instance-factory bridge page](INSTANCE_FACTORY_PEER_BRIDGE.md).

## Internal module inventory

Only the documented public Term/Type/Definition facades and foreign-package definition
bridges above are intended consumer seams. All other production owners are
package-private or otherwise project-internal and carry no stable compatibility
promise:

- `CoreTermShapeUntypedLowerer`, behind the public bounded Term facade, lowers
  the accepted non-binder Core family:
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
- `ConstructedDefinitionUntypedBackend` constructs the reusable immutable-val,
  true-parameterless-def, one-ordinary-parameter-def, and
  two-ordinary-parameter-def raw families;
  `PublicContextualMethodUntypedBackend` owns its separate bounded method.
  Neither is a general simple-Type-alias route.
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

Accepted package-private U-U mechanisms can perform bounded method-body and
class/Template transformations over existing raw graphs, preserving exact
objects where their contracts say so. The accepted single-parameter method
result-Type seam replaces only an explicit `Int`, `String`, or `Boolean` result
leaf, preserving the original parameter, parameter Type, RHS, non-target
members, and opaque owner children by exact identity while attributing fresh
shells and the leaf to their exact transformation sites. Parameter-Type rewrite
is a separate later capability. A future public programmatic exact
capture/view/rewrite algebra was selected architecturally, but no general
public exact-U transformation API exists today; optional `u*` syntax remains a
later decision.

There is no production public bridge from arbitrary `scala.meta.Term` to
`untpd.Tree`: the named public facade admits only the documented direct
intersection. There is no generic raw-tree family, no placement service, and
no stable published coordinate for this module today. A future candidate Maven
coordinate does not widen those API boundaries or stabilize the internal
machinery. The public bounded composition admits the accepted
Int/String/Boolean literal, infix, unary, tuple, conditional, direct
identifier/selection, one-list ordinary Apply, and transparent P0/binder-free
P1 families through core `TermShape`.
Typed Scalameta Term traversal instead belongs to the separate unpublished
`hybridScalametaFrontend` and returns caller-owned `q.reflect.Term`.

The direct-lowerer support is bounded to new source-free D construction from
project-owned `TermShape`; the separate U rewriter above owns limited existing-
tree identity/reconstruction semantics. The facade admits the existing direct
`new` and standard-`s` interpolation slices, but rejects ascription, Lambda1,
P2/P3 statement binders, and other raw Term families. The richer backend's
P2/P3 support does not widen the facade because the direct route has no
authoritative completed-Type sidecars. None of the definition-specific bridges
widens that boundary.
