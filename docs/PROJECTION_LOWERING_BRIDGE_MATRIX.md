# Projection, lowering, and bridge pipelines

This document is the concrete API inventory for the repository's independent
compiler-free and exact-compiler directions. It complements the
[syntax support matrix](SYNTAX_SUPPORT_MATRIX.md), which describes user-facing
quasiquote syntax, and the
[cross-surface capability matrix](CROSS_SURFACE_CAPABILITY_MATRIX.md), which
compares semantic families across directions.

A row here describes a real composition boundary. It does not imply that a
broader projector can fall back to a richer lowerer, that an internal API is
public, or that a source-visible API is available from a remotely released
artifact.

## Vocabulary and ownership

- **Projection** converts an external source AST into a project-owned semantic
  representation without using the Scala compiler. The usual direction is
  `scala.meta` AST to `TermShape`, `TypeNormalForm`, `DefinitionShape`, or a
  bounded semantic plan. N owns projection semantics. Projection is not exact
  Dotty raw-tree construction.
- **Authoring** is the compiler-free reverse direction for admitted families:
  a project-owned semantic value becomes a fresh `scala.meta` AST. N owns
  authoring semantics. Authoring is not exact Dotty lowering.
- **Lowering** converts an already validated project-owned semantic value or
  bounded plan into a lower-level target. U owns exact fresh lowering into
  `untpd`. A source-facing method that accepts Scalameta and performs both
  projection and lowering is a bridge, not the semantic lowerer itself.
- **Rewrite** transforms an existing exact raw tree while preserving or
  replacing exact objects according to a bounded identity, provenance, and
  origin contract. U owns this U-U direction. It remains distinct from U-D
  fresh lowering.
- **Bridge** composes layers behind one integration operation, normally
  `scala.meta AST -> projection -> project semantic value -> exact lowering`.
  C owns the cross-layer contract, composed diagnostics, public-boundary
  decision, peer lifecycle, and programme-level documentation. Exact-version
  bridge code normally lives in `dotty-internal`; there is no C product module.
- **Typed frontend** means the Q-owned, Quotes-aware `qr`/`qq`, `tqr`/`tqq`,
  and `dqr`/`dqq` construction and matching interpreters. These are not aliases
  for N projection, N authoring, U lowering, or U rewriting.

Ownership follows semantics, not class-name suffixes. In particular, not every
class named `Bridge` is physically implemented in a C module, and `Lowerer` is
not a universal lexical ownership marker.

## Status vocabulary

- `PUBLIC` — an accepted externally callable source API exists. Exact-version
  and remote-release qualifications still apply.
- `INTERNAL_READY` — an accepted reusable implementation exists behind a
  package-private boundary.
- `SPECIALIZED` — an accepted bounded integration exists for one concrete
  consumer family rather than the generic category.
- `IN_PROGRESS` — selected work is not yet an accepted current capability.
- `PLANNED` — the boundary is a future composition target with no current API.
- `NOT_SUPPORTED` — the shape lies outside the current semantic model or the
  named pipeline's admitted intersection.

## Generic category pipelines

| Category | Source input | Projection (N) | Projected semantic value | Lowerer (U-D) | Exact output | Composed bridge | Visibility and `Context` | Status | Important boundary |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Terms | `scala.meta.Term` | `quasiquotes.neutral.ScalametaTermProjection.project` | public `ProjectedTermShape`; project-owned `TermShape` plus truthful optional root span | package-private `quasiquotes.terms.dotty.CoreTermShapeUntypedLowerer.lower` | fresh recursively source-free `untpd.Tree` | `quasiquotes.terms.dotty.ScalametaTermUntypedBridge.lower` | projector and bridge are public source APIs; lowerer is package-private; lowering requires a Dotty `Context` | `PUBLIC` | The bridge is only the direct projector/lowerer intersection. It has no richer-backend fallback: ascription and Lambda1 project but fail exact lowering; binder-bearing local-val and local-def blocks also fail this direct route. Multiple argument lists, type arguments, named/repeated/contextual arguments, broader `new`, and unsupported syntax fail projection. |
| Types | `scala.meta.Type` | `quasiquotes.neutral.ScalametaTypeNormalFormProjection.project` | public `ProjectedTypeNormalForm`; project-owned `TypeNormalForm` plus truthful optional root span | package-private `quasiquotes.terms.dotty.CompletedTypeUntypedLowerer.lower` | fresh recursively source-free `untpd.Tree` representing a Type | `quasiquotes.types.dotty.ScalametaTypeUntypedBridge.lower` | projector and bridge are public source APIs; lowerer is package-private; no Dotty `Context` is required | `PUBLIC` | The exact intersection is `Int`/`String`/`Boolean`, fixed recursive `List`/`Option`/`Either`, Tuple2/3 syntax, and Function1/2 syntax. Source tuple/function syntax is preserved; names such as `Tuple2` and `Function1` are not reverse-recognized. A wider neutral success such as `AnyVal` remains an exact-lowering failure. |
| Definitions | supported `scala.meta.Defn` root | package-private `quasiquotes.neutral.ScalametaDefinitionProjection.project` | package-private `ProjectedDefinitionShape`; project-owned `DefinitionShape` plus truthful optional root span | package-private `quasiquotes.definitions.dotty.DefinitionShapeUntypedLowerer.lower` | fresh source-free `untpd.MemberDef` (`ValDef`, `DefDef`, or `TypeDef`) | `quasiquotes.definitions.dotty.ScalametaDefinitionUntypedBridge.lower` | bridge is a public source API; projector and lowerer remain package-private; lowering requires a Dotty `Context` | `PUBLIC` | Exactly five reusable families. The separate public `ScalametaDefinitionGeneratedOriginBridge` admits only the four concrete val/def families and returns deterministic generated provenance. Classes, traits, objects, generic/bounded aliases, broader clauses, modifiers, defaults, and arbitrary bodies remain unsupported. |

The public Term, Type, and Definition bridges expose stable `Failure(code, detail)`
boundaries and classify projection and exact-lowering failures separately.
Their outputs are fresh exact-version raw syntax with no fabricated typed
symbols or source provenance. Consumers still own placement and ordinary
compiler lifecycle.

### Definition families in the generic seam

The accepted internal Definition projector dispatches exactly these semantic
families and returns each family projector's result unchanged:

| Family | Projected semantic variant | Projection status | General exact status |
| --- | --- | --- | --- |
| Explicitly typed immutable `val` | `DefinitionShape.ImmutableVal` | `INTERNAL_READY` | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| True parameterless explicitly typed `def` | `DefinitionShape.ParameterlessDef` | `INTERNAL_READY` | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| One ordinary explicitly typed parameter in one clause | `DefinitionShape.SingleParameterDef` | `INTERNAL_READY` | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| Two ordinary explicitly typed parameters in one clause | `DefinitionShape.TwoParameterDef` | `INTERNAL_READY` | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| Simple non-generic unbounded Type alias | `DefinitionShape.SimpleTypeAlias` | `INTERNAL_READY` | source-free `PUBLIC`; generic generated-origin `NOT_SUPPORTED` |

The source-free public bridge selects the common exact lowerer for all five
families. The generated-origin public bridge deliberately routes only the four
ordinary val/def families through `ConstructedDefinition.fromShape` and the
package-private generated-origin adapter. The simple alias is rejected before
completion; specialized refined-alias authority does not widen this generic
category.

## Specialized bounded pipelines

Specialized bridges can be complete before the generic Definition category
because their plans, topology, provenance, diagnostics, and consumer contracts
are narrower and richer than `DefinitionShape`.

| Pipeline | Source and projection | Semantic authority | Exact path and bridge | Output | Status | Why it remains specialized |
| --- | --- | --- | --- | --- | --- | --- |
| Contextual method | bounded `scala.meta.Defn.Def` through the contextual-method dispatch/projectors | `DefinitionResultView` or `ScopedContextualMethodPlan`, depending on the admitted route | contextual generated-origin adapters behind public-for-JVM `ContextualMethodPeerBridge.lower` | positioned `untpd.DefDef` plus deterministic generated source | `SPECIALIZED` | Exactly one admitted generic/contextual method family; the caller owns placement, insertion, rollback, and ordinary typing. |
| Instance factory | package-private `ScalametaInstanceFactoryProjection.project` | package-private `InstanceFactoryPlan` with explicit binder and role semantics | `InstanceFactoryPlanUntypedLowerer` plus generated-origin adapter behind public-for-JVM `InstanceFactoryPeerBridge.lower` | positioned `untpd.DefDef` plus deterministic generated source | `SPECIALIZED` | Models one anonymous implementation factory with fixed member and binder roles; it is not a general Definition carrier. |
| Refined AUX Type alias | package-private `ScalametaAuxTypeAliasProjection.project` plus explicit expectations | package-private `AuxTypeAliasPlan` with three Type binders, bounds, target, and refinement member | plan adapter and alias lowerer plus generated-origin adapter behind public-for-JVM `AuxTypeAliasPeerBridge.lower` | positioned `untpd.TypeDef` plus deterministic generated source | `SPECIALIZED` | Its parameter, bound, refinement, provenance, and peer-diagnostic contract is deliberately richer than a simple non-generic unbounded alias. |

These exact-version bridges require a Dotty `Context`. Their public-for-JVM
visibility supports tightly coupled compiler/plugin consumers; it does not make
them stable general quasiquote syntax or remove the exact compiler-version
coupling.

## Fresh lowering versus existing-tree rewrite

The generic and specialized tables above describe U-D fresh construction.
U-U rewriting starts from an existing `untpd` graph and has a different
correctness question:

```text
existing untpd tree
  -> select an admitted original node by exact identity
  -> preserve or replace bounded children under the stated provenance rule
  -> rewritten untpd tree
```

An internal existing-tree rewrite result does not imply that N can project the
same syntax, that U-D can freshly lower it, or that a public bridge exists.
Conversely, a public Scalameta-to-fresh-tree bridge says nothing about
identity-preserving rewrites of existing compiler trees.

## Maintenance rule

When an accepted change adds, removes, renames, or widens a reusable N
projection or authoring boundary, a U-D lowerer, a U-U rewrite boundary, or a
C-composed bridge represented here, the accepting integration review must
audit this document before public documentation drift can be considered
closed. Selected, test-only, or pending work must remain visibly distinct from
accepted current capability.
