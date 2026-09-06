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

The lane labels are defined before the matrix:

| Label | Meaning |
| --- | --- |
| Q | Quotes-aware typed quasiquote/frontend world. |
| N | Neutral/compiler-free project semantics plus Scalameta interoperability; not public `n*` syntax. |
| U-D | **Exact fresh lowering** into new Dotty `untpd` syntax. |
| U-U | **Exact existing-tree transformation** with bounded raw identity/provenance guarantees. |
| C | Cross-layer composition, integration, API policy, and controller ownership; not another AST. |

U does not mean public `u*` syntax. No such syntax is selected today; it remains
a later optional layer. See the canonical
[semantic models and conversions guide](SEMANTIC_MODELS_AND_CONVERSIONS.md) for
the three representation worlds and loss model.

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

### Public semantic model and reverse-authoring status

| Category | Public semantic value | Scalameta Projection | Scalameta Authoring |
| --- | --- | --- | --- |
| Term | `TermShape`, with opaque binder-safe views/builders | Public, bounded | Public, bounded, fresh `Position.None`; includes accepted ascription, Lambda1, P2, and P3 families; grouping parentheses are not representable |
| Type | `TypeNormalForm` | Public, bounded | Public, bounded, fresh `Position.None` |
| Definition | `SemanticDefinition`, with smart constructors and typed views | Public `ScalametaDefinitionProjection.project(Defn)`, bounded to five families | Public `ScalametaDefinitionAuthoring.author(SemanticDefinition)`, bounded to five families and fresh `Position.None` |

### Public project-semantic source-free lowering

| Category | Public input | Public facade | `Context` | Exact output | Important boundary |
| --- | --- | --- | --- | --- | --- |
| Term | `TermShape` | `quasiquotes.terms.dotty.TermUntypedLowering.lower` | Required | fresh source-free `untpd.Tree` | Uses the richer completed-Term route, including admitted binder-safe Lambda1, P2, and P3 values. It is not generated-origin lowering, typing, or existing-tree rewriting. |
| Type | `TypeNormalForm` | `quasiquotes.types.dotty.TypeUntypedLowering.lower` | Not required | fresh source-free raw Type tree | Context-free bounded recursive Int/String/Boolean, List/Option/Either, tuple, and function intersection. |
| Definition | `SemanticDefinition` | `quasiquotes.definitions.dotty.DefinitionUntypedLowering.lower` | Required | fresh source-free `untpd.MemberDef` | Uses a private semantic adapter and private five-family exact lowerer without exposing their carriers. |

All three return stable facade-owned failures. None claims source recovery,
generated origin, owner/placement authority, typechecking, retyping, or
transformation of an existing raw tree.

The current public exact-version conveniences in the next table start from
Scalameta. Their delegation policy is category-specific rather than assumed
from similar names.

| Category | Source input | Projection (N) | Projected semantic value | Lowerer (U-D) | Exact output | Composed bridge | Visibility and `Context` | Status | Important boundary |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Terms (source-free) | `scala.meta.Term` | `quasiquotes.neutral.ScalametaTermProjection.project` | public `ProjectedTermShape`; project-owned `TermShape` plus truthful optional root span | package-private `quasiquotes.terms.dotty.CoreTermShapeUntypedLowerer.lower` | fresh recursively source-free `untpd.Tree` | `quasiquotes.terms.dotty.ScalametaTermUntypedBridge.lower` | projector and bridge are public source APIs; lowerer is package-private; lowering requires a Dotty `Context` | `PUBLIC` | The bridge remains separate and narrower than public `TermUntypedLowering`; it does not delegate or fall back to the richer facade. Ascription, Lambda1, and binder-bearing local blocks project but fail this direct exact route. |
| Terms (generated origin) | `scala.meta.Term` plus virtual source name | `quasiquotes.neutral.ScalametaTermProjection.project` | public `ProjectedTermShape`, then package-private completed `ConstructedTerm` | package-private `quasiquotes.terms.dotty.ConstructedTermGeneratedOriginAdapter.lower` | fresh positioned `untpd.Tree`, deterministic source, and `SourceFile` | `quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge.lower` | projector and bridge are public source APIs; completion and adapter remain package-private; requires a Dotty `Context` | `PUBLIC` | Includes the direct intersection plus completed Int/String/Boolean ascriptions, Lambda1, P2, and P3. Non-simple completed Type sidecars fail completion; projection exclusions remain unchanged. Placement, ownership, typing, and rollback remain caller-owned. |
| Types | `scala.meta.Type` | `quasiquotes.neutral.ScalametaTypeNormalFormProjection.project` | public `ProjectedTypeNormalForm`; project-owned `TypeNormalForm` plus truthful optional root span | public `quasiquotes.types.dotty.TypeUntypedLowering.lower` | fresh recursively source-free `untpd.Tree` representing a Type | `quasiquotes.types.dotty.ScalametaTypeUntypedBridge.lower` | projector, lowerer, and bridge are public source APIs; no Dotty `Context` is required | `PUBLIC` | The bridge delegates through `TypeUntypedLowering`, preserving its historical bridge diagnostics. The exact intersection is Int/String/Boolean, fixed List/Option/Either, tuple, and function syntax. |
| Definitions | supported `scala.meta.Defn` root | public `quasiquotes.neutral.ScalametaDefinitionProjection.project`; the bridge retains its private shape projection path | public `ProjectedDefinition` / `SemanticDefinition`; private `DefinitionShape` remains internal | public `DefinitionUntypedLowering` for semantic callers; the bridge retains its private `DefinitionShapeUntypedLowerer` composition | fresh source-free `untpd.MemberDef` (`ValDef`, `DefDef`, or `TypeDef`) | `quasiquotes.definitions.dotty.ScalametaDefinitionUntypedBridge.lower` | public semantic projection/authoring/lowering and bridge APIs; lowering requires a Dotty `Context` | `PUBLIC` | The bridge remains separate and non-delegating. Both public routes share the same bounded five-family meaning without making the private carrier public. The generated-origin bridge still admits only four concrete val/def families. |

The public Term, Type, and Definition bridges expose stable `Failure(code, detail)`
boundaries and classify projection and exact-lowering failures separately.
Their outputs are fresh exact-version raw syntax with no fabricated typed
symbols. Source-free results claim no provenance; generated-origin results
carry only their deterministic virtual source and truthful spans. Consumers
still own placement and ordinary compiler lifecycle.

### Definition families in the generic seam

The public Definition projection/authoring pair and semantic lowerer admit
exactly these families; their private dispatcher remains an implementation
detail:

| Family | Projected semantic variant | Projection status | General exact status |
| --- | --- | --- | --- |
| Explicitly typed immutable `val` | `SemanticDefinition` value view | `PUBLIC`, bounded | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| True parameterless explicitly typed `def` | `SemanticDefinition` method view with zero clauses | `PUBLIC`, bounded | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| One ordinary explicitly typed parameter in one clause | `SemanticDefinition` method view with persistent parameter scope | `PUBLIC`, bounded | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| Two ordinary explicitly typed parameters in one clause | `SemanticDefinition` method view with persistent parameter scope | `PUBLIC`, bounded | source-free `PUBLIC`; generic generated-origin `PUBLIC` |
| Simple non-generic unbounded Type alias | `SemanticDefinition` type-member view | `PUBLIC`, bounded | source-free `PUBLIC`; generic generated-origin `NOT_SUPPORTED` |

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
| Instance factory | package-private `ScalametaInstanceFactoryProjection.project`; package-private `ScalametaInstanceFactoryAuthoring.author` is the accepted reverse edge | package-private `InstanceFactoryPlan` with explicit five-role binder semantics and alpha-equivalent reprojection | `InstanceFactoryPlanUntypedLowerer` plus generated-origin adapter behind public-for-JVM `InstanceFactoryPeerBridge.lower` | fresh `Position.None` Scalameta `Defn.Def` on the N reverse edge; positioned `untpd.DefDef` plus deterministic generated source on U-D | `SPECIALIZED` | Models one anonymous implementation factory with fixed member and binder roles; neither reverse edge is a general Definition carrier or public `SemanticDefinition` adapter. |
| Refined AUX Type alias | package-private `ScalametaAuxTypeAliasProjection.project` plus explicit expectations | package-private `AuxTypeAliasPlan` with three Type binders, bounds, target, and refinement member | plan adapter and alias lowerer plus generated-origin adapter behind public-for-JVM `AuxTypeAliasPeerBridge.lower` | positioned `untpd.TypeDef` plus deterministic generated source | `SPECIALIZED` | Its parameter, bound, refinement, provenance, and peer-diagnostic contract is deliberately richer than a simple non-generic unbounded alias. |
| Hybrid existing-class member append | one admitted existing pre-Typer ordinary class plus one supported `scala.meta.Defn` and virtual source name | existing Definition projection/completion and existing-class capture/reconstruction authorities | public exact-version `ScalametaDefinitionClassMemberAppendBridge.append` composes `ScalametaDefinitionGeneratedOriginBridge` with package-private `ExistingUntpdClassMemberAppender` | fresh same-site class/Template shells, exact old members, exact appended generated `DefDef`/`ValDef`, and its generated source | `PUBLIC` | Exactly one generated member appended last. It is neither general class authoring nor a public universal tree editor; Macro-Paradise lifecycle/placement remains caller-owned. |

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

The accepted package-private single-parameter family has an exact view,
separate parameter-Type, result-Type, and RHS rewrites, and one atomic rewrite
of all three fields. The accepted exact-two-parameter family has an exact view
and an RHS-only rewrite preserving both parameter/type identities, the result
Type, non-target member identity/order, and truthful reconstruction linkage.
Each operation reconstructs only its admitted shells at truthful
transformation sites. None adds a public exact-U facade.

## Maintenance rule

When an accepted change adds, removes, renames, or widens a reusable N
projection or authoring boundary, a U-D lowerer, a U-U rewrite boundary, or a
C-composed bridge represented here, the accepting integration review must
audit this document before public documentation drift can be considered
closed. Selected, test-only, or pending work must remain visibly distinct from
accepted current capability.
