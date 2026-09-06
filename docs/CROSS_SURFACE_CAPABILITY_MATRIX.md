# Cross-surface capability matrix

This matrix separates the repository's independent semantic directions. A row
does not imply that adjacent columns compose into an end-to-end public API.
In particular, a neutral Scalameta projection and an internal exact-tree
lowerer are separate contracts until a named public façade owns their
composition, diagnostics, version policy, and placement semantics.

For the concrete API-level data flow behind these axes, including visibility,
`Context` requirements, and composed bridge boundaries, see the
[projection, lowering, and bridge pipeline matrix](PROJECTION_LOWERING_BRIDGE_MATRIX.md).

Before using the lane labels: **Q** is the Quotes-aware typed frontend; **N**
is the neutral compiler-free semantic/Scalameta world; **U-D** is **exact fresh
lowering**; **U-U** is **exact existing-tree transformation**; and **C** owns
cross-layer integration/API policy rather than another AST. N and U do not
name public `n*` or `u*` syntax families; `u*` remains later optional. The
canonical [semantic models and conversions guide](SEMANTIC_MODELS_AND_CONVERSIONS.md)
defines the three representations and their loss/provenance contracts.

Status vocabulary:

- `SUPPORTED` — available on the named public surface for this family;
- `BOUNDED` — available only under the stated structural restrictions;
- `INTERNAL` — implemented behind a non-public boundary;
- `NOT_YET` — not implemented on this direction;
- `NOT_APPLICABLE` — the direction does not operate on this kind of value.

The axes are:

- **Q construct/match** — the default current-Dotty public quasiquote syntax
  and typed-facing surface; a public compiler-free Core constructor is not by
  itself a Q-syntax capability;
- **typed Scalameta construct/match** — the unpublished opt-in typed frontend;
- **N project** — compiler-free `scala.meta` AST to project semantic values;
- **N author** — project semantic values to compiler-free `scala.meta` ASTs;
- **U-D fresh lower** — fresh exact compiler-tree lowering, public only through
  a named exact-version facade where the matrix says so and otherwise internal;
- **U-U existing rewrite** — package-private rewriting of an existing exact
  compiler tree while preserving the admitted graph and provenance contract.

## Terms

For Terms, `TermUntypedLowering` is the current public semantic-value facade.
It owns the richer completed and binder-safe source-free route. The older
`ScalametaTermUntypedBridge` remains a separate, narrower, non-delegating
source bridge; the generated-origin bridge is separate again.

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Literal / identifier | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — public semantic facade plus source-free and generated-origin Term bridges | `NOT_APPLICABLE` |
| Selection | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — public source-free and generated-origin exact-version Term bridges | `NOT_APPLICABLE` |
| Ordinary Apply | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — one ordinary positional list through both public exact-version Term bridges | `INTERNAL` — one selected existing Apply in a direct parameterless method body; bounded leaf or direct-identifier Apply argument replacement only |
| Infix | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — both public Term bridges; only the source-free route retains raw span-free `InfixOp` caveats | `NOT_APPLICABLE` |
| Unary | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Tuple | `BOUNDED` — arity 2 through 22 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — arity 2 through 22 through both public exact-version Term bridges | `NOT_APPLICABLE` |
| `if` with explicit `else` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Standard `s` interpolation | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — fresh `Position.None` standard-`s` AST with exact semantic round trip | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Type ascription | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — fresh primitive ascription with semantic reprojection | `BOUNDED` — public semantic facade; generated-origin bridge also admits completable Int/String/Boolean sidecars, while the source-free Scalameta bridge rejects | `NOT_APPLICABLE` |
| Lambda1 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — typed Lambda1 with opaque binder semantics | `BOUNDED` — public binder-safe semantic facade; generated-origin bridge also admits completable explicit parameter Types, while the source-free Scalameta bridge rejects | `NOT_APPLICABLE` |
| Fixed one-list `new` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — fully-qualified, non-generic, one ordinary list through both public exact-version Term bridges | `NOT_APPLICABLE` |
| Binder-free P1 block | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Single typed local immutable val (P2) | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — one local immutable value with opaque binder semantics | `BOUNDED` — public binder-safe semantic facade; generated-origin bridge also admits completable declared Types, while the source-free Scalameta bridge rejects | `NOT_APPLICABLE` |
| Source-owned local identity method (P3) | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` | `BOUNDED` — one local identity method with distinct opaque parameter/method binders | `BOUNDED` — public binder-safe semantic facade; generated-origin bridge also admits completable parameter/result Types, while the source-free Scalameta bridge rejects | `NOT_APPLICABLE` |
| Grouping parentheses | source grammar | source grammar | source grammar | source grammar | `BOUNDED` — transparent projection to the inner semantic shape | `NOT_YET` — not representable as a distinct project Term under Scalameta 4.17.3 | `NOT_APPLICABLE` — any lowered result follows the inner semantic shape | `NOT_APPLICABLE` |
| Rank-2 Term arguments in Apply / one-list New | `BOUNDED` | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` — the sequence is expanded before exact lowering | `NOT_APPLICABLE` |
| Rank-3 Term sequence | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |
| Dynamic selected-member construction | `BOUNDED` | `NOT_YET` | `BOUNDED` — validated `SelectedMemberName` in one explicit receiver-selection name field; unique accessible member only | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |
| Existing selected-Apply argument replacement | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `INTERNAL` — one existing leaf argument selected by exact identity in a direct parameterless method; replacement is one source-free leaf or one direct-identifier Apply with one to three leaf arguments; function and untouched arguments retain identity |

## Types

For Types, `TypeUntypedLowering` is the current context-free public semantic
facade. `ScalametaTypeUntypedBridge` delegates through it while retaining the
bridge's projection-stage diagnostics.

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Named Type | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — Int/String/Boolean through public `TypeUntypedLowering` and its delegating Scalameta bridge | `NOT_APPLICABLE` |
| Fixed `List` / `Option` / `Either` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — recursive fixed arities through the public exact-version facade | `NOT_APPLICABLE` |
| Tuple2 / Tuple3 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — tuple syntax through public exact-version `ScalametaTypeUntypedBridge` | `NOT_APPLICABLE` |
| Function1 / Function2 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — function syntax through public exact-version `ScalametaTypeUntypedBridge` | `NOT_APPLICABLE` |
| Reflected complete-Type holes / captures | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `INTERNAL` — completed semantic Types only | `NOT_APPLICABLE` |
| Runtime-length Type application arguments | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |

## Definitions

The public compiler-free Definition pair is current:
`ScalametaDefinitionProjection.project(Defn)` returns a `ProjectedDefinition`
whose `definition` is a `SemanticDefinition` and whose `sourceSpan` is
truthful optional metadata; `ScalametaDefinitionAuthoring.author` maps the
same five semantic families back to fresh `Position.None` syntax. Public
`DefinitionUntypedLowering` lowers those semantic values directly. The older
Scalameta Definition bridge remains a separate non-delegating composition
through private shape carriers.

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Immutable `val` | `INTERNAL` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` — public five-family semantic projection | `BOUNDED` — public five-family semantic authoring | `BOUNDED` — public semantic lowerer plus separate source-free and generated-origin Definition bridges | `NOT_APPLICABLE` |
| Parameterless ordinary `def` | `INTERNAL` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` — public five-family semantic projection | `BOUNDED` — public five-family semantic authoring | `BOUNDED` — public semantic lowerer plus separate source-free and generated-origin Definition bridges | `INTERNAL` — exact direct parameterless method-body replacement only; header and surrounding children retain the bounded identity/provenance contract |
| One ordinary parameter | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — public binder-aware semantic projection | `BOUNDED` — public binder-aware semantic authoring | `BOUNDED` — public semantic lowerer plus separate source-free and generated-origin Definition bridges | `INTERNAL` — exact view; separate parameter-Type, result-Type, and RHS rewrites; and one atomic all-three rewrite. Only admitted shells/sites are fresh; non-target members and opaque owner children preserve identity. |
| Exactly two ordinary parameters | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — public binder-aware semantic projection | `BOUNDED` — public binder-aware semantic authoring | `BOUNDED` — public semantic lowerer plus separate source-free and generated-origin Definition bridges | `INTERNAL` — exact view plus bounded RHS-only rewrite preserving both parameter/type identities, the result Type, non-target member identity/order, and truthful reconstruction linkage |
| Parameter-sequence capture | `NOT_APPLICABLE` | `BOUNDED` — one static ordinary parameter clause with 0 through 5 parameters plus one RHS capture | `NOT_APPLICABLE` | `BOUNDED` — same ranked `dqq` slice and original reflected captures | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` | `NOT_APPLICABLE` |
| Contextual method | `NOT_YET` — a public Core programmatic constructor exists, but no Q quasiquote syntax | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` — specialized projector | `INTERNAL` — specialized authorer | `INTERNAL` | `NOT_YET` — no accepted contextual-method existing-tree rewrite |
| Simple non-generic unbounded Type alias | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` — public five-family semantic projection | `BOUNDED` — public five-family semantic authoring | `BOUNDED` — public semantic and source-free Definition facades; generated-origin bridge rejects aliases | `NOT_APPLICABLE` |
| Bounded refined Type alias | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — specialized projector | `INTERNAL` — specialized authoring | `INTERNAL` | `NOT_APPLICABLE` |
| Class / trait / object | `NOT_YET` | `NOT_YET` | `NOT_YET` — a separate internal public-reflection class plan is not typed-Scalameta syntax | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` — no fresh raw class/trait/object lowerer; typed-reflection generation is a different surface | `NOT_APPLICABLE` |
| Anonymous implementation | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — specialized instance-factory projection | `INTERNAL` — specialized five-role instance-factory plan authoring with alpha-equivalent reprojection | `INTERNAL` — exact bounded instance-factory plan lowering, exposed only through its named exact-version peer bridge | `NOT_APPLICABLE` |

The public Definition projection/authoring pair adapts the five reusable
families to and from `SemanticDefinition`; its shape dispatcher and family
carriers remain private. The public exact-version
`ScalametaDefinitionUntypedBridge` independently composes those private shapes
with the common source-free lowerer for exactly the same five families. The
separate generated-origin bridge admits only the four concrete val/def
families. Specialized
contextual/refined-alias/instance-factory projectors, the specialized
refined-alias and instance-factory authorers, and named peer bridges do not
widen either generic boundary.

The public exact-version `ScalametaDefinitionClassMemberAppendBridge` is a
separate hybrid composition, not another cell on either fresh-lowering or
existing-rewrite axis. For the same four concrete generated-origin families it
authors one exact member through the existing Definition bridge, then delegates
append-last reconstruction of one admitted existing ordinary class to the
package-private U-U authority. Existing members keep identity/order and original
source, the appended member keeps its generated virtual source, and only the
class/Template shells are fresh at their original replacement site.

## Composition and rank summary

| Composition | Current status | Boundary |
| --- | --- | --- |
| `TermShape` -> fresh source-free `untpd.Tree` | `BOUNDED` | Public exact-version `TermUntypedLowering`; richer completed/binder-safe semantic route; requires Dotty `Context` |
| `TypeNormalForm` -> fresh source-free raw Type tree | `BOUNDED` | Public exact-version context-free `TypeUntypedLowering` |
| `SemanticDefinition` -> fresh source-free `untpd.MemberDef` | `BOUNDED` | Public exact-version `DefinitionUntypedLowering`; five reusable Definition families; requires Dotty `Context` |
| `scala.meta.Defn` <-> `SemanticDefinition` | `BOUNDED` | Public `ScalametaDefinitionProjection` / `ScalametaDefinitionAuthoring`; five reusable Definition families; projection carries optional source span and authoring is fresh `Position.None` |
| `scala.meta.Term` -> fresh `untpd.Tree` | `BOUNDED` | Public exact-version `ScalametaTermUntypedBridge`; direct non-binder and P0/P1 intersection only; source-free result |
| `scala.meta.Term` -> positioned generated-origin `untpd.Tree` | `BOUNDED` | Public exact-version `ScalametaTermGeneratedOriginBridge`; direct family plus completable ascription, Lambda1, P2, and P3; caller owns placement and insertion |
| `scala.meta.Type` -> fresh `untpd.Tree` | `BOUNDED` | Public exact-version `ScalametaTypeUntypedBridge`; recursive Int/String/Boolean, fixed List/Option/Either, Tuple2/3 syntax, and Function1/2 syntax; source-free result |
| `scala.meta.Defn` -> fresh source-free `untpd.MemberDef` | `BOUNDED` | Public exact-version `ScalametaDefinitionUntypedBridge`; five reusable Definition families |
| `scala.meta.Defn` -> positioned generated-origin `untpd.MemberDef` | `BOUNDED` | Public exact-version `ScalametaDefinitionGeneratedOriginBridge`; four concrete val/def families only; caller owns target admission and insertion |
| existing pre-Typer class + `scala.meta.Defn` -> rebuilt class | `BOUNDED` | Public exact-version `ScalametaDefinitionClassMemberAppendBridge`; exactly one generated member is appended last through the existing-tree authority, with caller-owned lifecycle and ordinary typing |
| `Term` -> `qr` scalar position | `BOUNDED` | Caller-owned reflected Term transport in admitted scalar positions |
| `Seq[Term]` -> `qr` Apply / one-list New arguments | `BOUNDED` | Exactly one rank-2 carrier in the admitted ordinary argument list |
| `TypeRepr` / `tqr` -> `tqr` Type position | `BOUNDED` | Complete reflected Type slots in admitted templates |
| `TypeRepr` / `tqr` -> `qr` complete constructor Type | `BOUNDED` | Complete Type of one fixed one-list `new`; no partial constructor-Type splice |
| `TypeRepr` / `tqr` -> bounded Definition parameter/result Types | `BOUNDED` | One-parameter and exact-two public Definition families only |
| `TypeRepr` / `tqr` -> `dqr` | `BOUNDED` | Complete declared-Type fields; exact admitted method shapes |
| `dqq` captured RHS -> Term APIs | `BOUNDED` | Original owner-sensitive RHS remains in its existing Quotes universe |
| Constructed external `DefDef` -> `qr` statement position | `NOT_YET` | No owner/placement/reownership contract |
| Whole or sequence Definition splice | `NOT_YET` | No public Definition-rank carrier |
| Symbol splice | `NOT_APPLICABLE` | Symbols are not public splice payloads in this model |

Rank 2 currently includes Term sequences in bounded Apply and one-list New
argument positions and Definition parameter-sequence matching in one static
ordinary `dqq` clause. The Definition matcher captures the original ordered
`Seq[ValDef]` plus RHS; it is not construction-side parameter splicing or a
sequence of whole Definitions. Rank 3 is not implemented. Type sequences and
whole-Definition sequences are not production capabilities. Symbol splicing is
not planned as source syntax.

The user-facing Q syntax view remains the
[syntax support matrix](SYNTAX_SUPPORT_MATRIX.md). Detailed caveats remain in
[supported syntax and limitations](SUPPORTED_SYNTAX_AND_LIMITATIONS.md).
