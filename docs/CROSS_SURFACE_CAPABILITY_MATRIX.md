# Cross-surface capability matrix

This matrix separates the repository's independent semantic directions. A row
does not imply that adjacent columns compose into an end-to-end public API.
In particular, a neutral Scalameta projection and an internal exact-tree
lowerer are separate contracts until a named public façade owns their
composition, diagnostics, version policy, and placement semantics.

For the concrete API-level data flow behind these axes, including visibility,
`Context` requirements, and composed bridge boundaries, see the
[projection, lowering, and bridge pipeline matrix](PROJECTION_LOWERING_BRIDGE_MATRIX.md).

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

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Literal / identifier | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — public source-free and generated-origin exact-version Term bridges | `NOT_APPLICABLE` |
| Selection | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — public source-free and generated-origin exact-version Term bridges | `NOT_APPLICABLE` |
| Ordinary Apply | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — one ordinary positional list through both public exact-version Term bridges | `INTERNAL` — one selected existing Apply in a direct parameterless method body; bounded leaf or direct-identifier Apply argument replacement only |
| Infix | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — both public Term bridges; only the source-free route retains raw span-free `InfixOp` caveats | `NOT_APPLICABLE` |
| Unary | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Tuple | `BOUNDED` — arity 2 through 22 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — arity 2 through 22 through both public exact-version Term bridges | `NOT_APPLICABLE` |
| `if` with explicit `else` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Standard `s` interpolation | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — fresh `Position.None` standard-`s` AST with exact semantic round trip | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Type ascription | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `BOUNDED` — public generated-origin bridge for completable Int/String/Boolean sidecars; source-free bridge rejects | `NOT_APPLICABLE` |
| Lambda1 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `BOUNDED` — public generated-origin bridge when the explicit parameter Type is completable; source-free bridge rejects | `NOT_APPLICABLE` |
| Fixed one-list `new` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — fully-qualified, non-generic, one ordinary list through both public exact-version Term bridges | `NOT_APPLICABLE` |
| Binder-free P1 block | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — both public exact-version Term bridges | `NOT_APPLICABLE` |
| Single typed local immutable val (P2) | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `BOUNDED` — public generated-origin bridge when the declared Type is completable; source-free bridge rejects | `NOT_APPLICABLE` |
| Source-owned local identity method (P3) | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` | `NOT_YET` | `BOUNDED` — public generated-origin bridge for the bounded local identity-method form with completable parameter/result Types; source-free bridge rejects | `NOT_APPLICABLE` |
| Rank-2 Term arguments in Apply / one-list New | `BOUNDED` | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` — the sequence is expanded before exact lowering | `NOT_APPLICABLE` |
| Rank-3 Term sequence | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |
| Dynamic selected-member construction | `BOUNDED` | `NOT_YET` | `BOUNDED` — validated `SelectedMemberName` in one explicit receiver-selection name field; unique accessible member only | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |
| Existing selected-Apply argument replacement | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `INTERNAL` — one existing leaf argument selected by exact identity in a direct parameterless method; replacement is one source-free leaf or one direct-identifier Apply with one to three leaf arguments; function and untouched arguments retain identity |

## Types

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Named Type | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `BOUNDED` — Int/String/Boolean through public exact-version `ScalametaTypeUntypedBridge` | `NOT_APPLICABLE` |
| Fixed `List` / `Option` / `Either` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — recursive fixed arities through the public exact-version facade | `NOT_APPLICABLE` |
| Tuple2 / Tuple3 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — tuple syntax through public exact-version `ScalametaTypeUntypedBridge` | `NOT_APPLICABLE` |
| Function1 / Function2 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — function syntax through public exact-version `ScalametaTypeUntypedBridge` | `NOT_APPLICABLE` |
| Reflected complete-Type holes / captures | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `INTERNAL` — completed semantic Types only | `NOT_APPLICABLE` |
| Runtime-length Type application arguments | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |

## Definitions

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Immutable `val` | `INTERNAL` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — reusable explicitly typed immutable-val projector | `NOT_YET` | `BOUNDED` — public source-free and generated-origin Definition facades | `NOT_APPLICABLE` |
| Parameterless ordinary `def` | `INTERNAL` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — reusable true-parameterless explicitly typed projector | `NOT_YET` | `BOUNDED` — public source-free and generated-origin Definition facades | `INTERNAL` — exact direct parameterless method-body replacement only; header and surrounding children retain the bounded identity/provenance contract |
| One ordinary parameter | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` — reusable one-ordinary-parameter projector | `NOT_YET` | `BOUNDED` — public source-free and generated-origin Definition facades | `NOT_YET` — the current existing-tree rewriter rejects parameter clauses |
| Exactly two ordinary parameters | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` — reusable exact-two-ordinary-parameter projector | `NOT_YET` | `BOUNDED` — public source-free and generated-origin Definition facades | `NOT_YET` — the current existing-tree rewriter rejects parameter clauses |
| Parameter-sequence capture | `NOT_APPLICABLE` | `BOUNDED` — one static ordinary parameter clause with 0 through 5 parameters plus one RHS capture | `NOT_APPLICABLE` | `BOUNDED` — same ranked `dqq` slice and original reflected captures | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` | `NOT_APPLICABLE` |
| Contextual method | `NOT_YET` — a public Core programmatic constructor exists, but no Q quasiquote syntax | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` — specialized projector | `NOT_YET` | `INTERNAL` | `NOT_YET` — the current existing-tree rewriter rejects parameter clauses |
| Simple non-generic unbounded Type alias | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — reusable simple-alias projector | `NOT_YET` | `BOUNDED` — public source-free Definition facade only; generic generated-origin route rejects aliases | `NOT_APPLICABLE` |
| Bounded refined Type alias | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — specialized projector | `INTERNAL` — specialized authoring | `INTERNAL` | `NOT_APPLICABLE` |
| Class / trait / object | `NOT_YET` | `NOT_YET` | `NOT_YET` — a separate internal public-reflection class plan is not typed-Scalameta syntax | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` — no fresh raw class/trait/object lowerer; typed-reflection generation is a different surface | `NOT_APPLICABLE` |
| Anonymous implementation | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — specialized instance-factory projection | `NOT_YET` | `INTERNAL` — exact bounded instance-factory plan lowering, exposed only through its named exact-version peer bridge | `NOT_APPLICABLE` |

The five reusable neutral Definition projectors remain separate package-private
family entries behind one common dispatcher. The public exact-version
`ScalametaDefinitionUntypedBridge` composes that dispatcher with the common
source-free lowerer for exactly those five families. The separate generated-
origin bridge admits only the four concrete val/def families. Specialized
contextual/refined-alias/instance-factory projectors and named peer bridges do
not widen either generic boundary.

## Composition and rank summary

| Composition | Current status | Boundary |
| --- | --- | --- |
| `scala.meta.Term` -> fresh `untpd.Tree` | `BOUNDED` | Public exact-version `ScalametaTermUntypedBridge`; direct non-binder and P0/P1 intersection only; source-free result |
| `scala.meta.Term` -> positioned generated-origin `untpd.Tree` | `BOUNDED` | Public exact-version `ScalametaTermGeneratedOriginBridge`; direct family plus completable ascription, Lambda1, P2, and P3; caller owns placement and insertion |
| `scala.meta.Type` -> fresh `untpd.Tree` | `BOUNDED` | Public exact-version `ScalametaTypeUntypedBridge`; recursive Int/String/Boolean, fixed List/Option/Either, Tuple2/3 syntax, and Function1/2 syntax; source-free result |
| `scala.meta.Defn` -> fresh source-free `untpd.MemberDef` | `BOUNDED` | Public exact-version `ScalametaDefinitionUntypedBridge`; five reusable Definition families |
| `scala.meta.Defn` -> positioned generated-origin `untpd.MemberDef` | `BOUNDED` | Public exact-version `ScalametaDefinitionGeneratedOriginBridge`; four concrete val/def families only; caller owns target admission and insertion |
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
