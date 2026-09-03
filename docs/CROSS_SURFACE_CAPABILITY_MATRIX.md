# Cross-surface capability matrix

This matrix separates the repository's independent semantic directions. A row
does not imply that adjacent columns compose into an end-to-end public API.
In particular, a neutral Scalameta projection and an internal exact-tree
lowerer are separate contracts until a named public façade owns their
composition, diagnostics, version policy, and placement semantics.

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
- **U-D fresh lower** — package-private fresh exact compiler-tree lowering;
- **U-U existing rewrite** — package-private rewriting of an existing exact
  compiler tree while preserving the admitted graph and provenance contract.

## Terms

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Literal / identifier | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `INTERNAL` | `NOT_APPLICABLE` |
| Selection | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `INTERNAL` | `NOT_APPLICABLE` |
| Ordinary Apply | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `INTERNAL` | `INTERNAL` — one selected existing Apply in a direct parameterless method body; bounded leaf or direct-identifier Apply argument replacement only |
| Infix | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Unary | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Tuple | `BOUNDED` — arity 2 through 22 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| `if` with explicit `else` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `INTERNAL` | `NOT_APPLICABLE` |
| Standard `s` interpolation | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` — fresh `Position.None` standard-`s` AST with exact semantic round trip | `INTERNAL` | `NOT_APPLICABLE` |
| Type ascription | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `INTERNAL` | `NOT_APPLICABLE` |
| Lambda1 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `INTERNAL` — completed parameter-Type sidecar required | `NOT_APPLICABLE` |
| Fixed one-list `new` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Binder-free P1 block | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Single typed local immutable val (P2) | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` | `INTERNAL` — requires completed-Type sidecars | `NOT_APPLICABLE` |
| Source-owned local identity method (P3) | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` | `NOT_YET` | `INTERNAL` — requires completed-Type sidecars | `NOT_APPLICABLE` |
| Rank-2 Term arguments in Apply / one-list New | `BOUNDED` | `BOUNDED` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` — the sequence is expanded before exact lowering | `NOT_APPLICABLE` |
| Rank-3 Term sequence | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |
| Dynamic selected-member construction | `BOUNDED` | `NOT_YET` | `BOUNDED` — validated `SelectedMemberName` in one explicit receiver-selection name field; unique accessible member only | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |
| Existing selected-Apply argument replacement | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `INTERNAL` — one existing leaf argument selected by exact identity in a direct parameterless method; replacement is one source-free leaf or one direct-identifier Apply with one to three leaf arguments; function and untouched arguments retain identity |

## Types

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Named Type | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `SUPPORTED` | `INTERNAL` | `NOT_APPLICABLE` |
| Fixed `List` / `Option` / `Either` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Tuple2 / Tuple3 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Function1 / Function2 | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `INTERNAL` | `NOT_APPLICABLE` |
| Reflected complete-Type holes / captures | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_APPLICABLE` | `NOT_APPLICABLE` | `INTERNAL` — completed semantic Types only | `NOT_APPLICABLE` |
| Runtime-length Type application arguments | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` |

## Definitions

| Family | Q construct | Q match | typed Scalameta construct | typed Scalameta match | N project | N author | U-D fresh lower | U-U existing rewrite |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Immutable `val` | `INTERNAL` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` | `NOT_APPLICABLE` |
| Parameterless ordinary `def` | `INTERNAL` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` | `INTERNAL` — exact direct parameterless method-body replacement only; header and surrounding children retain the bounded identity/provenance contract |
| One ordinary parameter | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` — no general Definition projector | `NOT_YET` | `INTERNAL` | `NOT_YET` — the current existing-tree rewriter rejects parameter clauses |
| Exactly two ordinary parameters | `BOUNDED` | `BOUNDED` | `BOUNDED` | `BOUNDED` | `NOT_YET` — no general ordinary-Definition projector | `NOT_YET` | `INTERNAL` | `NOT_YET` — the current existing-tree rewriter rejects parameter clauses |
| Parameter-sequence capture | `NOT_APPLICABLE` | `BOUNDED` — one static ordinary parameter clause with 0 through 5 parameters plus one RHS capture | `NOT_APPLICABLE` | `BOUNDED` — same ranked `dqq` slice and original reflected captures | `NOT_YET` | `NOT_YET` | `NOT_APPLICABLE` | `NOT_APPLICABLE` |
| Contextual method | `NOT_YET` — a public Core programmatic constructor exists, but no Q quasiquote syntax | `NOT_YET` | `NOT_YET` | `NOT_YET` | `BOUNDED` — specialized projector | `NOT_YET` | `INTERNAL` | `NOT_YET` — the current existing-tree rewriter rejects parameter clauses |
| Bounded refined Type alias | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — specialized projector | `INTERNAL` — specialized authoring | `INTERNAL` | `NOT_APPLICABLE` |
| Class / trait / object | `NOT_YET` | `NOT_YET` | `NOT_YET` — a separate internal public-reflection class plan is not typed-Scalameta syntax | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` — no fresh raw class/trait/object lowerer; typed-reflection generation is a different surface | `NOT_APPLICABLE` |
| Anonymous implementation | `NOT_YET` | `NOT_YET` | `NOT_YET` | `NOT_YET` | `INTERNAL` — specialized instance-factory projection | `NOT_YET` | `NOT_YET` — the specialized neutral semantic plan has no accepted exact lowering | `NOT_APPLICABLE` |

Specialized neutral Definition projectors are evidence for their named shapes;
they are not a reusable `scala.meta.Stat -> Definition` boundary. A general
Scalameta Definition-to-exact-tree public bridge therefore remains `NOT_YET`.

## Composition and rank summary

| Composition | Current status | Boundary |
| --- | --- | --- |
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
