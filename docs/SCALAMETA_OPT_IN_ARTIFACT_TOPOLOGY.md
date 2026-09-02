# Scalameta opt-in artifact topology

The selected topology keeps the Scalameta routes explicit and experimental.
They remain remotely unpublished today but are part of the candidate `0.3.0`
expanded release set:

| Role | Future coordinate shape | Cross policy | Direct project dependencies |
| --- | --- | --- | --- |
| compiler-free source-AST bridge | `com.github.dmytromitin:quasiquotes-scala3-neutral-scalameta_3:0.3.0` | Scala 3 binary | `core_3`, Scalameta 4.17.3 |
| typed Term, Type, and bounded Definition opt-in | `com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_<3.3.8-or-3.8.4-or-3.9.0>:0.3.0` | full Scala version | matching `frontend_<exact-scala>`, `neutral-scalameta_3` |
| exact-version peer backend | `com.github.dmytromitin:quasiquotes-scala3-dotty-internal_<3.3.8-or-3.8.4-or-3.9.0>:0.3.0` | full Scala version | `neutral-scalameta_3`, matching `scala3-compiler_3` |

Ordinary builds keep all three modules skipped. Only
`-Dquasiquotes.expandedRelease=true` enables them for explicit release-mode
staging; the aggregate and examples remain skipped. The first two rows are the
potential user-facing opt-in topology. The exact backend is a separately
version-coupled integration artifact, not a stable public raw-tree API. Final
Scala 3.9.0 is a required support line and has matching candidate coordinates.

## Semantic and public API boundary

The neutral coordinate owns public Scalameta AST authoring and bounded
projection into existing validated `core` results. It does not own another
semantic model. Consumers use ordinary Scalameta `q`, `t`, and `tparam`
quasiquotes directly; the project does not copy or export an `n*` macro engine.

The typed coordinate exposes only `quasiquotes.scalameta`:

- `ScalametaQuasiquotes.*` for explicit opt-in `qr`, `tqr`, and bounded `dqr`;
- `ScalametaQuasiPattern.*` for explicit opt-in `qq`, `tqq`, and bounded `dqq`;
- `quasiquotes.scalameta.Quasiquotes.*` as the additive direct-export umbrella
  for those same six established members;
- compact `TermFrontend` and `TypeFrontend` programmatic boundaries;
- ordered extractor results that preserve original caller-owned reflected
  subtrees.

The typed Scalameta `dqr`/`dqq` surface is limited to the exact current-Dotty
single-parameter identity-Definition overlap. It reuses the same typed
owner/binder lowerer and `SingleParameterDefinitionPattern`; it does not route
through the neutral contextual-method projector. That neutral route and the
exact `dottyInternal` pre-typer bridge remain separate contracts with different
outputs and placement semantics.

Its Term and Type mappers, dialect policy, selectors, parity inventories, and
evidence macros remain package-private. Ordinary imports from
`quasiquotes.construct`, `quasiquotes.matching`, and `quasiquotes.types`
continue to select current-Dotty.

The standard frontend likewise exposes
`quasiquotes.Quasiquotes.{qr, qq, tqr, tqq, dqr, dqq}` as an additive umbrella.
Both umbrella objects export their established hosts directly; the original
imports remain supported and authoritative for the underlying semantics.

The Type opt-in covers the same deliberately overlapping bounded matrix as the
current route: names, recursive fixed `List`/`Option`/`Either`, Tuple2/Tuple3,
Function1/Function2, ordered construction/capture slots, programmatic repeated
holes, and controlled failures. Selected/path-dependent types, broader
constructors, bounds, refinements, unions/intersections, match/type-lambda
families, semantic equality/subtyping, and constructor-position holes remain
outside that surface.

Only Scalameta parse failure may select the current parser. Unsupported mapped
syntax or any semantic, target-inspection, or lowering failure is terminal.
The current-Dotty frontend remains the default and reference/oracle. Parity is
required when both routes claim a feature, not as a lock-step delivery rule.

## Measured local consumability

Disposable coordinate-only builds on Scala 3.3.8, 3.8.4, and 3.9.0 validated the
binary-cross neutral closure, the 24-JAR full-cross typed closure, POM/source/
documentation artifacts, Term construction and matching, complete bounded
Type construction and matching, original capture identity, observable
parse-only fallback, and unchanged current-Dotty controls. Those local builds
contained no project references, checkout paths, source/class directories, or
controller paths.

This evidence and the candidate publication policy do not make the coordinates
remotely available. Released `0.2.0` retains its immutable 618-row
`core`/`frontend`
[API baseline](api-baselines/0.2.0.tsv).

## Peer backend boundary

`quasiquotes.definitions.dotty.ContextualMethodPeerBridge` accepts one admitted
Scalameta contextual method and a validated virtual source name, then returns
a positioned exact `untpd.DefDef` plus deterministic generated-source
provenance. It delegates projection semantics to the neutral/core layers and
performs exact lowering in `dottyInternal`.

AUXify uses this narrow path for its current `@apply` integration. Macro-
Paradise continues to own annotation lifecycle, companion merge, placement,
insertion, rollback, and typing. Neither fact creates a public `u*` family, a
generic raw-tree contract, or a requirement that other handlers use Scalameta.
