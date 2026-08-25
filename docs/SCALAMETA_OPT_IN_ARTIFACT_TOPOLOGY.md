# Scalameta opt-in artifact topology

The selected topology keeps the Scalameta routes explicit, experimental, and
remotely unpublished:

| Role | Future coordinate shape | Cross policy | Direct project dependencies |
| --- | --- | --- | --- |
| compiler-free source-AST bridge | `com.github.dmytromitin:quasiquotes-scala3-neutral-scalameta_3:<future-version>` | Scala 3 binary | `core_3`, Scalameta 4.17.3 |
| typed Term and Type opt-in | `com.github.dmytromitin:quasiquotes-scala3-scalameta-frontend_<exact-scala>:<future-version>` | full Scala version | matching `frontend_<exact-scala>`, `neutral-scalameta_3` |
| exact-version peer backend | `com.github.dmytromitin:quasiquotes-scala3-dotty-internal_<exact-scala>:<future-version>` | full Scala version | `neutral-scalameta_3`, matching `scala3-compiler_3` |

All three modules retain `publish / skip := true`. The first two rows are the
potential user-facing opt-in topology. The exact backend is a separately
version-coupled integration artifact, not a stable public raw-tree API.

## Semantic and public API boundary

The neutral coordinate owns public Scalameta AST authoring and bounded
projection into existing validated `core` results. It does not own another
semantic model. Consumers use ordinary Scalameta `q`, `t`, and `tparam`
quasiquotes directly; the project does not copy or export an `n*` macro engine.

The typed coordinate exposes only `quasiquotes.scalameta`:

- `ScalametaQuasiquotes.*` for explicit opt-in `qr` and `tqr`;
- `ScalametaQuasiPattern.*` for explicit opt-in `qq` and `tqq`;
- compact `TermFrontend` and `TypeFrontend` programmatic boundaries;
- ordered extractor results that preserve original caller-owned reflected
  subtrees.

Its Term and Type mappers, dialect policy, selectors, parity inventories, and
evidence macros remain package-private. Ordinary imports from
`quasiquotes.construct`, `quasiquotes.matching`, and `quasiquotes.types`
continue to select current-Dotty.

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

Disposable coordinate-only builds on Scala 3.3.8 and 3.8.4 validated the
binary-cross neutral closure, the 24-JAR full-cross typed closure, POM/source/
documentation artifacts, Term construction and matching, complete bounded
Type construction and matching, original capture identity, observable
parse-only fallback, and unchanged current-Dotty controls. Those local builds
contained no project references, checkout paths, source/class directories, or
controller paths.

This evidence does not make the coordinates remotely available. Released
`0.2.0` retains its immutable 618-row `core`/`frontend`
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
