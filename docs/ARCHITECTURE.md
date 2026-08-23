# Architecture

The dependency direction is intentionally one-way:

```text
frontend ----------------> core
neutralScalameta ---------> core
dottyInternal ---> neutralScalameta ---> core
hybridScalametaFrontend ---> frontend + neutralScalameta
publicApiExamples -> frontend
publicCoreExamples -> core
```

`core` owns compiler-free values, structural normal forms, templates,
construction and matching algorithms, and neutral source/diagnostic metadata.
Its compile and runtime classpaths must not contain Scala compiler artifacts.
Its package-private definition model reuses the same `BinderId` scope algebra
as Lambda1 for one or exactly two ordinary method parameters; display spelling
never replaces semantic identity. The public compiler-free constructors create
those package-private bound-reference representations and return only narrow
projections. The current source-metadata carrier remains package-private. The
exact definition backend lowers the one-parameter and exact-two variants
directly to ordinary raw `DefDef` trees in source-free mode and to canonical,
recursively positioned generated-origin trees. Both modes map project
`BinderId` values to validated parameter declaration spellings; they do not
manufacture compiler symbols or treat reference display text as binding. The
exact-two path remains a dedicated bounded adapter rather than a general
parameter-list abstraction.

`frontend` owns source parsing, macros, quoted reflection, source-to-core
adapters, and compiler-version-sensitive lowering. It uses full Scala compiler
version coordinates because its public surface and dependency graph are tied to
the compiler line.

`neutralScalameta` owns the remotely unpublished compiler-free Scalameta 4.17.3
source-AST boundary and the bounded structural projection into existing
validated core results. It is the selected future binary-cross neutral
coordinate. Its compile/runtime
classpath contains neither the Scala compiler implementation, `scala3-staging`,
nor SemanticDB. Scalameta remains absent from `core` and `frontend`.

`hybridScalametaFrontend` is the remotely unpublished implementation of the
selected future full-cross `quasiquotes-scala3-scalameta-frontend` coordinate.
It parses
with public Scalameta AST APIs, lowers directly into caller-`Quotes` reflected
terms or the existing typed pattern IR, and retains the exact Dotty frontend as
an explicit parser fallback and comparison oracle. The intended public surface
is confined to `quasiquotes.scalameta`: shared explicit `qr`/`tqr` and
`qq`/`tqq` import hosts plus programmatic `TermFrontend` and `TypeFrontend`
objects. The research lowerers, selector, dialect policy,
parity inventory, and evidence macros remain package-private. Selection is an
ordinary immutable call/import choice; it does not use a process-global or
environment default. The released `qr` and `qq` entrypoints still select the
current Dotty engine. The same module contains the Type-Q3 path that
maps public `scala.meta.Type` directly into the existing `TypeShape`,
`TypeNormalForm`, `TypeTemplate`, and `TypePattern` pipeline. It proves the
current public `tqr`/`tqq` matrix and exposes it only through the separate
Scalameta opt-in API; ordinary `tqr`/`tqq` remain current-Dotty. The experiment does not authorize
definition migration. The future coordinate adds Scalameta 4.17.3 and its
parser/tree closure only for opt-in consumers; it is not remotely published.

`dottyInternal` owns raw untyped-tree and compiler-internal adapters for
exact-version integration. It now uses full Scala-version crossing and is the
selected future version-coupled backend boundary for the first peer consumer.
The source remains part of this repository, but `publish / skip := true`
prevents an unsupported standalone artifact promise. The public-for-JVM-access
`ContextualMethodPeerBridge` is deliberately confined to one Scalameta
`Defn.Def` plus virtual-source-name input and one positioned `untpd.DefDef`
plus provenance output. It delegates to the existing neutral projection,
validated IR, raw lowering, and generated-origin position planner. Its compiler
types make it exact-version experimental API, not a stable raw quasiquote
family. Other adapters, planning objects, errors, and reverse projections stay
package-private. The backend does not print/reparse or manufacture comments,
tokens, formatting, symbols, or owners.

Its bounded term backend includes ordinary quoted standard-`s` interpolation:
compiler-free semantic parts and guest terms lower directly to
`untpd.InterpolatedString` and to generated-origin trees with recursively
validated parser-equivalent spans. The implementation does not parse in
production, expose raw trees publicly, or desugar through `StringContext`.

The aggregate root packages no production classes and is unpublished. The
module-graph verification task checks source ownership and rejects hidden
frontend/backend cycles.

Only `core` and compiler-matching `frontend` are existing release artifacts.
The Scalameta and exact-backend coordinates are future candidates only and
remain remotely skipped. Their experimental APIs are not included in the
released 618-row `core`/`frontend` baseline. That inventory does not promote
package-private implementation or create a compatibility promise beyond the
experimental versioning policy. See the
[Scalameta opt-in artifact topology](SCALAMETA_OPT_IN_ARTIFACT_TOPOLOGY.md).

The [execution-environment and representation guide](EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md)
maps these module boundaries to compile-time macros, runtime staging,
compiler-free structural values, parser results, and the current term/type/
definition representation inventory.
