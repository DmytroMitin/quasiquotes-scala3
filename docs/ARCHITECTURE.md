# Architecture

The dependency direction is intentionally one-way:

```text
frontend ----------------> core
neutralScalameta ---------> core
dottyInternal ---> neutralScalameta ---> core
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

`neutralScalameta` owns the unpublished compiler-free Scalameta 4.17.3 source
AST boundary and the bounded structural projection into existing validated
core results. It uses ordinary Scala 3 binary crossing. Its compile/runtime
classpath contains neither the Scala compiler implementation, `scala3-staging`,
nor SemanticDB. Scalameta remains absent from `core` and `frontend`.

`dottyInternal` owns raw untyped-tree and compiler-internal adapters that are
useful for exact-version integration proofs. The source remains part of this
repository, but `publish / skip := true` prevents an unsupported standalone
artifact promise. Its admitted Scalameta bridge lowers through the validated IR
and projects exact raw trees back to generated/no-position Scalameta definitions
structurally. It does not print/reparse or manufacture comments, tokens,
formatting, offsets, symbols, or owners.

Its bounded term backend includes ordinary quoted standard-`s` interpolation:
compiler-free semantic parts and guest terms lower directly to
`untpd.InterpolatedString` and to generated-origin trees with recursively
validated parser-equivalent spans. The implementation does not parse in
production, expose raw trees publicly, or desugar through `StringContext`.

The aggregate root packages no production classes and is unpublished. The
module-graph verification task checks source ownership and rejects hidden
frontend/backend cycles.

Only `core` and compiler-matching `frontend` are existing release artifacts.
The experimental neutral module remains unpublished, and its provisional APIs
are not included in the released API baseline. That inventory does not promote
package-private implementation or create a compatibility promise beyond the
experimental versioning policy.

The [execution-environment and representation guide](EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md)
maps these module boundaries to compile-time macros, runtime staging,
compiler-free structural values, parser results, and the current term/type/
definition representation inventory.
