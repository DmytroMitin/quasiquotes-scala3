# Architecture

The dependency direction is intentionally one-way:

```text
frontend --------> core
dottyInternal ---> core
publicApiExamples -> frontend
publicCoreExamples -> core
```

`core` owns compiler-free values, structural normal forms, templates,
construction and matching algorithms, and neutral source/diagnostic metadata.
Its compile and runtime classpaths must not contain Scala compiler artifacts.
Its package-private definition model reuses the same `BinderId` scope algebra
as Lambda1 for one ordinary method parameter; display spelling never replaces
semantic identity. The public compiler-free identity-method constructor creates
that package-private bound-reference representation and returns only a narrow
projection. The current source-metadata carrier and exact definition backends
deliberately stop before this new variant.

`frontend` owns source parsing, macros, quoted reflection, source-to-core
adapters, and compiler-version-sensitive lowering. It uses full Scala compiler
version coordinates because its public surface and dependency graph are tied to
the compiler line.

`dottyInternal` owns raw untyped-tree and compiler-internal adapters that are
useful for exact-version integration proofs. The source remains part of this
repository, but `publish / skip := true` prevents an unsupported standalone
artifact promise.

Its bounded term backend includes ordinary quoted standard-`s` interpolation:
compiler-free semantic parts and guest terms lower directly to
`untpd.InterpolatedString` and to generated-origin trees with recursively
validated parser-equivalent spans. The implementation does not parse in
production, expose raw trees publicly, or desugar through `StringContext`.

The aggregate root packages no production classes and is unpublished. The
module-graph verification task checks source ownership and rejects hidden
frontend/backend cycles.

Only `core` and compiler-matching `frontend` are intended release artifacts.
Their public surface is recorded in the reviewable API baseline. The inventory
does not promote package-private implementation or create a compatibility
promise beyond the experimental versioning policy.
