# quasiquotes-scala3

An experimental Scala 3 library for structural quasiquote research. The
repository separates compiler-free representations and algorithms from
compiler-coupled parsing, reflection, and lowering.

The project is a research proof of concept. Its API, coordinates, supported
syntax, and compatibility policy may change. No artifact is currently
available from a remote package repository.

## Modules

- `core` contains compiler-free term/type/definition values, construction,
  matching, source metadata, and stable diagnostic projections.
- `frontend` supplies Scala 3 compiler-coupled parsing, macros, quoted
  reflection adapters, and public source-oriented conveniences.
- `dottyInternal` contains exact-compiler internal adapters. Its source is
  present for review and testing, but its artifact is deliberately unpublished.
- `public-core-examples` and `public-api-examples` compile consumer code from
  outside the library packages.

## Try the source build

Requirements are JDK 25 and sbt 1.12.8. The required baseline is Scala 3.8.4.

```sh
sbt -batch clean test publicCoreExamples/test publicApiExamples/test \
  core/verifyCoreBoundary verifyModuleGraph package
```

The build serializes tasks and uses exported test/compile JARs with flat test
class-loader layering to keep the aggregate gate deterministic.

## Provisional local coordinates

Local publication experiments use version `0.1.0-SNAPSHOT`:

```scala
libraryDependencies +=
  "io.github.dmytromitin" %% "quasiquotes-scala3-core" % "0.1.0-SNAPSHOT"

libraryDependencies +=
  "io.github.dmytromitin" %
    "quasiquotes-scala3-frontend_3.8.4" % "0.1.0-SNAPSHOT"
```

`core` uses ordinary Scala 3 binary crossing. `frontend` uses full compiler
version crossing and must match the consuming compiler line. These coordinates
are provisional local evidence, not a release or remote-availability claim.

See [Getting started](docs/GETTING_STARTED.md),
[diagnostics](docs/DIAGNOSTICS.md),
[architecture](docs/ARCHITECTURE.md),
[supported syntax and limitations](docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md),
[compatibility](docs/COMPATIBILITY.md),
[public API shape compatibility review](docs/API_COMPATIBILITY_REVIEW.md),
[versioning and stability](docs/VERSIONING_AND_STABILITY.md), and the
[release process](docs/RELEASE_PROCESS.md).

The machine-readable [public API baseline](docs/PUBLIC_API_BASELINE.tsv)
contains 284 core and 291 frontend Scaladoc-visible entries. It excludes the
root, unpublished `dottyInternal`, and package-private internals and is a diff
baseline rather than a compatibility promise.

The structural type subset includes recursively nested `List` and `Option`
applications plus binary `Either`, including patterns, construction, quoted
lowering/inspection, typed ascriptions, and scoped type evidence. Constructor
admission remains fixed and deliberately excludes general name resolution.

The canonical first-use examples, including the complete Lambda1 macro path,
are mirrored from compiled external-package fixtures, and the repository's
snippet drift check compares them byte for byte.
Public type diagnostics describe the supported boundary without development
chronology or generated placeholder names.

## License

This project is licensed under the Apache License, Version 2.0. See
[LICENSE](LICENSE). Repository visibility and remote artifact publication
remain separate decisions; no artifact is currently available remotely.

Support expectations are intentionally conservative; see [Support](SUPPORT.md)
and [Security policy](SECURITY.md). A private security-reporting channel is not
yet available and remains a public-visibility gate.

The experimental frontend also supports bounded fully-qualified constructor
expressions such as `new java.lang.StringBuilder(16)` for `qr` construction and
structural patterns. See the supported-syntax document for the deliberate
generic, imported-name, multiple-list, and anonymous-class exclusions.

One ordinary explicitly typed Lambda1 form is also available for structural
`qr` construction and matching. Its project-owned binder identity provides
alpha-aware bound-reference comparison and same-text splice non-capture. The
unpublished exact internal backend supports that same bounded Lambda1 shape in
source-free and generated-origin modes; broader lambda/block syntax remains
excluded.
