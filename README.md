# quasiquotes-scala3

An experimental Scala 3 library for structural quasiquote research. The
repository separates compiler-free representations and algorithms from
compiler-coupled parsing, reflection, and lowering.

The project is a research proof of concept. Its API, coordinates, supported
syntax, and compatibility policy may change. No artifact is currently
available from a remote package repository.

## Quick start

`qr` constructs a Scala 3 quoted-reflection `Term` from source-like syntax
with structural splices.

<!-- snippet:readme-quick-start:start -->
```scala
import scala.quoted.*
import quasiquotes.construct.Quasiquotes.*

object ReadmeQuickStart:
  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  private def addImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*

    addImplTerm(left.asTerm, right.asTerm).asExprOf[Int]

  private def addImplTerm(using q: Quotes)(
      left: q.reflect.Term,
      right: q.reflect.Term
  ): q.reflect.Term =
    qr"$left + $right"
```
<!-- snippet:readme-quick-start:end -->

The layers are explicit: the inline macro receives typed staged `Expr[Int]`
values, converts them to low-level `quotes.reflect.Term` trees, uses `qr` for
source-like structural construction at that reflection-tree layer, and converts
the resulting `Term` back to `Expr[Int]`. A `quotes.reflect.Term` is generally a
typed quoted-reflection tree in macro use; it is not the compiler-internal raw
`dotty.tools.dotc.ast.untpd.Tree` used only by the unpublished `dottyInternal`
module.

This exact example is compiled from an external-package fixture. See
[Getting started](docs/GETTING_STARTED.md) for the larger construction,
matching, type, Lambda1, and compiler-free definition examples.

## Quasiquote surfaces

<!-- public-surface-table:start -->
| Surface | Form | Availability | Role |
| --- | --- | --- | --- |
| `qr"..."` | Interpolator | Public now | Construct a quoted-reflection `Term` from supported term syntax and structural splices. |
| `QuasiPattern.term(...)` | Functions | Public now | Compile a supported structural term pattern (`termOrThrow` is the throwing function variant); the reserved throwing `qq` extension is not a supported general pattern interpolator. |
| `QuasiTypequotes.tqr(...)` | Function, not an interpolator | Public research API | Construct a bounded compiler-free type value from a template and explicit bindings. |
| `QuasiTypequotes.tqq(...)` | Function, not an interpolator | Public research API | Compile a bounded structural type pattern. |
| `DefinitionConstruction` | Functions | Public now | Construct compiler-free contextual, single-parameter, and exact-two-parameter definition projections. |
| `dqr"..."` | Internal interpolator | Internal research | Package-private definition construction research surface; not public API. |
| `dqq` | — | Not yet | No definition pattern interpolator is implemented. |
<!-- public-surface-table:end -->

See the [syntax support matrix](docs/SYNTAX_SUPPORT_MATRIX.md) for the current
construct/match boundary and its deliberate limits.

## Modules

- `core` contains compiler-free term/type/definition values, construction,
  matching, source metadata, and stable diagnostic projections.
- `frontend` supplies Scala 3 compiler-coupled parsing, macros, quoted
  reflection adapters, and public source-oriented conveniences.
- `dottyInternal` contains exact-compiler internal adapters. Its source is
  present for review and testing, but its artifact is deliberately unpublished.
- `public-core-examples` and `public-api-examples` compile consumer code from
  outside the library packages.

In role, `frontend` is closest to Scala 2 quasiquotes: it owns source-like
quotation/pattern syntax and compiler-coupled construction and matching.
`core` is closer to a small Scalameta-like neutral structural model. This is
only an architectural analogy: the project neither reimplements all Scala 2
quasiquotes nor provides a Scalameta replacement or full-fidelity Scala AST.

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
[syntax support matrix](docs/SYNTAX_SUPPORT_MATRIX.md),
[exact constructor backend](docs/EXACT_BACKEND_CONSTRUCTOR_NEW.md),
[supported syntax and limitations](docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md),
[compatibility](docs/COMPATIBILITY.md),
[public API shape compatibility review](docs/API_COMPATIBILITY_REVIEW.md),
[versioning and stability](docs/VERSIONING_AND_STABILITY.md), and the
[release process](docs/RELEASE_PROCESS.md).

The machine-readable [public API baseline](docs/PUBLIC_API_BASELINE.tsv)
contains 305 core and 291 frontend Scaladoc-visible entries. It excludes the
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

The compiler-free public API constructs bounded single- and exact-two-parameter
methods whose bodies explicitly select a declared parameter. That public name
selection is converted once to the package-private binder-aware definition
core; a free same-text `CompletedTerm.reference` is never captured implicitly.
This is a semantic construction/projection API, not a source parser or
method-placement backend.

## License

This project is licensed under the Apache License, Version 2.0. See
[LICENSE](LICENSE). Repository visibility and remote artifact publication
remain separate decisions; no artifact is currently available remotely.

Support expectations are intentionally conservative; see [Support](SUPPORT.md)
and [Security policy](SECURITY.md). No private security-reporting channel is
currently offered or promised for this experimental research stage. Do not
post sensitive material publicly merely to obtain maintainer attention.

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

The unpublished exact internal definition backend also supports the bounded
single-ordinary-parameter and exact-two-ordinary-parameter definition shapes
behind the compiler-free public projections. It constructs source-free raw
trees or deterministic generated-origin trees without parsing in production,
resolving parameter references by project binder identity rather than display
text. This does not expose compiler trees publicly, generalize parameter-list
syntax, or add general method placement/owner support.
