# Getting started

The project has two deliberately different entry points. Use `core` for
compiler-free structural values. Use `frontend` only when source parsing,
quoted reflection, `qr`, or term patterns are required.

No artifact is available from a remote resolver yet. The declarations below
describe local-publication coordinates and use the retained development
version `0.1.0-SNAPSHOT`.

## Compiler-free core

```scala
libraryDependencies +=
  "io.github.dmytromitin" %% "quasiquotes-scala3-core" % "0.1.0-SNAPSHOT"
```

The core first-use path needs only `quasiquotes.publicapi.*`. It does not put
the Scala compiler implementation on the consumer classpath.

<!-- snippet:core-first-use:start -->
```scala
import quasiquotes.publicapi.*
import quasiquotes.parser.TermShape

object CoreFirstUseSnippet:
  val constructorShape = TermShape.New(
    "java.lang.StringBuilder",
    List(TermShape.Literal("16"))
  )

  val method: Either[PublicFailure, DefinitionResultView] =
    for
      show <- CompletedType.named("Show")
      a <- CompletedType.typeParameter("A")
      showA <- CompletedType.applied(show, Vector(a))
      instance <- CompletedTerm.reference("instance")
      result <- DefinitionConstruction.contextualMethod(
        "apply", "A", "instance", showA, showA, instance
      )
    yield result
```
<!-- snippet:core-first-use:end -->

On success, `method.map(_.toString)` contains:

```text
def apply[A](using instance: Show[A]): Show[A] = instance
```

On failure, inspect `PublicFailure.code`, `message`, and optional `anchor`.
For example, an empty type-application argument vector returns code
`invalid-type-application`, anchor `type-application`, and the message
`A type application requires at least one argument.`

## Matching-line frontend

The frontend coordinate includes the full Scala compiler version. It must
exactly match the compiler used by the consumer build:

```scala
libraryDependencies +=
  "io.github.dmytromitin" %
    s"quasiquotes-scala3-frontend_${scalaVersion.value}" %
    "0.1.0-SNAPSHOT"
```

The following fixture is compiled from outside all `quasiquotes.*` packages.
It demonstrates parsing, structural patterns, construction, `QuasiPattern`, a
real `qr` macro, and the explicit `toTypeRepr` extension import.

<!-- snippet:frontend-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern
import quasiquotes.types.{
  ConstructedType,
  QuasiTypeConstruct,
  TypeNormalForm,
  TypeNormalFormSource,
  TypePatternSource,
  TypeQuasiquoteError,
  toTypeRepr
}

object FrontendFirstUseSnippet:
  val parsed = TypeNormalFormSource.fromSource(
    "Either[List[Int], Option[String]]"
  )
  val pattern = TypePatternSource.fromSource(
    "Either[List[$left], Option[$right]]"
  )
  val constructed = QuasiTypeConstruct.fromTemplate(
    "Either[List[$left], Option[$right]]",
    "left" -> TypeNormalForm.STypeIdent("Int"),
    "right" -> TypeNormalForm.STypeIdent("String")
  )
  val termPattern = QuasiPattern.term("$left + $right")
  val constructorPattern = QuasiPattern.term(
    "new java.lang.StringBuilder($capacity)"
  )

  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  inline def capacity(value: Int): Int =
    ${ capacityImpl('value) }

  private def addImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*

    qr"${left.asTerm} + ${right.asTerm}".asExprOf[Int]

  private def capacityImpl(value: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*

    val created = qr"new java.lang.StringBuilder(${value.asTerm})"
    '{ ${ created.asExprOf[java.lang.StringBuilder] }.capacity() }

  private def lowerInsideMacro(
      value: ConstructedType
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    value.toTypeRepr
```
<!-- snippet:frontend-first-use:end -->

`TypeNormalFormSource.equalSources`, `TypePatternSource.fromSourceLocated`, and
`TypeTemplateSource.fromSource` are available for the corresponding focused
operations. Prefer the `Located` form when reporting source failures to a
person. See [Diagnostics](DIAGNOSTICS.md) for exact examples.

## Supported boundary

The type subset admits `Int`, `String`, `Boolean`, and (for inspection)
`AnyVal`; recursive `List` and `Option`; binary `Either`; Tuple2/Tuple3; and
Function1/Function2. General name resolution, `Map`, selected constructors such
as `scala.Either`, constructor holes such as `$F[Int]`, Tuple4, and Function3
are intentionally unsupported.

Term construction and matching are structural subsets too. Unsupported syntax
returns an error instead of falling back to unchecked compiler trees. The
source checkout remains the supported way to experiment until an explicitly
authorized remote release exists.

The frontend also admits one binder-aware ordinary lambda shape such as
`(x: Int) => x + 1` for `qr` construction and `QuasiPattern`. Binder names are
alpha-equivalent, free references remain distinct, and external splices are
not captured by same-text lambda parameters. See
[Supported syntax and limitations](SUPPORTED_SYNTAX_AND_LIMITATIONS.md#binder-aware-lambda1)
for the exact boundary and reflected body-hole ownership caveat.
