# Getting started

The project has two deliberately different entry points. Use `core` for
compiler-free structural values. Use `frontend` only when source parsing,
quoted reflection, `qr`, or term patterns are required.

For the distinct compile-time macro, runtime `staging.withQuotes`, runtime
`staging.run`, compiler-free `TermShape`, and compiler-backed parser modes, see
[Execution environments and AST representations](EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md).

The declarations below use the latest immutable Maven Central release,
`0.2.0`. The current source tree is the unpublished development version
`0.3.0-SNAPSHOT`; use it only through an intentional local build or
`publishLocal` workflow.

## Compiler-free core

```scala
libraryDependencies +=
  "com.github.dmytromitin" %% "quasiquotes-scala3-core" % "0.2.0"
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

## Single-parameter definition first use

The compiler-free core can also construct the bounded identity-method family.
The body factory is deliberately explicit: it creates a definition-parameter
reference, not a free same-text stable reference.

<!-- snippet:definition-first-use:start -->
```scala
import quasiquotes.publicapi.*

object DefinitionFirstUseSnippet:
  val identity: Either[PublicFailure, SingleParameterMethodResultView] =
    for
      intType <- CompletedType.named("Int")
      parameter <- CompletedTerm.definitionParameterReference("x")
      method <- DefinitionConstruction.singleParameterMethod(
        "id", "x", intType, intType, parameter
      )
    yield method
```
<!-- snippet:definition-first-use:end -->

On success, `identity.map(_.source)` contains
`def id(x: Int): Int = x`. The result exposes the name, parameter name and
type, result type, explicit body projection, and rendering. A free
`CompletedTerm.reference("x")` is rejected rather than silently captured.
This surface constructs a compiler-free semantic value; it does not place,
lower, or execute a Scala method.

## Two-parameter definition first use

The exact-two projection keeps parameter order explicit and resolves the body
name once at the public boundary. Internally, the selected parameter remains a
binder-identity reference rather than a textual substitution.

<!-- snippet:two-parameter-definition-first-use:start -->
```scala
import quasiquotes.publicapi.*

object TwoParameterDefinitionFirstUseSnippet:
  private val intType = CompletedType.named("Int")
  private val stringType = CompletedType.named("String")

  val first: Either[PublicFailure, TwoParameterMethodResultView] =
    for
      firstType <- intType
      secondType <- stringType
      parameter <- CompletedTerm.definitionParameterReference("x")
      method <- DefinitionConstruction.twoParameterMethod(
        "first", "x", firstType, "y", secondType, firstType, parameter
      )
    yield method

  val second: Either[PublicFailure, TwoParameterMethodResultView] =
    for
      firstType <- intType
      secondType <- stringType
      parameter <- CompletedTerm.definitionParameterReference("y")
      method <- DefinitionConstruction.twoParameterMethod(
        "second", "x", firstType, "y", secondType, secondType, parameter
      )
    yield method
```
<!-- snippet:two-parameter-definition-first-use:end -->

The parameter names must be distinct. The body may explicitly reference either
declared parameter, and its result type must equal that selected parameter's
type. Unknown definition-parameter references and free same-text references are
rejected with `invalid-two-parameter-method-contract` rather than captured.

## Matching-line frontend

The frontend coordinate includes the full Scala compiler version. It must
exactly match the compiler used by the consumer build:

```scala
libraryDependencies +=
  "com.github.dmytromitin" %
    s"quasiquotes-scala3-frontend_${scalaVersion.value}" %
    "0.2.0"
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

    qr"new java.lang.StringBuilder(${value.asTerm}).capacity()".asExprOf[Int]

  private def lowerInsideMacro(
      value: ConstructedType
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    value.toTypeRepr
```
<!-- snippet:frontend-first-use:end -->

## Reflected definition interpolation

Inside an active macro `Quotes`, `dqr` constructs exactly one caller-owned
single-parameter identity-like `DefDef`. The result is immediately inserted in
the same macro-generated local block; it is not portable to another `Quotes`
universe or arbitrary owner.

<!-- snippet:dqr-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*

object DqrFirstUseSnippet:
  inline def identity(value: Int): Int = ${ identityImpl('value) }

  private def identityImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    val parameterType: q.reflect.TypeRepr = TypeRepr.of[Int]
    val resultType: q.reflect.TypeRepr = TypeRepr.of[Int]
    val definition: q.reflect.DefDef =
      dqr"def boundedIdentity(value: $parameterType): $resultType = value"

    val method = definition.symbol
    val parameters = method.paramSymss.flatten
    val exactOwnerAndBinder =
      method.owner == Symbol.spliceOwner &&
        parameters.size == 1 &&
        parameters.head.owner == method &&
        definition.rhs.exists(_.symbol == parameters.head)

    if !exactOwnerAndBinder then
      report.errorAndAbort("external dqr owner/binder proof failed")

    Block(
      List(definition),
      Apply(Ref(method), List(value.asTerm))
    ).asExprOf[Int]
```
<!-- snippet:dqr-first-use:end -->

The two holes must be supported caller-owned `q.reflect.TypeRepr` values with
equal bounded normal forms. The method and parameter names are literal ordinary
identifiers, and the body must be the literal declared parameter. Body, name,
sequence, whole-definition, contextual, type-parameter, multi-parameter, and
multi-clause forms are rejected with `Invalid dqr definition template:`.

## Programmatic definition matching

`DefinitionPattern.singleParameter` is a recoverable source compiler for one
exact matcher grammar. `Left` means the pattern source is invalid. Once a
pattern is valid, a reflected target difference returns `None`, while success
returns the caller's original parameter `TypeRepr`, result `TypeRepr`, and RHS
`Term` without rebuilding them.

<!-- snippet:definition-pattern-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.matching.{
  DefinitionPattern,
  SingleParameterDefinitionMatch
}
import quasiquotes.matching.DefinitionPattern.*

object DefinitionPatternFirstUseSnippet:
  val configured = DefinitionPattern.singleParameter(
    "def boundedIdentity(value: Int): Int = $body"
  )

  def dqrIdentity(value: Int): Int =
    DefinitionPatternFirstUseMacros.matchDqr(value)

  def independent(value: Int): Int =
    DefinitionPatternFirstUseMacros.matchIndependent(value)

  def captureBody(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[q.reflect.Term] =
    target match
      case dqq"def boundedIdentity(value: Int): Int = $body" =>
        val originalBody: q.reflect.Term = body
        Some(originalBody)
      case _ => None

  def inspect(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    SingleParameterDefinitionMatch[q.reflect.TypeRepr, q.reflect.Term]
  ] =
    val pattern = DefinitionPattern
      .singleParameter("def boundedIdentity(value: Int): Int = $body")
      .toOption
      .get
    val matched = pattern.matchDefinition(target)

    matched.foreach { result =>
      val methodName: String = result.methodName
      val parameterName: String = result.parameterName
      val parameterType: q.reflect.TypeRepr = result.parameterType
      val resultType: q.reflect.TypeRepr = result.resultType
      val body: q.reflect.Term = result.body
      assert(
        methodName.nonEmpty && parameterName.nonEmpty &&
          parameterType != null && resultType != null && body != null
      )
    }
    matched
```
<!-- snippet:definition-pattern-first-use:end -->

The matcher source must have exactly the form
`def name(parameter: FixedType): FixedResultType = $body`. Parameter and result
types may differ. The compiled external fixture exercises both current `dqr`
output and an independently authored definition. `dqq` accepts the same exact
grammar in interpolated pattern syntax and captures only the complete original
RHS `Term`; ordinary target mismatches fall through to the next pattern case.

## Lambda1 construction and matching

This compiled external-package fixture constructs and invokes an `Int => Int`
lambda, matches an alpha-renamed target, distinguishes a free external
reference from a bound parameter, captures a complete body hole, and proves
that a same-text external splice is not captured.

<!-- snippet:lambda1-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern

object Lambda1FirstUseSnippet:
  inline def increment(value: Int): Int =
    ${ incrementImpl('value) }

  inline def alphaEquivalent: Boolean =
    ${ alphaEquivalentImpl }

  inline def freeReferenceDoesNotMatchBound(free: Int): Boolean =
    ${ freeReferenceDoesNotMatchBoundImpl('free) }

  inline def completeBodyHoleMatches: Boolean =
    ${ completeBodyHoleMatchesImpl }

  inline def preserveX(x: Int): Int => Int =
    ${ preserveXImpl('x) }

  private def incrementImpl(value: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*

    val fn = qr"(x: Int) => x + 1".asExprOf[Int => Int]
    '{ $fn($value) }

  private def alphaEquivalentImpl(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val target = '{ (renamed: Int) => renamed + 1 }
    Expr(QuasiPattern.termOrThrow("(x: Int) => x + 1").matchTerm(target.asTerm).isRight)

  private def freeReferenceDoesNotMatchBoundImpl(
      free: Expr[Int]
  )(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val target = '{ (_: Int) => $free }
    Expr(QuasiPattern.termOrThrow("(x: Int) => x").matchTerm(target.asTerm).isLeft)

  private def completeBodyHoleMatchesImpl(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val target = '{ (renamed: Int) => renamed + 1 }
    val matched = QuasiPattern.termOrThrow("(x: Int) => $body").matchTerm(target.asTerm)
    Expr(matched.exists(_.binding("body").nonEmpty))

  private def preserveXImpl(x: Expr[Int])(using Quotes): Expr[Int => Int] =
    import quotes.reflect.*

    val externalX = x.asTerm
    qr"(x: Int) => $externalX".asExprOf[Int => Int]
```
<!-- snippet:lambda1-first-use:end -->

For example, `increment(7)` returns `8`, and `preserveX(41)(999)` returns the
external `41`. A term returned by the complete body-hole match is the original
reflected target subtree. If it refers to the target lambda parameter, it is
scope- and owner-sensitive and must not be treated as a detached portable tree.

## Binder-free P1 block first use

P1 blocks contain one or more ordered expression prefixes and a distinct final
result. Every child must already belong to the supported Term subset. The
compiled external-package fixture uses the ordinary current-Dotty public
`qr`/`qq` surface:

<!-- snippet:p1-block-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern.*

object P1BlockFirstUseSnippet:
  inline def ordered(inline first: Unit, inline second: Unit, inline result: Int): Int =
    ${ orderedImpl('first, 'second, 'result) }

  inline def capture(inline expression: Int): (Int, Int, Int) =
    ${ captureImpl('expression) }

  private def orderedImpl(
      first: Expr[Unit],
      second: Expr[Unit],
      result: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*

    qr"{ ${first.asTerm}; ${second.asTerm}; ${result.asTerm} }".asExprOf[Int]

  private def captureImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int, Int)] =
    import q.reflect.*

    expression.asTerm match
      case qq"{ $first; $second; $result }" =>
        '{ (${first.asExprOf[Int]}, ${second.asExprOf[Int]}, ${result.asExprOf[Int]}) }
      case _ => '{ (-1, -1, -1) }
```
<!-- snippet:p1-block-first-use:end -->

Single-expression braces collapse as P0 wrappers. Binder-bearing blocks are not
P1; the next section documents the one admitted P2 form.

## Single typed local immutable val first use

P2 adds exactly one eager immutable simple local binder with an explicit
supported type. The initializer is outside its scope; only the final result may
resolve source-written references to it. The compiled fixture also proves
alpha-insensitive matching, initializer capture, and same-name external-splice
noncapture.

<!-- snippet:p2-local-val-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.QuasiPattern.*

object P2LocalValFirstUseSnippet:
  inline def bind(inline initializer: Int): Int =
    ${ bindImpl('initializer) }

  inline def alphaEquivalent(inline expression: Int): Boolean =
    ${ alphaEquivalentImpl('expression) }

  inline def captureInitializer(inline expression: Int): Int =
    ${ captureInitializerImpl('expression) }

  inline def preserveExternal(inline x: Int): Int =
    ${ preserveExternalImpl('x) }

  private def bindImpl(initializer: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"{ val x: Int = ${initializer.asTerm}; x }".asExprOf[Int]

  private def alphaEquivalentImpl(expression: Expr[Int])(using Quotes): Expr[Boolean] =
    import quotes.reflect.*
    expression.asTerm match
      case qq"{ val x: Int = $initializer; x }" => Expr(true)
      case _ => Expr(false)

  private def captureInitializerImpl(expression: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    expression.asTerm match
      case qq"{ val x: Int = $initializer; x }" => initializer.asExprOf[Int]
      case _ => Expr(-1)

  private def preserveExternalImpl(external: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"{ val x: Int = 1; ${external.asTerm} }".asExprOf[Int]
```
<!-- snippet:p2-local-val-first-use:end -->

Inferred, mutable, lazy, pattern, multiple, recursive/shadowing, and local
method forms are controlled rejections. The whole quasiquote tree may contain
at most one P2 binder, and a Lambda1 and P2 binder may not shadow one another
by source name. A distinct-name Lambda1 may coexist with the single P2 binder.
External splices containing owned definitions are rejected rather than
reowned, and same-text external splices retain their original symbol rather
than becoming source-binder shadowing.

## Bounded `qq` extractor first use

The external-package fixture below proves the extractor in the caller's active
`Quotes` path. Slots are ordered and distinct; binder spelling in the Scala
pattern is not used as semantic identity. A structural mismatch reaches the
ordinary fallback case, while malformed template source reports a controlled
macro-expansion error. For rich diagnostics and named or repeated holes, keep
using `QuasiPattern.term`, `termLocated`, or `termOrThrow`.

<!-- snippet:qq-extractor-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.matching.QuasiPattern.*

object QqExtractorFirstUseSnippet:
  inline def splitAddition(expression: Int): (Int, Int) =
    ${ splitAdditionImpl('expression) }

  inline def isAddition(expression: Int): Boolean =
    ${ isAdditionImpl('expression) }

  inline def nestedMiddle(expression: Int): Int =
    ${ nestedMiddleImpl('expression) }

  inline def literalAndCapture(expression: Int): Int =
    ${ literalAndCaptureImpl('expression) }

  inline def malformedTemplate: Unit =
    ${ malformedTemplateImpl }

  private def splitAdditionImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*

    expression.asTerm match
      case qq"$left + $right" =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def isAdditionImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    expression.asTerm match
      case qq"$left + $right" => Expr(true)
      case _ => Expr(false)

  private def nestedMiddleImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"($left + $middle) + $right" => middle.asExprOf[Int]
      case _ => Expr(-1)

  private def literalAndCaptureImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"qqCapture0 + $value" => value.asExprOf[Int]
      case _ => Expr(-1)

  private def malformedTemplateImpl(using q: Quotes): Expr[Unit] =
    import q.reflect.*

    Expr(1).asTerm match
      case qq"$value +" => '{ () }
      case _ => '{ () }
```
<!-- snippet:qq-extractor-first-use:end -->

`splitAddition` returns captures in left-to-right hole order. `isAddition`
shows ordinary fallthrough, `nestedMiddle` uses an already-supported nested
infix shape, and `literalAndCapture` keeps the literal identifier
`qqCapture0` distinct from the first synthetic ordinal slot.

## Bounded `tqr` and `tqq` type first use

This external-package fixture runs inside the caller's active `Quotes` path.
`tqr` constructs a reflected type from zero or more caller-owned `TypeRepr`
splices. `tqq` matches through the existing bounded normal form and returns the
original reflected target subtrees in left-to-right slot order. Unsupported
targets fall through; malformed or unsupported templates are controlled
macro-expansion failures. The ordinary recoverable functions remain available
under the same wildcard import.

<!-- snippet:type-interpolator-first-use:start -->
```scala
import scala.quoted.*

import quasiquotes.types.*
import quasiquotes.types.QuasiTypequotes.*

object TypeInterpolatorFirstUseSnippet:
  inline def constructionSummary: String = ${ constructionSummaryImpl }
  inline def captureSummary[T]: String = ${ captureSummaryImpl[T] }
  inline def zeroHoleMatches[T]: Boolean = ${ zeroHoleMatchesImpl[T] }
  inline def unsupportedTargetFallsThrough: Boolean = ${ unsupportedTargetFallsThroughImpl }
  inline def ordinaryApisCoexist: Boolean = ${ ordinaryApisCoexistImpl }

  private def constructionSummaryImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val element: q.reflect.TypeRepr = TypeRepr.of[String]
    val constructed: q.reflect.TypeRepr = tqr"Either[Int, List[$element]]"
    Expr(TargetTypeReprInspector.inspect(constructed).fold(_.message, _.render))

  private def captureSummaryImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    target match
      case tqq"Either[$left, $right]" =>
        Expr(
          List(left, right)
            .map(TargetTypeReprInspector.inspect(_).fold(_.message, _.render))
            .mkString(" then ")
        )
      case _ => Expr("no-match")

  private def zeroHoleMatchesImpl[T: Type](using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val target: q.reflect.TypeRepr = TypeRepr.of[T]
    Expr(target match
      case tqq"Int" => true
      case _ => false
    )

  private def unsupportedTargetFallsThroughImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val target: q.reflect.TypeRepr = TypeRepr.of[Map[Int, String]]
    Expr(target match
      case tqq"$captured" => false
      case _ => true
    )

  private def ordinaryApisCoexistImpl(using Quotes): Expr[Boolean] =
    val constructionFunction
        : (String, Seq[(String, TypeNormalForm)]) => Either[TypeQuasiquoteError, ConstructedType] =
      tqr
    val patternFunction
        : String => Either[TypeQuasiquoteError, QuasiTypePattern] =
      tqq
    Expr(
      constructionFunction("Int", Seq.empty).isRight &&
        patternFunction("Int").isRight
    )
```
<!-- snippet:type-interpolator-first-use:end -->

The pattern's Scala binder spelling does not create repeated-hole equality;
each interpolated slot has a distinct ordinal identity. Use the programmatic
`QuasiTypequotes.tqq("Either[$same, $same]")` form when named repeated-hole
equality is intended. Captures are compiler-owned reflected values and should
not be treated as detached portable types.

`TypeNormalFormSource.equalSources`, `TypePatternSource.fromSourceLocated`, and
`TypeTemplateSource.fromSource` are available for the corresponding focused
operations. Prefer the `Located` form when reporting source failures to a
person. See [Diagnostics](DIAGNOSTICS.md) for exact examples.

## Supported boundary

The type subset admits `Int`, `String`, `Boolean`, and (for inspection)
`AnyVal`; recursive `List` and `Option`; binary `Either`; Tuple2/Tuple3; and
Function1/Function2. General name resolution, `Map`, constructor holes such as
`$F[Int]`, Tuple4, and Function3 are intentionally unsupported.

## Experimental canonical global selected types

Macro and staging code can opt into the bounded programmatic selected-Type
surface by supplying exact typed witnesses explicitly:

```scala
import quasiquotes.types.{
  GlobalSelectedTypeEnvironment,
  GlobalSelectedTypeFrontend
}
import scala.quoted.*

def buildSelected(using q: Quotes): Either[quasiquotes.types.TypeQuasiquoteError, q.reflect.TypeRepr] =
  import q.reflect.*
  for
    environment <- GlobalSelectedTypeEnvironment.fromWitnesses(
      TypeRepr.of[my.project.TopLevel],
      TypeRepr.of[List[Int]]
    )
    result <- GlobalSelectedTypeFrontend.construct(
      "my.project.TopLevel",
      environment
    )
  yield result
```

The environment derives canonical Package/Type/Module owner paths from the
witnesses; callers cannot attach arbitrary source labels. Selected
`List`/`Option`/`Either` constructors are admitted only by exact full identity.
Alias spellings, import-shortened alternatives, local owners, and stable-term
paths remain deferred. This opt-in API does not change ordinary `tqr`/`tqq`.

Term construction and matching are structural subsets too. Unsupported syntax
returns an error instead of falling back to unchecked compiler trees. The
source checkout remains the supported way to experiment until an explicitly
authorized remote release exists.

The frontend admits exactly one binder-aware ordinary, explicitly typed lambda
parameter, such as `(x: Int) => x + 1`, for `qr` construction and
`QuasiPattern`. Binder names are alpha-equivalent, free references remain
distinct, and external splices are not captured by same-text lambda
parameters. See
[Supported syntax and limitations](SUPPORTED_SYNTAX_AND_LIMITATIONS.md#binder-aware-lambda1)
for the exact boundary and reflected body-hole ownership caveat.
