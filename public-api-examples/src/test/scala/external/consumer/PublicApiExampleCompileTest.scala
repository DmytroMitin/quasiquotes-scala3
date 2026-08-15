package external.consumer

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.Quotes

import quasiquotes.types.ConstructedType
import quasiquotes.types.QuasiTypeConstruct
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.TypeNormalFormSource
import quasiquotes.types.TypePatternSource
import quasiquotes.types.TypeTemplateSource
import quasiquotes.types.TypeQuasiquoteError
import quasiquotes.types.toTypeRepr
import quasiquotes.matching.QuasiPattern
import quasiquotes.source.DiagnosticPrecision

final class PublicApiExampleCompileTest extends munit.FunSuite:
  test("frontend source adapters are callable outside quasiquotes packages"):
    val intType = TypeNormalFormSource.fromSource("Int")
    val equalTypes = TypeNormalFormSource.equalSources("List[Int]", "List[Int]")
    val pattern = TypePatternSource.fromSource("List[$element]")
    val template = TypeTemplateSource.fromSource("List[$element]")

    assertEquals(intType, Right(TypeNormalForm.STypeIdent("Int")))
    assertEquals(equalTypes, Right(true))
    assert(pattern.isRight)
    assert(template.isRight)

  test("frontend construction reaches the compiler-free core through declared dependencies"):
    val constructed = QuasiTypeConstruct.fromTemplate(
      "List[$element]",
      "element" -> TypeNormalForm.STypeIdent("String")
    )

    assertEquals(constructed.map(_.source), Right("List[String]"))

  test("external frontend consumer parses matches and constructs nested Either types"):
    val normal = TypeNormalFormSource.fromSource(
      "Either[List[Int], Option[String]]"
    )
    val pattern = TypePatternSource.fromSource(
      "Either[List[$left], Option[$right]]"
    )
    val constructed = QuasiTypeConstruct.fromTemplate(
      "List[Either[$left, $right]]",
      "left" -> TypeNormalForm.STypeIdent("Int"),
      "right" -> TypeNormalForm.STypeIdent("String")
    )

    assertEquals(
      normal.map(_.render),
      Right(
        "STypeApply(STypeIdent(Either), [STypeApply(STypeIdent(List), [STypeIdent(Int)]), STypeApply(STypeIdent(Option), [STypeIdent(String)])])"
      )
    )
    assert(pattern.isRight)
    assertEquals(constructed.map(_.source), Right("List[Either[Int, String]]"))
    assert(quasiquotes.matching.QuasiPattern.term("foo($value)").isRight)
    assert(quasiquotes.matching.QuasiPattern.term("$value + $value").isRight)
    assertEquals(PublicUserSmokeMacros.add(2, 3), 5)
    assertEquals(PublicUserSmokeMacros.greeting("Ada"), "hello Ada")
    assertEquals(PublicUserSmokeMacros.lambdaIdentity(7)(7), 7)
    assertEquals(PublicUserSmokeMacros.lambdaPreservesOuter(41)(999), 41)
    assert(QuasiPattern.term("(x: Int) => x").isRight)

  test("documented frontend first use stays executable"):
    assert(FrontendFirstUseSnippet.parsed.isRight)
    assert(FrontendFirstUseSnippet.pattern.isRight)
    assertEquals(
      FrontendFirstUseSnippet.constructed.map(_.source),
      Right("Either[List[Int], Option[String]]")
    )
    assert(FrontendFirstUseSnippet.termPattern.isRight)
    assert(FrontendFirstUseSnippet.constructorPattern.isRight)
    assertEquals(FrontendFirstUseSnippet.add(2, 3), 5)
    assertEquals(FrontendFirstUseSnippet.capacity(16), 16)

  test("documented Lambda1 first use stays executable outside quasiquotes packages"):
    val free = 1
    assertEquals(Lambda1FirstUseSnippet.increment(7), 8)
    assert(Lambda1FirstUseSnippet.alphaEquivalent)
    assert(Lambda1FirstUseSnippet.freeReferenceDoesNotMatchBound(free))
    assert(Lambda1FirstUseSnippet.completeBodyHoleMatches)
    assertEquals(Lambda1FirstUseSnippet.preserveX(41)(999), 41)

  test("documented qq extractor first use stays in the external caller Quotes path"):
    assertEquals(extractAddition(20, 22), (20, 22))
    assert(!classifyNonAddition(42))
    assertEquals(extractNested(10, 42, 5), 42)
    assertEquals(extractWithSameTextLiteral(20, 22), 22)

  test("documented type interpolator first use stays in the external caller Quotes path"):
    assertEquals(
      TypeInterpolatorFirstUseSnippet.constructionSummary,
      "STypeApply(STypeIdent(Either), [STypeIdent(Int), STypeApply(STypeIdent(List), [STypeIdent(String)])])"
    )
    assertEquals(
      TypeInterpolatorFirstUseSnippet.captureSummary[Either[Int, Boolean]],
      "STypeIdent(Int) then STypeIdent(Boolean)"
    )
    assert(TypeInterpolatorFirstUseSnippet.zeroHoleMatches[Int])
    assert(!TypeInterpolatorFirstUseSnippet.zeroHoleMatches[String])
    assert(TypeInterpolatorFirstUseSnippet.unsupportedTargetFallsThrough)
    assert(TypeInterpolatorFirstUseSnippet.ordinaryApisCoexist)

  test("public dqr builds an owner-correct local identity method outside quasiquotes packages"):
    assertEquals(DqrFirstUseSnippet.identity(42), 42)
    assertEquals(DqrSelectiveImportSnippet.identity(41), 41)

  test("public single-parameter definition pattern preserves exact caller-owned trees"):
    assert(DefinitionPatternFirstUseSnippet.configured.isRight)
    assertEquals(DefinitionPatternFirstUseSnippet.dqrIdentity(42), 42)
    assertEquals(DefinitionPatternFirstUseSnippet.independent(123), 3)
    assertEquals(DefinitionPatternFirstUseMacros.preservesMixedReferences(2), 42)
    assert(DefinitionPatternFirstUseMacros.mismatchesAreNone)

  test("public single-parameter definition pattern rejects invalid configurations without leakage"):
    val invalidSources = Vector(
      null,
      "not a definition",
      "def parameterless: Int = $body",
      "def two(left: Int, right: Int): Int = $body",
      "def clauses(left: Int)(right: Int): Int = $body",
      "def contextual(using value: Int): Int = $body",
      "def polymorphic[A](value: Int): Int = $body",
      "def selected(value: Map[Int, String]): Int = $body",
      "def selected(value: Int): Map[Int, String] = $body",
      "def selected(value: Int): Int = value",
      "def selected(value: $input): Int = $body",
      "def selected(value: Int): $output = $body",
      "def $method(value: Int): Int = $body",
      "def selected($parameter: Int): Int = $body",
      "def `selected`(value: Int): Int = $body"
    )
    val errors = invalidSources.map(quasiquotes.matching.DefinitionPattern.singleParameter).map(_.swap.toOption.get)

    assert(errors.forall(_.message.nonEmpty))
    assert(errors.forall(error =>
      !error.message.contains("Phase") &&
        !error.message.contains("DefinitionTemplate") &&
        !error.message.contains("__") &&
        !error.message.contains("dotty.tools") &&
        !error.message.contains("quotes.reflect")
    ))

  test("public dqr reports a hostile null literal part without internal leakage"):
    val errors = typeCheckErrors("external.consumer.DqrNegativeMacros.nullLiteralPart")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Invalid dqr definition template:")))
    assert(errors.forall(error => !error.message.contains("NullPointerException")))

  test("public dqr rejects every excluded definition and splice category cleanly"):
    val failures = Vector(
      typeCheckErrors("external.consumer.DqrNegativeMacros.malformed"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.parameterless"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.twoParameters"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.multipleClauses"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.contextualParameter"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.typeParameter"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.bodyHole"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.nameHole"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.wholeDefinitionHole"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.wrongArity"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.unsupportedType"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.unequalTypes"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.wrongBodyBinder"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.constructorSyntax"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.otherDefinitionSyntax"),
      typeCheckErrors("external.consumer.DqrNegativeMacros.sequenceShapedHoles")
    )
    val messages = failures.flatten.map(_.message)

    assert(failures.forall(_.nonEmpty))
    assert(messages.forall(_.contains("Invalid dqr definition template:")))
    assert(messages.forall(message =>
      !message.contains("PublicDefinitionQuasiquote") &&
        !message.contains("DefinitionName") &&
        !message.contains("dotty.tools") &&
        !message.contains("quotes.reflect")
    ))

  test("public dqr rejects a non-TypeRepr splice at the Scala signature boundary"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.construct.Quasiquotes.*
        def invalid(using q: Quotes): q.reflect.DefDef =
          import q.reflect.*
          val notATypeRepr = "Int"
          val resultType = TypeRepr.of[Int]
          dqr"def id(x: $notATypeRepr): $resultType = x"
      }"""
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("TypeRepr")))

  test("external frontend consumer receives actionable located diagnostics"):
    val failures = Vector(
      TypePatternSource.fromSourceLocated("Map[Int, String]").swap.toOption.get,
      TypePatternSource.fromSourceLocated("scala.Either[Int, String]").swap.toOption.get,
      TypePatternSource.fromSourceLocated("Either[Int]").swap.toOption.get,
      TypePatternSource.fromSourceLocated("List[Int, String]").swap.toOption.get,
      TypePatternSource.fromSourceLocated("$F[Int]").swap.toOption.get
    )
    val messages = failures.map(_.diagnostic.message)

    assert(messages(0).contains("Unsupported applied type constructor `Map`"))
    assert(messages(1).contains("Selected type constructor syntax `scala.Either[...]`"))
    assert(messages(2).contains("Expected exactly 2 type arguments for `Either`, but found 1."))
    assert(messages(3).contains("Expected exactly 1 type argument for `List`, but found 2."))
    assert(messages(4).contains("Type-constructor hole `$F[...]` is not supported"))
    assert(failures.forall(_.location.exists(_.precision == DiagnosticPrecision.WholeSource)))
    assert(messages.forall(message => !message.contains("Phase") && !message.contains("__tqhole_")))

    val missing = QuasiTypeConstruct
      .fromTemplateLocated("Either[$left, $right]", "right" -> TypeNormalForm.STypeIdent("String"))
      .swap.toOption.get
    assertEquals(missing.diagnostic.message, "Missing type-construction binding `$left`.")
    assertEquals(missing.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))

    val repeated = QuasiTypeConstruct
      .fromTemplateLocated("Either[$left, $left]", Map.empty)
      .swap.toOption.get
    assertEquals(repeated.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))

    val extra = QuasiTypeConstruct
      .fromTemplateLocated(
        "List[$element]",
        "element" -> TypeNormalForm.STypeIdent("Int"),
        "unused" -> TypeNormalForm.STypeIdent("String")
      )
      .swap.toOption.get
    assert(extra.diagnostic.message.contains("Unexpected type-construction binding(s): `$unused`."))
    assertEquals(extra.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))

    val malformedType = TypeTemplateSource.fromSourceLocated("Int)").swap.toOption.get
    assertEquals(malformedType.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))

    val malformedTerm = QuasiPattern.termLocated("foo; bar").swap.toOption.get
    assertEquals(malformedTerm.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))

  // Compiling this method proves that lowering needs the explicit frontend
  // extension import even though no macro is run here.
  private def lowerInsideMacro(
      constructed: ConstructedType
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    constructed.toTypeRepr

  private def extractAddition(left: Int, right: Int): (Int, Int) =
    QqExtractorFirstUseSnippet.splitAddition(left + right)

  private def classifyNonAddition(value: Int): Boolean =
    QqExtractorFirstUseSnippet.isAddition(value)

  private def extractNested(left: Int, middle: Int, right: Int): Int =
    QqExtractorFirstUseSnippet.nestedMiddle((left + middle) + right)

  private def extractWithSameTextLiteral(qqCapture0: Int, value: Int): Int =
    QqExtractorFirstUseSnippet.literalAndCapture(qqCapture0 + value)
