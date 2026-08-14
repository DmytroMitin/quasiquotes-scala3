package external.consumer

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
