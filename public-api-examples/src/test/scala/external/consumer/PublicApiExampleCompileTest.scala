package external.consumer

import scala.collection.mutable.ArrayBuffer
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
  test("compiler-backed runtime parsing needs no active Quotes"):
    val (source, shape, rawClass) = RuntimeParserExample.summary.toOption
      .getOrElse(fail("expected runtime parser success"))
    assertEquals(source, "1 + 2")
    assertEquals(shape, "Infix(Literal(1), +, Literal(2))")
    assert(rawClass.startsWith("dotty.tools.dotc.ast."), rawClass)

  test("source-backed compile-time qr and qq remain executable"):
    assertEquals(StagingNoSpanExamples.add(1, 2), 3)

  test("staging.withQuotes matches source-free qr trees without evaluation"):
    assertEquals(StagingNoSpanExamples.inspectWithQuotes(1, 2), ("1", "2"))

  test("staging.run compiles and evaluates a source-free qr and qq result"):
    assertEquals(StagingNoSpanExamples.runAdd(1, 2), 3)

  test("runtime-staged Expr values expose unavailable source positions"):
    val evidence = StagingNoSpanExamples.sourceFreePositionEvidence(1)
    assert(
      evidence.sourceCode.fold(_.contains("NoSpan"), _.isEmpty),
      evidence.toString
    )
    assert(evidence.start.left.exists(_.contains("NoSpan")), evidence.toString)
    assert(evidence.end.left.exists(_.contains("NoSpan")), evidence.toString)

  test("generated staging arguments use the macro diagnostic fallback"):
    assert(
      quasiquotes.construct.StagingGeneratedPositionExamples
        .generatedArgumentPositionFallsBack(1)
    )

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

  test("documented P1 block first use preserves order and captures outside quasiquotes packages"):
    val log = ArrayBuffer.empty[Int]
    def mark(value: Int): Int =
      log += value
      value

    assertEquals(
      P1BlockFirstUseSnippet.ordered(
        { mark(1); () },
        { mark(2); () },
        mark(3)
      ),
      3
    )
    assertEquals(log.toList, List(1, 2, 3))
    assertEquals(
      P1BlockFirstUseSnippet.capture {
        mark(4)
        mark(5)
        mark(6)
      },
      (4, 5, 6)
    )

  test("documented P2 local val first use binds and matches outside quasiquotes packages"):
    assertEquals(P2LocalValFirstUseSnippet.bind(42), 42)
    assert(
      P2LocalValFirstUseSnippet.alphaEquivalent {
        val renamed: Int = 7
        renamed
      }
    )
    assertEquals(
      P2LocalValFirstUseSnippet.captureInitializer {
        val renamed: Int = 9
        renamed
      },
      9
    )
    val x = 41
    assertEquals(P2LocalValFirstUseSnippet.preserveExternal(x), 41)

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

  test("why-quasiquotes comparisons stay executable outside library packages"):
    assertEquals(WhyQuasiquotesCurrentExamples.standardAdd(20, 22), 42)
    assertEquals(WhyQuasiquotesCurrentExamples.quasiquoteAdd(20, 22), 42)
    assertEquals(WhyQuasiquotesCurrentExamples.manualSplit(20, 22), (20, 22))
    assertEquals(WhyQuasiquotesCurrentExamples.quasiquoteSplit(20, 22), (20, 22))
    assert(WhyQuasiquotesCurrentExamples.nestedTypeConstructionAgrees)
    assert(WhyQuasiquotesCurrentExamples.nestedTypePatternAgrees)

  test("north-star manual reflection baselines stay executable outside library packages"):
    assertEquals(NorthStarManualReflectionExamples.dynamicAppliedTypeArity, 2)
    assert(NorthStarManualReflectionExamples.refinementAliasIsString)
    assertEquals(
      NorthStarManualReflectionExamples.constructExistingProduct[NorthStarProduct](20, 22),
      NorthStarProduct(20, 22)
    )

  test("validated dynamic selected-member names close the manual Select.unique gap"):
    val receiver = new SelectedMemberUserTarget

    assertEquals(SelectedMemberNameFirstUseSnippet.fixed(receiver, 40), 41)
    assertEquals(SelectedMemberNameFirstUseSnippet.manual(receiver, 40), 41)
    assertEquals(SelectedMemberNameFirstUseSnippet.dynamic(receiver, "ordinary", 40), 41)
    assertEquals(SelectedMemberNameFirstUseSnippet.dynamic(receiver, "+", 40), 42)
    assertEquals(SelectedMemberNameFirstUseSnippet.dynamic(receiver, "type", 40), 43)
    assertEquals(
      SelectedMemberNameFirstUseSnippet.dynamic(receiver, "safe spaced name", 40),
      44
    )

  test("public dqr builds an owner-correct local identity method outside quasiquotes packages"):
    assertEquals(DqrFirstUseSnippet.identity(42), 42)
    assertEquals(DqrSelectiveImportSnippet.identity(41), 41)

  test("public single-parameter definition pattern preserves exact caller-owned trees"):
    assert(DefinitionPatternFirstUseSnippet.configured.isRight)
    assertEquals(DefinitionPatternFirstUseSnippet.dqrIdentity(42), 42)
    assertEquals(DefinitionPatternFirstUseSnippet.independent(123), 3)
    assertEquals(DefinitionPatternFirstUseMacros.preservesMixedReferences(2), 42)
    assert(DefinitionPatternFirstUseMacros.mismatchesAreNone)

  test("public dqq captures only the original body and composes with public imports"):
    assertEquals(DefinitionPatternFirstUseMacros.dqqMatchDqr(42), 42)
    assertEquals(DefinitionPatternFirstUseMacros.dqqMatchIndependent(123), 3)
    assertEquals(DefinitionPatternFirstUseMacros.dqqPreservesMixedReferences(2), 42)
    assert(DefinitionPatternFirstUseMacros.dqqMismatchesFallThrough)
    assertEquals(DefinitionPatternFirstUseMacros.dqqSelectiveImport(41), 41)
    assertEquals(DefinitionPatternFirstUseMacros.dqqWildcardImports(40), 40)

  test("public dqq rejects hostile contexts and excluded template shapes without leakage"):
    val failures = Vector(
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqNullContext"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqNullLiteralPart"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqZeroSlots"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqTwoSlots"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqMethodNameSlot"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqParameterNameSlot"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqParameterTypeSlot"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqResultTypeSlot"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqPartialRhsSlot"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqTwoParameters"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqContextualParameter"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqTypeParameter"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqWrongDefinitionCategory"),
      typeCheckErrors("external.consumer.DefinitionPatternFirstUseMacros.dqqMalformed")
    )
    val messages = failures.flatten.map(_.message)

    assert(failures.forall(_.nonEmpty))
    assert(messages.forall(_.contains("Invalid dqq definition-pattern template:")))
    assert(messages.forall(message =>
      !message.contains("DefinitionTemplate") &&
        !message.contains("SingleParameterDefinition") &&
        !message.contains("dotty.tools") &&
        !message.contains("quotes.reflect") &&
        !message.contains("NullPointerException")
    ))

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
