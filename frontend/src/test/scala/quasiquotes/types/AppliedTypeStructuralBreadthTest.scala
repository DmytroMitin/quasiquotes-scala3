package quasiquotes.types

import quasiquotes.source.SourceSpan

class AppliedTypeStructuralBreadthTest extends munit.FunSuite:
  private val representativeResults = List(
    result("List[Option[Int]]", QuasiTypeExamples.structuralNormalFormSummary("List[Option[Int]]"), QuasiTypeExamples.targetNormalFormSummary("List[Option[Int]]"), QuasiTypeExamples.normalFormLoweringMessage("List[Option[Int]]")),
    result("Option[List[String]]", QuasiTypeExamples.structuralNormalFormSummary("Option[List[String]]"), QuasiTypeExamples.targetNormalFormSummary("Option[List[String]]"), QuasiTypeExamples.normalFormLoweringMessage("Option[List[String]]")),
    result("Either[Int, String]", QuasiTypeExamples.structuralNormalFormSummary("Either[Int, String]"), QuasiTypeExamples.targetNormalFormSummary("Either[Int, String]"), QuasiTypeExamples.normalFormLoweringMessage("Either[Int, String]")),
    result("Either[List[Int], Option[String]]", QuasiTypeExamples.structuralNormalFormSummary("Either[List[Int], Option[String]]"), QuasiTypeExamples.targetNormalFormSummary("Either[List[Int], Option[String]]"), QuasiTypeExamples.normalFormLoweringMessage("Either[List[Int], Option[String]]")),
    result("List[Either[Int, String]]", QuasiTypeExamples.structuralNormalFormSummary("List[Either[Int, String]]"), QuasiTypeExamples.targetNormalFormSummary("List[Either[Int, String]]"), QuasiTypeExamples.normalFormLoweringMessage("List[Either[Int, String]]")),
    result("Either[Option[Int], List[Either[String, Boolean]]]", QuasiTypeExamples.structuralNormalFormSummary("Either[Option[Int], List[Either[String, Boolean]]]"), QuasiTypeExamples.targetNormalFormSummary("Either[Option[Int], List[Either[String, Boolean]]]"), QuasiTypeExamples.normalFormLoweringMessage("Either[Option[Int], List[Either[String, Boolean]]]")),
    result("Either[(Int, String), (Boolean, Int) => String]", QuasiTypeExamples.structuralNormalFormSummary("Either[(Int, String), (Boolean, Int) => String]"), QuasiTypeExamples.targetNormalFormSummary("Either[(Int, String), (Boolean, Int) => String]"), QuasiTypeExamples.normalFormLoweringMessage("Either[(Int, String), (Boolean, Int) => String]"))
  )

  test("source normal forms and TypeRepr inspection round-trip recursively"):
    representativeResults.foreach { case (source, normal, inspected, lowered) =>
      assertEquals(inspected, normal, clue(source))
      assert(!lowered.startsWith("Cannot lower"), clue(source, lowered))
    }

  test("nested and binary patterns bind ordered arguments and enforce repeats"):
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("Either[$a, $b]", "Either[Int, String]"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("Either[$a, $a]", "Either[Int, Int]"),
      "matched=true bindings=a=STypeIdent(Int)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("Either[$a, $a]", "Either[Int, String]"),
      "matched=false"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary(
        "List[Either[$a, Option[$b]]]",
        "List[Either[Int, Option[String]]]"
      ),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary(
        "Either[List[$a], Option[$a]]",
        "Either[List[Int], Option[String]]"
      ),
      "matched=false"
    )

  test("templates recursively construct nested and binary types"):
    val intForm = TypeNormalForm.STypeIdent("Int")
    val stringForm = TypeNormalForm.STypeIdent("String")
    assertEquals(
      QuasiTypeConstruct
        .fromTemplate("Either[$a, $b]", "a" -> intForm, "b" -> stringForm)
        .map(_.source),
      Right("Either[Int, String]")
    )
    assertEquals(
      QuasiTypeConstruct
        .fromTemplate(
          "Either[List[$a], Option[$b]]",
          "a" -> intForm,
          "b" -> stringForm
        )
        .map(_.source),
      Right("Either[List[Int], Option[String]]")
    )
    assertEquals(
      QuasiTypeConstruct
        .fromTemplate("List[Either[$a, String]]", "a" -> intForm)
        .map(_.source),
      Right("List[Either[Int, String]]")
    )

  test("constructed TypeRepr and scoped Type evidence close binary applications"):
    val roundTrip = QuasiTypeExamples.constructedTypeReprRoundtripSummary(
      "Either[List[$a], Option[$b]]",
      "a",
      "Int",
      "b",
      "String"
    )
    assert(roundTrip.endsWith("matched=true"), clue(roundTrip))
    val bridged = ConstructedTypeBridgeExamples.bridgeSummary(
      "Either[List[$a], Option[$b]]",
      "a",
      "Int",
      "b",
      "String"
    )
    assert(bridged.endsWith("matched=true"), clue(bridged))
    val nested = ConstructedTypeBridgeExamples
      .normalFormBridgeSummary("Either[Option[Int], List[Either[String, Boolean]]]")
    assert(nested.endsWith("matched=true"), clue(nested))

  test("ordinary Scala values typecheck under nested and binary ascriptions"):
    val nested: List[Option[Int]] = List(Some(1), None)
    val binary: Either[List[Int], Option[String]] = Left(List(1, 2))
    val deep: Either[Option[Int], List[Either[String, Boolean]]] =
      Right(List(Left("value"), Right(true)))
    assertEquals(nested.flatten, List(1))
    assertEquals(binary, Left(List(1, 2)))
    assertEquals(deep, Right(List(Left("value"), Right(true))))

  test("unsupported constructor, arity, selection, and constructor holes stay rejected"):
    val negativePatterns = List(
      "Map[$a, $b]",
      "Foo[$a]",
      "scala.Either[$a, $b]",
      "Either[$a]",
      "Either[$a, $b, $c]",
      "List[$a, $b]",
      "$F[Int]",
      "$F[Int, String]"
    )
    negativePatterns.foreach { source =>
      val error = TypePatternSource.fromSource(source).swap.toOption.get
      assert(!error.message.contains("__tqhole_"), clue(source, error.message))
    }
    List(
      "Map[Int, String]",
      "Foo[Int]",
      "scala.Either[Int, String]",
      "Either[Int]",
      "Either[Int, String, Boolean]",
      "List[Int, String]"
    ).foreach(source => assert(TypeNormalFormSource.fromSource(source).isLeft, clue(source)))

  test("nested and repeated hole origins remain ordered and exact"):
    val source = "Either[List[$a], Option[Either[$b, $a]]]"
    val pattern = TypePattern.rewriteSourceMapped(source)
    val template = TypeTemplate.rewriteSourceMapped(source)
    val expected = Vector(
      SourceSpan(source.indexOf("$a"), source.indexOf("$a") + 2),
      SourceSpan(source.indexOf("$b"), source.indexOf("$b") + 2),
      SourceSpan(source.lastIndexOf("$a"), source.lastIndexOf("$a") + 2)
    )
    assertEquals(pattern.occurrences.map(_.originalSpan), expected)
    assertEquals(template.occurrences.map(_.originalSpan), expected)

  test("legacy and located wrong-arity diagnostics agree and restore hole names"):
    val source = "Either[$a]"
    val legacy = TypePatternSource.fromSource(source).swap.toOption.get
    val located = TypePatternSource.fromSourceLocated(source).swap.toOption.get
    assertEquals(located.diagnostic, legacy)
    assert(legacy.message.contains("$a"))
    assert(!legacy.message.contains("__tqhole_"))
    assertEquals(located.location.map(_.span), Some(SourceSpan(0, TypePattern.rewriteSourceMapped(source).generatedSource.length)))

  test("structural equality keeps application order and grouping significant"):
    assert(!QuasiTypeExamples.structuralMatches("Either[Int, String]", "Either[String, Int]"))
    assert(!QuasiTypeExamples.structuralMatches("Either[Int, String]", "List[Int]"))
    assert(!QuasiTypeExamples.structuralMatches("List[Option[Int]]", "Option[List[Int]]"))
    assert(!QuasiTypeExamples.structuralMatches(
      "Either[(Int, String), Boolean]",
      "Either[Int, (String, Boolean)]"
    ))

  private def result(
      source: String,
      normal: String,
      inspected: String,
      lowered: String
  ): (String, String, String, String) =
    (source, normal, inspected, lowered)
