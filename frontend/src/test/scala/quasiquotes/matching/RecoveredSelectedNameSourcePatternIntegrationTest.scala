package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.{DiagnosticLocationMapper, TermShape, TinyTermParser}
import quasiquotes.source.SourceSpan

final class RecoveredSelectedNameSourcePatternIntegrationTest extends munit.FunSuite:
  test("real source parser compiler adapter and N061 pipeline matches direct nullary and unary targets"):
    val cases = Vector(
      (
        "$receiver.$member",
        TermShape.Select(identifier("service"), "ordinary"),
        Vector("receiver"),
        Map("receiver" -> identifier("service")),
        Map("member" -> selected("ordinary"))
      ),
      (
        "$receiver.$member()",
        TermShape.Apply(TermShape.Select(identifier("service"), "nullary"), Nil),
        Vector("receiver"),
        Map("receiver" -> identifier("service")),
        Map("member" -> selected("nullary"))
      ),
      (
        "$receiver.$member($arg)",
        TermShape.Apply(
          TermShape.Select(identifier("service"), "unary"),
          List(TermShape.Literal("1"))
        ),
        Vector("receiver", "arg"),
        Map("receiver" -> identifier("service"), "arg" -> TermShape.Literal("1")),
        Map("member" -> selected("unary"))
      )
    )

    cases.foreach { case (source, target, ordinaryNames, termBindings, selectedBindings) =>
      val (mapped, recovered) = compileAndRecover(source)
      val result = recovered.matchTarget(target).fold(error => fail(error.message), identity)

      assertEquals(recovered.ordinaryTermHoleNames, ordinaryNames)
      assertEquals(result.termBindings, termBindings)
      assertEquals(result.selectedNameBindings, selectedBindings)
      assert(recovered.selectedOccurrence eq mapped.occurrences.find(_.name == "member").get)
      assertEquals(recovered.selectedOccurrence.originalSpan, SourceSpan(10, 17))
    }

  test("real pipeline recovers a nonzero inner selected-name ordinal"):
    val (_, recovered) = compileAndRecover("$receiver.$member.fixed")

    assertEquals(recovered.validatedPattern.occurrence.selectOrdinal, 1)
    assertEquals(
      recovered
        .matchTarget(
          TermShape.Select(
            TermShape.Select(identifier("service"), "ordinary"),
            "fixed"
          )
        )
        .toOption
        .get,
      NeutralSelectedNameMatchResult(
        Map("receiver" -> identifier("service")),
        Map("member" -> selected("ordinary"))
      )
    )

  test("public QuasiPattern values remain unchanged and parser failures stay source-mapped upstream"):
    val publicPattern = QuasiPattern.term("$receiver.$member($arg)").toOption.get

    assertEquals(publicPattern.input, "$receiver.$member($arg)")
    assertEquals(
      publicPattern.placeholderSource,
      "__qqhole_receiver.__qqhole_member(__qqhole_arg)"
    )
    assertEquals(
      publicPattern.pattern,
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_member"),
        List(TermPattern.Hole("arg"))
      )
    )

    val malformedMapped = PatternSource.synthesizeMapped("$receiver.$member(").toOption.get
    val parseError = TinyTermParser
      .parse(malformedMapped.patternSource.source)
      .left
      .toOption
      .get
    val located = QuasiPattern.termLocated("$receiver.$member(").left.toOption.get
    assert(located.diagnostic.isInstanceOf[PatternError.ParseFailure])
    assertEquals(
      located.location,
      DiagnosticLocationMapper.fromParseError(parseError, malformedMapped.originMap)
    )

  private def compileAndRecover(
      source: String
  ): (MappedPatternSource, RecoveredSelectedNameSourcePattern) =
    val mapped = PatternSource.synthesizeMapped(source).fold(error => fail(error.message), identity)
    val parsed = TinyTermParser.parse(mapped.patternSource.source).fold(throw _, identity)
    val compiled = PatternCompiler
      .compileLocated(parsed.rawTree, mapped.generatedHoleIndex)
      .fold(error => fail(error.error.message), identity)
    val recovered = RecoveredSelectedNameSourcePattern
      .recover(mapped, compiled)
      .fold(error => fail(error.message), identity)
    (mapped, recovered)

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
