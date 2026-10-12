package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.{
  DiagnosticLocationMapper,
  TermShape,
  TinyTermParser
}
final class N068NormalizedSelectedNameSourceIntegrationTest
    extends munit.FunSuite:

  test("real parser compiler and N065 validate distinct direct nullary and unary selected sources"):
    val cases = Vector(
      (
        "$receiver.$first.$second",
        TermShape.Select(
          TermShape.Select(identifier("service"), "alpha"),
          "beta"
        ),
        Map("receiver" -> identifier("service"))
      ),
      (
        "$receiver.$first.$second()",
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          Nil
        ),
        Map("receiver" -> identifier("service"))
      ),
      (
        "$receiver.$first.$second($arg)",
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          List(TermShape.Literal("1"))
        ),
        Map(
          "receiver" -> identifier("service"),
          "arg" -> TermShape.Literal("1")
        )
      )
    )

    cases.foreach { case (source, target, ordinaryBindings) =>
      val (_, validated) = compileAndValidate(
        source,
        Vector(
          SelectedNameSourcePosition(1, 1),
          SelectedNameSourcePosition(2, 0)
        )
      )
      val result = SelectedNamePatternMatcher
        .matchTarget(validated, target)
        .fold(error => fail(error.message), identity)

      assertEquals(result.termBindings, ordinaryBindings)
      assertEquals(
        result.selectedNameBindings,
        Map("first" -> selected("alpha"), "second" -> selected("beta"))
      )
    }

  test("repeated selected names match equal targets and reject unequal targets"):
    val (_, validated) = compileAndValidate(
      "$receiver.$member.$member($arg)",
      Vector(
        SelectedNameSourcePosition(1, 1),
        SelectedNameSourcePosition(2, 0)
      )
    )
    val equalTarget = TermShape.Apply(
      TermShape.Select(
        TermShape.Select(identifier("service"), "same"),
        "same"
      ),
      List(TermShape.Literal("1"))
    )
    val unequalTarget = TermShape.Apply(
      TermShape.Select(
        TermShape.Select(identifier("service"), "inner"),
        "outer"
      ),
      List(TermShape.Literal("1"))
    )

    assertEquals(
      SelectedNamePatternMatcher
        .matchTarget(validated, equalTarget)
        .map(_.selectedNameBindings),
      Right(Map("member" -> selected("same")))
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(validated, unequalTarget),
      Left(
        NeutralSelectedNameMatchError.RepeatedSelectedNameCaptureMismatch(
          "member"
        )
      )
    )

  test("nonzero selected ordinal coexists with a fixed selected name"):
    val (_, validated) = compileAndValidate(
      "$receiver.$first.fixed.$second",
      Vector(
        SelectedNameSourcePosition(1, 2),
        SelectedNameSourcePosition(2, 0)
      )
    )
    val target = TermShape.Select(
      TermShape.Select(
        TermShape.Select(identifier("service"), "alpha"),
        "fixed"
      ),
      "beta"
    )

    assertEquals(
      SelectedNamePatternMatcher
        .matchTarget(validated, target)
        .map(_.selectedNameBindings),
      Right(
        Map("first" -> selected("alpha"), "second" -> selected("beta"))
      )
    )

  test("literal transport collisions and ordinary repeated holes survive the real pipeline"):
    val (collision, collisionValidated) = compileAndValidate(
      "__qqhole_member__selected_1.$member.$member",
      Vector(
        SelectedNameSourcePosition(0, 1),
        SelectedNameSourcePosition(1, 0)
      )
    )
    assertEquals(
      collision.selectedOccurrences.map(_.transport),
      Vector(
        "__qqhole_member__selected_1_1",
        "__qqhole_member__selected_0"
      )
    )
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          collisionValidated,
          TermShape.Select(
            TermShape.Select(
              identifier("__qqhole_member__selected_1"),
              "same"
            ),
            "same"
          )
        )
        .isRight
    )

    val (_, repeatedOrdinary) = compileAndValidate(
      "$receiver.$first.$second($receiver)",
      Vector(
        SelectedNameSourcePosition(1, 1),
        SelectedNameSourcePosition(2, 0)
      )
    )
    assert(
      SelectedNamePatternMatcher
        .matchTarget(
          repeatedOrdinary,
          TermShape.Apply(
            TermShape.Select(
              TermShape.Select(identifier("service"), "alpha"),
              "beta"
            ),
            List(identifier("service"))
          )
        )
        .isRight
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(
        repeatedOrdinary,
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          List(identifier("other"))
        )
      ),
      Left(
        NeutralSelectedNameMatchError.RepeatedTermCaptureMismatch("receiver")
      )
    )

  test("stale selected ordinals fail against the actual compiled pattern"):
    val normalized = normalize(
      "$receiver.$first.$second",
      Vector(
        SelectedNameSourcePosition(1, 0),
        SelectedNameSourcePosition(2, 1)
      )
    )
    val compiled = compile(normalized)

    assert(
      ValidatedSelectedNamePattern
        .createTwo(
          compiled,
          normalized.selectedOccurrences.head,
          normalized.selectedOccurrences(1)
        )
        .left
        .toOption
        .exists(
          _.isInstanceOf[
            NeutralSelectedNameMatchError.InvalidSelectedNameOccurrence
          ]
        )
    )

  test("missing and extra selected source positions fail before frontend parsing"):
    val source = "$receiver.$first.$second"
    val first = SelectedNameSourcePosition(1, 1)
    val second = SelectedNameSourcePosition(2, 0)

    assertEquals(
      SelectedNameSourceNormalizer.normalize(source, Vector(first)),
      Left(SelectedNameSourceNormalizationError.SelectedPositionCount(1))
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        source,
        Vector(first, second, SelectedNameSourcePosition(0, 2))
      ),
      Left(SelectedNameSourceNormalizationError.SelectedPositionCount(3))
    )

  test("parser diagnostics map through the private normalized source map"):
    val normalized = normalize(
      "$receiver.$first.$second)",
      Vector(
        SelectedNameSourcePosition(1, 1),
        SelectedNameSourcePosition(2, 0)
      )
    )
    val parseError =
      TinyTermParser.parse(normalized.generatedSource).left.toOption.get
    val location =
      DiagnosticLocationMapper.fromParseError(parseError, normalized.sourceMap)

    assert(location.nonEmpty)
    assert(
      location.toVector
        .flatMap(_.origins)
        .exists(_.isInstanceOf[quasiquotes.source.SourceOrigin.OriginalText])
    )

  test("public QuasiPattern source and pattern remain byte-for-source unchanged"):
    val source = "$receiver.$member.$member($arg)"
    val publicPattern = QuasiPattern.term(source).toOption.get

    assertEquals(publicPattern.input, source)
    assertEquals(
      publicPattern.placeholderSource,
      "__qqhole_receiver.__qqhole_member.__qqhole_member(__qqhole_arg)"
    )
    assertEquals(
      publicPattern.pattern,
      TermPattern.Apply(
        TermPattern.Select(
          TermPattern.Select(
            TermPattern.Hole("receiver"),
            "__qqhole_member"
          ),
          "__qqhole_member"
        ),
        List(TermPattern.Hole("arg"))
      )
    )

  private def compileAndValidate(
      source: String,
      positions: Vector[SelectedNameSourcePosition]
  ): (NormalizedSelectedNameSource, ValidatedSelectedNamePattern) =
    val normalized = normalize(source, positions)
    val compiled = compile(normalized)
    val validated = ValidatedSelectedNamePattern
      .createTwo(
        compiled,
        normalized.selectedOccurrences.head,
        normalized.selectedOccurrences(1)
      )
      .fold(error => fail(error.message), identity)
    (normalized, validated)

  private def normalize(
      source: String,
      positions: Vector[SelectedNameSourcePosition]
  ): NormalizedSelectedNameSource =
    SelectedNameSourceNormalizer
      .normalize(source, positions)
      .fold(error => fail(error.message), identity)

  private def compile(
      normalized: NormalizedSelectedNameSource
  ): TermPattern =
    val parsed = TinyTermParser
      .parse(normalized.generatedSource)
      .fold(throw _, identity)
    PatternCompiler
      .compileLocated(parsed.rawTree, normalized.ordinaryHoleIndex)
      .fold(error => fail(error.error.message), identity)

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
