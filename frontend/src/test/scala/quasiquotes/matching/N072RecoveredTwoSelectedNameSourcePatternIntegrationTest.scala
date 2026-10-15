package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.source.SourceSpan

final class N072RecoveredTwoSelectedNameSourcePatternIntegrationTest
    extends munit.FunSuite:

  test("real normalizer parser compiler recovery and matcher pipeline handles direct nullary and unary forms"):
    val cases = Vector(
      (
        "$receiver.$first.$second",
        TermShape.Select(
          TermShape.Select(identifier("service"), "alpha"),
          "beta"
        )
      ),
      (
        "$receiver.$first.$second()",
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          Nil
        )
      ),
      (
        "$receiver.$first.$second($arg)",
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          List(TermShape.Literal("1"))
        )
      )
    )

    cases.foreach { case (source, target) =>
      val (normalized, recovered) = compileAndRecover(
        source,
        Vector(
          SelectedNameSourcePosition(1, 1),
          SelectedNameSourcePosition(2, 0)
        )
      )
      val result = recovered.matchTarget(target).fold(error => fail(error.message), identity)

      assertEquals(recovered.generatedSource, normalized.generatedSource)
      assertEquals(recovered.sourceMap, normalized.sourceMap)
      assertEquals(
        result.selectedNameBindings,
        Map("first" -> selected("alpha"), "second" -> selected("beta"))
      )
    }

  test("real pipeline preserves repeated selected spans and repeated ordinary equality"):
    val source = "$receiver.$member.$member($receiver)"
    val (_, recovered) = compileAndRecover(
      source,
      Vector(
        SelectedNameSourcePosition(1, 1),
        SelectedNameSourcePosition(2, 0)
      )
    )

    assertEquals(
      recovered.selectedSourceOccurrences.map(_.originalSpan),
      Vector(SourceSpan(18, 25), SourceSpan(10, 17))
    )
    assertEquals(recovered.ordinaryTermHoleNames, Vector("receiver", "receiver"))
    assert(
      recovered
        .matchTarget(
          TermShape.Apply(
            TermShape.Select(
              TermShape.Select(identifier("service"), "same"),
              "same"
            ),
            List(identifier("service"))
          )
        )
        .isRight
    )
    assertEquals(
      recovered.matchTarget(
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "same"),
            "same"
          ),
          List(identifier("other"))
        )
      ),
      Left(NeutralSelectedNameMatchError.RepeatedTermCaptureMismatch("receiver"))
    )

  test("real pipeline handles fixed middle names and deterministic transport collisions"):
    val (_, fixed) = compileAndRecover(
      "$receiver.$first.fixed.$second",
      Vector(
        SelectedNameSourcePosition(1, 2),
        SelectedNameSourcePosition(2, 0)
      )
    )
    assertEquals(fixed.selectedOccurrences.map(_.selectOrdinal), Vector(0, 2))

    val (collision, recovered) = compileAndRecover(
      "__qqhole_member__selected_1.$member.$member",
      Vector(
        SelectedNameSourcePosition(0, 1),
        SelectedNameSourcePosition(1, 0)
      )
    )
    assertEquals(
      recovered.selectedOccurrences.map(_.transport),
      Vector(
        "__qqhole_member__selected_0",
        "__qqhole_member__selected_1_1"
      )
    )
    assertEquals(recovered.publicMapped, collision.publicMapped)
    assertEquals(recovered.sourceMap, collision.sourceMap)

  test("real compiled metadata with stale declared ordinals fails closed"):
    val source = "$receiver.$first.$second"
    val actualPositions = Vector(
      SelectedNameSourcePosition(1, 1),
      SelectedNameSourcePosition(2, 0)
    )
    val normalized = normalize(source, actualPositions)
    val compiled = compile(normalized)

    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover(
          source,
          Vector(
            SelectedNameSourcePosition(1, 0),
            SelectedNameSourcePosition(2, 1)
          ),
          compiled
        )
        .isLeft
    )

  private def compileAndRecover(
      source: String,
      positions: Vector[SelectedNameSourcePosition]
  ): (NormalizedSelectedNameSource, RecoveredTwoSelectedNameSourcePattern) =
    val normalized = normalize(source, positions)
    val compiled = compile(normalized)
    val recovered = RecoveredTwoSelectedNameSourcePattern
      .recover(source, positions, compiled)
      .fold(error => fail(error.message), identity)
    (normalized, recovered)

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
