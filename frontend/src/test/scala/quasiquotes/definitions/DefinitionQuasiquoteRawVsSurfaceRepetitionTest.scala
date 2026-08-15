package quasiquotes.definitions

import quasiquotes.definitions.parser.*
import quasiquotes.source.*

class DefinitionQuasiquoteRawVsSurfaceRepetitionTest extends munit.FunSuite:
  import DefinitionQuasiquotes.*
  import DefinitionQuasiquoteTestFixtures.*
  import DefinitionTemplateHoleCategory.*

  test("surface repetition assigns distinct identities and exact argument origins") {
    val repeated = bodyTerm("value")
    val result =
      dqr"def pair: (Int, Int) = ($repeated, $repeated)".toOption.get

    assertEquals(
      result.sourceEvidence.interpolationOccurrences.map(_.semanticIdentity),
      Vector("definitionArgument0", "definitionArgument1")
    )
    assertEquals(
      result.sourceEvidence.interpolationOccurrences.map(_.origin.argumentIndex),
      Vector(0, 1)
    )
  }

  test("raw repeated-hole semantics retain one identity and whole-definition fallback") {
    val located = DefinitionTemplateSourceAdapter
      .parseLocated(
        "def pair: (Int, Int) = ($same, $same)",
        Vector(
          CategorizedDefinitionHoleOccurrence("same", BodyTerm),
          CategorizedDefinitionHoleOccurrence("same", BodyTerm)
        )
      )
      .toOption
      .get
    val failure = located.complete(Map.empty, Map.empty).left.toOption.get

    assertEquals(
      failure.diagnostic,
      DefinitionConstructionError.MissingTermBinding("same")
    )
    assertEquals(failure.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assert(
      failure.location.toVector.flatMap(_.origins).exists(
        _.isInstanceOf[SourceOrigin.RewrittenHole]
      )
    )
  }
