package quasiquotes.definitions

import quasiquotes.parser.BinderId
import quasiquotes.definitions.parser.{
  CategorizedDefinitionHoleOccurrence,
  DefinitionTemplateHoleCategory,
  DefinitionTemplateSourceAdapter
}
import quasiquotes.source.{SourceId, SourceSpan}

class LocatedDefinitionTemplateTest extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*

  private val source = "def answer: $T = $value"
  private val located =
    DefinitionTemplateSourceAdapter
      .parseLocated(
        source,
        Vector(
          CategorizedDefinitionHoleOccurrence("T", DefinitionType),
          CategorizedDefinitionHoleOccurrence("value", BodyTerm)
        )
      )
      .toOption
      .get

  test("carrier preserves compiler-free semantic and complete source evidence") {
    assertEquals(located.sourceId, located.sourceMap.generatedSourceId)
    assertEquals(located.body.sourceMap, located.sourceMap)
    assertEquals(
      located.definitionTypeOccurrences.map(_.name),
      Vector("T")
    )
    assert(located.render.contains("sourceMetadata=present"))
    assert(!located.getClass.getName.contains("dotty.tools"))
  }

  test("factory rejects source identity disagreement") {
    val result =
      LocatedDefinitionTemplate.create(
        located.template,
        SourceId("wrong-definition-source"),
        located.sourceMap,
        located.components,
        located.definitionTypeOccurrences,
        located.body
      )
    assert(result.isLeft)
  }

  test("factory rejects missing or duplicated occurrence partitions") {
    val result =
      LocatedDefinitionTemplate.create(
        located.template,
        located.sourceId,
        located.sourceMap,
        located.components,
        Vector.empty,
        located.body
      )
    assert(result.isLeft)
  }

  test("coverage validator rejects gaps overlaps and incomplete endpoints") {
    assert(
      LocatedDefinitionTemplate
        .validateCoverageForTest(
          5,
          Vector(SourceSpan(0, 2), SourceSpan(3, 5))
        )
        .isLeft
    )
    assert(
      LocatedDefinitionTemplate
        .validateCoverageForTest(
          5,
          Vector(SourceSpan(0, 3), SourceSpan(2, 5))
        )
        .isLeft
    )
    assert(
      LocatedDefinitionTemplate
        .validateCoverageForTest(5, Vector(SourceSpan(0, 4)))
        .isLeft
    )
    assertEquals(
      LocatedDefinitionTemplate.validateCoverageForTest(
        5,
        Vector(SourceSpan(0, 2), SourceSpan(2, 5))
      ),
      Right(())
    )
  }

  test("legacy located metadata rejects single-parameter templates without parameter spans") {
    val single = DefinitionTemplate
      .singleParameterDef(
        DefinitionName.plain("answer").toOption.get,
        BinderId(0),
        DefinitionName.plain("x").toOption.get,
        quasiquotes.types.TypeTemplate.TTIdent("Int"),
        quasiquotes.types.TypeTemplate.TTIdent("Int"),
        located.body.template
      )
      .toOption
      .get
    val result = LocatedDefinitionTemplate.create(
      single,
      located.sourceId,
      located.sourceMap,
      located.components,
      located.definitionTypeOccurrences,
      located.body
    )

    assertEquals(
      result.left.toOption.get.message,
      "Invalid definition source metadata: single-parameter definition templates require separate parameter-name and parameter-type evidence."
    )
  }
