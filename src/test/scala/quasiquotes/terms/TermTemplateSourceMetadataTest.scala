package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.source.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class TermTemplateSourceMetadataTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val generatedTerm = "__term_transport"

  private def semanticTemplate: TermTemplate =
    template(
      ident(generatedTerm),
      termEntries = Vector("value" -> generatedTerm),
      termOccurrences = Vector(TermHoleOccurrence("value", 0))
    ).toOption.get

  private def termMetadata(
      generatedSourceId: SourceId = SourceId("generated-term-template")
  ): (
      GeneratedSourceMap,
      Vector[LocatedTermHoleOccurrence]
  ) =
    val originalId = SourceId("original-term-template")
    val originalSpan = SourceSpan(0, 6)
    val generatedSpan = SourceSpan(0, generatedTerm.length)
    val occurrence =
      HoleOccurrence(
        "value",
        generatedTerm,
        originalSpan,
        generatedSpan,
        HoleRole.TermTemplate
      )
    val sourceMap =
      GeneratedSourceMap(
        generatedTerm,
        generatedSourceId,
        Vector(
          GeneratedSegment(
            generatedSpan,
            SourceOrigin.RewrittenHole(
              originalId,
              originalSpan,
              "value",
              HoleRole.TermTemplate
            )
          )
        )
      )
    (
      sourceMap,
      Vector(
        LocatedTermHoleOccurrence(
          TermHoleOccurrence("value", 0),
          occurrence
        )
      )
    )

  test("accepts exact located term occurrence and complete map coverage") {
    val (sourceMap, occurrences) = termMetadata()
    val located =
      LocatedTermTemplate.create(
        semanticTemplate,
        sourceMap,
        occurrences,
        Vector.empty
      )

    assert(located.isRight)
    assert(located.toOption.get.render.endsWith("sourceMetadata=present)"))
  }

  test("source metadata is outside TermTemplate and ConstructedTerm semantic equality") {
    val (firstMap, firstOccurrences) =
      termMetadata(SourceId("first-generated"))
    val (secondMap, secondOccurrences) =
      termMetadata(SourceId("second-generated"))
    val first =
      LocatedTermTemplate
        .create(
          semanticTemplate,
          firstMap,
          firstOccurrences,
          Vector.empty
        )
        .toOption
        .get
    val second =
      LocatedTermTemplate
        .create(
          semanticTemplate,
          secondMap,
          secondOccurrences,
          Vector.empty
        )
        .toOption
        .get
    val binding =
      ConstructedTerm.fromShape(TermShape.Literal("1")).toOption.get
    val firstResult =
      first
        .complete(Map("value" -> binding), Map.empty)
        .toOption
        .get
    val secondResult =
      second
        .complete(Map("value" -> binding), Map.empty)
        .toOption
        .get

    assertEquals(first.template, second.template)
    assertEquals(firstResult, secondResult)
  }

  test("located completion uses an exact occurrence for a unique missing binding") {
    val (sourceMap, occurrences) = termMetadata()
    val located =
      LocatedTermTemplate
        .create(
          semanticTemplate,
          sourceMap,
          occurrences,
          Vector.empty
        )
        .toOption
        .get
    val error =
      located.complete(Map.empty, Map.empty).left.toOption.get

    assertEquals(
      error.diagnostic,
      TermConstructionError.MissingTermBinding("value")
    )
    assertEquals(
      error.location.map(_.precision),
      Some(DiagnosticPrecision.ExactOccurrence)
    )
    assertEquals(
      error.location.map(_.span),
      Some(SourceSpan(0, generatedTerm.length))
    )
  }

  test("accepts exact located type occurrence in typed-sidecar preorder") {
    val generatedType = "__type_transport"
    val source = s"value: $generatedType"
    val generatedSpan = SourceSpan(7, source.length)
    val originalId = SourceId("original-typed-template")
    val generatedId = SourceId("generated-typed-template")
    val originalSpan = SourceSpan(7, 11)
    val typeOccurrence =
      HoleOccurrence(
        "tpe",
        generatedType,
        originalSpan,
        generatedSpan,
        HoleRole.TypeTemplate
      )
    val sourceMap =
      GeneratedSourceMap(
        source,
        generatedId,
        Vector(
          GeneratedSegment(
            SourceSpan(0, 7),
            SourceOrigin.OriginalText(originalId, SourceSpan(0, 7))
          ),
          GeneratedSegment(
            generatedSpan,
            SourceOrigin.RewrittenHole(
              originalId,
              originalSpan,
              "tpe",
              HoleRole.TypeTemplate
            )
          )
        )
      )
    val semantic =
      template(
        TermShape.Typed(ident("value"), generatedType),
        typeEntries = Vector("tpe" -> generatedType),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      ).toOption.get
    val located =
      LocatedTermTemplate.create(
        semantic,
        sourceMap,
        Vector.empty,
        Vector(typeOccurrence)
      )

    assert(located.isRight)
    val completed =
      located.toOption.get
        .complete(
          Map.empty,
          Map("tpe" -> TypeNormalForm.STypeIdent("Int"))
        )
        .toOption
        .get
    assertEquals(completed.ascriptionTypes, Vector(TypeNormalForm.STypeIdent("Int")))
  }

  test("rejects incomplete coverage and wrong occurrence categories") {
    val (sourceMap, occurrences) = termMetadata()
    val gap =
      GeneratedSourceMap(
        sourceMap.generatedSource,
        sourceMap.generatedSourceId,
        Vector(
          sourceMap.segments.head.copy(
            generatedSpan = SourceSpan(1, sourceMap.generatedSource.length)
          )
        )
      )
    val wrongRole =
      occurrences.map(occurrence =>
        occurrence.copy(
          source = occurrence.source.copy(role = HoleRole.TermPattern)
        )
      )

    assert(
      LocatedTermTemplate
        .create(semanticTemplate, gap, occurrences, Vector.empty)
        .isLeft
    )
    assert(
      LocatedTermTemplate
        .create(semanticTemplate, sourceMap, wrongRole, Vector.empty)
        .isLeft
    )
  }
