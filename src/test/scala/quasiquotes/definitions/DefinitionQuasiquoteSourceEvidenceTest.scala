package quasiquotes.definitions

import quasiquotes.definitions.parser.*
import quasiquotes.source.*

class DefinitionQuasiquoteSourceEvidenceTest extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*
  import DefinitionQuasiquoteTestFixtures.*

  private final case class Fixture(
      assembly: DefinitionQuasiquoteAssembly,
      located: LocatedDefinitionTemplate,
      occurrences: Vector[HoleOccurrence]
  )

  test("validated evidence rejects mismatched identity and incomplete maps") {
    val fixture = surfaceFixture()
    val wrongIdentity = DefinitionQuasiquoteSourceEvidence.create(
      SourceId("other-source"),
      fixture.located.sourceMap,
      fixture.located.components,
      fixture.assembly.occurrences,
      fixture.occurrences
    )
    assertInvalid(wrongIdentity)

    val original = fixture.located.sourceMap
    val incomplete = GeneratedSourceMap(
      original.generatedSource,
      original.generatedSourceId,
      original.segments.drop(1)
    )
    assertInvalid(
      DefinitionQuasiquoteSourceEvidence.create(
        fixture.located.sourceId,
        incomplete,
        fixture.located.components,
        fixture.assembly.occurrences,
        fixture.occurrences
      )
    )

    val first = original.segments.head
    val second = original.segments(1)
    intercept[IllegalArgumentException] {
      GeneratedSourceMap(
        original.generatedSource,
        original.generatedSourceId,
        Vector(
          first,
          GeneratedSegment(
            SourceSpan(first.generatedSpan.end - 1, second.generatedSpan.end),
            second.origin
          )
        ) ++ original.segments.drop(2)
      )
    }
  }

  test("validated evidence rejects component spans outside the source") {
    val fixture = surfaceFixture()
    val length = fixture.located.sourceMap.generatedSource.length
    val invalidComponents = DefinitionComponentSpans
      .create(
        SourceSpan(0, length + 10),
        fixture.located.components.name,
        fixture.located.components.declaredType,
        fixture.located.components.body
      )
      .toOption
      .get
    assertInvalid(
      DefinitionQuasiquoteSourceEvidence.create(
        fixture.located.sourceId,
        fixture.located.sourceMap,
        invalidComponents,
        fixture.assembly.occurrences,
        fixture.occurrences
      )
    )
  }

  test("validated evidence rejects skipped indices identities roles and assembled spans") {
    val fixture = surfaceFixture()
    val first = fixture.assembly.occurrences.head
    val skipped = replacement(first, argumentIndex = 1)
    assertInvalid(create(fixture, Vector(skipped)))

    val duplicated = replacement(first)
    assertInvalid(create(fixture, Vector(duplicated, duplicated)))

    val wrongIdentity = replacement(first, semanticIdentity = "definitionArgument9")
    assertInvalid(create(fixture, Vector(wrongIdentity)))

    val wrongRole = replacement(
      first,
      category = InterpolationCategory.DefinitionBodyTypeSplice,
      role = HoleRole.DefinitionBodyTypeTemplate,
      origin = SourceOrigin.InterpolationArgument(
        SourceId.DefinitionConstructionTemplate,
        0,
        InterpolationCategory.DefinitionBodyTypeSplice
      )
    )
    assertInvalid(create(fixture, Vector(wrongRole)))

    val wrongSpan = replacement(
      first,
      assembledMarkerSpan = SourceSpan(
        first.assembledMarkerSpan.start,
        first.assembledMarkerSpan.end - 1
      )
    )
    assertInvalid(create(fixture, Vector(wrongSpan)))

    val wrongFinal = fixture.occurrences.head.copy(
      generatedSpan = SourceSpan(
        fixture.occurrences.head.generatedSpan.start,
        fixture.occurrences.head.generatedSpan.end - 1
      )
    )
    assertInvalid(
      DefinitionQuasiquoteSourceEvidence.create(
        fixture.located.sourceId,
        fixture.located.sourceMap,
        fixture.located.components,
        fixture.assembly.occurrences,
        Vector(wrongFinal)
      )
    )
  }

  test("validated evidence rejects missing duplicate raw and wrong-category origins") {
    val fixture = surfaceFixture()
    val map = fixture.located.sourceMap
    val markerSpan = fixture.occurrences.head.generatedSpan

    val missing = replaceMarkerOrigin(
      map,
      markerSpan,
      SourceOrigin.LiteralPart(SourceId.DefinitionConstructionTemplate, 0, SourceSpan(0, 1))
    )
    assertInvalid(create(fixture, sourceMap = missing))

    val raw = replaceMarkerOrigin(
      map,
      markerSpan,
      SourceOrigin.RewrittenHole(
        SourceId.DefinitionConstructionTemplate,
        fixture.occurrences.head.originalSpan,
        fixture.occurrences.head.name,
        fixture.occurrences.head.role
      )
    )
    assertInvalid(create(fixture, sourceMap = raw))

    val wrongCategory = replaceMarkerOrigin(
      map,
      markerSpan,
      SourceOrigin.InterpolationArgument(
        SourceId.DefinitionConstructionTemplate,
        0,
        InterpolationCategory.DefinitionBodyTypeSplice
      )
    )
    assertInvalid(create(fixture, sourceMap = wrongCategory))

    val extraOrigin = map.segments.updated(
      0,
      GeneratedSegment(map.segments.head.generatedSpan, fixture.assembly.occurrences.head.origin)
    )
    assertInvalid(
      create(
        fixture,
        sourceMap = GeneratedSourceMap(
          map.generatedSource,
          map.generatedSourceId,
          extraOrigin
        )
      )
    )
  }

  private def surfaceFixture(): Fixture =
    val assembly = DefinitionQuasiquoteAssembly
      .create(
        Vector("def answer: ", " = 1"),
        Vector(DefinitionArguments.definitionType(tpe("Int")))
      )
      .toOption
      .get
    val located = DefinitionTemplateSourceAdapter
      .parseLocatedMapped(
        assembly.source,
        Vector(
          CategorizedDefinitionHoleOccurrence(
            "definitionArgument0",
            DefinitionType
          )
        ),
        assembly.sourceMap
      )
      .fold(error => fail(error.diagnostic.message), identity)
    Fixture(assembly, located, located.definitionTypeOccurrences)

  private def create(
      fixture: Fixture,
      assemblyOccurrences: Vector[DefinitionQuasiquoteAssemblyOccurrence] = Vector.empty,
      sourceMap: GeneratedSourceMap | Null = null
  ) =
    DefinitionQuasiquoteSourceEvidence.create(
      fixture.located.sourceId,
      Option(sourceMap).getOrElse(fixture.located.sourceMap),
      fixture.located.components,
      Option.when(assemblyOccurrences.nonEmpty)(assemblyOccurrences)
        .getOrElse(fixture.assembly.occurrences),
      fixture.occurrences
    )

  private def replacement(
      original: DefinitionQuasiquoteAssemblyOccurrence,
      argumentIndex: Int = 0,
      semanticIdentity: String = "definitionArgument0",
      category: InterpolationCategory = InterpolationCategory.DefinitionTypeSplice,
      role: HoleRole = HoleRole.DefinitionTypeTemplate,
      assembledMarkerSpan: SourceSpan = null,
      origin: SourceOrigin.InterpolationArgument = null
  ): DefinitionQuasiquoteAssemblyOccurrence =
    new DefinitionQuasiquoteAssemblyOccurrence(
      argumentIndex,
      semanticIdentity,
      category,
      role,
      Option(assembledMarkerSpan).getOrElse(original.assembledMarkerSpan),
      Option(origin).getOrElse(original.origin)
    )

  private def replaceMarkerOrigin(
      sourceMap: GeneratedSourceMap,
      markerSpan: SourceSpan,
      origin: SourceOrigin
  ): GeneratedSourceMap =
    GeneratedSourceMap(
      sourceMap.generatedSource,
      sourceMap.generatedSourceId,
      sourceMap.segments.map { segment =>
        if segment.generatedSpan == markerSpan then GeneratedSegment(markerSpan, origin)
        else segment
      }
    )

  private def assertInvalid(
      result: Either[DefinitionQuasiquoteError, DefinitionQuasiquoteSourceEvidence]
  ): Unit =
    assert(
      result.left.toOption.get
        .isInstanceOf[DefinitionQuasiquoteError.InvalidCompletedSourceEvidence]
    )
