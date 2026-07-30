package quasiquotes.definitions

import quasiquotes.definitions.parser.{
  CategorizedDefinitionHoleOccurrence,
  DefinitionTemplateHoleCategory,
  DefinitionTemplateSourceAdapter
}
import quasiquotes.parser.TermShape
import quasiquotes.source.{
  DiagnosticLocation,
  DiagnosticPrecision,
  HoleRole,
  SourceOrigin,
  SourceSpan
}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class LocatedDefinitionTemplateNamespaceDiagnosticsTest
    extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*

  private val intTerm =
    ConstructedTerm.fromShape(TermShape.Literal("1")).toOption.get
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val invalidType = TypeNormalForm.STypeIdent("AnyVal")

  test("same spelling across declaration type and body term selects the term namespace") {
    val source = "def answer: $same = $same"
    val located =
      parsed(
        source,
        occurrence("same", DefinitionType),
        occurrence("same", BodyTerm)
      )
    val failure =
      located
        .complete(Map.empty, Map("same" -> intType))
        .swap
        .toOption
        .get

    assertEquals(
      failure.diagnostic,
      DefinitionConstructionError.MissingTermBinding("same")
    )
    assertExactOrigin(
      failure.location,
      source,
      "$same",
      occurrenceIndex = 1,
      HoleRole.DefinitionBodyTermTemplate,
      excludedRoles = Set(HoleRole.DefinitionTypeTemplate)
    )
  }

  test("same spelling across declaration type and body term selects the type namespace") {
    val source = "def answer: $same = $same"
    val located =
      parsed(
        source,
        occurrence("same", DefinitionType),
        occurrence("same", BodyTerm)
      )
    val failure =
      located
        .complete(Map("same" -> intTerm), Map.empty)
        .swap
        .toOption
        .get

    assertEquals(
      failure.diagnostic,
      DefinitionConstructionError.MissingTypeBinding("same")
    )
    assertExactOrigin(
      failure.location,
      source,
      "$same",
      occurrenceIndex = 0,
      HoleRole.DefinitionTypeTemplate,
      excludedRoles = Set(HoleRole.DefinitionBodyTermTemplate)
    )
  }

  test("invalid type binding ignores a same-named term occurrence") {
    val source = "def answer: $same = $same"
    val located =
      parsed(
        source,
        occurrence("same", DefinitionType),
        occurrence("same", BodyTerm)
      )
    val failure =
      located
        .complete(
          Map("same" -> intTerm),
          Map("same" -> invalidType)
        )
        .swap
        .toOption
        .get

    failure.diagnostic match
      case DefinitionConstructionError.InvalidTypeBinding(
            "same",
            detail
          ) =>
        assert(detail.nonEmpty)
      case other =>
        fail(s"Expected invalid `same` type binding, received $other")
    assertExactOrigin(
      failure.location,
      source,
      "$same",
      occurrenceIndex = 0,
      HoleRole.DefinitionTypeTemplate,
      excludedRoles = Set(HoleRole.DefinitionBodyTermTemplate)
    )
  }

  test("namespace projection precedes uniqueness across all three transport roles") {
    val source = "def answer: $same = ($same: $same)"
    val located =
      parsed(
        source,
        occurrence("same", DefinitionType),
        occurrence("same", BodyTerm),
        occurrence("same", BodyType)
      )

    val missingTerm =
      located
        .complete(Map.empty, Map("same" -> intType))
        .swap
        .toOption
        .get
    assertEquals(
      missingTerm.diagnostic,
      DefinitionConstructionError.MissingTermBinding("same")
    )
    assertExactOrigin(
      missingTerm.location,
      source,
      "$same",
      occurrenceIndex = 1,
      HoleRole.DefinitionBodyTermTemplate,
      excludedRoles = Set(
        HoleRole.DefinitionTypeTemplate,
        HoleRole.DefinitionBodyTypeTemplate
      )
    )

    val missingType =
      located
        .complete(Map("same" -> intTerm), Map.empty)
        .swap
        .toOption
        .get
    assertEquals(
      missingType.diagnostic,
      DefinitionConstructionError.MissingTypeBinding("same")
    )
    assertWholeDefinition(missingType.location, located)
  }

  test("repeated term occurrences use whole-definition fallback") {
    val located =
      parsed(
        "def answer: Int = ($same, $same)",
        occurrence("same", BodyTerm),
        occurrence("same", BodyTerm)
      )
    val failure =
      located.complete(Map.empty, Map.empty).swap.toOption.get

    assertEquals(
      failure.diagnostic,
      DefinitionConstructionError.MissingTermBinding("same")
    )
    assertWholeDefinition(failure.location, located)
  }

  test("repeated type occurrences use whole-definition fallback") {
    val located =
      parsed(
        "def answer: $same = (1: $same)",
        occurrence("same", DefinitionType),
        occurrence("same", BodyType)
      )
    val failure =
      located.complete(Map.empty, Map.empty).swap.toOption.get

    assertEquals(
      failure.diagnostic,
      DefinitionConstructionError.MissingTypeBinding("same")
    )
    assertWholeDefinition(failure.location, located)
  }

  test("unique declaration, body type, and body term occurrences remain exact") {
    val declarationSource = "def answer: $T = 1"
    val declaration =
      parsed(declarationSource, occurrence("T", DefinitionType))
        .complete(Map.empty, Map.empty)
        .swap
        .toOption
        .get
    assertExactOrigin(
      declaration.location,
      declarationSource,
      "$T",
      0,
      HoleRole.DefinitionTypeTemplate
    )

    val bodyTypeSource = "def answer: Int = (1: $T)"
    val bodyType =
      parsed(bodyTypeSource, occurrence("T", BodyType))
        .complete(Map.empty, Map.empty)
        .swap
        .toOption
        .get
    assertExactOrigin(
      bodyType.location,
      bodyTypeSource,
      "$T",
      0,
      HoleRole.DefinitionBodyTypeTemplate
    )

    val bodyTermSource = "def answer: Int = $value"
    val bodyTerm =
      parsed(bodyTermSource, occurrence("value", BodyTerm))
        .complete(Map.empty, Map.empty)
        .swap
        .toOption
        .get
    assertExactOrigin(
      bodyTerm.location,
      bodyTermSource,
      "$value",
      0,
      HoleRole.DefinitionBodyTermTemplate
    )
  }

  test("errors without a source-owned binding occurrence stay conservative") {
    val located = parsed("def answer: Int = 1")
    val failure =
      located
        .complete(Map("extra" -> intTerm), Map.empty)
        .swap
        .toOption
        .get

    assertEquals(
      failure.diagnostic,
      DefinitionConstructionError.UnexpectedTermBinding("extra")
    )
    assertWholeDefinition(failure.location, located)
  }

  private def parsed(
      source: String,
      occurrences: CategorizedDefinitionHoleOccurrence*
  ): LocatedDefinitionTemplate =
    DefinitionTemplateSourceAdapter
      .parseLocated(source, occurrences.toVector)
      .fold(error => fail(error.diagnostic.message), identity)

  private def occurrence(
      name: String,
      category: DefinitionTemplateHoleCategory
  ): CategorizedDefinitionHoleOccurrence =
    CategorizedDefinitionHoleOccurrence(name, category)

  private def assertExactOrigin(
      location: Option[DiagnosticLocation],
      source: String,
      token: String,
      occurrenceIndex: Int,
      expectedRole: HoleRole,
      excludedRoles: Set[HoleRole] = Set.empty
  ): Unit =
    val selected = location.getOrElse(fail("Expected an exact location"))
    val expectedStart = nthIndexOf(source, token, occurrenceIndex)
    val expectedSpan =
      SourceSpan(expectedStart, expectedStart + token.length)

    assertEquals(selected.precision, DiagnosticPrecision.ExactOccurrence)
    assertEquals(selected.origins.size, 1)
    assert(
      selected.origins.exists {
        case SourceOrigin.RewrittenHole(
              _,
              originalSpan,
              semanticName,
              role
            ) =>
          semanticName == token.drop(1) &&
          originalSpan == expectedSpan &&
          role == expectedRole
        case _ =>
          false
      }
    )
    assert(
      selected.origins.forall {
        case SourceOrigin.RewrittenHole(_, _, _, role) =>
          !excludedRoles(role)
        case _ =>
          true
      }
    )

  private def assertWholeDefinition(
      location: Option[DiagnosticLocation],
      located: LocatedDefinitionTemplate
  ): Unit =
    val selected =
      location.getOrElse(fail("Expected whole-definition fallback"))
    assertEquals(selected.precision, DiagnosticPrecision.WholeSource)
    assertEquals(selected.span, located.components.definition)
    assert(selected.origins.nonEmpty)

  private def nthIndexOf(
      source: String,
      token: String,
      occurrenceIndex: Int
  ): Int =
    Iterator
      .iterate(source.indexOf(token))(
        previous => source.indexOf(token, previous + token.length)
      )
      .drop(occurrenceIndex)
      .next()
