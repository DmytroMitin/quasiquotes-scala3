package quasiquotes.construct

import quasiquotes.source.*

class DefinitionQuasiquoteMacroAnchorTest extends munit.FunSuite:
  private val sourceId = SourceId.DefinitionConstructionTemplate

  test("exact definition type body term and body type origins select their argument") {
    val categories = Vector(
      InterpolationCategory.DefinitionTypeSplice,
      InterpolationCategory.DefinitionBodyTermSplice,
      InterpolationCategory.DefinitionBodyTypeSplice
    )

    categories.foreach { category =>
      assertEquals(
        DefinitionQuasiquoteMacroAnchorSelector.select(
          exact(Vector(argument(2, category)))
        ),
        MacroDiagnosticAnchor.DefinitionInterpolationArgument(2)
      )
    }
  }

  test("whole-source literal-only and mixed evidence select macro expansion") {
    val interpolation = argument(
      0,
      InterpolationCategory.DefinitionBodyTermSplice
    )
    val literal = SourceOrigin.LiteralPart(sourceId, 0, SourceSpan(0, 1))

    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(
        location(Vector(interpolation), DiagnosticPrecision.WholeSource)
      ),
      MacroDiagnosticAnchor.MacroExpansion
    )
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(exact(Vector(literal))),
      MacroDiagnosticAnchor.MacroExpansion
    )
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(
        exact(Vector(literal, interpolation))
      ),
      MacroDiagnosticAnchor.MacroExpansion
    )
  }

  test("multiple conflicting argument indices and non-definition categories fall back") {
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(
        exact(
          Vector(
            argument(0, InterpolationCategory.DefinitionTypeSplice),
            argument(1, InterpolationCategory.DefinitionTypeSplice)
          )
        )
      ),
      MacroDiagnosticAnchor.MacroExpansion
    )
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(
        exact(
          Vector(
            argument(0, InterpolationCategory.DefinitionTypeSplice),
            argument(0, InterpolationCategory.DefinitionBodyTypeSplice)
          )
        )
      ),
      MacroDiagnosticAnchor.MacroExpansion
    )
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(
        exact(Vector(argument(0, InterpolationCategory.TermSplice)))
      ),
      MacroDiagnosticAnchor.MacroExpansion
    )
  }

  test("duplicate identical definition origins remain one exact argument") {
    val origin = argument(
      3,
      InterpolationCategory.DefinitionBodyTypeSplice
    )
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(
        exact(Vector(origin, origin))
      ),
      MacroDiagnosticAnchor.DefinitionInterpolationArgument(3)
    )
  }

  test("missing location falls back") {
    assertEquals(
      DefinitionQuasiquoteMacroAnchorSelector.select(None),
      MacroDiagnosticAnchor.MacroExpansion
    )
    intercept[IllegalArgumentException] {
      DiagnosticLocation(
        sourceId,
        SourceSpan(0, 1),
        Vector.empty,
        DiagnosticPrecision.ExactOccurrence
      )
    }
  }

  test("unusable position bounds are rejected by the shared resolver policy") {
    assert(!MacroArgumentPositionResolver.isUsableBounds(-1, 0))
    assert(!MacroArgumentPositionResolver.isUsableBounds(3, 2))
    assert(MacroArgumentPositionResolver.isUsableBounds(3, 3))
  }

  private def exact(
      origins: Vector[SourceOrigin]
  ): Option[DiagnosticLocation] =
    location(origins, DiagnosticPrecision.ExactOccurrence)

  private def location(
      origins: Vector[SourceOrigin],
      precision: DiagnosticPrecision
  ): Option[DiagnosticLocation] =
    Some(DiagnosticLocation(sourceId, SourceSpan(0, 1), origins, precision))

  private def argument(
      index: Int,
      category: InterpolationCategory
  ): SourceOrigin =
    SourceOrigin.InterpolationArgument(sourceId, index, category)
