package quasiquotes.construct

import quasiquotes.source.*

class MacroDiagnosticAnchorTest extends munit.FunSuite:
  private val generatedId = SourceId("anchor-test-generated")
  private val templateId = SourceId.TermConstructionTemplate

  private def location(origins: Vector[SourceOrigin]): Option[DiagnosticLocation] =
    Some(
      DiagnosticLocation(
        generatedId,
        SourceSpan(0, 1),
        origins,
        DiagnosticPrecision.ExactOccurrence
      )
    )

  test("one term interpolation selects its argument index") {
    assertEquals(
      MacroDiagnosticAnchorSelector.select(location(Vector(termOrigin(2)))),
      MacroDiagnosticAnchor.TermInterpolationArgument(2)
    )
  }

  test("constructed-type interpolation literal and location absence select macro expansion") {
    val constructed = SourceOrigin.InterpolationArgument(templateId, 0, InterpolationCategory.ConstructedTypeSplice)
    val literal = SourceOrigin.LiteralPart(templateId, 0, SourceSpan(0, 1))

    assertEquals(MacroDiagnosticAnchorSelector.select(location(Vector(constructed))), MacroDiagnosticAnchor.MacroExpansion)
    assertEquals(MacroDiagnosticAnchorSelector.select(location(Vector(literal))), MacroDiagnosticAnchor.MacroExpansion)
    assertEquals(MacroDiagnosticAnchorSelector.select(None), MacroDiagnosticAnchor.MacroExpansion)
  }

  test("literal plus term and two different term indices are ambiguous") {
    val literal = SourceOrigin.LiteralPart(templateId, 0, SourceSpan(0, 1))

    assertEquals(
      MacroDiagnosticAnchorSelector.select(location(Vector(literal, termOrigin(0)))),
      MacroDiagnosticAnchor.MacroExpansion
    )
    assertEquals(
      MacroDiagnosticAnchorSelector.select(location(Vector(termOrigin(0), termOrigin(1)))),
      MacroDiagnosticAnchor.MacroExpansion
    )
  }

  test("duplicate identical term origins still select that argument") {
    assertEquals(
      MacroDiagnosticAnchorSelector.select(location(Vector(termOrigin(3), termOrigin(3)))),
      MacroDiagnosticAnchor.TermInterpolationArgument(3)
    )
  }

  test("negative interpolation indices are rejected by the source model") {
    intercept[IllegalArgumentException] {
      SourceOrigin.InterpolationArgument(templateId, -1, InterpolationCategory.TermSplice)
    }
  }

  test("rewritten-hole and original-text origins select macro expansion") {
    val originalId = SourceId.TermPattern
    val original = SourceOrigin.OriginalText(originalId, SourceSpan(0, 1))
    val rewritten = SourceOrigin.RewrittenHole(originalId, SourceSpan(0, 2), "x", HoleRole.TermPattern)

    assertEquals(MacroDiagnosticAnchorSelector.select(location(Vector(original))), MacroDiagnosticAnchor.MacroExpansion)
    assertEquals(MacroDiagnosticAnchorSelector.select(location(Vector(rewritten))), MacroDiagnosticAnchor.MacroExpansion)
  }

  private def termOrigin(index: Int): SourceOrigin =
    SourceOrigin.InterpolationArgument(templateId, index, InterpolationCategory.TermSplice)
