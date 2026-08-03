package quasiquotes.construct

import scala.compiletime.testing.typeCheckErrors

class QuasiquoteDiagnosticIntegrationTest extends munit.FunSuite:
  test("located lowerer retains exact selected spans and legacy error projection") {
    assertEquals(
      QuasiquoteDiagnosticExamples.locatedLoweringSummary,
      List(
        "term-in-type|Term splice `__qq_term_hole_1` is not valid as the complete type of an expression ascription.|[19,35)",
        "type-in-term|Constructed-type splice `__qq_type_hole_0` is not valid in term position.|[0,16)",
        "unknown|Unknown categorized quasiquote placeholder `__qq_type_hole_99`.|[0,17)",
        "unsupported-type-position|Constructed-type splice `__qq_type_hole_0` is not supported inside method type arguments; only the complete type of an expression ascription is supported.|[9,25)",
        "type-lowering|Cannot lower unsupported constructed type normal form to TypeRepr: AnyVal|[4,20)",
        "generic|same-span=true|has-span=true",
        "legacy|same-error=true"
      )
    )
  }

  test("located builder maps lowerer and parse spans through the generated source map") {
    assertEquals(
      QuasiquoteDiagnosticExamples.locatedBuilderSummary,
      List(
        "term|arg:1:TermSplice",
        "constructed|arg:0:ConstructedTypeSplice",
        "parse|literal:0:[0,8)|TrailingInput: Trailing input after parsed expression at offset 3: '; bar'",
        "legacy|same-error=true",
        "success|same-tree=true"
      )
    )
  }

  test("Quotes resolver uses a valid term position and falls back for unavailable anchors") {
    assertEquals(
      QuasiquoteDiagnosticExamples.positionResolverSummary(
        40 + 2
      ),
      "term-selected=true term-valid=true invalid-fallback=true negative-fallback=true type-fallback=true"
    )
  }

  test("term splice used as a type reports at the multiline user argument") {
    val errors = typeCheckErrors("""quasiquotes.construct.QuasiquoteDiagnosticExamples.invalidTermInType(
  40 + 2
)""")

    assertEquals(errors.size, 1)
    assertEquals(
      errors.head.message,
      "Term splice `__qq_term_hole_1` is not valid as the complete type of an expression ascription."
    )
  }

  test("constructed-type splice in term position keeps its message and compiles through fallback reporting") {
    val errors = typeCheckErrors(
      "quasiquotes.construct.QuasiquoteDiagnosticExamples.invalidConstructedTypeInTerm"
    )

    assertEquals(errors.size, 1)
    assertEquals(
      errors.head.message,
      "Constructed-type splice `__qq_type_hole_0` is not valid in term position."
    )
  }

  test("literal parse failure keeps its message and compiles through fallback reporting") {
    val errors = typeCheckErrors(
      "quasiquotes.construct.QuasiquoteDiagnosticExamples.invalidLiteralParse"
    )

    assertEquals(errors.size, 1)
    assertEquals(
      errors.head.message,
      "TrailingInput: Trailing input after parsed expression at offset 3: '; bar'"
    )
  }

  test("valid quasiquote construction remains unchanged") {
    assertEquals(QuasiquoteDiagnosticExamples.validQuasiquote(41), 42)
  }
