package quasiquotes.types

class QuasiTypeReprTest extends munit.FunSuite:
  test("lowers the supported simple TypeRepr subset") {
    val supported = QuasiTypeExamples.supportedConstructionSummary

    assert(supported.exists(_.startsWith("Int -> ")))
    assert(supported.exists(_.startsWith("String -> ")))
    assert(supported.exists(_.startsWith("Boolean -> ")))
    assert(supported.exists(_.startsWith("List[Int] -> ")))
    assert(supported.exists(_.startsWith("Option[String] -> ")))
    assert(supported.exists(_.startsWith("(Int, String) -> ")))
    assert(supported.exists(_.startsWith("Int => String -> ")))
  }

  test("matches exact supported TypeRepr shapes") {
    assert(QuasiTypeExamples.matches("Int", "Int"))
    assert(QuasiTypeExamples.matches("String", "String"))
    assert(QuasiTypeExamples.matches("Boolean", "Boolean"))
    assert(QuasiTypeExamples.matches("List[Int]", "List[Int]"))
    assert(QuasiTypeExamples.matches("List[String]", "List[String]"))
    assert(QuasiTypeExamples.matches("Option[String]", "Option[String]"))
    assert(QuasiTypeExamples.matches("(Int, String)", "(Int, String)"))
    assert(QuasiTypeExamples.matches("Int => String", "Int => String"))
  }

  test("rejects non-equal supported TypeRepr shapes") {
    assert(!QuasiTypeExamples.matches("Int", "String"))
    assert(!QuasiTypeExamples.matches("Int", "AnyVal"))
    assert(!QuasiTypeExamples.matches("List[Int]", "List[String]"))
    assert(!QuasiTypeExamples.matches("List[Int]", "Option[Int]"))
    assert(!QuasiTypeExamples.matches("Option[String]", "Option[Int]"))
    assert(!QuasiTypeExamples.matches("(Int, String)", "(String, Int)"))
    assert(!QuasiTypeExamples.matches("Int => String", "String => Int"))
  }

  test("renders a stable type equality summary for examples and docs") {
    assertEquals(QuasiTypeExamples.matchSummary("List[Int]", "List[Int]"), "List[Int] == List[Int]")
    assertEquals(QuasiTypeExamples.matchSummary("List[Int]", "List[String]"), "List[Int] != List[String]")
  }

  test("renders structural type normal forms for the supported subset") {
    assertEquals(QuasiTypeExamples.structuralNormalFormSummary("Int"), "STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.structuralNormalFormSummary("List[Int]"), "STypeApply(STypeIdent(List), [STypeIdent(Int)])")
    assertEquals(QuasiTypeExamples.structuralNormalFormSummary("Option[String]"), "STypeApply(STypeIdent(Option), [STypeIdent(String)])")
    assertEquals(QuasiTypeExamples.structuralNormalFormSummary("(Int, String)"), "STypeTuple([STypeIdent(Int), STypeIdent(String)])")
    assertEquals(QuasiTypeExamples.structuralNormalFormSummary("Int => String"), "STypeFunction([STypeIdent(Int)], STypeIdent(String))")
  }

  test("matches supported structural type normal forms") {
    assert(QuasiTypeExamples.structuralMatches("Int", "Int"))
    assert(QuasiTypeExamples.structuralMatches("String", "String"))
    assert(QuasiTypeExamples.structuralMatches("Boolean", "Boolean"))
    assert(QuasiTypeExamples.structuralMatches("List[Int]", "List[Int]"))
    assert(QuasiTypeExamples.structuralMatches("Option[String]", "Option[String]"))
    assert(QuasiTypeExamples.structuralMatches("(Int, String)", "(Int, String)"))
    assert(QuasiTypeExamples.structuralMatches("Int => String", "Int => String"))
  }

  test("rejects non-equal structural type normal forms") {
    assert(!QuasiTypeExamples.structuralMatches("Int", "String"))
    assert(!QuasiTypeExamples.structuralMatches("Int", "AnyVal"))
    assert(!QuasiTypeExamples.structuralMatches("List[Int]", "List[String]"))
    assert(!QuasiTypeExamples.structuralMatches("List[Int]", "Option[Int]"))
    assert(!QuasiTypeExamples.structuralMatches("Option[String]", "Option[Int]"))
    assert(!QuasiTypeExamples.structuralMatches("(Int, String)", "(String, Int)"))
    assert(!QuasiTypeExamples.structuralMatches("Int => String", "String => Int"))
  }

  test("structural equality agrees with exact rendered TypeRepr equality on the current boundary") {
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Int", "Int"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("String", "String"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Boolean", "Boolean"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("List[Int]", "List[Int]"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Option[String]", "Option[String]"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("(Int, String)", "(Int, String)"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Int => String", "Int => String"), "exact=true structural=true")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Int", "String"), "exact=false structural=false")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Int", "AnyVal"), "exact=false structural=false")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("List[Int]", "List[String]"), "exact=false structural=false")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("List[Int]", "Option[Int]"), "exact=false structural=false")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Option[String]", "Option[Int]"), "exact=false structural=false")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("(Int, String)", "(String, Int)"), "exact=false structural=false")
    assertEquals(QuasiTypeExamples.equalityComparisonSummary("Int => String", "String => Int"), "exact=false structural=false")
  }

  test("unsupported type syntax fails clearly") {
    assert(QuasiTypeExamples.unsupportedMessage("List[?]").contains("Unsupported type shape"))
    assert(QuasiTypeExamples.structuralNormalFormSummary("List[?]").contains("Unsupported type shape"))
    assert(QuasiTypeExamples.unsupportedMessage("{ type A = Int }").contains("Unsupported type shape"))
    assert(QuasiTypeExamples.structuralNormalFormSummary("{ type A = Int }").contains("Unsupported type shape"))
  }

  test("scala.Int remains an unresolved selected-alias boundary") {
    val message = QuasiTypeExamples.unsupportedMessage("scala.Int")

    assert(message.contains("Selected type syntax is not supported"))
    assert(QuasiTypeExamples.structuralNormalFormSummary("scala.Int").contains("Selected type syntax is not supported"))
    assert(!QuasiTypeExamples.matches("Int", "scala.Int"))
    assert(!QuasiTypeExamples.structuralMatches("Int", "scala.Int"))
    assert(!QuasiTypeExamples.matches("Int", "A.B"))
    assert(!QuasiTypeExamples.structuralMatches("Int", "A.B"))
  }
