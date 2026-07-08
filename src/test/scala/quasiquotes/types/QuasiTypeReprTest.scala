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

  test("source matching reports TypeNormalForm as the no-hole matching substrate") {
    assertEquals(
      QuasiTypeExamples.matchingSubstrateSummary("List[Int]"),
      "source=TypeNormalForm targetTypeRepr=TypeNormalForm exact-rendered-TypeRepr=debug"
    )
  }

  test("inspects supported target TypeRepr values as structural normal forms") {
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("Int"), "STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("String"), "STypeIdent(String)")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("Boolean"), "STypeIdent(Boolean)")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("List[Int]"), "STypeApply(STypeIdent(List), [STypeIdent(Int)])")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("List[String]"), "STypeApply(STypeIdent(List), [STypeIdent(String)])")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("Option[String]"), "STypeApply(STypeIdent(Option), [STypeIdent(String)])")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("(Int, String)"), "STypeTuple([STypeIdent(Int), STypeIdent(String)])")
    assertEquals(QuasiTypeExamples.targetNormalFormSummary("Int => String"), "STypeFunction([STypeIdent(Int)], STypeIdent(String))")
  }

  test("source and target TypeRepr normal forms agree on supported and rejected cases") {
    assertEquals(QuasiTypeExamples.targetInspectionComparisonSummary("Int", "Int"), "source=STypeIdent(Int) target=STypeIdent(Int) matched=true")
    assertEquals(QuasiTypeExamples.targetInspectionComparisonSummary("List[Int]", "List[Int]"), "source=STypeApply(STypeIdent(List), [STypeIdent(Int)]) target=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true")
    assertEquals(QuasiTypeExamples.targetInspectionComparisonSummary("Option[String]", "Option[String]"), "source=STypeApply(STypeIdent(Option), [STypeIdent(String)]) target=STypeApply(STypeIdent(Option), [STypeIdent(String)]) matched=true")
    assertEquals(QuasiTypeExamples.targetInspectionComparisonSummary("(Int, String)", "(Int, String)"), "source=STypeTuple([STypeIdent(Int), STypeIdent(String)]) target=STypeTuple([STypeIdent(Int), STypeIdent(String)]) matched=true")
    assertEquals(QuasiTypeExamples.targetInspectionComparisonSummary("Int => String", "Int => String"), "source=STypeFunction([STypeIdent(Int)], STypeIdent(String)) target=STypeFunction([STypeIdent(Int)], STypeIdent(String)) matched=true")
    assertEquals(QuasiTypeExamples.targetInspectionComparisonSummary("List[Int]", "List[String]"), "source=STypeApply(STypeIdent(List), [STypeIdent(Int)]) target=STypeApply(STypeIdent(List), [STypeIdent(String)]) matched=false")
  }

  test("direct target TypeRepr matching reports TypeNormalForm after Phase 17 migration") {
    assertEquals(
      QuasiTypeExamples.matchingSubstrateSummary("List[Int]"),
      "source=TypeNormalForm targetTypeRepr=TypeNormalForm exact-rendered-TypeRepr=debug"
    )
  }

  test("matches simple type-hole patterns and exposes bindings") {
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("$t", "Int"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("List[$t]", "List[Int]"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("Option[$t]", "Option[String]"), "matched=true bindings=t=STypeIdent(String)")
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("($a, $b)", "(Int, String)"), "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)")
  }

  test("tqq function syntax delegates to type-hole pattern matching") {
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("$t", "Int"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("List[$t]", "List[Int]"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("Option[$t]", "Option[String]"), "matched=true bindings=t=STypeIdent(String)")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("($a, $b)", "(Int, String)"), "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("($t, $t)", "(Int, Int)"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("$t => $t", "Int => Int"), "matched=true bindings=t=STypeIdent(Int)")
  }

  test("tqq function syntax preserves rejected and unsupported boundaries") {
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("($t, $t)", "(Int, String)"), "matched=false")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("$t => $t", "Int => String"), "matched=false")
    assertEquals(QuasiTypeExamples.tqqTypePatternMatchSummary("List[$t]", "Option[Int]"), "matched=false")
    assert(QuasiTypeExamples.tqqTypePatternMatchSummary("List[$t]", "scala.Int").contains("Selected type syntax is not supported"))
    assert(QuasiTypeExamples.tqqTypePatternMatchSummary("List[$t]", "List[?]").contains("Unsupported type shape"))
  }

  test("tqq function syntax matches explicit QuasiTypePattern repr behavior") {
    assertEquals(
      QuasiTypeExamples.tqqEquivalenceSummary("List[$t]", "List[Int]"),
      "explicit=t=STypeIdent(Int) tqq=t=STypeIdent(Int)"
    )
    assertEquals(
      QuasiTypeExamples.tqqEquivalenceSummary("($t, $t)", "(Int, String)"),
      "explicit=no-match tqq=no-match"
    )
  }

  test("constructs type templates with explicit type-hole bindings") {
    assertEquals(
      QuasiTypeConstruct.fromTemplate("$t", "t" -> TypeNormalForm.STypeIdent("Int")).map(_.normalForm.render),
      Right("STypeIdent(Int)")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("List[$t]", "t" -> TypeNormalForm.STypeIdent("Int")).map(_.source),
      Right("List[Int]")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("Option[$t]", "t" -> TypeNormalForm.STypeIdent("String")).map(_.source),
      Right("Option[String]")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("($a, $b)", "a" -> TypeNormalForm.STypeIdent("Int"), "b" -> TypeNormalForm.STypeIdent("String")).map(_.source),
      Right("(Int, String)")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("$a => $b", "a" -> TypeNormalForm.STypeIdent("Int"), "b" -> TypeNormalForm.STypeIdent("String")).map(_.source),
      Right("Int => String")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("($t, $t)", "t" -> TypeNormalForm.STypeIdent("Int")).map(_.source),
      Right("(Int, Int)")
    )
  }

  test("tqr function delegates to type-template construction") {
    assertEquals(
      QuasiTypequotes.tqr("List[$t]", "t" -> TypeNormalForm.STypeIdent("Int")).map(_.source),
      Right("List[Int]")
    )
  }

  test("type construction rejects missing, extra, unsupported template, and unsupported binding cases") {
    assertEquals(
      QuasiTypeConstruct.fromTemplate("List[$t]").left.map(_.message),
      Left("Missing type-construction binding `t`")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("List[$t]", "t" -> TypeNormalForm.STypeIdent("Int"), "extra" -> TypeNormalForm.STypeIdent("String")).left.map(_.message),
      Left("Extra type-construction binding(s): extra")
    )
    assert(QuasiTypeConstruct.fromTemplate("List[?]", "t" -> TypeNormalForm.STypeIdent("Int")).left.exists(_.message.contains("Unsupported type construction template shape")))
    assert(QuasiTypeConstruct.fromTemplate("scala.Int").left.exists(_.message.contains("Selected type syntax is not supported")))
    assert(QuasiTypeConstruct.fromTemplate("A.B").left.exists(_.message.contains("Selected type syntax is not supported")))
    assertEquals(
      QuasiTypeConstruct.fromTemplate("List[$t]", "t" -> TypeNormalForm.STypeIdent("AnyVal")).left.map(_.message),
      Left("Unsupported constructed type identifier for Phase 21: AnyVal")
    )
  }

  test("type construction and type matching are dual over TypeNormalForm bindings") {
    assertEquals(QuasiTypeExamples.typeConstructionDualitySummary("List[$t]", "List[Int]"), "List[Int]")
    assertEquals(QuasiTypeExamples.typeConstructionDualitySummary("($a, $b)", "(Int, String)"), "(Int, String)")
  }

  test("repeated type holes enforce structural normal-form equality") {
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("($t, $t)", "(Int, Int)"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("$t => $t", "Int => Int"), "matched=true bindings=t=STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("($t, $t)", "(Int, String)"), "matched=false")
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("$t => $t", "Int => String"), "matched=false")
  }

  test("type-hole patterns preserve unsupported and rejected boundaries") {
    assertEquals(QuasiTypeExamples.typePatternMatchSummary("List[$t]", "Option[Int]"), "matched=false")
    assert(QuasiTypeExamples.typePatternMatchSummary("List[$t]", "List[?]").contains("Unsupported type shape"))
    assert(QuasiTypeExamples.typePatternMatchSummary("List[$t]", "scala.Int").contains("Selected type syntax is not supported"))
    assert(QuasiTypeExamples.typePatternMatchSummary("List[$t]", "A.B").contains("Selected type syntax is not supported"))
  }

  test("type-hole binding lookup accepts bare and dollar-prefixed names") {
    assertEquals(QuasiTypeExamples.typePatternBindingSummary("List[$t]", "List[Int]", "t"), "STypeIdent(Int)")
    assertEquals(QuasiTypeExamples.typePatternBindingSummary("List[$t]", "List[Int]", "$t"), "STypeIdent(Int)")
  }

  test("type-hole required binding reports missing names clearly") {
    val result = TypeMatchResult(Map("t" -> TypeNormalForm.STypeIdent("Int")))

    assertEquals(result.requiredBinding("t").map(_.render), Right("STypeIdent(Int)"))
    assertEquals(result.requiredBinding("$t").map(_.render), Right("STypeIdent(Int)"))
    assertEquals(
      result.requiredBinding("$missing").left.map(_.message),
      Left("Missing type-hole binding `missing`; available bindings: t")
    )
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
    assert(QuasiTypeExamples.targetNormalFormSummary("scala.Int").contains("Selected type syntax is not supported"))
  }
