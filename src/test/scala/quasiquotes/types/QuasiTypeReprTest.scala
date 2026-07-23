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
    assertEquals(
      QuasiTypeExamples.structuralNormalFormSummary("(Int, String, Boolean)"),
      "STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)])"
    )
    assertEquals(
      QuasiTypeExamples.structuralNormalFormSummary("(Int, String) => Boolean"),
      "STypeFunction([STypeIdent(Int), STypeIdent(String)], STypeIdent(Boolean))"
    )
  }

  test("Tuple3 and Function2 structural equality preserves order and shape") {
    assert(QuasiTypeExamples.structuralMatches("(Int, String, Boolean)", "(Int, String, Boolean)"))
    assert(!QuasiTypeExamples.structuralMatches("(Int, String, Boolean)", "(String, Int, Boolean)"))
    assert(QuasiTypeExamples.structuralMatches("(Int, String) => Boolean", "(Int, String) => Boolean"))
    assert(!QuasiTypeExamples.structuralMatches("(Int, String) => Boolean", "(String, Int) => Boolean"))
    assert(!QuasiTypeExamples.structuralMatches("(Int, String) => Boolean", "((Int, String)) => Boolean"))
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

  test("Tuple3 and Function2 patterns support distinct and repeated holes") {
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("($a, $b, $c)", "(Int, String, Boolean)"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String), c=STypeIdent(Boolean)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("($t, $t, $u)", "(Int, Int, String)"),
      "matched=true bindings=t=STypeIdent(Int), u=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("($t, $t, $u)", "(Int, String, String)"),
      "matched=false"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("($a, $b) => $a", "(Int, String) => Int"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternMatchSummary("($a, $b) => $a", "(Int, String) => String"),
      "matched=false"
    )
  }

  test("Tuple3 and Function2 flow through tqq pattern and repr compatibility paths") {
    assertEquals(
      QuasiTypeExamples.tqqTypePatternMatchSummary("($a, $b, $c)", "(Int, String, Boolean)"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String), c=STypeIdent(Boolean)"
    )
    assertEquals(
      QuasiTypeExamples.tqqTypePatternMatchSummary("($a, $b) => $a", "(Int, String) => Int"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.patternAliasEquivalenceSummary("($a, $b, $c)", "(Int, String, Boolean)"),
      "pattern=a=STypeIdent(Int), b=STypeIdent(String), c=STypeIdent(Boolean) repr=a=STypeIdent(Int), b=STypeIdent(String), c=STypeIdent(Boolean)"
    )
    assertEquals(
      QuasiTypeExamples.locatedPatternSummary("(Int, String, Boolean)"),
      "success=true legacySuccess=true expected=true holes=false"
    )
    assertEquals(
      QuasiTypeExamples.locatedPatternSummary("($a, $b) => $r"),
      "success=true legacySuccess=true expected=false holes=true"
    )
  }

  test("Tuple3 and Function2 patterns match real TypeRepr targets") {
    assertEquals(
      QuasiTypeExamples.typePatternTypeReprMatchSummary("($a, $b, $c)", "(Int, String, Boolean)"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String), c=STypeIdent(Boolean)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternTypeReprMatchSummary("($t, $t, $u)", "(Int, Int, String)"),
      "matched=true bindings=t=STypeIdent(Int), u=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternTypeReprMatchSummary("($a, $b) => $a", "(Int, String) => Int"),
      "matched=true bindings=a=STypeIdent(Int), b=STypeIdent(String)"
    )
    assertEquals(
      QuasiTypeExamples.typePatternTypeReprMatchSummary("($a, $b) => $a", "(Int, String) => String"),
      "matched=false"
    )
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

  test("QuasiTypePattern pattern alias preserves repr compatibility") {
    assertEquals(
      QuasiTypeExamples.patternAliasEquivalenceSummary("List[$t]", "List[Int]"),
      "pattern=t=STypeIdent(Int) repr=t=STypeIdent(Int)"
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

  test("constructs Tuple3 and Function2 templates with stable source rendering") {
    val intForm = TypeNormalForm.STypeIdent("Int")
    val stringForm = TypeNormalForm.STypeIdent("String")
    val booleanForm = TypeNormalForm.STypeIdent("Boolean")

    assertEquals(
      QuasiTypeConstruct.fromTemplate(
        "($a, $b, $c)",
        "a" -> intForm,
        "b" -> stringForm,
        "c" -> booleanForm
      ).map(_.source),
      Right("(Int, String, Boolean)")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate(
        "($a, $b) => $r",
        "a" -> intForm,
        "b" -> stringForm,
        "r" -> booleanForm
      ).map(_.source),
      Right("(Int, String) => Boolean")
    )
    assertEquals(
      QuasiTypeConstruct.fromTemplate("($t, $t, $u)", "t" -> intForm, "u" -> stringForm).map(_.source),
      Right("(Int, Int, String)")
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

  test("constructed simple type normal forms lower to TypeRepr and inspect back") {
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("$t", "t", "Int"),
      "constructed=STypeIdent(Int) inspected=STypeIdent(Int) matched=true"
    )
  }

  test("constructed applied type normal forms lower to TypeRepr and inspect back") {
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("List[$t]", "t", "Int"),
      "constructed=STypeApply(STypeIdent(List), [STypeIdent(Int)]) inspected=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true"
    )
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("Option[$t]", "t", "String"),
      "constructed=STypeApply(STypeIdent(Option), [STypeIdent(String)]) inspected=STypeApply(STypeIdent(Option), [STypeIdent(String)]) matched=true"
    )
  }

  test("constructed tuple type normal forms lower to TypeRepr and inspect back") {
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("($a, $b)", "a", "Int", "b", "String"),
      "constructed=STypeTuple([STypeIdent(Int), STypeIdent(String)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(String)]) matched=true"
    )
  }

  test("constructed function type normal forms lower to TypeRepr and inspect back") {
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("$a => $b", "a", "Int", "b", "String"),
      "constructed=STypeFunction([STypeIdent(Int)], STypeIdent(String)) inspected=STypeFunction([STypeIdent(Int)], STypeIdent(String)) matched=true"
    )
  }

  test("constructed Tuple3 and Function2 normal forms lower and inspect back") {
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("($a, String, Boolean)", "a", "Int"),
      "constructed=STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)]) matched=true"
    )
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("($a, String) => Boolean", "a", "Int"),
      "constructed=STypeFunction([STypeIdent(Int), STypeIdent(String)], STypeIdent(Boolean)) inspected=STypeFunction([STypeIdent(Int), STypeIdent(String)], STypeIdent(Boolean)) matched=true"
    )
  }

  test("Tuple3 and Function2 compose existing List and Option components") {
    assertEquals(
      QuasiTypeExamples.targetNormalFormSummary("(List[Int], Option[String], Boolean)"),
      "STypeTuple([STypeApply(STypeIdent(List), [STypeIdent(Int)]), STypeApply(STypeIdent(Option), [STypeIdent(String)]), STypeIdent(Boolean)])"
    )
    assertEquals(
      QuasiTypeExamples.targetNormalFormSummary("(List[Int], String) => Option[Boolean]"),
      "STypeFunction([STypeApply(STypeIdent(List), [STypeIdent(Int)]), STypeIdent(String)], STypeApply(STypeIdent(Option), [STypeIdent(Boolean)]))"
    )
  }

  test("direct real Tuple3 and Function2 TypeRepr values inspect structurally") {
    assertEquals(
      QuasiTypeExamples.phase37DirectTargetNormalFormSummary("tuple3"),
      "STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)])"
    )
    assertEquals(
      QuasiTypeExamples.phase37DirectTargetNormalFormSummary("function2"),
      "STypeFunction([STypeIdent(Int), STypeIdent(String)], STypeIdent(Boolean))"
    )
  }

  test("constructed repeated type holes lower to TypeRepr and inspect back") {
    assertEquals(
      QuasiTypeExamples.constructedTypeReprRoundtripSummary("($t, $t)", "t", "Int"),
      "constructed=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) matched=true"
    )
  }

  test("type matching construction and TypeRepr lowering are dual over TypeNormalForm") {
    assertEquals(
      QuasiTypeExamples.typeConstructionLoweringDualitySummary("List[$t]", "List[Int]"),
      "constructed=STypeApply(STypeIdent(List), [STypeIdent(Int)]) inspected=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true"
    )
  }

  test("constructed TypeRepr lowering rejects unsupported normal forms clearly") {
    assertEquals(
      QuasiTypeExamples.normalFormLoweringMessage("AnyVal"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: AnyVal"
    )
    assertEquals(
      QuasiTypeExamples.rawIdentifierLoweringMessage("MyType"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: MyType"
    )
    assertEquals(
      QuasiTypeExamples.rawAppliedLoweringMessage("Either", "Int"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: Either[Int]"
    )
    assertEquals(
      QuasiTypeExamples.normalFormLoweringMessage("List[List[Int]]"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: List[List[Int]]"
    )
    assertEquals(
      QuasiTypeExamples.normalFormLoweringMessage("(Int, Boolean)"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: (Int, Boolean)"
    )
    assertEquals(
      QuasiTypeExamples.normalFormLoweringMessage("Boolean => Int"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: Boolean => Int"
    )
    assertEquals(
      QuasiTypeExamples.rawTupleArityLoweringMessage,
      "Cannot lower unsupported constructed type normal form to TypeRepr: (Int, String, Boolean, Int)"
    )
    assertEquals(
      QuasiTypeExamples.rawFunctionArityLoweringMessage,
      "Cannot lower unsupported constructed type normal form to TypeRepr: (Int, String, Boolean) => Int"
    )
    assertEquals(
      QuasiTypeExamples.constructedTypeReprLoweringMessage("List[$t]", "t", "AnyVal"),
      "Unsupported constructed type identifier for Phase 21: AnyVal"
    )
  }

  test("constructed simple types bridge to scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("$t", "t", "Int"),
      "constructed=STypeIdent(Int) evidence=STypeIdent(Int) matched=true"
    )
  }

  test("normal-form convenience bridges to the same scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.normalFormBridgeSummary("Int"),
      "normalForm=STypeIdent(Int) evidence=STypeIdent(Int) matched=true"
    )
  }

  test("template convenience bridges to the same scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.templateBridgeSummary("List[$t]", "t", "Int"),
      "constructed=STypeApply(STypeIdent(List), [STypeIdent(Int)]) evidence=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true"
    )
  }

  test("constructed applied types bridge to scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("List[$t]", "t", "Int"),
      "constructed=STypeApply(STypeIdent(List), [STypeIdent(Int)]) evidence=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true"
    )
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("Option[$t]", "t", "String"),
      "constructed=STypeApply(STypeIdent(Option), [STypeIdent(String)]) evidence=STypeApply(STypeIdent(Option), [STypeIdent(String)]) matched=true"
    )
  }

  test("constructed tuple and function types bridge to scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("($a, $b)", "a", "Int", "b", "String"),
      "constructed=STypeTuple([STypeIdent(Int), STypeIdent(String)]) evidence=STypeTuple([STypeIdent(Int), STypeIdent(String)]) matched=true"
    )
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("$a => $b", "a", "Int", "b", "String"),
      "constructed=STypeFunction([STypeIdent(Int)], STypeIdent(String)) evidence=STypeFunction([STypeIdent(Int)], STypeIdent(String)) matched=true"
    )
  }

  test("constructed Tuple3 and Function2 bridge to scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("($a, String, Boolean)", "a", "Int"),
      "constructed=STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)]) evidence=STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)]) matched=true"
    )
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("($a, String) => Boolean", "a", "Int"),
      "constructed=STypeFunction([STypeIdent(Int), STypeIdent(String)], STypeIdent(Boolean)) evidence=STypeFunction([STypeIdent(Int), STypeIdent(String)], STypeIdent(Boolean)) matched=true"
    )
  }

  test("constructed repeated holes bridge to scoped high-level Type evidence") {
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("($t, $t)", "t", "Int"),
      "constructed=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) evidence=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) matched=true"
    )
  }

  test("high-level Type bridge preserves construction and lowering failures") {
    assertEquals(
      ConstructedTypeBridgeExamples.missingBindingMessage("List[$t]"),
      "Missing type-construction binding `t`"
    )
    assertEquals(
      ConstructedTypeBridgeExamples.bridgeSummary("List[$t]", "t", "Int", "extra", "String"),
      "Extra type-construction binding(s): extra"
    )
    assert(ConstructedTypeBridgeExamples.bridgeSummary("scala.Int", "t", "Int").contains("Selected type syntax is not supported"))
    assert(ConstructedTypeBridgeExamples.bridgeSummary("List[?]", "t", "Int").contains("Unsupported type construction template shape"))
    assertEquals(
      ConstructedTypeBridgeExamples.unsupportedNormalFormMessage("AnyVal"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: AnyVal"
    )
  }

  test("high-level Type bridge leaves equality and syntax boundaries unchanged") {
    assert(!QuasiTypeExamples.matches("Int", "scala.Int"))
    assert(ConstructedTypeBridgeExamples.bridgeSummary("scala.Int", "t", "Int").contains("Selected type syntax is not supported"))
    // The continuation returns this stable String; its dependent Type[t] evidence does not escape.
    assert(ConstructedTypeBridgeExamples.bridgeSummary("$t", "t", "Int").isInstanceOf[String])
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

  test("Tuple4 and Function3 remain unsupported across normal forms patterns and templates") {
    val tuple4 = "(Int, String, Boolean, Int)"
    val function3 = "(Int, String, Boolean) => Int"

    assert(TypeNormalForm.fromSource(tuple4).left.exists(_.message.contains("Unsupported tuple type shape")))
    assert(TypeNormalForm.fromSource(function3).left.exists(_.message.contains("Unsupported function type shape")))
    assert(TypePattern.fromSource(tuple4).left.exists(_.message.contains("Unsupported tuple type pattern shape")))
    assert(TypePattern.fromSource(function3).left.exists(_.message.contains("Unsupported function type pattern shape")))
    assert(TypeTemplate.fromSource(tuple4).left.exists(_.message.contains("Unsupported tuple type construction template shape")))
    assert(TypeTemplate.fromSource(function3).left.exists(_.message.contains("Unsupported function type construction template shape")))

    val tuple4Form = TypeNormalForm.STypeTuple(List(
      TypeNormalForm.STypeIdent("Int"),
      TypeNormalForm.STypeIdent("String"),
      TypeNormalForm.STypeIdent("Boolean"),
      TypeNormalForm.STypeIdent("Int")
    ))
    val function3Form = TypeNormalForm.STypeFunction(
      List(
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("String"),
        TypeNormalForm.STypeIdent("Boolean")
      ),
      TypeNormalForm.STypeIdent("Int")
    )
    assertEquals(
      TypeTemplate.validateConstructed(tuple4Form).left.map(_.message),
      Left("Unsupported constructed tuple type for Phase 21: (Int, String, Boolean, Int)")
    )
    assertEquals(
      TypeTemplate.validateConstructed(function3Form).left.map(_.message),
      Left("Unsupported constructed function type for Phase 21: (Int, String, Boolean) => Int")
    )
    assert(QuasiTypeExamples.unsupportedMessage("(Int, String, Boolean, Int)").contains("Unsupported type shape for Phase 13 TypeRepr lowering"))
    assert(QuasiTypeExamples.unsupportedMessage("(Int, String, Boolean) => Int").contains("Unsupported type shape for Phase 13 TypeRepr lowering"))
    assert(QuasiTypeExamples.phase37DirectTargetNormalFormSummary("tuple4").contains("Unsupported target TypeRepr shape"))
    assert(QuasiTypeExamples.phase37DirectTargetNormalFormSummary("function3").contains("Unsupported target TypeRepr shape"))
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
