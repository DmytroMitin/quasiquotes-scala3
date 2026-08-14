package quasiquotes.types

class TypeInterpolatorTest extends munit.FunSuite:
  test("tqr constructs zero, one, and multiple slots through neutral semantics"):
    assertEquals(
      TypeInterpolatorMacros.constructionSummary,
      "STypeIdent(Int) | STypeApply(STypeIdent(List), [STypeIdent(String)]) | STypeApply(STypeIdent(Either), [STypeIdent(Int), STypeIdent(Boolean)])"
    )

  test("tqq supports zero-hole matching and ordinary fallthrough"):
    assert(TypeInterpolatorMacros.zeroHoleMatches[Int])
    assert(!TypeInterpolatorMacros.zeroHoleMatches[String])
    assert(TypeInterpolatorMacros.unsupportedTargetFallsThrough)

  test("tqq returns one and multiple caller-owned captures in source order"):
    assertEquals(TypeInterpolatorMacros.oneCaptureSummary[List[String]], "STypeIdent(String)")
    assertEquals(
      TypeInterpolatorMacros.twoCaptureSummary[Either[Int, Boolean]],
      "STypeIdent(Int) then STypeIdent(Boolean)"
    )

  test("tqq captures are the original reflected target subtrees"):
    assert(TypeInterpolatorMacros.originalSubtreeProvenance)

  test("type syntax and existing function APIs coexist under wildcard and selective imports"):
    assert(TypeInterpolatorMacros.wildcardImportCompatibility)
    assert(TypeInterpolatorMacros.selectiveImportCompatibility)

  test("programmatic repeated type holes retain equality semantics"):
    assert(TypeInterpolatorMacros.programmaticRepeatedHoleCompatibility)
