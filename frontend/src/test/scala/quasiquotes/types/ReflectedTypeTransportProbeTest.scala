package quasiquotes.types

final class ReflectedTypeTransportProbeTest extends munit.FunSuite:
  test("a tqr result directly supplies a reflected constructor type without Any"):
    assertEquals(ReflectedTypeTransportProbeMacros.constructorFromTqr("probe"), 5)

  test("TypeTree.tpe and Type evidence enter the same TypeRepr transport"):
    assert(ReflectedTypeTransportProbeMacros.typeTreeAndTypeEvidenceAgree[List[Int]])
