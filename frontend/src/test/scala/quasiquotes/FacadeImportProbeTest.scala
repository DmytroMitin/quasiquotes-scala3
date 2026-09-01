package quasiquotes

final class FacadeImportProbeTest extends munit.FunSuite:
  test("one umbrella import exposes the current term, type, and definition families"):
    assert(FacadeImportProbeMacros.umbrellaWorks)

  test("selective domain imports work and legacy imports coexist through aliases"):
    assert(FacadeImportProbeMacros.selectiveAndLegacyWork)

  test("plain exports preserve all interpolation families in a macro Quotes universe"):
    assert(FacadeImportProbeMacros.plainExportsWork)

  test("a forwarding facade preserves transparent ranked qq extraction"):
    assert(FacadeImportProbeMacros.rankedFacadeWorks)
