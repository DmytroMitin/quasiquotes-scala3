package quasiquotes.matching

private object TermPatternExtractorProofScope:
  private val left = 20
  private val right = 22
  private val qqCapture0 = 20
  private val captured = 22
  private val nestedValue = 42

  private def f(value: Int): Int = value
  private def g(value: Int): Int = value

  val ordered: (Int, Int) =
    TermPatternExtractorMacros.orderedCapture(left + right)
  val mismatch: Boolean =
    TermPatternExtractorMacros.matchesPlus(left)
  val nested: Int =
    TermPatternExtractorMacros.nestedCapture(f(g(nestedValue)))
  val literalAndCapture: Int =
    TermPatternExtractorMacros.literalAndCapture(qqCapture0 + captured)

class TermPatternExtractorTest extends munit.FunSuite:
  test("qq extracts caller-Quotes terms in left-to-right slot order"):
    assertEquals(TermPatternExtractorProofScope.ordered, (20, 22))

  test("qq turns an ordinary structural mismatch into pattern fallthrough"):
    assert(!TermPatternExtractorProofScope.mismatch)

  test("qq supports an already-admitted nested structural form"):
    assertEquals(TermPatternExtractorProofScope.nested, 42)

  test("qq keeps a same-text literal identifier distinct from its capture slot"):
    assertEquals(TermPatternExtractorProofScope.literalAndCapture, 22)
