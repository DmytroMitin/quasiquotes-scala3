package quasiquotes.q002

private object Q002SequenceTermCaptureTargets:
  def empty(): List[Int] = Nil
  def one(first: Int): List[Int] = List(first)
  def many(first: Int, second: Int, third: Int): List[Int] = List(first, second, third)
  def two(first: Int, second: Int): List[Int] = List(first, second)
  def four(first: Int, second: Int, third: Int, fourth: Int): List[Int] =
    List(first, second, third, fourth)

private object Q002SequenceTermCaptureScope:
  private val scalarLeft = 20
  private val scalarRight = 22
  val empty: List[Int] =
    Q002SequenceTermCaptureMacros.captureArguments(Q002SequenceTermCaptureTargets.empty())
  val one: List[Int] =
    Q002SequenceTermCaptureMacros.captureArguments(Q002SequenceTermCaptureTargets.one(1))
  val many: List[Int] =
    Q002SequenceTermCaptureMacros.captureArguments(Q002SequenceTermCaptureTargets.many(1, 2, 3))

  val tail: (Int, List[Int]) =
    Q002SequenceTermCaptureMacros.captureTail(Q002SequenceTermCaptureTargets.many(1, 2, 3))
  val init: (List[Int], Int) =
    Q002SequenceTermCaptureMacros.captureInit(Q002SequenceTermCaptureTargets.many(1, 2, 3))
  val emptyMiddle: Option[(Int, List[Int], Int)] =
    Q002SequenceTermCaptureMacros.captureMiddle(Q002SequenceTermCaptureTargets.two(1, 9))
  val oneMiddle: Option[(Int, List[Int], Int)] =
    Q002SequenceTermCaptureMacros.captureMiddle(Q002SequenceTermCaptureTargets.many(1, 2, 9))
  val manyMiddle: Option[(Int, List[Int], Int)] =
    Q002SequenceTermCaptureMacros.captureMiddle(Q002SequenceTermCaptureTargets.four(1, 2, 3, 9))
  val insufficient: Option[(Int, List[Int], Int)] =
    Q002SequenceTermCaptureMacros.captureMiddle(Q002SequenceTermCaptureTargets.one(1))
  val prefixMismatch: Boolean =
    Q002SequenceTermCaptureMacros.fixedEndsMatch(Q002SequenceTermCaptureTargets.many(2, 3, 9))
  val suffixMismatch: Boolean =
    Q002SequenceTermCaptureMacros.fixedEndsMatch(Q002SequenceTermCaptureTargets.many(1, 3, 8))
  val scalar: (Int, Int) =
    Q002SequenceTermCaptureMacros.scalarCapture(scalarLeft + scalarRight)

class Q002SequenceTermCaptureTest extends munit.FunSuite:
  test("public qq binds one immediate rank-2 capture as an ordered Term sequence"):
    assertEquals(Q002SequenceTermCaptureScope.empty, Nil)
    assertEquals(Q002SequenceTermCaptureScope.one, List(1))
    assertEquals(Q002SequenceTermCaptureScope.many, List(1, 2, 3))

  test("ranked Apply matching preserves fixed prefix and suffix semantics"):
    assertEquals(Q002SequenceTermCaptureScope.tail, (1, List(2, 3)))
    assertEquals(Q002SequenceTermCaptureScope.init, (List(1, 2), 3))
    assertEquals(Q002SequenceTermCaptureScope.emptyMiddle, Some((1, Nil, 9)))
    assertEquals(Q002SequenceTermCaptureScope.oneMiddle, Some((1, List(2), 9)))
    assertEquals(Q002SequenceTermCaptureScope.manyMiddle, Some((1, List(2, 3), 9)))

  test("ranked Apply matching fails without leaking partial prefix or suffix bindings"):
    assertEquals(Q002SequenceTermCaptureScope.insufficient, None)
    assert(!Q002SequenceTermCaptureScope.prefixMismatch)
    assert(!Q002SequenceTermCaptureScope.suffixMismatch)

  test("ordinary scalar qq keeps exact Term binder types and behavior"):
    assertEquals(Q002SequenceTermCaptureScope.scalar, (20, 22))
