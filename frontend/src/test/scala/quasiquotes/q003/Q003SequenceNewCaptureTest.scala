package quasiquotes.q003

final class Q003Constructor():
  def this(first: Int) = this()
  def this(first: Int, second: Int) = this()
  def this(first: Int, second: Int, third: Int) = this()
  def this(first: Int, second: Int, third: Int, fourth: Int) = this()

final class Q003OtherConstructor():
  def this(first: Int, second: Int) = this()

private object Q003SequenceNewCaptureScope:
  val empty: List[Int] =
    Q003SequenceNewCaptureMacros.captureArguments(new Q003Constructor())
  val one: List[Int] =
    Q003SequenceNewCaptureMacros.captureArguments(new Q003Constructor(1))
  val many: List[Int] =
    Q003SequenceNewCaptureMacros.captureArguments(new Q003Constructor(1, 2, 3))
  val emptyTail: (Int, List[Int]) =
    Q003SequenceNewCaptureMacros.captureTail(new Q003Constructor(1))
  val oneTail: (Int, List[Int]) =
    Q003SequenceNewCaptureMacros.captureTail(new Q003Constructor(1, 2))
  val manyTail: (Int, List[Int]) =
    Q003SequenceNewCaptureMacros.captureTail(new Q003Constructor(1, 2, 3, 4))
  val emptyInit: (List[Int], Int) =
    Q003SequenceNewCaptureMacros.captureInit(new Q003Constructor(9))
  val oneInit: (List[Int], Int) =
    Q003SequenceNewCaptureMacros.captureInit(new Q003Constructor(1, 9))
  val manyInit: (List[Int], Int) =
    Q003SequenceNewCaptureMacros.captureInit(new Q003Constructor(1, 2, 3, 9))
  val emptyMiddle: Option[(Int, List[Int], Int)] =
    Q003SequenceNewCaptureMacros.captureMiddle(new Q003Constructor(1, 9))
  val oneMiddle: Option[(Int, List[Int], Int)] =
    Q003SequenceNewCaptureMacros.captureMiddle(new Q003Constructor(1, 2, 9))
  val manyMiddle: Option[(Int, List[Int], Int)] =
    Q003SequenceNewCaptureMacros.captureMiddle(new Q003Constructor(1, 2, 3, 9))
  val insufficient: Option[(Int, List[Int], Int)] =
    Q003SequenceNewCaptureMacros.captureMiddle(new Q003Constructor(1))
  val prefixMismatch: Boolean =
    Q003SequenceNewCaptureMacros.fixedEndsMatch(new Q003Constructor(2, 3, 9))
  val suffixMismatch: Boolean =
    Q003SequenceNewCaptureMacros.fixedEndsMatch(new Q003Constructor(1, 3, 8))
  val identityMismatch: Boolean =
    Q003SequenceNewCaptureMacros.constructorMatches(new Q003OtherConstructor(1, 2))
  val scalar: Int =
    Q003SequenceNewCaptureMacros.scalarCapture(new Q003Constructor(42))

class Q003SequenceNewCaptureTest extends munit.FunSuite:
  test("fixed New binds zero, one, and many direct arguments as an ordered Term sequence"):
    assertEquals(Q003SequenceNewCaptureScope.empty, Nil)
    assertEquals(Q003SequenceNewCaptureScope.one, List(1))
    assertEquals(Q003SequenceNewCaptureScope.many, List(1, 2, 3))

  test("ranked New preserves fixed prefix and suffix semantics"):
    assertEquals(Q003SequenceNewCaptureScope.emptyTail, (1, Nil))
    assertEquals(Q003SequenceNewCaptureScope.oneTail, (1, List(2)))
    assertEquals(Q003SequenceNewCaptureScope.manyTail, (1, List(2, 3, 4)))
    assertEquals(Q003SequenceNewCaptureScope.emptyInit, (Nil, 9))
    assertEquals(Q003SequenceNewCaptureScope.oneInit, (List(1), 9))
    assertEquals(Q003SequenceNewCaptureScope.manyInit, (List(1, 2, 3), 9))
    assertEquals(Q003SequenceNewCaptureScope.emptyMiddle, Some((1, Nil, 9)))
    assertEquals(Q003SequenceNewCaptureScope.oneMiddle, Some((1, List(2), 9)))
    assertEquals(Q003SequenceNewCaptureScope.manyMiddle, Some((1, List(2, 3), 9)))

  test("ranked New rejects identity, length, prefix, and suffix mismatches without leakage"):
    assertEquals(Q003SequenceNewCaptureScope.insufficient, None)
    assert(!Q003SequenceNewCaptureScope.prefixMismatch)
    assert(!Q003SequenceNewCaptureScope.suffixMismatch)
    assert(!Q003SequenceNewCaptureScope.identityMismatch)

  test("ordinary scalar New capture retains exact Term typing and behavior"):
    assertEquals(Q003SequenceNewCaptureScope.scalar, 42)
