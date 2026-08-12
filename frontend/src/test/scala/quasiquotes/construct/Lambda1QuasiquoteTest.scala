package quasiquotes.construct

private object Lambda1QuasiquoteFixtures:
  val identity: Int => Any = Lambda1StructuralExamples.identity
  val addOne: Int => Any = Lambda1StructuralExamples.addOne
  val stringIdentity: String => Any = Lambda1StructuralExamples.stringIdentity
  private def double(value: Int): Int = value * 2
  val call: Int => Any = Lambda1StructuralExamples.call

  private val x = 40
  val captureAvoiding: Int => Any = Lambda1StructuralExamples.preserveOuter(x)

  val unsafeSpliceMessage: String =
    Lambda1StructuralExamples.unsafeSpliceMessage {
      val x = 1
      x + 1
    }

class Lambda1QuasiquoteTest extends munit.FunSuite:
  test("qr constructs genuine one-parameter explicitly typed lambdas") {
    assertEquals(Lambda1QuasiquoteFixtures.identity(7), 7)
    assertEquals(Lambda1QuasiquoteFixtures.addOne(7), 8)
    assertEquals(Lambda1QuasiquoteFixtures.stringIdentity("value"), "value")
  }

  test("qr lowers source-written binder references inside applications") {
    assertEquals(Lambda1QuasiquoteFixtures.call(7), 14)
  }

  test("qr preserves an external same-text identifier instead of capturing it") {
    assertEquals(Lambda1QuasiquoteFixtures.captureAvoiding(2), 42)
  }

  test("qr rejects a lambda-body splice containing owned definitions") {
    assert(Lambda1QuasiquoteFixtures.unsafeSpliceMessage.contains("owned definitions"))
    assert(!Lambda1QuasiquoteFixtures.unsafeSpliceMessage.contains("Symbol"))
    assert(!Lambda1QuasiquoteFixtures.unsafeSpliceMessage.contains("owner"))
  }
