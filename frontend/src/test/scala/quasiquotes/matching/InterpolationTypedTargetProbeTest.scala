package quasiquotes.matching

private object InterpolationTypedTargetExamples:
  private val name = "Ada"
  private val left = "a"
  private val right = "b"
  private def foo(value: String): String = value.reverse

  val plain = InterpolationTypedTargetProbe.inspect(s"plain")
  val direct = InterpolationTypedTargetProbe.inspect(s"hello $name")
  val braced = InterpolationTypedTargetProbe.inspect(s"value = ${foo(name)}")
  val multiple = InterpolationTypedTargetProbe.inspect(s"$left / $right")
  val literalDollar = InterpolationTypedTargetProbe.inspect(s"literal $$ dollar")
  val ordinaryCall = InterpolationTypedTargetProbe.inspect(
    StringContext("hello ", "").s(name)
  )

class InterpolationTypedTargetProbeTest extends munit.FunSuite:
  test("typed interpolation evidence retains reliable direct source code") {
    assertEquals(InterpolationTypedTargetExamples.plain.sourceCode, Some("s\"plain\""))
    assertEquals(InterpolationTypedTargetExamples.direct.sourceCode, Some("s\"hello $name\""))
    assertEquals(
      InterpolationTypedTargetExamples.braced.sourceCode,
      Some("s\"value = ${foo(name)}\"")
    )
  }

  test("typed interpolation calls expose StringContext, s, and repeated wrappers") {
    val evidence = List(
      InterpolationTypedTargetExamples.direct,
      InterpolationTypedTargetExamples.braced,
      InterpolationTypedTargetExamples.multiple
    )
    evidence.foreach { item =>
      assert(item.treeStructure.contains("StringContext"))
      assert(item.treeStructure.contains("Repeated"))
      assert(item.normalizedView.startsWith("InterpolatedString(s,"))
    }
  }

  test("plain and literal-dollar s syntax remain syntax-preserving normalized views") {
    assertEquals(
      InterpolationTypedTargetExamples.plain.normalizedView,
      "InterpolatedString(s, [\"plain\"], [])"
    )
    assertEquals(
      InterpolationTypedTargetExamples.literalDollar.normalizedView,
      "InterpolatedString(s, [\"literal $ dollar\"], [])"
    )
  }

  test("an ordinary StringContext call is not classified as source interpolation") {
    assert(!InterpolationTypedTargetExamples.ordinaryCall.normalizedView.startsWith("InterpolatedString("))
  }
