package quasiquotes.matching

private object ConstructorNewTypedExamples:
  private val cond = true
  private val x = 8
  private def foo(value: Int): Int = value * 2
  private def consume(value: java.lang.StringBuilder): Int = value.capacity()

  val empty = ConstructorNewTypedPreflight.inspect(new java.lang.StringBuilder())
  val capacity = ConstructorNewTypedPreflight.inspect(new java.lang.StringBuilder(16))
  val exception = ConstructorNewTypedPreflight.inspect(new java.lang.RuntimeException("boom"))
  val conditional = ConstructorNewTypedPreflight.inspect(new java.lang.StringBuilder(if cond then 8 else 16))
  val nestedArgument = ConstructorNewTypedPreflight.inspect(new java.lang.StringBuilder(foo(x)))
  val selected = ConstructorNewTypedPreflight.inspect((new java.lang.StringBuilder(16)).toString)
  val outerApplication = ConstructorNewTypedPreflight.inspect(consume(new java.lang.StringBuilder(16)))

class ConstructorNewTypedPreflightTest extends munit.FunSuite:
  private val examples = Vector(
    ConstructorNewTypedExamples.empty,
    ConstructorNewTypedExamples.capacity,
    ConstructorNewTypedExamples.exception,
    ConstructorNewTypedExamples.conditional,
    ConstructorNewTypedExamples.nestedArgument,
    ConstructorNewTypedExamples.selected,
    ConstructorNewTypedExamples.outerApplication
  )

  test("typed constructor evidence retains genuine New and <init> nodes") {
    examples.foreach { evidence =>
      println(s"CONSTRUCTOR_NEW_TYPED_PREFLIGHT $evidence")
      assert(evidence.hasGenuineNew)
      assert(evidence.hasInitSelect)
      assert(evidence.constructorSymbolFullName.endsWith(".<init>"))
    }
  }

  test("typed constructor class identity, result type, and argument count are stable") {
    assertEquals(ConstructorNewTypedExamples.empty.constructorClassName, "java.lang.StringBuilder")
    assertEquals(ConstructorNewTypedExamples.empty.argumentCount, 0)
    assertEquals(ConstructorNewTypedExamples.capacity.argumentCount, 1)
    assertEquals(ConstructorNewTypedExamples.exception.constructorClassName, "java.lang.RuntimeException")
    assert(ConstructorNewTypedExamples.capacity.resultType.contains("StringBuilder"))
  }

  test("typed composition retains the nested constructor application") {
    List(ConstructorNewTypedExamples.selected, ConstructorNewTypedExamples.outerApplication).foreach { evidence =>
      assert(evidence.recursiveShape.contains("java.lang.StringBuilder"))
      assert(evidence.recursiveShape.contains("<init>"))
    }
  }
