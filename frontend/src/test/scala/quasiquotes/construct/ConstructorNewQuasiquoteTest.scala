package quasiquotes.construct

class ConstructorNewQuasiquoteTest extends munit.FunSuite:
  test("qr constructs genuine constructor expressions with empty, literal, and outer-hole arguments") {
    assertEquals(ConstructorNewExamples.emptyBuilderLength, 0)
    assertEquals(ConstructorNewExamples.literalBuilderCapacity, 24)
    assertEquals(ConstructorNewExamples.stringBuilderCapacity(16), 16)
    assertEquals(ConstructorNewExamples.stringBuilderCapacity(32), 32)
    assertEquals(ConstructorNewExamples.exceptionMessage("boom"), "boom")
    val structure = ConstructorNewExamples.treeStructure(16)
    assert(structure.contains("New("))
    assert(structure.contains("<init>"))
  }

  test("source inspection selects the bounded constructor shape and composes structurally") {
    val parsed = quasiquotes.parser.TinyTermParser.parseOrThrow(
      "foo(new java.lang.StringBuilder(if cond then 8 else 16))"
    )
    assertEquals(
      parsed.shape.render,
      "Apply(Ident(foo), [New(java.lang.StringBuilder, [If(Ident(cond), Literal(8), Literal(16))])])"
    )
  }

  test("constructor boundaries fail deterministically") {
    val failures = List(
      ConstructorNewExamples.failureMessage("new StringBuilder(16)"),
      ConstructorNewExamples.failureMessage("new java.lang.StringBuilder[Int](16)"),
      ConstructorNewExamples.failureMessage("new java.lang.StringBuilder(16)(17)"),
      ConstructorNewExamples.failureMessage("new java.lang.StringBuilder(capacity = 16)"),
      ConstructorNewExamples.failureMessage("new java.lang.StringBuilder(16) { }"),
      ConstructorNewExamples.failureMessage("new no.such.Constructor(1)"),
      ConstructorNewExamples.failureMessage("new java.lang.StringBuilder(true, false)")
    )
    assert(failures.forall(_ != "unexpected success"))
    assert(failures.forall(message => !message.contains("Phase") && !message.contains("prompt")))
  }
