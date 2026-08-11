package quasiquotes.parser

class ConstructorNewCoreTest extends munit.FunSuite:
  test("constructor shapes preserve identity, order, and recursive rendering") {
    val shape = TermShape.New(
      "java.lang.StringBuilder",
      List(TermShape.Literal("16"), TermShape.Identifier("capacity", false))
    )
    assertEquals(
      shape.render,
      "New(java.lang.StringBuilder, [Literal(16), Ident(capacity)])"
    )
    assertNotEquals(
      shape,
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Identifier("capacity", false), TermShape.Literal("16"))
      )
    )
  }

  test("constructor-name policy admits only bounded fully-qualified plain names") {
    assertEquals(ConstructorNamePolicy.validate("java.lang.StringBuilder"), Right("java.lang.StringBuilder"))
    List(
      "StringBuilder",
      "java.lang.StringBuilder[Int]",
      "java.lang.`StringBuilder`",
      "java.lang.Outer$Inner",
      "java..StringBuilder"
    ).foreach(name => assert(ConstructorNamePolicy.validate(name).isLeft))
  }
