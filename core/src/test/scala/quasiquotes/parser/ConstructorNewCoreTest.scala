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

  test("constructor source-path policy admits one or more plain source segments") {
    List(
      "A",
      "example.A",
      "foo.bar.Baz",
      "_",
      "_A",
      "a1.B2"
    ).foreach(path => assertEquals(ConstructorSourcePathPolicy.validate(path), Right(path)))

    List[String](
      null,
      "",
      ".A",
      "A.",
      "a..A",
      "`A`",
      "a.`A`",
      "A[B]",
      "a.A[B]",
      "a b.A",
      "A$",
      "a.Outer$Inner"
    ).foreach(path => assert(ConstructorSourcePathPolicy.validate(path).isLeft, clues(path)))
  }

  test("legacy constructor-name policy retains its qualified-only admission and diagnostics") {
    assertEquals(
      ConstructorNamePolicy.validate("A"),
      Left("constructor names must be fully qualified with at least two plain identifier segments")
    )
    assertEquals(ConstructorNamePolicy.validate("example.A"), Right("example.A"))
    assertEquals(
      ConstructorNamePolicy.validate("example.`A`"),
      Left("constructor names must use plain identifier segments without backticks, type arguments, or binary-name spelling")
    )
  }
