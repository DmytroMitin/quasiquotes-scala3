package quasiquotes.parser

class TinyTypeParserTest extends munit.FunSuite:
  private val acceptedShapes = List(
    "Int" -> "TypeIdent(Int)",
    "String" -> "TypeIdent(String)",
    "Boolean" -> "TypeIdent(Boolean)",
    "List[Int]" -> "TypeApply(TypeIdent(List), [TypeIdent(Int)])",
    "Option[String]" -> "TypeApply(TypeIdent(Option), [TypeIdent(String)])",
    "scala.Int" -> "TypeSelect(TypeIdent(scala), Int)",
    "A.B" -> "TypeSelect(TypeIdent(A), B)",
    "(Int, String)" -> "TypeTuple([TypeIdent(Int), TypeIdent(String)])",
    "(Int, String, Boolean)" -> "TypeTuple([TypeIdent(Int), TypeIdent(String), TypeIdent(Boolean)])",
    "Int => String" -> "TypeFunction([TypeIdent(Int)], TypeIdent(String))",
    "(Int, String) => Boolean" -> "TypeFunction([TypeIdent(Int), TypeIdent(String)], TypeIdent(Boolean))"
  )

  acceptedShapes.foreach { (input, expected) =>
    test(s"parse type shape for $input") {
      val parsed = TinyTypeParser.parseOrThrow(input)
      assertEquals(parsed.shape.render, expected)
    }
  }

  test("raw type parse tree remains available for future lowering") {
    val parsed = TinyTypeParser.parseOrThrow("List[Int]")
    assertEquals(parsed.rawTree.getClass.getSimpleName.nonEmpty, true)
    assert(clue(parsed.rawStructure).contains("AppliedTypeTree"))
    assert(clue(parsed.rawStructure).contains("Int"))
  }

  test("marks wildcard type arguments as unsupported instead of accepting them silently") {
    val parsed = TinyTypeParser.parseOrThrow("List[?]")
    assert(clue(parsed.shape.render).startsWith("TypeApply(TypeIdent(List), [TypeUnsupported("))
  }

  test("preserves Tuple3 and Function2 as distinct ordered raw shapes") {
    val tuple = TinyTypeParser.parseOrThrow("(Int, String, Boolean)").shape
    val function = TinyTypeParser.parseOrThrow("(Int, String) => Boolean").shape

    assertEquals(
      tuple,
      TypeShape.Tuple(List(
        TypeShape.Identifier("Int"),
        TypeShape.Identifier("String"),
        TypeShape.Identifier("Boolean")
      ))
    )
    assertEquals(
      function,
      TypeShape.Function(
        List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")),
        TypeShape.Identifier("Boolean")
      )
    )
  }

  test("parser observation remains broader than project semantic arity support") {
    assertEquals(
      TinyTypeParser.parseOrThrow("(Int, String, Boolean, Int)").shape,
      TypeShape.Tuple(List(
        TypeShape.Identifier("Int"),
        TypeShape.Identifier("String"),
        TypeShape.Identifier("Boolean"),
        TypeShape.Identifier("Int")
      ))
    )
    assertEquals(
      TinyTypeParser.parseOrThrow("(Int, String, Boolean) => Int").shape,
      TypeShape.Function(
        List(
          TypeShape.Identifier("Int"),
          TypeShape.Identifier("String"),
          TypeShape.Identifier("Boolean")
        ),
        TypeShape.Identifier("Int")
      )
    )
  }

  test("double parentheses preserve a unary function whose argument is Tuple2") {
    assertEquals(
      TinyTypeParser.parseOrThrow("((Int, String)) => Boolean").shape,
      TypeShape.Function(
        List(TypeShape.Tuple(List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")))),
        TypeShape.Identifier("Boolean")
      )
    )
  }

  test("rejects trailing input after a parsed type fragment") {
    TinyTypeParser.parse("Int)") match
      case Left(error) =>
        assertEquals(error.kind, ParseErrorKind.TrailingInput)
        assert(clue(error.summary).nonEmpty)
      case Right(parsed) =>
        fail(s"expected rejection, got ${parsed.shape.render}")
  }
