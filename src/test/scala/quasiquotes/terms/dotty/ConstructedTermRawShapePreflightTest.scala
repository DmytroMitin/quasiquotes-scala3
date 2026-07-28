package quasiquotes.terms.dotty

import quasiquotes.parser.{TinyTermParser, TinyTypeParser}

class ConstructedTermRawShapePreflightTest extends munit.FunSuite:
  private val termSnapshots = Vector(
    "value" -> "Ident(value)",
    "1" -> "Number(1,Whole(10))",
    "-1" -> "Number(-1,Whole(10))",
    "\"text\"" -> "Literal(\"text\")",
    "true" -> "Literal(true)",
    "service.answer" -> "Select(Ident(service), answer)",
    "f(left, right)" -> "Apply(Ident(f), [Ident(left), Ident(right)])",
    "left + right" -> "InfixOp(Ident(left),Ident(+),Ident(right))",
    "!condition" -> "PrefixOp(!,Ident(condition))",
    "value: Int" -> "Typed(Ident(value),Ident(Int))",
    "(left, right)" -> "Tuple([Ident(left), Ident(right)])",
    "if condition then left else right" ->
      "If(Ident(condition),Ident(left),Ident(right))",
    "(value)" -> "Parens(Ident(value))"
  )

  private val typeSnapshots = Vector(
    "Int" -> "Ident(Int)",
    "List[Int]" -> "AppliedTypeTree(Ident(List), [Ident(Int)])",
    "Option[String]" -> "AppliedTypeTree(Ident(Option), [Ident(String)])",
    "(Int, String)" -> "Tuple([Ident(Int), Ident(String)])",
    "(Int, String, Boolean)" ->
      "Tuple([Ident(Int), Ident(String), Ident(Boolean)])",
    "Int => String" -> "Function([Ident(Int)], Ident(String))",
    "(Int, String) => Boolean" ->
      "Function([Ident(Int), Ident(String)], Ident(Boolean))"
  )

  termSnapshots.foreach { case (source, expected) =>
    test(s"parser term raw shape: $source") {
      assertEquals(TinyTermParser.parseOrThrow(source).rawStructure, expected)
    }
  }

  typeSnapshots.foreach { case (source, expected) =>
    test(s"parser type raw shape: $source") {
      assertEquals(TinyTypeParser.parseOrThrow(source).rawStructure, expected)
    }
  }
