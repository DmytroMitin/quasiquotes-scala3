package quasiquotes.parser

object ParserDemo:
  val Examples: List[String] = List(
    "foo",
    "1",
    "\"abc\"",
    "foo.bar",
    "foo(x)",
    "foo.bar(x)",
    "__hole0",
    "foo(__hole0)",
    "foo.bar(__hole0)",
    "__hole0(__hole1)"
  )

  def reportFor(input: String): String =
    TinyTermParser.parse(input) match
      case Right(parsed) =>
        s"""Input: $input
           |Shape: ${parsed.shape.render}
           |Raw: ${parsed.rawStructure}""".stripMargin
      case Left(error) =>
        s"""Input: $input
           |Parse error: ${error.messages.mkString("; ")}""".stripMargin

@main def runParserDemo(): Unit =
  ParserDemo.Examples.foreach { example =>
    println(ParserDemo.reportFor(example))
    println()
  }
