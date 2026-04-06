package quasiquotes.parser

object ParserDemo:
  val AcceptedExamples: List[String] = List(
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

  val RejectedExamples: List[String] = List(
    "foo bar",
    "foo(x) y",
    "foo; bar",
    "foo)",
    "foo(__hole0) junk",
    "__hole0 __hole1",
    ""
  )

  def reportFor(input: String): String =
    TinyTermParser.parse(input) match
      case Right(parsed) =>
        s"""Input: $input
           |Result: success
           |Shape: ${parsed.shape.render}
           |Raw: ${parsed.rawStructure}""".stripMargin
      case Left(error) =>
        s"""Input: $input
           |Result: failure (${error.kind})
           |Error: ${error.summary}""".stripMargin

@main def runParserDemo(): Unit =
  println("Accepted examples")
  println()
  ParserDemo.AcceptedExamples.foreach { example =>
    println(ParserDemo.reportFor(example))
    println()
  }

  println("Rejected examples")
  println()
  ParserDemo.RejectedExamples.foreach { example =>
    println(ParserDemo.reportFor(example))
    println()
  }
