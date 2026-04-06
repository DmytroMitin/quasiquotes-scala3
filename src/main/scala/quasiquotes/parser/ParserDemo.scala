package quasiquotes.parser

import quasiquotes.construct.QuasiquoteMacroExamples
import quasiquotes.construct.QuasiquoteMacroExamples.DemoCase

object ParserDemo:
  val AcceptedExamples: List[String] = List(
    "foo",
    "1",
    "\"abc\"",
    "foo.bar",
    "foo(x)",
    "foo.bar(x)",
    "foo + bar",
    "foo.bar + __hole0",
    "f(g(__hole0))",
    "foo(bar(baz))",
    "foo.bar(baz(__hole0))",
    "(foo)",
    "(foo.bar(__hole0))",
    "f((__hole0))",
    "(__hole0 + __hole1)",
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

  def quasiquoteReportFor(demo: DemoCase): String =
    s"""Input: ${demo.input}
       |Placeholder source: ${demo.placeholderSource}
       |Result: success
       |Tree: ${demo.treeStructure}
       |Evaluated: ${demo.substitutedResult}""".stripMargin

private object NamedInfixScope:
  private val foo = 2
  private val bar = 5
  val demo: DemoCase = QuasiquoteMacroExamples.namedInfixSummary

private object NamedSelectInfixScope:
  private object foo:
    val bar = 4
  val demo: DemoCase = QuasiquoteMacroExamples.namedSelectInfixSummary(3)

private object NestedNamedApplicationScope:
  private def foo(value: Int): Int = value + 10
  private def bar(value: Int): Int = value * 2
  private val baz = 3
  val demo: DemoCase = QuasiquoteMacroExamples.nestedNamedApplicationSummary

private object NestedSelectApplicationScope:
  private object foo:
    def bar(value: Int): Int = value + 4
  private def baz(value: Int): Int = value * 3
  val demo: DemoCase = QuasiquoteMacroExamples.nestedSelectApplicationSummary(2)

private object ParenthesizedNamedScope:
  private val foo = 11
  val demo: DemoCase = QuasiquoteMacroExamples.parenthesizedNamedSummary

private object ParenthesizedSelectedHoleScope:
  private object foo:
    def bar(value: Int): Int = value + 6
  val demo: DemoCase = QuasiquoteMacroExamples.parenthesizedSelectedHoleSummary(3)

private object NestedParenHoleScope:
  val demo: DemoCase = QuasiquoteMacroExamples.nestedParenHoleSummary(7)

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

  println("Macro usability examples")
  println()
  println(s"emitIntLiteral => ${QuasiquoteMacroExamples.emitIntLiteral}")
  println(s"""emitStringLiteral => "${QuasiquoteMacroExamples.emitStringLiteral}"""")
  println(s"callSelectedMethodViaHole(2) => ${QuasiquoteMacroExamples.callSelectedMethodViaHole(2)}")
  println(s"callFunctionHole(2) => ${QuasiquoteMacroExamples.callFunctionHole(2)}")
  println(s"""stringLength("abcd") => ${QuasiquoteMacroExamples.stringLength("abcd")}""")
  println()
  println(ParserDemo.quasiquoteReportFor(QuasiquoteMacroExamples.holeInfixSummary(2, 3)))
  println()
  println(ParserDemo.quasiquoteReportFor(NamedInfixScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(NamedSelectInfixScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(QuasiquoteMacroExamples.nestedFunctionHoleSummary(2)))
  println()
  println(ParserDemo.quasiquoteReportFor(NestedNamedApplicationScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(NestedSelectApplicationScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParenthesizedNamedScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParenthesizedSelectedHoleScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(NestedParenHoleScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(QuasiquoteMacroExamples.parenthesizedInfixSummary(4, 5)))
  println(s"unsupportedSyntaxMessage => ${QuasiquoteMacroExamples.unsupportedSyntaxMessage}")
