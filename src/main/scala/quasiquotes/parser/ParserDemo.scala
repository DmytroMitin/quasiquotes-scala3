package quasiquotes.parser

import quasiquotes.construct.QuasiquoteMacroExamples
import quasiquotes.construct.QuasiquoteMacroExamples.DemoCase
import quasiquotes.matching.QuasiquoteMatchExamples
import quasiquotes.matching.QuasiquoteMatchExamples.MatchDemo
import quasiquotes.matching.QuasiquoteMatchExamples.NormalizationDemo

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

  def matchReportFor(demo: MatchDemo): String =
    s"""Mode: ${demo.mode}
       |Pattern: ${demo.pattern}
       |Target: ${demo.target}
       |Result: ${if demo.success then "success" else "failure"}
       |Bindings: ${if demo.bindings.nonEmpty then demo.bindings.mkString(", ") else "(none)"}
       |Detail: ${demo.detail}""".stripMargin

  def normalizationReportFor(demo: NormalizationDemo): String =
    s"""Pattern: ${demo.pattern}
       |Target: ${demo.target}
       |
       |Before normalization:
       |${indent(matchReportFor(demo.before))}
       |
       |After normalization:
       |${indent(matchReportFor(demo.after))}""".stripMargin

  private def indent(text: String): String =
    text.linesIterator.map("  " + _).mkString("\n")

  object NamedInfixScope:
    private val foo = 2
    private val bar = 5
    val demo: DemoCase = QuasiquoteMacroExamples.namedInfixSummary

  object NamedSelectInfixScope:
    private object foo:
      val bar = 4
    val demo: DemoCase = QuasiquoteMacroExamples.namedSelectInfixSummary(3)

  object NestedNamedApplicationScope:
    private def foo(value: Int): Int = value + 10
    private def bar(value: Int): Int = value * 2
    private val baz = 3
    val demo: DemoCase = QuasiquoteMacroExamples.nestedNamedApplicationSummary

  object NestedSelectApplicationScope:
    private object foo:
      def bar(value: Int): Int = value + 4
    private def baz(value: Int): Int = value * 3
    val demo: DemoCase = QuasiquoteMacroExamples.nestedSelectApplicationSummary(2)

  object ParenthesizedNamedScope:
    private val foo = 11
    val demo: DemoCase = QuasiquoteMacroExamples.parenthesizedNamedSummary

  object ParenthesizedSelectedHoleScope:
    private object foo:
      def bar(value: Int): Int = value + 6
    val demo: DemoCase = QuasiquoteMacroExamples.parenthesizedSelectedHoleSummary(3)

  object NestedParenHoleScope:
    val demo: DemoCase = QuasiquoteMacroExamples.nestedParenHoleSummary(7)

  object MatchAnyScope:
    private def foo(value: Int): Int = value + 1
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$x", foo(1))

  object MatchFooApplicationScope:
    private def foo(value: Int): Int = value + 10
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatch("foo($x)", foo(1))

  object MatchFunctionHoleScope:
    private def bar(value: Int): Int = value + 1
    private val baz = 2
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$f($x)", bar(baz))

  object MatchSelectionApplicationScope:
    private object foo:
      def bar(value: Int): Int = value + 5
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatch("foo.bar($x)", foo.bar(3))

  object MatchInfixScope:
    private val a = 2
    private val b = 3
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $y", a + b)
    val repeatedSuccess: MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", a + a)
    val repeatedFailure: MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", a + b)

  object MatchNestedScope:
    private def f(value: Int): Int = value + 1
    private def g(value: Int): Int = value * 2
    private val h = 3
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatch("f(g($x))", f(g(h)))

  object MatchParenScope:
    private val z = 7
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("(($x))", ((z)))

  object MatchUnsupportedScope:
    val demo: MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("if $x then $y else $z", 1)

  object NormalizationParenScope:
    private val z = 7
    val demo: NormalizationDemo = QuasiquoteMatchExamples.summarizeNormalization("(($x))", ((z)))

  object NormalizationInfixScope:
    private val a = 2
    private val b = 3
    val demo: NormalizationDemo = QuasiquoteMatchExamples.summarizeNormalization("$x + $y", a + b)

  object MatchMacroProofScope:
    private val a = 2
    private val b = 3
    private def f(value: Int): Int = value + 1
    private def g(value: Int): Int = value * 2
    private val h = 3
    val infixRaw: String = QuasiquoteMatchExamples.classifyInfixRaw(a + b)
    val infix: String = QuasiquoteMatchExamples.classifyInfix(a + b)
    val nested: String = QuasiquoteMatchExamples.classifyNested(f(g(h)))

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
  println(ParserDemo.quasiquoteReportFor(ParserDemo.NamedInfixScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParserDemo.NamedSelectInfixScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(QuasiquoteMacroExamples.nestedFunctionHoleSummary(2)))
  println()
  println(ParserDemo.quasiquoteReportFor(ParserDemo.NestedNamedApplicationScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParserDemo.NestedSelectApplicationScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParserDemo.ParenthesizedNamedScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParserDemo.ParenthesizedSelectedHoleScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(ParserDemo.NestedParenHoleScope.demo))
  println()
  println(ParserDemo.quasiquoteReportFor(QuasiquoteMacroExamples.parenthesizedInfixSummary(4, 5)))
  println(s"unsupportedSyntaxMessage => ${QuasiquoteMacroExamples.unsupportedSyntaxMessage}")
  println()
  println("Matching examples")
  println()
  val matchExamples = List(
    ParserDemo.MatchAnyScope.demo,
    ParserDemo.MatchFooApplicationScope.demo,
    ParserDemo.MatchFunctionHoleScope.demo,
    ParserDemo.MatchSelectionApplicationScope.demo,
    ParserDemo.MatchInfixScope.demo,
    ParserDemo.MatchNestedScope.demo,
    ParserDemo.MatchParenScope.demo,
    ParserDemo.MatchInfixScope.repeatedSuccess,
    ParserDemo.MatchInfixScope.repeatedFailure,
    ParserDemo.MatchUnsupportedScope.demo
  )
  matchExamples.foreach { example =>
    println(ParserDemo.matchReportFor(example))
    println()
  }
  println("Normalization before/after")
  println()
  val normalizationExamples = List(
    ParserDemo.NormalizationParenScope.demo,
    ParserDemo.NormalizationInfixScope.demo
  )
  normalizationExamples.foreach { example =>
    println(ParserDemo.normalizationReportFor(example))
    println()
  }
  println(s"Macro matching proof (infix, raw) => ${ParserDemo.MatchMacroProofScope.infixRaw}")
  println(s"Macro matching proof (infix) => ${ParserDemo.MatchMacroProofScope.infix}")
  println(s"Macro matching proof (nested) => ${ParserDemo.MatchMacroProofScope.nested}")
