package quasiquotes.construct

import scala.quoted.*

object QuasiquoteMacroExamples:
  private object demo:
    def bar(x: Int): Int = x + 1

  final case class DemoCase(
      label: String,
      input: String,
      placeholderSource: String,
      treeStructure: String,
      substitutedResult: String
  )

  inline def emitIntLiteral: Int = ${ emitIntLiteralImpl }

  inline def emitStringLiteral: String = ${ emitStringLiteralImpl }

  inline def callSelectedMethodViaHole(x: Int): Int = ${ callSelectedMethodViaHoleImpl('x) }

  inline def callFunctionHole(x: Int): Int = ${ callFunctionHoleImpl('x) }

  inline def stringLength(value: String): Int = ${ stringLengthImpl('value) }

  inline def addHoles(x: Int, y: Int): Int = ${ addHolesImpl('x, 'y) }

  inline def nestedFunctionHoles(x: Int): Int = ${ nestedFunctionHolesImpl('x) }

  inline def parenthesizedAdd(x: Int, y: Int): Int = ${ parenthesizedAddImpl('x, 'y) }

  inline def typedHole(x: Int): Int = ${ typedHoleImpl('x) }

  inline def typedHoleApplication(x: Int): Int = ${ typedHoleApplicationImpl('x) }

  inline def tupleHoles(x: Int, y: Int): (Int, Int) = ${ tupleHolesImpl('x, 'y) }

  inline def nestedTupleHoles(x: Int, y: Int, z: Int): (Int, (Int, Int)) = ${ nestedTupleHolesImpl('x, 'y, 'z) }

  inline def ifHoles(cond: Boolean, x: Int, y: Int): Int = ${ ifHolesImpl('cond, 'x, 'y) }

  inline def holeInfixSummary(x: Int, y: Int): DemoCase = ${ holeInfixSummaryImpl('x, 'y) }

  inline def nestedFunctionHoleSummary(x: Int): DemoCase = ${ nestedFunctionHoleSummaryImpl('x) }

  inline def tupleApplicationSummary(x: Int, y: Int): DemoCase = ${ tupleApplicationSummaryImpl('x, 'y) }

  inline def ifApplicationSummary(cond: Boolean, x: Int, y: Int): DemoCase = ${ ifApplicationSummaryImpl('cond, 'x, 'y) }

  inline def parenthesizedInfixSummary(x: Int, y: Int): DemoCase = ${ parenthesizedInfixSummaryImpl('x, 'y) }

  inline def namedInfixSummary: DemoCase = ${ namedInfixSummaryImpl }

  inline def namedSelectInfixSummary(x: Int): DemoCase = ${ namedSelectInfixSummaryImpl('x) }

  inline def nestedNamedApplicationSummary: DemoCase = ${ nestedNamedApplicationSummaryImpl }

  inline def nestedSelectApplicationSummary(x: Int): DemoCase = ${ nestedSelectApplicationSummaryImpl('x) }

  inline def parenthesizedNamedSummary: DemoCase = ${ parenthesizedNamedSummaryImpl }

  inline def parenthesizedSelectedHoleSummary(x: Int): DemoCase = ${ parenthesizedSelectedHoleSummaryImpl('x) }

  inline def nestedParenHoleSummary(x: Int): DemoCase = ${ nestedParenHoleSummaryImpl('x) }

  inline def unsupportedSyntaxMessage: String = ${ unsupportedSyntaxMessageImpl }

  inline def unsupportedComplexTypeAscriptionMessage: String = ${ unsupportedComplexTypeAscriptionMessageImpl }

  private def emitIntLiteralImpl(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"1".asExprOf[Int]

  private def emitStringLiteralImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr""""abc"""".asExprOf[String]

  private def callSelectedMethodViaHoleImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val demoTerm = '{ demo }.asTerm
    val xTerm = x.asTerm
    qr"$demoTerm.bar($xTerm)".asExprOf[Int]

  private def callFunctionHoleImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val functionTerm = Select.unique('{ (n: Int) => n + 1 }.asTerm, "apply")
    val xTerm = x.asTerm
    qr"$functionTerm($xTerm)".asExprOf[Int]

  private def stringLengthImpl(value: Expr[String])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val valueTerm = value.asTerm
    qr"$valueTerm.length".asExprOf[Int]

  private def addHolesImpl(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"${x.asTerm} + ${y.asTerm}".asExprOf[Int]

  private def nestedFunctionHolesImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val fTerm = Select.unique('{ (n: Int) => n + 1 }.asTerm, "apply")
    val gTerm = Select.unique('{ (n: Int) => n * 2 }.asTerm, "apply")
    qr"$fTerm($gTerm(${x.asTerm}))".asExprOf[Int]

  private def parenthesizedAddImpl(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"(${x.asTerm} + ${y.asTerm})".asExprOf[Int]

  private def typedHoleImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"${x.asTerm}: Int".asExprOf[Int]

  private def typedHoleApplicationImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val functionTerm = Select.unique('{ (n: Int) => n + 1 }.asTerm, "apply")
    qr"$functionTerm(${x.asTerm}: Int)".asExprOf[Int]

  private def tupleHolesImpl(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[(Int, Int)] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"(${x.asTerm}, ${y.asTerm})".asExprOf[(Int, Int)]

  private def nestedTupleHolesImpl(x: Expr[Int], y: Expr[Int], z: Expr[Int])(using Quotes): Expr[(Int, (Int, Int))] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"(${x.asTerm}, (${y.asTerm}, ${z.asTerm}))".asExprOf[(Int, (Int, Int))]

  private def ifHolesImpl(cond: Expr[Boolean], x: Expr[Int], y: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"if ${cond.asTerm} then ${x.asTerm} else ${y.asTerm}".asExprOf[Int]

  private def holeInfixSummaryImpl(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val xTerm = x.asTerm
    val yTerm = y.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("", " + ", ""), Seq(xTerm, yTerm)).toOption.get.source
    val term = qr"$xTerm + $yTerm"
    '{ DemoCase("""qr"$x + $y"""", "$x + $y", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def nestedFunctionHoleSummaryImpl(x: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val fTerm = Select.unique('{ (n: Int) => n + 1 }.asTerm, "apply")
    val gTerm = Select.unique('{ (n: Int) => n * 2 }.asTerm, "apply")
    val xTerm = x.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("", "(", "(", "))"), Seq(fTerm, gTerm, xTerm)).toOption.get.source
    val term = qr"$fTerm($gTerm($xTerm))"
    '{ DemoCase("""qr"$f($g($x))"""", "$f($g($x))", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def tupleApplicationSummaryImpl(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val xTerm = x.asTerm
    val yTerm = y.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("foo((", ", ", "))"), Seq(xTerm, yTerm)).toOption.get.source
    val term = qr"foo(($xTerm, $yTerm))"
    '{ DemoCase("""qr"foo(($x, $y))"""", "foo(($x, $y))", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def ifApplicationSummaryImpl(cond: Expr[Boolean], x: Expr[Int], y: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val condTerm = cond.asTerm
    val xTerm = x.asTerm
    val yTerm = y.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("foo(if ", " then ", " else ", ")"), Seq(condTerm, xTerm, yTerm)).toOption.get.source
    val term = qr"foo(if $condTerm then $xTerm else $yTerm)"
    '{ DemoCase("""qr"foo(if $cond then $x else $y)"""", "foo(if $cond then $x else $y)", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def parenthesizedInfixSummaryImpl(x: Expr[Int], y: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val xTerm = x.asTerm
    val yTerm = y.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("(", " + ", ")"), Seq(xTerm, yTerm)).toOption.get.source
    val term = qr"($xTerm + $yTerm)"
    '{ DemoCase("""qr"($x + $y)"""", "($x + $y)", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def namedInfixSummaryImpl(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val term = qr"foo + bar"
    '{ DemoCase("""qr"foo + bar"""", "foo + bar", "foo + bar", ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def namedSelectInfixSummaryImpl(x: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val xTerm = x.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("foo.bar + ", ""), Seq(xTerm)).toOption.get.source
    val term = qr"foo.bar + $xTerm"
    '{ DemoCase("""qr"foo.bar + $x"""", "foo.bar + $x", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def nestedNamedApplicationSummaryImpl(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val term = qr"foo(bar(baz))"
    '{ DemoCase("""qr"foo(bar(baz))"""", "foo(bar(baz))", "foo(bar(baz))", ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def nestedSelectApplicationSummaryImpl(x: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val xTerm = x.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("foo.bar(baz(", "))"), Seq(xTerm)).toOption.get.source
    val term = qr"foo.bar(baz($xTerm))"
    '{ DemoCase("""qr"foo.bar(baz($x))"""", "foo.bar(baz($x))", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def parenthesizedNamedSummaryImpl(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val term = qr"(foo)"
    '{ DemoCase("""qr"(foo)"""", "(foo)", "(foo)", ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def parenthesizedSelectedHoleSummaryImpl(x: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val xTerm = x.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("(foo.bar(", "))"), Seq(xTerm)).toOption.get.source
    val term = qr"(foo.bar($xTerm))"
    '{ DemoCase("""qr"(foo.bar($x))"""", "(foo.bar($x))", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def nestedParenHoleSummaryImpl(x: Expr[Int])(using Quotes): Expr[DemoCase] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val fTerm = Select.unique('{ (n: Int) => n + 1 }.asTerm, "apply")
    val xTerm = x.asTerm
    val placeholderSource = PlaceholderSource.synthesize(Seq("", "((", "))"), Seq(fTerm, xTerm)).toOption.get.source
    val term = qr"$fTerm(($xTerm))"
    '{ DemoCase("""qr"$f(($x))"""", "$f(($x))", ${ Expr(placeholderSource) }, ${ Expr(term.show(using Printer.TreeStructure)) }, ${ term.asExprOf[Int] }.toString) }

  private def unsupportedSyntaxMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiquoteBuilder.build(Seq("1 + 2"), Nil) match
      case Left(error) => Expr(error.message)
      case Right(term) => Expr(term.show)

  private def unsupportedComplexTypeAscriptionMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiquoteBuilder.build(Seq("", ": List[Int]"), Seq('{ 1 }.asTerm)) match
      case Left(error) => Expr(error.message)
      case Right(term) => Expr(term.show(using Printer.TreeStructure))
