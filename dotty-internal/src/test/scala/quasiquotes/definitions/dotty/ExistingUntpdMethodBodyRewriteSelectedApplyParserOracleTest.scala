package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdMethodBodyRewriteSelectedApplyParserOracleTest
    extends munit.FunSuite:
  test("parser exposes one selected-member argument") {
    characterize("service.invoke(20)", List("20" -> "Number"))
  }

  test("parser exposes two selected-member arguments in order") {
    characterize(
      "service.invoke(x, 20)",
      List("x" -> "Ident", "20" -> "Number")
    )
  }

  test("parser exposes three mixed selected-member arguments in order") {
    characterize(
      "service.invoke(true, \"value\", x)",
      List(
        "true" -> "Literal",
        "\"value\"" -> "Literal",
        "x" -> "Ident"
      )
    )
  }

  private def characterize(
      expression: String,
      expectedArguments: List[(String, String)]
  ): Unit =
    withContext {
      characterizeInContext(expression, expectedArguments)(using summon[Context])
    }

  private def characterizeInContext(
      expression: String,
      expectedArguments: List[(String, String)]
  )(using outerContext: Context): Unit =
    val source = s"class U013ParserOracle:\n  def change: Any = $expression\n"
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U013ParserOracle.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    val target = allTrees(parsed).collectFirst {
      case value: untpd.DefDef if value.name.toString == "change" => value
    }.getOrElse(fail("missing change method"))
    val expressionStart = source.indexOf(expression)
    val apply = target.rhs.asInstanceOf[untpd.Apply]
    val selection = apply.fun.asInstanceOf[untpd.Select]
    val qualifier = selection.qualifier.asInstanceOf[untpd.Ident]

    assertEquals(qualifier.name.toString, "service")
    assertEquals(selection.name.toString, "invoke")
    assert(selection.name.isTermName)
    assertEquals(
      apply.args.map(_.getClass.getSimpleName),
      expectedArguments.map(_._2)
    )
    assertEquals(apply.span.start, expressionStart)
    assertEquals(apply.span.end, expressionStart + expression.length)
    assertEquals(apply.span.point, expressionStart + "service.invoke".length)
    assertEquals(selection.span.start, expressionStart)
    assertEquals(selection.span.end, expressionStart + "service.invoke".length)
    assertEquals(qualifier.span.start, expressionStart)
    assertEquals(qualifier.span.end, expressionStart + "service".length)
    assertEquals(selection.span.point, expressionStart + "service.".length)
    assertEquals(qualifier.span.point, expressionStart)

    (Vector[untpd.Tree](apply, selection, qualifier) ++ apply.args).foreach { tree =>
      assert(tree.source.exists, clues(tree))
      assert(tree.span.exists, clues(tree))
      assertEquals(tree.symbol, NoSymbol)
    }
    var argumentCursor = expressionStart + "service.invoke(".length
    expectedArguments.zip(apply.args).foreach { case ((slice, _), argument) =>
      val argumentStart = source.indexOf(slice, argumentCursor)
      assert(argumentStart >= argumentCursor, clues(slice, argumentCursor))
      assertEquals(argument.span.start, argumentStart)
      assertEquals(argument.span.point, argumentStart)
      assertEquals(argument.span.end, argumentStart + slice.length)
      assertEquals(source.substring(argument.span.start, argument.span.end), slice)
      argumentCursor = argument.span.end
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
