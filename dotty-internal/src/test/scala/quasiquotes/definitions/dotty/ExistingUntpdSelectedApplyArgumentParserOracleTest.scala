package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSelectedApplyArgumentParserOracleTest extends munit.FunSuite:
  test("parser exposes a selected Apply with two distinct direct argument objects") {
    characterize(
      "service.invoke(oldArg, keptArg)",
      List("oldArg" -> "Ident", "keptArg" -> "Ident")
    )
  }

  test("parser exposes three selected Apply arguments in exact source order") {
    characterize(
      "service.invoke(oldArg, 2, true)",
      List("oldArg" -> "Ident", "2" -> "Number", "true" -> "Literal")
    )
  }

  private def characterize(
      expression: String,
      expectedArguments: List[(String, String)]
  ): Unit =
    withContext {
      characterizeInContext(expression, expectedArguments)
    }

  private def characterizeInContext(
      expression: String,
      expectedArguments: List[(String, String)]
  )(using outerContext: Context): Unit =
      val source = s"class U014Oracle:\n  def change: Any = $expression\n"
      val reporter = new StoreReporter(null)
      val unit = CompilationUnit("U014Oracle.scala", source)
      given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
      val parsed = new Parsers.Parser(unit.source).parse()
      assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
      val target = allTrees(parsed).collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val apply = target.rhs.asInstanceOf[untpd.Apply]
      val selection = apply.fun.asInstanceOf[untpd.Select]
      val qualifier = selection.qualifier.asInstanceOf[untpd.Ident]
      val expressionStart = source.indexOf(expression)

      assertEquals(qualifier.name.toString, "service")
      assertEquals(selection.name.toString, "invoke")
      assert(selection.name.isTermName)
      assertEquals(apply.args.map(_.getClass.getSimpleName), expectedArguments.map(_._2))
      assertEquals(apply.span.start, expressionStart)
      assertEquals(apply.span.end, expressionStart + expression.length)
      assertEquals(selection.span.start, expressionStart)
      assertEquals(selection.span.point, expressionStart + "service.".length)
      assertEquals(qualifier.span.start, expressionStart)
      assertEquals(qualifier.span.point, expressionStart)
      assert(apply.args.combinations(2).forall {
        case List(left, right) => !left.eq(right)
        case _ => true
      })
      (Vector[untpd.Tree](apply, selection, qualifier) ++ apply.args).foreach { tree =>
        assert(tree.source.exists, clues(tree))
        assert(tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol)
      }
      var cursor = expressionStart + "service.invoke(".length
      expectedArguments.zip(apply.args).foreach { case ((slice, _), argument) =>
        val start = source.indexOf(slice, cursor)
        assert(start >= cursor, clues(slice, cursor))
        assertEquals(argument.span.start, start)
        assertEquals(argument.span.point, start)
        assertEquals(argument.span.end, start + slice.length)
        assertEquals(source.substring(argument.span.start, argument.span.end), slice)
        cursor = argument.span.end
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
