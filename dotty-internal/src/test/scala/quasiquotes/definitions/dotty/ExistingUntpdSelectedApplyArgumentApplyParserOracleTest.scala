package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSelectedApplyArgumentApplyParserOracleTest extends munit.FunSuite:
  List(
    "helper(20)" -> List("20" -> "Number"),
    "helper(x, 20)" -> List("x" -> "Ident", "20" -> "Number")
  ).foreach { case (expression, expectedArguments) =>
    test(s"locks the direct-call replacement oracle for $expression") {
      withContext {
        val prefix = "class U015Oracle:\n  def change: Any = service.invoke("
        val source = prefix + expression + ", keptArg)\n"
        val outerExpression = s"service.invoke($expression, keptArg)"
        val outerStart = source.indexOf(outerExpression)
        val replacementStart = source.indexOf(expression, outerStart)
        val target = parseTarget(source)
        val outer = target.rhs.asInstanceOf[untpd.Apply]
        val selection = outer.fun.asInstanceOf[untpd.Select]
        val qualifier = selection.qualifier.asInstanceOf[untpd.Ident]
        val replacement = outer.args.head.asInstanceOf[untpd.Apply]
        val replacementFunction = replacement.fun.asInstanceOf[untpd.Ident]
        val sibling = outer.args(1)
        assertEquals(qualifier.name.toString, "service")
        assertEquals(selection.name.toString, "invoke")
        assert(selection.name.isTermName)
        assertEquals(outer.args.size, 2)
        assertEquals(sibling.asInstanceOf[untpd.Ident].name.toString, "keptArg")
        assertEquals(replacementFunction.name.toString, "helper")
        assertEquals(replacement.args.map(_.getClass.getSimpleName), expectedArguments.map(_._2))

        assertEquals(outer.span.start, outerStart)
        assertEquals(outer.span.point, selection.span.end)
        assertEquals(outer.span.end, outerStart + outerExpression.length)
        assertEquals(outer.source.content.slice(outer.span.start, outer.span.end).mkString, outerExpression)

        assertEquals(selection.span.start, outerStart)
        assertEquals(selection.span.point, outerStart + "service.".length)
        assertEquals(selection.span.end, outerStart + "service.invoke".length)
        assertEquals(selection.source.content.slice(selection.span.start, selection.span.end).mkString, "service.invoke")

        assertEquals(qualifier.span.start, outerStart)
        assertEquals(qualifier.span.point, outerStart)
        assertEquals(qualifier.span.end, outerStart + "service".length)
        assertEquals(qualifier.source.content.slice(qualifier.span.start, qualifier.span.end).mkString, "service")

        assertEquals(replacement.span.start, replacementStart)
        assertEquals(
          replacement.source.content.slice(replacement.span.start, replacement.span.end).mkString,
          expression
        )
        assertEquals(replacement.span.end, replacementStart + expression.length)
        assertEquals(replacement.span.point, replacement.fun.span.end)

        assertEquals(replacementFunction.span.start, replacementStart)
        assertEquals(replacementFunction.span.point, replacementStart)
        assertEquals(replacementFunction.span.end, replacementStart + "helper".length)
        assertEquals(
          replacementFunction.source.content.slice(replacementFunction.span.start, replacementFunction.span.end).mkString,
          "helper"
        )

        var argumentSearchStart = replacementStart + "helper(".length
        expectedArguments.zip(replacement.args).foreach { case ((expectedSource, _), argument) =>
          val argumentStart = source.indexOf(expectedSource, argumentSearchStart)
          assertEquals(argument.span.start, argumentStart)
          assertEquals(argument.span.point, argumentStart)
          assertEquals(argument.span.end, argumentStart + expectedSource.length)
          assertEquals(argument.source.content.slice(argument.span.start, argument.span.end).mkString, expectedSource)
          argumentSearchStart = argument.span.end
        }

        val siblingStart = source.indexOf("keptArg", replacement.span.end)
        assertEquals(sibling.span.start, siblingStart)
        assertEquals(sibling.span.point, siblingStart)
        assertEquals(sibling.span.end, siblingStart + "keptArg".length)
        assertEquals(sibling.source.content.slice(sibling.span.start, sibling.span.end).mkString, "keptArg")
        assert(outer.args.head eq replacement)
        assert(outer.args(1) eq sibling)
        (outer +: selection +: selection.qualifier +: sibling +:
          replacement +: replacement.fun +: replacement.args.toVector).foreach { tree =>
          assert(tree.source.exists)
          assert(tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
        }
      }
    }
  }

  private def parseTarget(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U015Oracle.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    val root = parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]
    root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
      case value: untpd.DefDef if value.name.toString == "change" => value
    }.getOrElse(fail("missing change method"))

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
