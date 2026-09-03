package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSelectedApplyArgumentSelectedApplyParserOracleTest extends munit.FunSuite:
  List(
    "helper.make(20)" -> List("20" -> "Number"),
    "helper.make(x, 20)" -> List("x" -> "Ident", "20" -> "Number")
  ).foreach { case (expression, expectedArguments) =>
    test(s"locks the selected-member replacement oracle for $expression") {
      withContext {
        val prefix = "class U016Oracle:\n  def change: Any = service.invoke("
        val source = prefix + expression + ", keptArg)\n"
        val outerExpression = s"service.invoke($expression, keptArg)"
        val outerStart = source.indexOf(outerExpression)
        val replacementStart = source.indexOf(expression, outerStart)
        val target = parseTarget(source)
        val outer = target.rhs.asInstanceOf[untpd.Apply]
        val outerSelection = outer.fun.asInstanceOf[untpd.Select]
        val outerQualifier = outerSelection.qualifier.asInstanceOf[untpd.Ident]
        val replacement = outer.args.head.asInstanceOf[untpd.Apply]
        val replacementSelection = replacement.fun.asInstanceOf[untpd.Select]
        val replacementQualifier = replacementSelection.qualifier.asInstanceOf[untpd.Ident]
        val sibling = outer.args(1)

        assertEquals(outerQualifier.name.toString, "service")
        assertEquals(outerSelection.name.toString, "invoke")
        assertEquals(replacementQualifier.name.toString, "helper")
        assertEquals(replacementSelection.name.toString, "make")
        assert(replacementSelection.name.isTermName)
        assertEquals(replacement.args.map(_.getClass.getSimpleName), expectedArguments.map(_._2))
        assertEquals(sibling.asInstanceOf[untpd.Ident].name.toString, "keptArg")

        assertEquals(outer.span.start, outerStart)
        assertEquals(outer.span.point, outerSelection.span.end)
        assertEquals(outer.span.end, outerStart + outerExpression.length)
        assertEquals(outer.source.content.slice(outer.span.start, outer.span.end).mkString, outerExpression)

        assertEquals(replacement.span.start, replacementStart)
        assertEquals(replacement.span.point, replacementSelection.span.end)
        assertEquals(replacement.span.end, replacementStart + expression.length)
        assertEquals(replacement.source.content.slice(replacement.span.start, replacement.span.end).mkString, expression)

        assertEquals(replacementSelection.span.start, replacementStart)
        assertEquals(replacementSelection.span.point, replacementStart + "helper.".length)
        assertEquals(replacementSelection.span.end, replacementStart + "helper.make".length)
        assertEquals(
          replacementSelection.source.content
            .slice(replacementSelection.span.start, replacementSelection.span.end).mkString,
          "helper.make"
        )

        assertEquals(replacementQualifier.span.start, replacementStart)
        assertEquals(replacementQualifier.span.point, replacementStart)
        assertEquals(replacementQualifier.span.end, replacementStart + "helper".length)
        assertEquals(
          replacementQualifier.source.content
            .slice(replacementQualifier.span.start, replacementQualifier.span.end).mkString,
          "helper"
        )

        var argumentSearchStart = replacementStart + "helper.make(".length
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
        assert(outer.args.head.eq(replacement))
        assert(outer.args(1).eq(sibling))
        (outer +: outerSelection +: outerQualifier +: sibling +:
          replacement +: replacementSelection +: replacementQualifier +:
          replacement.args.toVector).foreach { tree =>
          assert(tree.source.exists)
          assert(tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
        }
      }
    }
  }

  private def parseTarget(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U016Oracle.scala", source)
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
