package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSelectedApplyArgumentWrapApplySiblingParserOracleTest
    extends munit.FunSuite:
  List(
    ("product(freshValue)", List("Ident")),
    ("product(20)", List("Number")),
    ("product(true)", List("Literal")),
    ("product(freshValue, 4, true)", List("Ident", "Number", "Literal"))
  ).foreach { case (siblingSource, argumentKinds) =>
    test(s"locks helper(oldArg, $siblingSource) parser topology") {
      withContext {
        val expression = s"helper(oldArg, $siblingSource)"
        val source =
          s"class U020Oracle:\n  def change: Any = service.invoke($expression, keptArg)\n"
        val target = parseTarget(source)
        val outer = target.rhs.asInstanceOf[untpd.Apply]
        val wrapper = outer.args.head.asInstanceOf[untpd.Apply]
        val function = wrapper.fun.asInstanceOf[untpd.Ident]
        val original = wrapper.args(0)
        val sibling = wrapper.args(1).asInstanceOf[untpd.Apply]
        val siblingFunction = sibling.fun.asInstanceOf[untpd.Ident]
        val siblingArguments = sibling.args
        val wrapperStart = source.indexOf(expression)
        val originalStart = source.indexOf("oldArg", wrapperStart)
        val siblingStart = source.indexOf(siblingSource, originalStart + "oldArg".length)

        assertEquals(function.name.toString, "helper")
        assertEquals(wrapper.args.size, 2)
        assertEquals(original.getClass.getSimpleName, "Ident")
        assertEquals(sibling.getClass.getSimpleName, "Apply")
        assertEquals(siblingFunction.name.toString, "product")
        assertEquals(siblingArguments.map(_.getClass.getSimpleName), argumentKinds)
        assertEquals(wrapper.span.start, wrapperStart)
        assertEquals(wrapper.span.point, function.span.end)
        assertEquals(wrapper.span.end, wrapperStart + expression.length)
        assertEquals(original.span.start, originalStart)
        assertEquals(sibling.span.start, siblingStart)
        assertEquals(sibling.span.end, siblingStart + siblingSource.length)
        assert(wrapper.args(0).eq(original))
        assert(wrapper.args(1).eq(sibling))
        Vector[untpd.Tree](outer, wrapper, function, original, sibling,
          siblingFunction).++(siblingArguments).foreach { tree =>
          assert(tree.source.exists)
          assert(tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
        }
      }
    }
  }

  private def parseTarget(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U020Oracle.scala", source)
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
