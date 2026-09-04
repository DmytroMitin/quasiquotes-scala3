package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingParserOracleTest
    extends munit.FunSuite:
  List(
    ("catalog.product(freshValue)", List("Ident")),
    ("catalog.product(4)", List("Number")),
    ("catalog.product(true)", List("Literal")),
    ("catalog.product(freshValue, 4, true)", List("Ident", "Number", "Literal"))
  ).foreach { case (siblingSource, expectedKinds) =>
    test(s"locks helper(oldArg, $siblingSource) parser topology") {
      withContext {
        val expression = s"helper(oldArg, $siblingSource)"
        val source = s"class U021Oracle:\n  def change: Any = service.invoke($expression, keptArg)\n"
        val target = parseTarget(source)
        val wrapper = target.rhs.asInstanceOf[untpd.Apply].args.head.asInstanceOf[untpd.Apply]
        val sibling = wrapper.args(1).asInstanceOf[untpd.Apply]
        val selection = sibling.fun.asInstanceOf[untpd.Select]
        val qualifier = selection.qualifier.asInstanceOf[untpd.Ident]
        assertEquals(wrapper.fun.asInstanceOf[untpd.Ident].name.toString, "helper")
        assertEquals(wrapper.args.size, 2)
        assertEquals(selection.name.toString, "product")
        assertEquals(qualifier.name.toString, "catalog")
        assertEquals(sibling.args.map(_.getClass.getSimpleName), expectedKinds)
        Vector[untpd.Tree](wrapper, wrapper.fun, wrapper.args.head, sibling, selection,
          qualifier).++(sibling.args).foreach { tree =>
          assert(tree.source.exists)
          assert(tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
        }
      }
    }
  }

  private def parseTarget(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U021Oracle.scala", source)
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
