package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSelectedApplyArgumentWrapParserOracleTest extends munit.FunSuite:
  List("oldArg" -> "Ident", "20" -> "Number").foreach {
    case (wrappedSource, wrappedKind) =>
      test(s"locks the unary direct-Ident wrapper oracle for helper($wrappedSource)") {
        withContext {
          val expression = s"helper($wrappedSource)"
          val prefix = "class U018Oracle:\n  def change: Any = service.invoke("
          val source = prefix + expression + ", keptArg)\n"
          val outerExpression = s"service.invoke($expression, keptArg)"
          val outerStart = source.indexOf(outerExpression)
          val wrapperStart = source.indexOf(expression, outerStart)
          val childStart = source.indexOf(wrappedSource, wrapperStart + "helper(".length)
          val target = parseTarget(source)
          val outer = target.rhs.asInstanceOf[untpd.Apply]
          val outerSelection = outer.fun.asInstanceOf[untpd.Select]
          val wrapper = outer.args.head.asInstanceOf[untpd.Apply]
          val wrapperFunction = wrapper.fun.asInstanceOf[untpd.Ident]
          val wrapped = wrapper.args.head
          val sibling = outer.args(1)

          assertEquals(outerSelection.qualifier.asInstanceOf[untpd.Ident].name.toString, "service")
          assertEquals(outerSelection.name.toString, "invoke")
          assert(outerSelection.name.isTermName)
          assertEquals(outer.args.size, 2)
          assertEquals(wrapperFunction.name.toString, "helper")
          assertEquals(wrapper.args.size, 1)
          assertEquals(wrapped.getClass.getSimpleName, wrappedKind)
          assertEquals(sibling.asInstanceOf[untpd.Ident].name.toString, "keptArg")

          assertEquals(outer.span.start, outerStart)
          assertEquals(outer.source.content.slice(outer.span.start, outer.span.end).mkString,
            outerExpression)
          assertEquals(wrapper.span.start, wrapperStart)
          assertEquals(wrapper.span.point, wrapper.fun.span.end)
          assertEquals(wrapper.span.end, wrapperStart + expression.length)
          assertEquals(wrapper.source.content.slice(wrapper.span.start, wrapper.span.end).mkString,
            expression)
          assertEquals(wrapperFunction.span.start, wrapperStart)
          assertEquals(wrapperFunction.span.end, wrapperStart + "helper".length)
          assertEquals(wrapped.span.start, childStart)
          assertEquals(wrapped.span.point, childStart)
          assertEquals(wrapped.span.end, childStart + wrappedSource.length)
          assertEquals(wrapped.source.content.slice(wrapped.span.start, wrapped.span.end).mkString,
            wrappedSource)
          assert(wrapper.args.head.eq(wrapped))
          assert(outer.args.head.eq(wrapper))

          Vector[untpd.Tree](
            outer,
            outerSelection,
            outerSelection.qualifier,
            wrapper,
            wrapperFunction,
            wrapped,
            sibling
          ).foreach { tree =>
            assert(tree.source.exists)
            assert(tree.span.exists)
            assertEquals(tree.symbol, NoSymbol)
          }
        }
      }
  }

  private def parseTarget(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U018Oracle.scala", source)
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
