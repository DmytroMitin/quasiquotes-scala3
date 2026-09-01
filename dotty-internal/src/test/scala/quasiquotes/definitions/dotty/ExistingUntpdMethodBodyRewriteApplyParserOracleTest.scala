package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdMethodBodyRewriteApplyParserOracleTest extends munit.FunSuite:
  test("parser exposes direct Ident Apply with exact one-argument spans") {
    characterize("f(20)", List("Number"))
  }

  test("parser exposes direct Ident Apply with exact multi-leaf spans") {
    characterize("f(true, \"value\", x)", List("Literal", "Literal", "Ident"))
  }

  private def characterize(expression: String, expectedArguments: List[String]): Unit =
    withContext {
      characterizeInContext(expression, expectedArguments)(using summon[Context])
    }

  private def characterizeInContext(
      expression: String,
      expectedArguments: List[String]
  )(using outerContext: Context): Unit =
    val source = s"class U005ParserOracle:\n  def change: Int = $expression\n"
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U005ParserOracle.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    val target = collectTrees(parsed).collectFirst {
      case value: untpd.DefDef if value.name.toString == "change" => value
    }.getOrElse(fail("missing change method"))
    val apply = target.rhs.asInstanceOf[untpd.Apply]
    val function = apply.fun.asInstanceOf[untpd.Ident]

    assertEquals(function.name.toString, "f")
    assertEquals(apply.args.map(_.getClass.getSimpleName), expectedArguments)
    assertEquals(apply.span.start, source.indexOf(expression))
    assertEquals(apply.span.end, source.indexOf(expression) + expression.length)
    assertEquals(function.span.start, source.indexOf(expression))
    assertEquals(function.span.end, source.indexOf(expression) + 1)
    assert(apply.source.exists)
    assert(function.source.exists)
    apply.args.foreach { argument =>
      assert(argument.source.exists)
      assert(argument.span.exists)
      assert(argument.span.start >= apply.span.start)
      assert(argument.span.end <= apply.span.end)
    }

  private def collectTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
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
