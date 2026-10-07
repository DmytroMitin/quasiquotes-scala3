package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingContextualMethodCaptureRewriteModelFitTest extends munit.FunSuite:
  test("contextual parameter parser representation") {
    withContext {
      val method = parseClass(
        "class ContextualOnly:\n  def value(using x: Int): Int = x\n"
      ).rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
      val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]

      assertEquals(parameter.mods.flags, Flags.Param | Flags.Given)
    }
  }

  test("C1 and C2 retain exact contextual flags, clause order, origins, and pre-Typer cleanliness") {
    withContext {
      val fixtures = Vector(
        (
          "class ContextualOnly:\n  def value(using x: Int): Int = x\n",
          Vector(Vector("x")),
          Vector(Vector(Flags.Param | Flags.Given))
        ),
        (
          "class RenamedContextual:\n  def answer(using context: Int): Int = context\n",
          Vector(Vector("context")),
          Vector(Vector(Flags.Param | Flags.Given))
        ),
        (
          "class OrdinaryThenContextual:\n  def combine(x: Int)(using y: Int): Int = x + y\n",
          Vector(Vector("x"), Vector("y")),
          Vector(Vector(Flags.Param), Vector(Flags.Param | Flags.Given))
        ),
        (
          "class RenamedMixed:\n  def merge(left: Int)(using right: Int): Int = left + right\n",
          Vector(Vector("left"), Vector("right")),
          Vector(Vector(Flags.Param), Vector(Flags.Param | Flags.Given))
        )
      )
      fixtures.foreach { case (source, names, flags) =>
        val root = parseClass(source)
        val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
        assertEquals(method.mods.flags, Flags.Method)
        assertEquals(method.paramss.map(_.size).toVector, names.map(_.size))
        method.paramss.zip(names).zip(flags).foreach { case ((clause, expectedNames), expectedFlags) =>
          clause.zip(expectedNames).zip(expectedFlags).foreach { case ((raw, expectedName), expected) =>
            val parameter = raw.asInstanceOf[untpd.ValDef]
            assertEquals(parameter.name.toString, expectedName)
            assertEquals(parameter.mods.flags, expected)
            assertEquals(parameter.mods.is(Flags.Given), expected.is(Flags.Given))
            assert(!parameter.mods.is(Flags.Implicit))
            assert(parameter.tpt.isInstanceOf[untpd.Ident])
            assert(parameter.rhs.isEmpty)
            assert(parameter.source.exists)
            assert(parameter.span.exists)
            assert(parameter.tpt.source.eq(parameter.source))
          }
        }
        assert(method.tpt.isInstanceOf[untpd.Ident])
        assert(method.source.exists)
        assert(method.span.exists)
        assert(method.rhs.source.eq(method.source))
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  test("current private and public failures remain distinct for C1 and C2") {
    withContext {
      val fixtures = Vector(
        "class ContextualBoundary:\n  def value(using x: Int): Int = x\n" ->
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "class MixedBoundary:\n  def combine(x: Int)(using y: Int): Int = x + y\n" ->
          "UNSUPPORTED_PARAMETER_TOPOLOGY"
      )
      fixtures.foreach { case (source, privateCode) =>
        val root = parseClass(source)
        val captured = ExistingUntpdClassMemberFilter.capture(root).fold(
          problem => fail(problem.message),
          identity
        )
        val privateFailure =
          ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0).left.toOption.get
        assertEquals(privateFailure.code, privateCode)

        val publicCapture = ExistingClassUntypedRewrite.capture(root).fold(
          problem => fail(problem.message),
          identity
        )
        val publicFailure =
          publicCapture.method(publicCapture.members.head.ref).left.toOption.get
        assertEquals(publicFailure.code, "UNSUPPORTED_STRUCTURE")
        assert(publicFailure.detail.contains(privateCode))
      }
      assertEquals(
        ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary.code,
        "ORDINARY"
      )
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U054ExistingContextual.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
