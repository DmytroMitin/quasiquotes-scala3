package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdTwoParameterMethodRawCharacterizationTest extends munit.FunSuite:
  test("characterizes one ordinary heterogeneous two-parameter method") {
    withContext {
      val root = parseClass(
        """class PairOps:
          |  val before: Int = 1
          |  def combine(x: Int, y: String): Boolean = x.toString == y
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val method = template.body(1).asInstanceOf[untpd.DefDef]

      assertEquals(template.body.size, 3)
      assertEquals(method.paramss.size, 1)
      assertEquals(method.paramss.head.size, 2)
      val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
      val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]
      assertEquals(first.name.toString, "x")
      assertEquals(second.name.toString, "y")
      assertEquals(first.mods.flags, Flags.Param)
      assertEquals(second.mods.flags, Flags.Param)
      assert(first.rhs.isEmpty)
      assert(second.rhs.isEmpty)
      assert(!first.tpt.isEmpty)
      assert(!second.tpt.isEmpty)
      assert(!method.tpt.isEmpty)
      assert(!method.rhs.isEmpty)
      assert(ExistingUntpdClassMemberFilter.allTrees(method).forall(tree =>
        tree != null && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ))
    }
  }

  test("characterizes opaque parameter result and RHS islands without semantic projection") {
    withContext {
      val root = parseClass(
        """class OpaquePair:
          |  def fold(entries: Map[String, List[Int]], fallback: Either[Int, String]): (Int, String) =
          |    entries.toList match
          |      case (name, values) :: _ => (values.sum, name)
          |      case Nil => fallback.fold(value => (value, "missing"), value => (0, value))
          |""".stripMargin
      )
      val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
      val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
      val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]

      assertEquals(first.tpt.getClass.getSimpleName, "AppliedTypeTree")
      assertEquals(second.tpt.getClass.getSimpleName, "AppliedTypeTree")
      assertEquals(method.tpt.getClass.getSimpleName, "Tuple")
      assertEquals(method.rhs.getClass.getSimpleName, "Match")
      assert(first.tpt.eq(method.paramss.head(0).asInstanceOf[untpd.ValDef].tpt))
      assert(second.tpt.eq(method.paramss.head(1).asInstanceOf[untpd.ValDef].tpt))
    }
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U033TwoParameterRaw.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
