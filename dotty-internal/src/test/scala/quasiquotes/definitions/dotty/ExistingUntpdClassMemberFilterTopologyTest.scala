package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdClassMemberFilterTopologyTest extends munit.FunSuite:
  private val Source =
    """@classMarker
      |final class VersionAdapter:
      |  @memberMarker def kept(): Int = 42
      |  val opaque: Int = 7
      |  type Alias = Int
      |  object Nested
      |  def same(): Int = 1
      |  def same(value: Int): Int = value
      |  def removedForThisLine(): Int = apiThatDoesNotExistHere()
      |""".stripMargin

  test("ordinary class topology keeps direct members outside the Template constructor") {
    withContext {
      val root = parseClass(Source)
      val template = root.rhs.asInstanceOf[untpd.Template]
      val body = template.body

      assert(root.source.exists)
      assert(root.span.exists)
      assert(template.source.exists)
      assert(template.span.exists)
      assert(template.constr.isInstanceOf[untpd.DefDef])
      assertEquals(root.mods.annotations.size, 1)
      assertEquals(body.size, 7)
      assertEquals(
        body.map(memberLabel),
        List("def:kept", "val:opaque", "type:Alias", "object:Nested", "def:same", "def:same", "def:removedForThisLine")
      )
      assertEquals(body.head.asInstanceOf[untpd.DefDef].mods.annotations.size, 1)
      body.foreach { member =>
        assert(member.source.exists, clues(memberLabel(member)))
        assert(member.span.exists, clues(memberLabel(member)))
      }

      println(
        s"U023_TOPOLOGY root=${root.getClass.getSimpleName} template=${template.getClass.getSimpleName} " +
          s"constructor=${template.constr.getClass.getSimpleName} parents=${template.parentsOrDerived.size} " +
          s"derived=${template.derived.size} self=${template.self.getClass.getSimpleName} " +
          s"members=${body.map(memberLabel).mkString(",")}"
      )
    }
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U023Topology.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def memberLabel(tree: untpd.Tree): String = tree match
    case value: untpd.DefDef => s"def:${value.name}"
    case value: untpd.ValDef => s"val:${value.name}"
    case value: untpd.ModuleDef => s"object:${value.name}"
    case value: untpd.TypeDef => s"type:${value.name}"
    case other => other.getClass.getSimpleName

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
