package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile
import scala.meta.*

final class ExistingMethodQuotedRewriteSnapshotTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  private val replacement = q"def echo(value: Int): Int = _root_.java.lang.Math.abs(value)"

  for
    role <- Vector("root","method","parameter")
    when <- Vector("capture","selection","plan")
    mutation <- Vector("copy","privateWithin","annotations")
  do
    test(s"$role setMods $mutation after $when cannot recertify the original owner"):
      given Context = (new ContextBase).initialCtx
      val root = fixture()
      val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
      val target: untpd.DefTree = role match
        case "root" => root
        case "method" => method
        case _ => method.paramss.head.head.asInstanceOf[untpd.ValDef]
      checkMutation(root,when) {
        val original = target.mods
        val changed = mutation match
          case "copy" => original.copy()
          case "privateWithin" => original.copy(privateWithin = typeName("DifferentOwner"))
          case _ => original.copy(annotations = original.annotations :+ original.annotations.head)
        assertEquals(changed.flags,original.flags)
        assert(!changed.eq(original))
        target.setMods(changed)
      }

  for
    role <- Vector("root","method","parameter")
    when <- Vector("capture","selection","plan")
  do
    test(s"$role annotation descendant span mutation after $when invalidates the snapshot"):
      given Context = (new ContextBase).initialCtx
      val root = fixture()
      val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
      val target: untpd.DefTree = role match
        case "root" => root
        case "method" => method
        case _ => method.paramss.head.head.asInstanceOf[untpd.ValDef]
      val annotation = target.mods.annotations.head
      val descendant = ExistingUntpdClassMemberFilter.allTrees(annotation).collectFirst {
        case literal: untpd.Literal => literal
      }.get
      val originalMods = target.mods
      checkMutation(root,when) {
        val span = descendant.span
        descendant.span = span.withEnd(span.end - 1)
        assert(target.mods.eq(originalMods))
        assert(target.mods.annotations.head.eq(annotation))
      }

  for when <- Vector("capture","selection","plan") do
    test(s"equal but distinct SourceFile buffers remain independently protected after $when"):
      given Context = (new ContextBase).initialCtx
      val parsed = fixture()
      val firstSource = parsed.source
      val secondSource = new SourceFile(firstSource.file,firstSource.content.clone())
      assertEquals(firstSource,secondSource)
      assert(!firstSource.eq(secondSource))
      assert(!firstSource.content.eq(secondSource.content))
      val template = parsed.rhs.asInstanceOf[untpd.Template]
      val sibling = template.body(1).cloneIn(secondSource)
      val changedTemplate = untpd.cpy.Template(template)(template.constr,template.parentsOrDerived,template.derived,template.self,template.body.updated(1,sibling))
      val root = untpd.cpy.TypeDef(parsed)(parsed.name,changedTemplate)
      assert(root.source.eq(firstSource)); assert(sibling.source.eq(secondSource))
      checkMutation(root,when) {
        val original = firstSource.content.apply(0)
        secondSource.content(0) = if original == 'X' then 'Y' else 'X'
        assertEquals(firstSource.content.apply(0),original)
      }

  private def checkMutation(root: untpd.TypeDef, when: String)(mutate: => Unit)(using Context): Unit =
    val capture = right(ExistingClassUntypedRewrite.capture(root))
    val member = capture.members.head.ref
    // Demonstrate the unchanged annotated/mixed-source fixture really is admitted.
    val initial = right(ExistingMethodQuotedRewrite.select(capture,member))
    val initialPlan = right(initial.replace(capture.emptyPlan,replacement))
    assert(ExistingClassUntypedRewrite(capture,initialPlan).isRight)
    val selection = if when == "capture" then None else Some(right(ExistingMethodQuotedRewrite.select(capture,member)))
    val plan = if when == "plan" then Some(right(selection.get.replace(capture.emptyPlan,replacement))) else None
    mutate
    when match
      case "capture" => assert(ExistingMethodQuotedRewrite.select(capture,member).isLeft)
      case "selection" => assert(selection.get.replace(capture.emptyPlan,replacement).isLeft)
      case _ => assert(ExistingClassUntypedRewrite(capture,plan.get).isLeft)
    assert(capture.emptyPlan.isEmpty)

  private def fixture()(using Context): untpd.TypeDef =
    val unit = CompilationUnit("u080-snapshot.scala","@ann(\"root\") class C { @ann(\"method\") def echo(@ann(\"parameter\") value: AnyVal): AnyVal = value; val sibling: Int = 7 }")
    val reporter = new StoreReporter(null)
    val ctx = summon[Context].fresh.setCompilationUnit(unit).setReporter(reporter)
    val tree = new Parsers.Parser(unit.source)(using ctx).parse()
    assert(!reporter.hasErrors)
    tree.asInstanceOf[untpd.PackageDef].stats.collectFirst { case t: untpd.TypeDef => t }.get

  private def right[E,A](value: Either[E,A]): A = value.fold(p => fail(p.toString),identity)
