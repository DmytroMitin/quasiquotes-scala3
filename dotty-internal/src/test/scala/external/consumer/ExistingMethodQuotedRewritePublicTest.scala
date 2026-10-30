package external.consumer

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import quasiquotes.definitions.dotty.{ExistingClassUntypedRewrite, ExistingMethodQuotedRewrite}
import scala.meta.*

/** The authoring recipe uses only exported Quasiquotes APIs. */
object U080PublicRecipe:
  import ExistingClassUntypedRewrite.*
  def quote(selection: ExistingMethodQuotedRewrite.Selection): Either[Failure, Defn.Def] = selection.projected match
    case q"def echo($value: AnyVal): AnyVal = $body" => value match
      case term: Term.Name => Right(q"def echo($term: Int): Int = _root_.java.lang.Math.abs($term)")
      case _ => Left(Failure("NAME", "Term.Name required"))
    case q"def magnitude($number: AnyVal): AnyVal = $body" => number match
      case term: Term.Name => Right(q"def magnitude($term: Int): Int = _root_.java.lang.StrictMath.abs($term)")
      case _ => Left(Failure("NAME", "Term.Name required"))
    case _ => Left(Failure("PATTERN", "method pattern required"))

  def rewrite(root: untpd.TypeDef, name: String)(using Context): Either[Failure, Result] =
    for
      captured <- ExistingClassUntypedRewrite.capture(root)
      member <- captured.members.find(_.diagnosticName.contains(name)).toRight(Failure("MEMBER", name))
      selected <- ExistingMethodQuotedRewrite.select(captured, member.ref)
      replacement <- quote(selected)
      plan <- selected.replace(captured.emptyPlan, replacement)
      result <- ExistingClassUntypedRewrite(captured, plan)
    yield result

final class ExistingMethodQuotedRewritePublicTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  test("real public q echo and magnitude return immutable atomic plans preserving siblings"):
    given Context = (new ContextBase).initialCtx
    Vector(("echo","value"),("magnitude","number")).foreach { (name,parameter) =>
      val root = parse(s"class C { val before: Int = 1; def $name($parameter: AnyVal): AnyVal = $parameter; val after: Int = 7 }")
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val member = captured.members(1)
      val selection = right(ExistingMethodQuotedRewrite.select(captured,member.ref))
      val empty = captured.emptyPlan
      val plan = right(selection.replace(empty,right(U080PublicRecipe.quote(selection))))
      assert(empty.isEmpty); assertEquals(empty.editCount,0)
      assert(!plan.eq(empty)); assertEquals(plan.editCount,3)
      val result = right(ExistingClassUntypedRewrite(captured,plan))
      assert(result.changed)
      assert(result.directMemberIdentities(0).sameObjectAs(captured.members(0).identity))
      assert(result.directMemberIdentities(2).sameObjectAs(captured.members(2).identity))
      val recaptured = right(ExistingClassUntypedRewrite.capture(root))
      assert(selection.replace(recaptured.emptyPlan,right(U080PublicRecipe.quote(selection))).isLeft)
      assert(ExistingClassUntypedRewrite(recaptured,plan).isLeft)
      assert(selection.replace(plan,right(U080PublicRecipe.quote(selection))).isLeft)
      assertEquals(plan.editCount,3)
      assert(plan.omit(member.ref).isLeft)
      assert(selection.replace(right(empty.omit(member.ref)),right(U080PublicRecipe.quote(selection))).isLeft)
    }

  test("strict input topology and own-parameter body are mandatory"):
    given Context = (new ContextBase).initialCtx
    Vector(
      "def echo[A](value: AnyVal): AnyVal = value",
      "def echo[A](value: A): A = value",
      "def echo(value: => AnyVal): AnyVal = value",
      "def echo(value: AnyVal = 1): AnyVal = value",
      "def echo(value: AnyVal*): AnyVal = 1",
      "def echo(using value: AnyVal): AnyVal = value",
      "def echo(value: AnyVal, other: AnyVal): AnyVal = value",
      "def echo(value: AnyVal)(other: AnyVal): AnyVal = value",
      "def echo(value: AnyVal): AnyVal = 1"
    ).foreach { source =>
      val capture = right(ExistingClassUntypedRewrite.capture(parse(s"class C { $source }")))
      assert(ExistingMethodQuotedRewrite.select(capture,capture.members.head.ref).isLeft,clues(source))
    }

  test("malformed quotes cannot silently import free or unrooted bindings"):
    given Context = (new ContextBase).initialCtx
    val capture = right(ExistingClassUntypedRewrite.capture(parse("class C { def echo(value: AnyVal): AnyVal = value }")))
    val selected = right(ExistingMethodQuotedRewrite.select(capture,capture.members.head.ref))
    val empty = capture.emptyPlan
    Vector[Defn.Def](
      q"def other(value: Int): Int = _root_.java.lang.Math.abs(value)",
      q"def echo(other: Int): Int = _root_.java.lang.Math.abs(other)",
      q"def echo(value: Long): Long = _root_.java.lang.Math.abs(value)",
      q"def echo[A](value: Int): Int = _root_.java.lang.Math.abs(value)",
      q"def echo(value: Int = 1): Int = _root_.java.lang.Math.abs(value)",
      q"def echo(value: => Int): Int = _root_.java.lang.Math.abs(value)",
      q"def echo(value: Int*): Int = _root_.java.lang.Math.abs(value)",
      q"def echo(value: Int, other: Int): Int = _root_.java.lang.Math.abs(other)",
      q"def echo(value: Int)(other: Int): Int = _root_.java.lang.Math.abs(value)",
      q"def echo(value: Int): Int = _root_.java.lang.Math.abs(free)",
      q"def echo(value: Int): Int = _root_.java.lang.Math.abs(1)",
      q"def echo(value: Int): Int = Math.abs(value)",
      q"def echo(value: Int): Int = java.lang.Math.abs(value)",
      q"def echo(value: Int): Int = _root_.java.Math.abs(value)",
      q"def echo(value: Int): Int = _root_.java.lang.System.abs(value)",
      q"def echo(value: Int): Int = _root_.java.lang.Math.max(value)",
      q"def echo(value: Int): Int = value",
      q"def echo(value: Int): Int = ((value: Int) => _root_.java.lang.Math.abs(value))",
      q"def echo(value: Int): Int = _root_.java.lang.Math.abs(value, value)",
      q"def echo(value: Int): Int = _root_.java.lang.Math.abs(value = value)",
      q"def echo(value: Int): Int = { val value: Int = 1; _root_.java.lang.Math.abs(value) }"
    ).foreach { replacement =>
      assert(selected.replace(empty,replacement).isLeft,clues(replacement.syntax))
      assert(empty.isEmpty)
    }
    val foreign = right(ExistingClassUntypedRewrite.capture(parse("class D { def echo(value: AnyVal): AnyVal = value }")))
    assert(ExistingMethodQuotedRewrite.select(capture,foreign.members.head.ref).isLeft)
    assert(ExistingMethodQuotedRewrite.select(capture,null).isLeft)
    assert(ExistingMethodQuotedRewrite.select(null,capture.members.head.ref).isLeft)

  test("foreign callers cannot construct a Selection or invoke the private rooted plan route"):
    val errors = scala.compiletime.testing.typeCheckErrors("""
      new _root_.quasiquotes.definitions.dotty.ExistingMethodQuotedRewrite.Selection(null,null,null,null,null,null)
    """)
    assert(errors.exists(_.message.contains("cannot be accessed")),clues(errors))
    val route = scala.compiletime.testing.typeCheckErrors("""
      def forge(p: _root_.quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.EditPlan) =
        p.replaceQuotedBody(null,null)
    """)
    assert(route.exists(_.message.contains("cannot be accessed")),clues(route))

  private def right[E,A](value: Either[E,A]): A = value.fold(p => fail(p.toString),identity)
  private def parse(source: String)(using Context): untpd.TypeDef =
    val unit = CompilationUnit("u080-public.scala",source)
    val reporter = new StoreReporter(null)
    val ctx = summon[Context].fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source)(using ctx).parse()
    assert(!reporter.hasErrors)
    parsed.asInstanceOf[untpd.PackageDef].stats.collectFirst { case t: untpd.TypeDef => t }.get
