package external.consumer

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context,ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.terms.TermShapeBindingView
import scala.compiletime.testing.typeCheckErrors
import scala.meta.*

/** Uses only CURRENT public Capture/Plan/Result and neutral/binder APIs. */
final class QuotedQualifierCurrentPublicBoundaryTest extends munit.FunSuite:
  test("foreign consumer transfers own binder while public apply distinguishes lexical and rooted calls"):
    given Context = (new ContextBase).initialCtx
    Vector(false,true).foreach { rooted =>
      val root = parse("class Foreign { val Math: Int = 99; val StrictMath: Int = 99; def echo(value: AnyVal): AnyVal = value; def magnitude(number: AnyVal): AnyVal = number; val untouched: Int = 7 }")
      val capture = right(ExistingClassUntypedRewrite.capture(root))
      val replacements: Vector[Defn.Def] = if rooted then Vector(
        q"def echo(value: Int): Int = _root_.java.lang.Math.abs(value)",
        q"def magnitude(number: Int): Int = _root_.java.lang.StrictMath.abs(number)")
      else Vector(q"def echo(value: Int): Int = Math.abs(value)", q"def magnitude(number: Int): Int = StrictMath.abs(number)")
      val plan = replacements.foldLeft(capture.emptyPlan) { (current,replacement) =>
        val projected = ScalametaDefinitionProjection.project(replacement).toOption.get.definition
        val output = projected.asMethod.get
        val member = capture.members.find(_.diagnosticName.contains(projected.name.source)).get
        val method = right(capture.method(member.ref))
        val parameter = method.parameterClauses.head.parameters.head
        val outputBinder = output.parameterScope.binder(0,0).toOption.get
        val body = output.body.get.asInstanceOf[TermShape.Apply]
        val bound = TermShapeBindingView.inspect(body.arguments.head).toOption.get.boundReference.get
        assertEquals(bound.binder,outputBinder)
        assert(bound.binder != right(method.parameterScope.binder(parameter.ref)))
        val transferred = TermShape.Apply(body.function,List(right(method.parameterScope.reference(parameter.ref))))
        val p1 = right(current.replaceParameterType(parameter.ref,output.parameterClauses.head.parameters.head.declaredType))
        val p2 = right(p1.replaceResultType(method.ref,output.resultType))
        right(p2.replaceBody(method.ref,transferred))
      }
      assertEquals(plan.editCount,6)
      val applied = ExistingClassUntypedRewrite(capture,plan)
      if rooted then
        assert(applied.left.toOption.get.detail.contains("SEMANTIC_APPLY_FUNCTION_UNSUPPORTED"))
      else
        val result = right(applied)
        assert(result.changed)
        Vector(0,1,4).foreach(i => assert(result.directMemberIdentities(i).sameObjectAs(capture.members(i).identity)))
      val original = root.rhs.asInstanceOf[untpd.Template].body
      assertEquals(original(2).asInstanceOf[untpd.DefDef].tpt.asInstanceOf[untpd.Ident].name.toString,"AnyVal")
    }

  test("magnitude Name narrowing and test oracle visibility are real compiler boundaries"):
    val badName = typeCheckErrors("""
      import scala.meta.*
      def replacement(tree: Defn.Def): Defn.Def = tree match {
        case q"def magnitude($number: AnyVal): AnyVal = $body" =>
          q"def magnitude($number: Int): Int = StrictMath.abs($number)"
      }
    """)
    assert(badName.exists(e => e.message.contains("Term") && e.message.contains("Name")),clues(badName))
    val goodName = typeCheckErrors("""
      import scala.meta.*
      def replacement(tree: Defn.Def): Defn.Def = tree match {
        case q"def magnitude($number: AnyVal): AnyVal = $body" => number match {
          case term: Term.Name => q"def magnitude($term: Int): Int = _root_.java.lang.StrictMath.abs($term)"
          case _ => throw new IllegalArgumentException()
        }
        case _ => throw new IllegalArgumentException()
      }
    """)
    assertEquals(goodName,Nil)
    val privateOracle = typeCheckErrors("""
      val oracle = _root_.quasiquotes.definitions.dotty.QuotedRootedQualifierAuthorityModel
    """)
    assert(privateOracle.nonEmpty)
    assert(privateOracle.exists(e => e.message.contains("cannot be accessed")),clues(privateOracle))

  private def right[A](v: Either[Failure,A]): A = v.fold(p => fail(p.message),identity)
  private def parse(source: String)(using outer: Context): untpd.TypeDef =
    val unit = CompilationUnit("ForeignQualifier.scala",source)
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse().asInstanceOf[untpd.PackageDef]
    assert(reporter.pendingMessages.isEmpty)
    parsed.stats.head.asInstanceOf[untpd.TypeDef]
