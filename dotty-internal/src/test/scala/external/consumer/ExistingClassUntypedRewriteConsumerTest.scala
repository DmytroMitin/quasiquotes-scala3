package external.consumer

import scala.compiletime.testing.typeCheckErrors

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingClassUntypedRewriteConsumerTest extends munit.FunSuite:
  test("foreign callers capture ordered public views with exact opaque identities"):
    withContext:
      val root = parseClass(
        """class PublicViews:
          |  def one(x: Int): Int = x
          |  val value: Int = 1
          |  type Alias = Int
          |  def two(left: Int, right: String): Int = left
          |""".stripMargin
      )
      val captured = right(ExistingClassUntypedRewrite.capture(root))

      assertEquals(captured.members.map(_.index), Vector(0, 1, 2, 3))
      assertEquals(captured.members.map(_.kind.code), Vector("METHOD", "VALUE", "TYPE_MEMBER", "METHOD"))
      assertEquals(captured.members.map(_.diagnosticName), Vector(Some("one"), Some("value"), Some("Alias"), Some("two")))
      assert(captured.classIdentity.sameObjectAs(captured.classIdentity))
      assert(captured.members.head.identity.sameObjectAs(captured.members.head.identity))
      assert(!captured.classIdentity.sameObjectAs(right(ExistingClassUntypedRewrite.capture(parseClass(
        "class PublicViews:\n  def one(x: Int): Int = x\n  val value: Int = 1\n  type Alias = Int\n  def two(left: Int, right: String): Int = left\n"
      )).map(_.classIdentity))))

      val one = right(captured.method(captured.members(0).ref))
      assertEquals(one.diagnosticName, "one")
      assertEquals(one.parameterClauses.map(_.kind.code), Vector("ORDINARY"))
      assertEquals(one.parameterClauses.head.parameters.map(_.diagnosticName), Vector("x"))
      assertEquals(right(one.parameterClauses.head.parameters.head.declaredType.semantic), intType)
      assertEquals(right(one.resultType.semantic), intType)
      assert(right(one.body.semantic).isInstanceOf[TermShape])

      val two = right(captured.method(captured.members(3).ref))
      assertEquals(two.parameterClauses.head.parameters.map(_.diagnosticName), Vector("left", "right"))
      assert(two.parameterClauses.head.parameters(0).identity.sameObjectAs(two.parameterClauses.head.parameters(0).identity))
      assert(!two.parameterClauses.head.parameters(0).identity.sameObjectAs(two.parameterClauses.head.parameters(1).identity))

      assertCode(captured.member(-1), "SELECTION_FAILED")
      assertCode(captured.member(4), "SELECTION_FAILED")
      assertCode(captured.method(captured.members(1).ref), "SELECTION_FAILED")

  test("bounded slot projection stays independent from exact capture and parameter scope"):
    withContext:
      val captured = right(ExistingClassUntypedRewrite.capture(parseClass(
        """class PartialProjection:
          |  def unsupportedType(x: scala.Int): scala.Int = x
          |  def unsupportedBody(x: Int): Int = x match
          |    case _ => x
          |""".stripMargin
      )))
      val unsupportedType = right(captured.method(captured.members(0).ref))
      assertCode(unsupportedType.parameterClauses.head.parameters.head.declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      assertCode(unsupportedType.resultType.semantic, "UNSUPPORTED_STRUCTURE")

      val unsupportedBody = right(captured.method(captured.members(1).ref))
      assertCode(unsupportedBody.body.semantic, "UNSUPPORTED_STRUCTURE")
      val parameter = unsupportedBody.parameterClauses.head.parameters.head
      val firstBinder = right(unsupportedBody.parameterScope.binder(parameter.ref))
      val secondBinder = right(unsupportedBody.parameterScope.binder(parameter.ref))
      assertEquals(firstBinder, secondBinder)
      val firstReference = right(unsupportedBody.parameterScope.reference(parameter.ref))
      val secondReference = right(unsupportedBody.parameterScope.reference(parameter.ref))
      assertEquals(firstReference, secondReference)
      assert(firstReference.isInstanceOf[TermShape])

      val unsupportedTopology = right(ExistingClassUntypedRewrite.capture(parseClass(
        "class UnsupportedTopology:\n  def three(a: Int, b: Int, c: Int): Int = a\n"
      )))
      assertCode(unsupportedTopology.method(unsupportedTopology.members.head.ref), "UNSUPPORTED_STRUCTURE")

      val foreign = right(ExistingClassUntypedRewrite.capture(parseClass(
        "class ForeignScope:\n  def unsupportedBody(x: Int): Int = x\n"
      )))
      val foreignMethod = right(foreign.method(foreign.members.head.ref))
      val foreignParameter = foreignMethod.parameterClauses.head.parameters.head
      assertCode(unsupportedBody.parameterScope.binder(foreignParameter.ref), "SELECTION_FAILED")
      assertCode(unsupportedBody.parameterScope.reference(foreignParameter.ref), "SELECTION_FAILED")

  test("immutable plans reject foreign refs duplicates conflicts and foreign binder graphs"):
    withContext:
      val captured = right(ExistingClassUntypedRewrite.capture(parseClass(
        """class Plans:
          |  def change(x: AnyVal): AnyVal = x
          |  val removeMe: Int = 1
          |  val keepMe: Int = 2
          |""".stripMargin
      )))
      val method = right(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val reference = right(method.parameterScope.reference(parameter.ref))
      val body = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "abs"),
        List(reference)
      )

      val p1 = right(captured.emptyPlan.replaceParameterType(parameter.ref, intType))
      val p2 = right(p1.replaceResultType(method.ref, intType))
      val p3 = right(p2.replaceBody(method.ref, body))
      assertEquals((p3.isEmpty, p3.editCount), (false, 3))
      assertEquals((captured.emptyPlan.isEmpty, captured.emptyPlan.editCount), (true, 0))
      assertCode(p1.replaceParameterType(parameter.ref, intType), "EDIT_CONFLICT")
      assertCode(p2.replaceResultType(method.ref, intType), "EDIT_CONFLICT")
      assertCode(p3.replaceBody(method.ref, body), "EDIT_CONFLICT")

      val omitted = right(captured.emptyPlan.omit(captured.members(1).ref))
      assertCode(omitted.omit(captured.members(1).ref), "EDIT_CONFLICT")
      assertCode(p1.omit(captured.members.head.ref), "EDIT_CONFLICT")
      assertCode(right(captured.emptyPlan.omit(captured.members.head.ref)).replaceResultType(method.ref, intType), "EDIT_CONFLICT")

      val foreign = right(ExistingClassUntypedRewrite.capture(parseClass(
        "class PlansForeign:\n  def change(x: AnyVal): AnyVal = x\n"
      )))
      val foreignMethod = right(foreign.method(foreign.members.head.ref))
      val foreignParameter = foreignMethod.parameterClauses.head.parameters.head
      assertCode(captured.emptyPlan.omit(foreign.members.head.ref), "SELECTION_FAILED")
      assertCode(captured.emptyPlan.replaceParameterType(foreignParameter.ref, intType), "SELECTION_FAILED")
      assertCode(captured.emptyPlan.replaceResultType(foreignMethod.ref, intType), "SELECTION_FAILED")
      assertCode(captured.emptyPlan.replaceBody(foreignMethod.ref, TermShape.Literal("1")), "SELECTION_FAILED")
      val foreignReference = right(foreignMethod.parameterScope.reference(foreignParameter.ref))
      assertCode(captured.emptyPlan.replaceBody(method.ref, foreignReference), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(captured.emptyPlan.replaceParameterType(parameter.ref, null), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(captured.emptyPlan.replaceResultType(method.ref, null), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(captured.emptyPlan.replaceBody(method.ref, null), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(ExistingClassUntypedRewrite(null, captured.emptyPlan), "MISSING_INPUT")
      assertCode(ExistingClassUntypedRewrite(captured, null), "MISSING_INPUT")
      assertCode(ExistingClassUntypedRewrite(captured, foreign.emptyPlan), "SELECTION_FAILED")

  test("no-op U1 U2 U3 omission and generated families compose through the public facade"):
    withContext:
      val root = parseClass(
        """class Composition:
          |  def change(x: AnyVal): AnyVal = x
          |  val removeMe: Int = 1
          |  val keepMe: Int = 2
          |""".stripMargin
      )
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "abs"),
        List(right(method.parameterScope.reference(parameter.ref)))
      )

      val noop = right(ExistingClassUntypedRewrite(captured, captured.emptyPlan))
      assert(!noop.changed)
      assert(noop.tree.eq(root))
      assertEquals(noop.directMemberIdentities.size, 3)
      noop.directMemberIdentities.zip(captured.members.map(_.identity)).foreach((after, before) =>
        assert(after.sameObjectAs(before))
      )

      val u1Plan = right(captured.emptyPlan.append(valueDefinition("added", "4"), "generated/U1.scala"))
      val u1 = right(ExistingClassUntypedRewrite(captured, u1Plan))
      assert(u1.changed)
      assertEquals(u1.directMemberIdentities.size, 4)
      assert(u1.directMemberIdentities.head.sameObjectAs(captured.members.head.identity))

      val u2Plan = right(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, intType)
        p2 <- p1.replaceResultType(method.ref, intType)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)
      val u2 = right(ExistingClassUntypedRewrite(captured, u2Plan))
      assert(u2.changed)
      assert(!u2.directMemberIdentities.head.sameObjectAs(captured.members.head.identity))
      assert(u2.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assert(u2.directMemberIdentities(2).sameObjectAs(captured.members(2).identity))

      val generated = Vector(
        valueDefinition("addedValue", "5"),
        methodDefinition("addedMethod"),
        aliasDefinition("AddedAlias")
      )
      val u3Plan = right(generated.zipWithIndex.foldLeft[Either[Failure, EditPlan]](Right(u2Plan)) { case (planResult, (definition, index)) =>
        planResult.flatMap(_.append(definition, s"generated/U3-$index.scala"))
      })
      val u3 = right(ExistingClassUntypedRewrite(captured, u3Plan))
      assertEquals(u3.directMemberIdentities.size, 6)
      assert(u3.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))

      val composition = right(for
        p1 <- captured.emptyPlan.omit(captured.members(1).ref)
        p2 <- p1.replaceParameterType(parameter.ref, intType)
        p3 <- p2.replaceResultType(method.ref, intType)
        p4 <- p3.replaceBody(method.ref, body)
        p5 <- p4.append(aliasDefinition("ComposedAlias"), "generated/Composition.scala")
      yield p5)
      val first = right(ExistingClassUntypedRewrite(captured, composition))
      val second = right(ExistingClassUntypedRewrite(captured, composition))
      assert(first.changed && second.changed)
      assert(!first.tree.eq(second.tree))
      assertEquals(first.directMemberIdentities.size, 3)
      assert(first.directMemberIdentities(1).sameObjectAs(captured.members(2).identity))

      assertCode(captured.emptyPlan.append(null, "generated/Missing.scala"), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(captured.emptyPlan.append(valueDefinition("badPath", "1"), null), "SEMANTIC_FRAGMENT_INVALID")
      assertCode(captured.emptyPlan.append(valueDefinition("badPath", "1"), " invalid.scala"), "SEMANTIC_FRAGMENT_INVALID")

  test("foreign code sees only the selected public surface"):
    assert(typeCheckErrors("new quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.Capture(null)").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.MemberRef()").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.MethodRef()").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.ParameterRef()").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.ExactIdentity(null)").nonEmpty)
    assert(typeCheckErrors("classOf[quasiquotes.definitions.dotty.ExistingUntpdOrdinaryMethodDescriptor]").nonEmpty)
    assert(typeCheckErrors("quasiquotes.parser.BinderId(0)").nonEmpty)
    assert(typeCheckErrors("(x: quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.MemberView) => x.tree").nonEmpty)

  private def valueDefinition(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(name(label), intType, TermShape.Literal(literal)))

  private def methodDefinition(label: String): SemanticDefinition =
    semantic(SemanticDefinition.concreteMethod(name(label), Vector.empty, intType)(_ => Right(TermShape.Literal("6"))))

  private def aliasDefinition(label: String): SemanticDefinition =
    semantic(SemanticDefinition.typeAlias(name(label), intType))

  private def name(value: String): DefinitionName =
    semantic(DefinitionName.fromSource(value))

  private val intType = TypeNormalForm.STypeIdent("Int")

  private def semantic[A](result: Either[DefinitionSemanticError, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def right[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def assertCode[A](result: Either[Failure, A], expected: String): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, expected, clues(problem))
      assert(problem.detail.nonEmpty)
      assert(problem.message.nonEmpty)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("ExistingClassUntypedRewriteConsumer.scala", source)
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
    run(using (new ContextBase).initialCtx)
