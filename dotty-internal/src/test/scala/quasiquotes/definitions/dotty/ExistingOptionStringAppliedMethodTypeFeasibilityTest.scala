package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource

import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingOptionStringAppliedMethodTypeFeasibilityTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  import U071OptionStringAppliedMethodTypeOracle.*

  test("P0 public no-op preserves the exact existing owner and direct member identities"):
    withContext:
      val captured = publicCapture(optionService("value"))
      val result = publicRight(ExistingClassUntypedRewrite(captured, captured.emptyPlan))

      assert(!result.changed)
      assert(result.tree.eq(captured.exact.originalRoot))
      assertEquals(result.directMemberIdentities.size, captured.members.size)
      result.directMemberIdentities.zip(captured.members).foreach { (identity, member) =>
        assert(identity.sameObjectAs(member.identity))
      }

  test("current foreign-consumable public facade rejects exact Option[String] at U040"):
    withContext:
      val captured = publicCapture(optionService("value"))
      val method = publicRight(captured.method(captured.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = publicRight(method.parameterScope.reference(parameter.ref))
      val plan = publicRight(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, optionString)
        p2 <- p1.replaceResultType(method.ref, optionString)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)

      ExistingClassUntypedRewrite(captured, plan) match
        case Left(problem) =>
          assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
          assert(problem.detail.contains("U040 SEMANTIC_TYPE_FAMILY_UNSUPPORTED"), clues(problem))
          assert(problem.detail.contains("STypeApply"), clues(problem))
        case Right(value) => fail("public U040 unexpectedly admitted Option[String]: " + value)

  test("test-only oracle proves parameter-only, result-only, combined identity-body and unchanged-body matrices"):
    withContext:
      Vector(
        (true, false, false),
        (false, true, false),
        (true, true, true),
        (true, true, false)
      ).foreach { (parameterEdit, resultEdit, bodyEdit) =>
        val root = optionService("value")
        val captured = publicCapture(root)
        val method = publicRight(captured.method(captured.members(1).ref))
        val parameter = method.parameterClauses.head.parameters.head
        val body = publicRight(method.parameterScope.reference(parameter.ref))
        val plan0 = captured.emptyPlan
        val plan1 = if parameterEdit then publicRight(plan0.replaceParameterType(parameter.ref, optionString)) else plan0
        val plan2 = if resultEdit then publicRight(plan1.replaceResultType(method.ref, optionString)) else plan1
        val plan3 = if bodyEdit then publicRight(plan2.replaceBody(method.ref, body)) else plan2
        val before = snapshot(root)

        val proof = oracleRight(U071OptionStringAppliedMethodTypeOracle(captured, plan3))

        assertEquals(proof.parameterType.nonEmpty, parameterEdit)
        assertEquals(proof.resultType.nonEmpty, resultEdit)
        assertEquals(proof.bodyEdited, bodyEdit)
        assert(proof.untouchedIdentity)
        assert(proof.orderPreserved)
        assert(!proof.positionedMethod.eq(proof.oldMethod))
        assertEquals(snapshot(root), before)
        assertAppliedSite(proof.parameterType)
        assertAppliedSite(proof.resultType)
      }

  test("position evidence attributes only the new outer application to the old primitive site"):
    withContext:
      val captured = publicCapture(optionService("value"))
      val capturedDescriptor = descriptor(captured)
      val proof = oracleRight(positionOptionString(capturedDescriptor.parameterClauses.head.head.tpt))
      val loweredNodes = allTrees(proof.lowered)
      val positionedNodes = allTrees(proof.positioned)

      assertEquals(loweredNodes.size, 3)
      assertEquals(positionedNodes.size, 3)
      assert(loweredNodes.forall(node => !node.source.exists && !node.span.exists))
      assertEquals(proof.positioned.source, proof.oldSite.source)
      assertEquals(proof.positioned.span, proof.oldSite.span)
      assert(positionedNodes.drop(1).forall(node => node.source.exists && node.span.exists))
      assert(positionedNodes.drop(1).forall(_.source.path == "generated/U071OptionString.scala"))
      assert(positionedNodes.forall(positioned => loweredNodes.forall(lowered => !positioned.eq(lowered))))

      given dotty.tools.dotc.util.SourceFile = NoSource
      val unpositionedChildren = untpd.AppliedTypeTree(
        untpd.Ident(typeName("Option")),
        List(untpd.Ident(typeName("String")))
      ).cloneIn(proof.oldSite.source).withSpan(proof.oldSite.span)
      assertOracleCode(
        validatePositionedForTest(proof.lowered, unpositionedChildren, proof.oldSite),
        "POSITION_EVIDENCE_INVALID"
      )

      val wrongConstructor = untpd.Ident(typeName("Option"))
        .cloneIn(proof.oldSite.source)
        .withSpan(proof.oldSite.span)
      val wrongArgument = untpd.Ident(typeName("String"))
        .cloneIn(proof.oldSite.source)
        .withSpan(proof.oldSite.span)
      val falselyAttributed = untpd.AppliedTypeTree(wrongConstructor, List(wrongArgument))
        .cloneIn(proof.oldSite.source)
        .withSpan(proof.oldSite.span)
      assertOracleCode(
        validatePositionedForTest(proof.lowered, falselyAttributed, proof.oldSite),
        "POSITION_EVIDENCE_INVALID"
      )

  test("the exact oracle rejects every non-Option[String] family, arity and nesting"):
    withContext:
      val unsupported = Vector[TypeNormalForm](
        TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(TypeNormalForm.STypeIdent("Int"))),
        TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(TypeNormalForm.STypeIdent("String"))),
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(TypeNormalForm.STypeIdent("String"))))
        ),
        TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), Nil),
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(TypeNormalForm.STypeIdent("String"), TypeNormalForm.STypeIdent("Boolean"))
        ),
        TypeNormalForm.STypeIdent("Option"),
        TypeNormalForm.STypeTuple(List(TypeNormalForm.STypeIdent("String"), TypeNormalForm.STypeIdent("String"))),
        TypeNormalForm.STypeFunction(List(TypeNormalForm.STypeIdent("String")), TypeNormalForm.STypeIdent("String"))
      )
      unsupported.foreach { semantic =>
        val captured = publicCapture(optionService("value"))
        val method = publicRight(captured.method(captured.members(1).ref))
        val parameter = method.parameterClauses.head.parameters.head
        val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter.ref, semantic))
        assertOracleCode(U071OptionStringAppliedMethodTypeOracle(captured, plan), "PARAMETER_INTENT_UNSUPPORTED")
      }

  test("G2 dependency, local Option binder, owner Option alias, by-name and repeated wrappers remain closed"):
    withContext:
      val fixtures = Vector(
        ("class G2:\n  def echo[A](value: A): A = value\n", "LOCAL_TYPE_BINDER_UNSUPPORTED"),
        ("class LocalOption:\n  def echo[Option](value: String): String = value\n", "LOCAL_TYPE_BINDER_UNSUPPORTED"),
        ("class OwnerShadow:\n  type Option[A] = List[A]\n  def echo(value: String): String = value\n", "OPTION_SHADOWING_UNSUPPORTED"),
        ("class ByName:\n  def echo(value: => String): String = value\n", "PARAMETER_WRAPPER_UNSUPPORTED"),
        ("class Repeated:\n  def echo(value: String*): String = value.head\n", "PARAMETER_WRAPPER_UNSUPPORTED")
      )
      fixtures.foreach { (source, expected) =>
        val captured = publicCapture(parseClass(source))
        val methodIndex = captured.members.indexWhere(_.kind == MemberKind.Method)
        val method = publicRight(captured.method(captured.members(methodIndex).ref))
        val parameter = method.parameterClauses.head.parameters.head
        val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter.ref, optionString))
        assertOracleCode(U071OptionStringAppliedMethodTypeOracle(captured, plan), expected)
      }

  test("foreign capture-local references and copied roots cannot be reused as authority"):
    withContext:
      val first = publicCapture(optionService("value"))
      val second = publicCapture(optionService("value"))
      val firstMethod = publicRight(first.method(first.members(1).ref))
      val foreignMethod = publicRight(second.method(second.members(1).ref))
      val foreignParameter = foreignMethod.parameterClauses.head.parameters.head

      assertPublicCode(first.emptyPlan.replaceParameterType(foreignParameter.ref, optionString), "SELECTION_FAILED")
      assertPublicCode(first.emptyPlan.replaceResultType(foreignMethod.ref, optionString), "SELECTION_FAILED")
      assertPublicCode(first.emptyPlan.replaceBody(foreignMethod.ref, TermShape.Literal("0")), "SELECTION_FAILED")

      val localParameter = firstMethod.parameterClauses.head.parameters.head
      val plan = publicRight(first.emptyPlan.replaceParameterType(localParameter.ref, optionString))
      assertOracleCode(U071OptionStringAppliedMethodTypeOracle(second, plan), "FOREIGN_PLAN")

  private def optionService(body: String)(using Context): untpd.TypeDef =
    parseClass(
      s"""class OptionService:
         |  val untouchedBefore: Int = 1
         |  def echo(value: String): String = $body
         |  val untouchedAfter: Int = 2
         |""".stripMargin
    )

  private def publicCapture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def descriptor(capture: Capture)(using Context): ExistingUntpdOrdinaryMethodDescriptor.Descriptor =
    ExistingUntpdOrdinaryMethodDescriptor.capture(capture.exact, 1)
      .fold(problem => fail(problem.message), identity)

  private def assertAppliedSite(value: Option[PositionedType])(using Context): Unit = value.foreach { proof =>
    assertEquals(allTrees(proof.lowered).size, 3)
    assertEquals(allTrees(proof.positioned).size, 3)
    assertEquals(proof.positioned.source, proof.oldSite.source)
    assertEquals(proof.positioned.span, proof.oldSite.span)
    assert(allTrees(proof.positioned).drop(1).forall(node => node.source.exists && node.span.exists))
    assert(allTrees(proof.positioned).drop(1).forall(_.source.path == "generated/U071OptionString.scala"))
  }

  private def snapshot(root: untpd.TypeDef)(using Context): Vector[(untpd.Tree, Any, Any)] =
    allTrees(root).map(tree => (tree, tree.source, tree.span))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def assertOracleCode[A](result: Either[Error, A], expected: String): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, expected, clues(problem))
      assert(problem.detail.nonEmpty)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def assertPublicCode[A](result: Either[Failure, A], expected: String): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(problem))
    case Right(value) => fail(s"expected $expected, found success $value")

  private def publicRight[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def oracleRight[A](result: Either[Error, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U071OptionStringFeasibility.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats.collect { case root: untpd.TypeDef => root } match
          case root :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
