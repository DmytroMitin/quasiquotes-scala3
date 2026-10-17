package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource

import quasiquotes.parser.TermShape
import quasiquotes.types.{ResolvedTypeNameId, ResolvedTypeOwnerKind, ResolvedTypeOwnerSegment, TypeNormalForm}
import quasiquotes.types.dotty.TypeUntypedLowering

final class CanonicalOptionConstructorAuthorityModelFitTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  import U071OptionStringAppliedMethodTypeOracle.optionString
  import U072CanonicalOptionConstructorAuthorityOracle.*

  test("A B C and fully rooted pair retain exact parser topology and recursive generated provenance"):
    withContext:
      val captured = publicCapture(service("value"))
      val descriptor = capturedDescriptor(captured)

      Candidate.values.foreach { candidate =>
        val positioned = oracleRight(positionCandidate(descriptor.parameterClauses.head.head.tpt, candidate))
        val generatedNodes = allTrees(positioned.generated)
        val positionedNodes = allTrees(positioned.positioned)

        assert(exactTopology(positioned.generated, candidate))
        assert(exactTopology(positioned.positioned, candidate))
        assertEquals(positioned.positioned.source, positioned.oldSite.source)
        assertEquals(positioned.positioned.span, positioned.oldSite.span)
        assert(positionedNodes.drop(1).forall(_.source.path == candidate.generatedPath))
        assert(positionedNodes.drop(1).forall(_.span.exists))
        assert(positionedNodes.forall(node => generatedNodes.forall(other => !node.eq(other))))
        assert(positionedNodes.forall(_.symbol == dotty.tools.dotc.core.Symbols.NoSymbol))
      }

  test("private candidate matrix covers parameter-only result-only and combined capture-local body"):
    withContext:
      Candidate.values.foreach { candidate =>
        Vector(
          (true, false, false),
          (false, true, false),
          (true, true, false),
          (true, true, true)
        ).foreach { (parameterEdit, resultEdit, bodyEdit) =>
          val root = service("value")
          val captured = publicCapture(root)
          val method = publicRight(captured.method(captured.members.head.ref))
          val parameter = method.parameterClauses.head.parameters.head
          val body = publicRight(method.parameterScope.reference(parameter.ref))
          val plan0 = captured.emptyPlan
          val plan1 = if parameterEdit then publicRight(plan0.replaceParameterType(parameter.ref, optionString)) else plan0
          val plan2 = if resultEdit then publicRight(plan1.replaceResultType(method.ref, optionString)) else plan1
          val plan3 = if bodyEdit then publicRight(plan2.replaceBody(method.ref, body)) else plan2
          val before = snapshot(root)

          val proof = oracleRight(U072CanonicalOptionConstructorAuthorityOracle(captured, plan3, candidate))

          assertEquals(proof.parameterType.nonEmpty, parameterEdit)
          assertEquals(proof.resultType.nonEmpty, resultEdit)
          assertEquals(proof.bodyEdited, bodyEdit)
          assert(proof.untouchedIdentity)
          assert(proof.orderPreserved)
          assertEquals(snapshot(root), before)
        }
      }

  test("public facade remains closed to exact Option[String] STypeApply"):
    withContext:
      val captured = publicCapture(service("value"))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter.ref, optionString))

      ExistingClassUntypedRewrite(captured, plan) match
        case Left(problem) =>
          assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
          assert(problem.detail.contains("U040 SEMANTIC_TYPE_FAMILY_UNSUPPORTED"), clues(problem))
          assert(problem.detail.contains("STypeApply"), clues(problem))
        case Right(value) => fail("U072 must not deliver public Option[String] support: " + value)

  test("canonical declaration identities exist as a distinct model but current lowering does not admit them"):
    val pkg = (name: String) => ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    val optionId = ResolvedTypeNameId(Vector(pkg("scala")), "Option")
    val stringId = ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
    val canonical = TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(optionId),
      List(TypeNormalForm.STypeResolved(stringId))
    )

    assertNotEquals(canonical, optionString)
    assertEquals(optionId.canonicalSource, "scala.Option")
    assertEquals(stringId.canonicalSource, "java.lang.String")
    TypeUntypedLowering.lower(canonical) match
      case Left(problem) =>
        assertEquals(problem.code, "UNSUPPORTED_SEMANTIC_VALUE")
        assert(problem.detail.contains("STypeResolved"), clues(problem))
      case Right(value) => fail("U072 must not admit canonical resolved constructors in production: " + value)

  test("malformed provenance candidate mismatch stale plan and foreign capture fail without partial mutation"):
    withContext:
      val firstRoot = service("value")
      val first = publicCapture(firstRoot)
      val second = publicCapture(service("value"))
      val descriptor = capturedDescriptor(first)
      val generated = oracleRight(positionCandidate(descriptor.parameterClauses.head.head.tpt, Candidate.Unqualified))

      assertOracleCode(
        validatePositionedForTest(generated.generated, generated.generated, generated.oldSite, Candidate.Unqualified),
        "POSITION_EVIDENCE_INVALID"
      )
      assertOracleCode(
        validatePositionedForTest(generated.generated, generated.positioned, generated.oldSite, Candidate.Rooted),
        "POSITION_EVIDENCE_INVALID"
      )
      given dotty.tools.dotc.util.SourceFile = NoSource
      val sourceFreeChildren = untpd.AppliedTypeTree(
        untpd.Ident(typeName("Option")),
        List(untpd.Ident(typeName("String")))
      ).cloneIn(generated.oldSite.source).withSpan(generated.oldSite.span)
      assertOracleCode(
        validatePositionedForTest(generated.generated, sourceFreeChildren, generated.oldSite, Candidate.Unqualified),
        "POSITION_EVIDENCE_INVALID"
      )

      val firstMethod = publicRight(first.method(first.members.head.ref))
      val firstParameter = firstMethod.parameterClauses.head.parameters.head
      val plan = publicRight(first.emptyPlan.replaceParameterType(firstParameter.ref, optionString))
      val before = snapshot(firstRoot)
      assertOracleCode(U072CanonicalOptionConstructorAuthorityOracle(second, plan, Candidate.RootedPair), "FOREIGN_PLAN")
      assertEquals(snapshot(firstRoot), before)

      val foreignMethod = publicRight(second.method(second.members.head.ref))
      val foreignParameter = foreignMethod.parameterClauses.head.parameters.head
      assertPublicCode(first.emptyPlan.replaceParameterType(foreignParameter.ref, optionString), "SELECTION_FAILED")
      assertPublicCode(first.emptyPlan.replaceBody(foreignMethod.ref, TermShape.Literal("0")), "SELECTION_FAILED")

  test("G1 G2 direct Option or String Type members and parameter wrappers remain fail-closed"):
    withContext:
      val fixtures = Vector(
        ("class G1:\n  def echo[Option](value: String): String = value\n", "LOCAL_TYPE_BINDER_UNSUPPORTED"),
        ("class G2:\n  def echo[A](value: A): A = value\n", "LOCAL_TYPE_BINDER_UNSUPPORTED"),
        ("class LocalString:\n  def echo[String](value: Int): Int = value\n", "LOCAL_TYPE_BINDER_UNSUPPORTED"),
        ("class OwnerOption:\n  type Option[A] = List[A]\n  def echo(value: String): String = value\n", "OPTION_SHADOWING_UNSUPPORTED"),
        ("class OwnerString:\n  type String = Int\n  def echo(value: Int): Int = value\n", "OPTION_SHADOWING_UNSUPPORTED"),
        ("class ByName:\n  def echo(value: => String): String = value\n", "PARAMETER_WRAPPER_UNSUPPORTED"),
        ("class Repeated:\n  def echo(value: String*): String = value.head\n", "PARAMETER_WRAPPER_UNSUPPORTED")
      )
      fixtures.foreach { (source, expected) =>
        val captured = publicCapture(parseClass(source))
        val methodIndex = captured.members.indexWhere(_.kind == MemberKind.Method)
        val method = publicRight(captured.method(captured.members(methodIndex).ref))
        val parameter = method.parameterClauses.head.parameters.head
        val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter.ref, optionString))
        assertOracleCode(U072CanonicalOptionConstructorAuthorityOracle(captured, plan, Candidate.RootedPair), expected)
      }

  private def service(body: String)(using Context): untpd.TypeDef =
    parseClass(
      s"""class Service:
         |  def echo(value: String): String = $body
         |  val untouchedBefore: Int = 1
         |  val untouchedAfter: Int = 2
         |""".stripMargin
    )

  private def publicCapture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def capturedDescriptor(capture: Capture)(using Context): ExistingUntpdOrdinaryMethodDescriptor.Descriptor =
    ExistingUntpdOrdinaryMethodDescriptor.capture(capture.exact, 0)
      .fold(problem => fail(problem.message), identity)

  private def snapshot(root: untpd.TypeDef)(using Context): Vector[(untpd.Tree, Any, Any)] =
    allTrees(root).map(tree => (tree, tree.source, tree.span))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def assertOracleCode[A](result: Either[Error, A], expected: String): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(problem))
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
    val unit = CompilationUnit("U072CanonicalOptionModelFit.scala", source)
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
