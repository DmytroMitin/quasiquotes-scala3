package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

final class ExistingRootedOptionSemanticProjectionAuthorityTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*

  test("P0-P1 certified U073 rewrite projects both rooted slots to the exact existing resolved semantic value"):
    withContext:
      val root = parseClass(fixtureSource)
      val capture = rightPublic(ExistingClassUntypedRewrite.capture(root))
      val method = rightPublic(capture.method(capture.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = rightPublic(method.parameterScope.reference(parameter.ref))
      val plan = rightPublic(for
        p1 <- capture.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)

      val certified = rightPrototype(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .rewriteAndCertify(capture, plan)
      )
      val recapture = rightPublic(ExistingClassUntypedRewrite.capture(certified.result.tree))
      val recapturedMethod = rightPublic(recapture.method(recapture.members(1).ref))
      val recapturedParameter = recapturedMethod.parameterClauses.head.parameters.head

      assertPublicProjectionBoundary(recapturedParameter.declaredType.semantic)
      assertPublicProjectionBoundary(recapturedMethod.resultType.semantic)
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .inspectUncertifiedParameter(recapture, recapturedMethod.ref, recapturedParameter.ref),
        "GENERATED_AUTHORITY_REQUIRED"
      )
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .inspectUncertifiedResult(recapture, recapturedMethod.ref),
        "GENERATED_AUTHORITY_REQUIRED"
      )
      assertEquals(
        rightPrototype(certified.projectParameter(recapture, recapturedMethod.ref, recapturedParameter.ref)),
        canonicalOptionString
      )
      assertEquals(
        rightPrototype(certified.projectResult(recapture, recapturedMethod.ref)),
        canonicalOptionString
      )

  test("P2 ordinary parser-rooted spelling is classified separately and cannot self-authorize resolved semantics"):
    withContext:
      val capture = rightPublic(ExistingClassUntypedRewrite.capture(parseClass(
        """class ParsedRooted:
          |  val before: Int = 1
          |  def echo(value: _root_.scala.Option[_root_.java.lang.String]): _root_.scala.Option[_root_.java.lang.String] = value
          |  val after: Int = 2
          |""".stripMargin
      )))
      val method = rightPublic(capture.method(capture.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head

      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .inspectUncertifiedParameter(capture, method.ref, parameter.ref),
        "ORIGINAL_ROOTED_SOURCE_UNCERTIFIED"
      )
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .inspectUncertifiedResult(capture, method.ref),
        "ORIGINAL_ROOTED_SOURCE_UNCERTIFIED"
      )
      assertPublicProjectionBoundary(parameter.declaredType.semantic)
      assertPublicProjectionBoundary(method.resultType.semantic)

  test("P3 aliases shadows missing roots and wrong owners never become the canonical resolved pair"):
    withContext:
      val cases = Vector(
        "imported Option alias" ->
          "import scala.{List as Option}\nclass C:\n  val before: Int = 1\n  def echo(value: Option[String]): Option[String] = value\n  val after: Int = 2",
        "local Option member" ->
          "class C:\n  type Option[A] = List[A]\n  def echo(value: Option[String]): Option[String] = value\n  val after: Int = 2",
        "imported String alias" ->
          "import scala.{Int as String}\nclass C:\n  val before: Int = 1\n  def echo(value: _root_.scala.Option[String]): _root_.scala.Option[String] = value\n  val after: Int = 2",
        "shadowed term Option" ->
          "class C:\n  val Option: Int = 1\n  def echo(value: Option[String]): Option[String] = value\n  val after: Int = 2",
        "shadowed scala namespace" ->
          "object aliases:\n  object scala:\n    type Option[A] = List[A]\nclass C:\n  import aliases.scala\n  def echo(value: scala.Option[java.lang.String]): scala.Option[java.lang.String] = value\n  val after: Int = 2",
        "missing root" ->
          "class C:\n  val before: Int = 1\n  def echo(value: scala.Option[java.lang.String]): scala.Option[java.lang.String] = value\n  val after: Int = 2",
        "rooted constructor bare String" ->
          "class C:\n  val before: Int = 1\n  def echo(value: _root_.scala.Option[String]): _root_.scala.Option[String] = value\n  val after: Int = 2",
        "wrong package owner" ->
          "class C:\n  val before: Int = 1\n  def echo(value: _root_.scala.collection.immutable.List[_root_.java.lang.String]): _root_.scala.collection.immutable.List[_root_.java.lang.String] = value\n  val after: Int = 2"
      )

      cases.foreach { case (label, source) =>
        val capture = rightPublic(ExistingClassUntypedRewrite.capture(parseClass(source)))
        val methodIndex = capture.members.indexWhere(_.diagnosticName.contains("echo"))
        val method = rightPublic(capture.method(capture.members(methodIndex).ref))
        val parameter = method.parameterClauses.head.parameters.head
        assertPrototypeCode(
          ExistingRootedOptionSemanticProjectionAuthorityPrototype
            .inspectUncertifiedParameter(capture, method.ref, parameter.ref),
          "CANONICAL_SHAPE_MISMATCH",
          label
        )
        assertPrototypeCode(
          ExistingRootedOptionSemanticProjectionAuthorityPrototype
            .inspectUncertifiedResult(capture, method.ref),
          "CANONICAL_SHAPE_MISMATCH",
          label
        )
      }

  test("P5 G1 primitive projection remains admitted and G2 local dependency remains fail closed"):
    withContext:
      val g1Capture = rightPublic(ExistingClassUntypedRewrite.capture(parseClass(fixtureSource)))
      val g1Method = rightPublic(g1Capture.method(g1Capture.members(1).ref))
      val g1Parameter = g1Method.parameterClauses.head.parameters.head
      assertEquals(g1Parameter.declaredType.semantic, Right(TypeNormalForm.STypeIdent("String")))
      assertEquals(g1Method.resultType.semantic, Right(TypeNormalForm.STypeIdent("String")))

      val g2Capture = rightPublic(ExistingClassUntypedRewrite.capture(parseClass(
        """class GenericService:
          |  val before: Int = 1
          |  def keep[A](value: A): A = value
          |  val after: Int = 2
          |""".stripMargin
      )))
      val g2Method = rightPublic(g2Capture.method(g2Capture.members(1).ref))
      val g2Parameter = g2Method.parameterClauses.head.parameters.head
      assertLocalDependencyBoundary(g2Parameter.declaredType.semantic)
      assertLocalDependencyBoundary(g2Method.resultType.semantic)

  test("P4-P6 certificate is capture-local and an independent body transaction preserves both raw Type graphs"):
    withContext:
      val root = parseClass(fixtureSource)
      val capture = rightPublic(ExistingClassUntypedRewrite.capture(root))
      val method = rightPublic(capture.method(capture.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = rightPublic(method.parameterScope.reference(parameter.ref))
      val plan = rightPublic(for
        p1 <- capture.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)
      val certified = rightPrototype(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype.rewriteAndCertify(capture, plan)
      )
      val recapture = rightPublic(ExistingClassUntypedRewrite.capture(certified.result.tree))
      val recapturedMethod = rightPublic(recapture.method(recapture.members(1).ref))
      val recapturedParameter = recapturedMethod.parameterClauses.head.parameters.head
      val parameterTree = selectedParameter(certified.result.tree).tpt
      val resultTree = selectedMethod(certified.result.tree).tpt

      val foreignCapture = rightPublic(ExistingClassUntypedRewrite.capture(certified.result.tree))
      val foreignMethod = rightPublic(foreignCapture.method(foreignCapture.members(1).ref))
      val foreignParameter = foreignMethod.parameterClauses.head.parameters.head
      assertPrototypeCode(
        certified.projectParameter(recapture, foreignMethod.ref, foreignParameter.ref),
        "CERTIFICATE_METHOD_MISMATCH",
        "foreign method reference"
      )
      assertPrototypeCode(
        certified.projectResult(foreignCapture, recapturedMethod.ref),
        "CERTIFICATE_METHOD_MISMATCH",
        "foreign capture"
      )
      assertPrototypeCode(
        certified.projectParameter(recapture, method.ref, parameter.ref),
        "CERTIFICATE_METHOD_MISMATCH",
        "stale first-capture references"
      )

      val otherRoot = parseClass(fixtureSource)
      val otherCapture = rightPublic(ExistingClassUntypedRewrite.capture(otherRoot))
      val otherMethod = rightPublic(otherCapture.method(otherCapture.members(1).ref))
      val otherParameter = otherMethod.parameterClauses.head.parameters.head
      val otherPlan = rightPublic(for
        p1 <- otherCapture.emptyPlan.replaceParameterType(otherParameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(otherMethod.ref, canonicalOptionString)
      yield p2)
      val otherCertified = rightPrototype(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .rewriteAndCertify(otherCapture, otherPlan)
      )
      val otherRecapture = rightPublic(ExistingClassUntypedRewrite.capture(otherCertified.result.tree))
      val otherRecapturedMethod = rightPublic(otherRecapture.method(otherRecapture.members(1).ref))
      val otherRecapturedParameter = otherRecapturedMethod.parameterClauses.head.parameters.head
      assertPrototypeCode(
        certified.projectParameter(
          otherRecapture,
          otherRecapturedMethod.ref,
          otherRecapturedParameter.ref
        ),
        "CERTIFICATE_CAPTURE_MISMATCH",
        "different rewritten owner"
      )

      rightPrototype(certified.projectParameter(recapture, recapturedMethod.ref, recapturedParameter.ref))
      rightPrototype(certified.projectResult(recapture, recapturedMethod.ref))
      val secondBody = rightPublic(recapturedMethod.parameterScope.reference(recapturedParameter.ref))
      val bodyOnly = rightPublic(ExistingClassUntypedRewrite(
        recapture,
        rightPublic(recapture.emptyPlan.replaceBody(recapturedMethod.ref, secondBody))
      ))
      assert(selectedParameter(bodyOnly.tree).tpt.eq(parameterTree))
      assert(selectedMethod(bodyOnly.tree).tpt.eq(resultTree))
      assert(!selectedMethod(bodyOnly.tree).eq(selectedMethod(certified.result.tree)))

  test("P4 trusted issuer refuses incomplete noncanonical and foreign public plans"):
    withContext:
      val capture = rightPublic(ExistingClassUntypedRewrite.capture(parseClass(fixtureSource)))
      val method = rightPublic(capture.method(capture.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head

      val parameterOnly = rightPublic(
        capture.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
      )
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .rewriteAndCertify(capture, parameterOnly),
        "CANONICAL_PLAN_REQUIRED",
        "missing result intent"
      )

      val primitivePlan = rightPublic(for
        p1 <- capture.emptyPlan.replaceParameterType(
          parameter.ref,
          TypeNormalForm.STypeIdent("String")
        )
        p2 <- p1.replaceResultType(method.ref, TypeNormalForm.STypeIdent("String"))
      yield p2)
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .rewriteAndCertify(capture, primitivePlan),
        "CANONICAL_PLAN_REQUIRED",
        "noncanonical semantic intent"
      )

      val otherCapture = rightPublic(ExistingClassUntypedRewrite.capture(parseClass(fixtureSource)))
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .rewriteAndCertify(otherCapture, parameterOnly),
        "PLAN_CAPTURE_MISMATCH",
        "foreign plan"
      )

  test("P4 forged outer sites child provenance token text typed splice and copied nodes reject before semantics"):
    withContext:
      val root = parseClass(fixtureSource)
      val originalOuter = selectedParameter(root).tpt
      val capture = rightPublic(ExistingClassUntypedRewrite.capture(root))
      val method = rightPublic(capture.method(capture.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val plan = rightPublic(for
        p1 <- capture.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
        p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
      yield p2)
      val certified = rightPrototype(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype.rewriteAndCertify(capture, plan)
      )
      val rooted = selectedParameter(certified.result.tree).tpt
      val (option, string) = rooted match
        case untpd.AppliedTypeTree(constructor, List(argument)) => constructor -> argument
        case other => fail(s"expected rooted applied Type, found $other")

      assertEquals(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .validateGeneratedCandidate(rooted, originalOuter),
        Right(())
      )

      val wrongOuter = atOuterSite(
        option,
        string,
        SourceFile.virtual("forged/Outer.scala", "Option[String]"),
        originalOuter
      )
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .validateGeneratedCandidate(wrongOuter, originalOuter),
        "OUTER_SITE_MISMATCH",
        "outer source"
      )

      val wrongOuterSpan = atOuterSite(option, string, originalOuter.source, originalOuter)
        .withSpan(originalOuter.span.withPoint(originalOuter.span.point + 1))
      assertPrototypeCode(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .validateGeneratedCandidate(wrongOuterSpan, originalOuter),
        "OUTER_SITE_MISMATCH",
        "outer span"
      )

      val wrongSourceOption = option
        .cloneIn(SourceFile.virtual("generated/Wrong.scala", "_root_.scala.Option"))
        .withSpan(option.span)
      val wrongContentOption = option
        .cloneIn(SourceFile.virtual(
          "generated/CanonicalResolvedOptionStringMethodType.scala",
          "_root_.scala.List[_root_.java.lang.String]"
        ))
        .withSpan(option.span)
      val missingSourceOption = option.cloneIn(NoSource).withSpan(option.span)
      val swappedSpanOption = option.cloneIn(option.source).withSpan(string.span)
      val copiedOption = option.cloneIn(option.source).withSpan(option.span)
      val wrongToken = untpd.Ident(typeName("List"))
        .cloneIn(option.source)
        .withSpan(option.span)
      val typedSplice = untpd.TypedSplice(option)
        .cloneIn(option.source)
        .withSpan(option.span)
      val symbol = newSymbol(NoSymbol, typeName("InjectedType"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(typeName("Option"))
        .withType(symbol.typeRef)
        .cloneIn(option.source)
        .withSpan(option.span)

      val invalidChildren = Vector(
        "wrong child source" -> atOuterSite(wrongSourceOption, string, originalOuter.source, originalOuter),
        "wrong child content" -> atOuterSite(wrongContentOption, string, originalOuter.source, originalOuter),
        "missing child source" -> atOuterSite(missingSourceOption, string, originalOuter.source, originalOuter),
        "swapped child span" -> atOuterSite(swappedSpanOption, string, originalOuter.source, originalOuter),
        "wrong token text" -> atOuterSite(wrongToken, string, originalOuter.source, originalOuter),
        "typed splice" -> atOuterSite(typedSplice, string, originalOuter.source, originalOuter),
        "symbol bearing" -> atOuterSite(symbolBearing, string, originalOuter.source, originalOuter),
        "empty tree" -> untpd.EmptyTree
      )
      invalidChildren.foreach { case (label, candidate) =>
        assertPrototypeCode(
          ExistingRootedOptionSemanticProjectionAuthorityPrototype
            .validateGeneratedCandidate(candidate, originalOuter),
          "GENERATED_PROVENANCE_INVALID",
          label
        )
        assertPrototypeCode(
          certified.validateParameterCandidate(candidate),
          "CERTIFICATE_GRAPH_MISMATCH",
          label
        )
      }

      val copiedCandidate = atOuterSite(copiedOption, string, originalOuter.source, originalOuter)
      assertEquals(
        ExistingRootedOptionSemanticProjectionAuthorityPrototype
          .validateGeneratedCandidate(copiedCandidate, originalOuter),
        Right(())
      )
      assertPrototypeCode(
        certified.validateParameterCandidate(copiedCandidate),
        "CERTIFICATE_GRAPH_MISMATCH",
        "copy-injected child"
      )

  private def atOuterSite(
      constructor: untpd.Tree,
      argument: untpd.Tree,
      source: SourceFile,
      oldOuter: untpd.Tree
  )(using Context): untpd.Tree =
    given SourceFile = NoSource
    untpd.AppliedTypeTree(constructor, argument :: Nil)
      .cloneIn(source)
      .withSpan(oldOuter.span)

  private def selectedMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]

  private def selectedParameter(root: untpd.TypeDef)(using Context): untpd.ValDef =
    selectedMethod(root).paramss.head.head.asInstanceOf[untpd.ValDef]

  private val fixtureSource =
    """class ProjectionService:
      |  val before: Int = 1
      |  def echo(value: String): String = value
      |  val after: Int = 2
      |""".stripMargin

  private val canonicalOptionString: TypeNormalForm =
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(
        ResolvedTypeNameId(Vector(packageOwner("scala")), "Option")
      ),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(
            Vector(packageOwner("java"), packageOwner("lang")),
            "String"
          )
        )
      )
    )

  private def packageOwner(name: String): ResolvedTypeOwnerSegment =
    ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)

  private def assertPublicProjectionBoundary(
      result: Either[Failure, TypeNormalForm]
  ): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
      assert(problem.detail.contains("U038 UNSUPPORTED_SELECTED_TYPE"), clues(problem))
    case Right(value) => fail(s"public rooted projection unexpectedly returned ${value.render}")

  private def assertLocalDependencyBoundary(
      result: Either[Failure, TypeNormalForm]
  ): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
      assert(problem.detail.contains("LOCAL_TYPE_BINDER_DEPENDENCY_UNSUPPORTED"), clues(problem))
    case Right(value) => fail(s"local dependent Type unexpectedly returned ${value.render}")

  private def assertPrototypeCode[A](
      result: Either[ExistingRootedOptionSemanticProjectionAuthorityPrototype.Error, A],
      expected: String,
      label: String = ""
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected, clues(label, problem))
    case Right(value) => fail(s"$label expected $expected, found success $value")

  private def rightPublic[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def rightPrototype[A](
      result: Either[ExistingRootedOptionSemanticProjectionAuthorityPrototype.Error, A]
  ): A = result.fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U075SemanticProjectionAuthority.scala", source)
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
