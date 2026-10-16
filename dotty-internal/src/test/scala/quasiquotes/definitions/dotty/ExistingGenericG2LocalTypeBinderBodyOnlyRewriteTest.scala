package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{DefinitionName, DefinitionSemanticError, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

final class ExistingGenericG2LocalTypeBinderBodyOnlyRewriteTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("canonical G2 exposes only its term clause and keeps dependent Type semantics opaque"):
    withContext:
      val root = parseClass(canonical("GenericDependent", "x"))
      val rawMethod = directMembers(root).head.asInstanceOf[untpd.DefDef]
      val rawParameter = rawMethod.paramss(1).head.asInstanceOf[untpd.ValDef]
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))

      assertEquals(method.parameterClauses.map(_.kind.code), Vector("ORDINARY"))
      assertEquals(method.parameterClauses.map(_.parameters.map(_.diagnosticName)), Vector(Vector("x")))
      val parameter = method.parameterClauses.head.parameters.head
      assertEquals((parameter.clauseIndex, parameter.parameterIndex), (0, 0))
      assert(parameter.identity.sameObjectAs(new ExactIdentity(rawParameter)))
      assert(parameter.declaredType.identity.sameObjectAs(new ExactIdentity(rawParameter.tpt)))
      assert(method.resultType.identity.sameObjectAs(new ExactIdentity(rawMethod.tpt)))
      assertPublicCode(parameter.declaredType.semantic, "UNSUPPORTED_STRUCTURE")
      assertPublicCode(method.resultType.semantic, "UNSUPPORTED_STRUCTURE")
      assert(method.parameterScope.binder(parameter.ref).isRight)
      assert(method.parameterScope.reference(parameter.ref).isRight)
      assertEquals(method.body.semantic, method.parameterScope.reference(parameter.ref))

  test("G2 body-only rewrite preserves every dependent and opaque declaration identity"):
    withContext:
      val root = parseClass(canonical("GenericBodyOnly", "throw new RuntimeException(\"baseline\")"))
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val originalMethod = originalTemplate.body.head.asInstanceOf[untpd.DefDef]
      val originalTypeClause = originalMethod.paramss.head
      val originalTypeParameter = originalTypeClause.head.asInstanceOf[untpd.TypeDef]
      val originalBounds = originalTypeParameter.rhs
      val originalParameter = originalMethod.paramss(1).head.asInstanceOf[untpd.ValDef]
      val originalParameterType = originalParameter.tpt
      val originalResultType = originalMethod.tpt
      val originalSibling = originalTemplate.body(1)
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val reference = publicRight(method.parameterScope.reference(parameter.ref))
      val plan = publicRight(captured.emptyPlan.replaceBody(method.ref, reference))
      val applied = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewrittenTemplate = applied.tree.rhs.asInstanceOf[untpd.Template]
      val rewrittenMethod = rewrittenTemplate.body.head.asInstanceOf[untpd.DefDef]

      assert(applied.changed)
      assert(!applied.tree.eq(root))
      assert(!rewrittenMethod.eq(originalMethod))
      assert(rewrittenMethod.paramss.head.eq(originalTypeClause))
      assert(rewrittenMethod.paramss.head.head.eq(originalTypeParameter))
      assert(rewrittenMethod.paramss.head.head.asInstanceOf[untpd.TypeDef].rhs.eq(originalBounds))
      assert(rewrittenMethod.paramss(1).head.eq(originalParameter))
      assert(rewrittenMethod.paramss(1).head.asInstanceOf[untpd.ValDef].tpt.eq(originalParameterType))
      assert(rewrittenMethod.tpt.eq(originalResultType))
      assert(!rewrittenMethod.rhs.eq(originalMethod.rhs))
      assert(rewrittenMethod.mods.eq(originalMethod.mods))
      assertEquals((rewrittenMethod.source, rewrittenMethod.span), (originalMethod.source, originalMethod.span))
      assert(rewrittenTemplate.body(1).eq(originalSibling))
      assert(applied.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(applied.tree)

  test("G2 dependent Type edits fail during atomic apply, alone and with valid owner edits"):
    withContext:
      val root = parseClass(
        "class GenericAtomic:\n" +
          "  def keep[A](x: A): A = x\n" +
          "  val obsolete: Int = 1\n" +
          "  val untouched: Int = 7\n"
      )
      val originalMembers = directMembers(root)
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      val parameter = method.parameterClauses.head.parameters.head
      val reference = publicRight(method.parameterScope.reference(parameter.ref))

      val parameterOnly = publicRight(captured.emptyPlan.replaceParameterType(parameter.ref, IntType))
      val resultOnly = publicRight(captured.emptyPlan.replaceResultType(method.ref, IntType))
      val both = publicRight(parameterOnly.replaceResultType(method.ref, IntType))
      Vector(parameterOnly, resultOnly, both).foreach { plan =>
        assertPublicCode(ExistingClassUntypedRewrite(captured, plan), "UNSUPPORTED_STRUCTURE")
      }

      val combined = publicRight(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, IntType)
        p2 <- p1.replaceResultType(method.ref, IntType)
        p3 <- p2.replaceBody(method.ref, reference)
        p4 <- p3.omit(captured.members(1).ref)
        p5 <- p4.append(generatedValue("bonus", "5"), "generated/U069Atomic.scala")
      yield p5)
      assertEquals(combined.editCount, 5)
      assertPublicCode(ExistingClassUntypedRewrite(captured, combined), "UNSUPPORTED_STRUCTURE")
      assert(directMembers(root).zip(originalMembers).forall((left, right) => left.eq(right)))
      assertPreTyperClean(root)

  test("G2 rejects literal and free-identifier bodies during apply while ordinary methods retain them"):
    withContext:
      val root = parseClass(canonical("GenericBodyBoundary", "x"))
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val method = publicRight(captured.method(captured.members.head.ref))
      Vector[TermShape](
        TermShape.Literal("\"not-A\""),
        TermShape.Identifier("helper", false)
      ).foreach { replacement =>
        val plan = publicRight(captured.emptyPlan.replaceBody(method.ref, replacement))
        assertPublicCode(ExistingClassUntypedRewrite(captured, plan), "UNSUPPORTED_STRUCTURE")
      }

      val ordinaryRoot = parseClass("class OrdinaryBodyBoundary:\n  def keep(x: String): String = x\n")
      val ordinary = publicRight(ExistingClassUntypedRewrite.capture(ordinaryRoot))
      val ordinaryMethod = publicRight(ordinary.method(ordinary.members.head.ref))
      val ordinaryPlan = publicRight(ordinary.emptyPlan.replaceBody(
        ordinaryMethod.ref,
        TermShape.Literal("\"ok\"")
      ))
      assert(ExistingClassUntypedRewrite(ordinary, ordinaryPlan).isRight)

  test("G2 private projection and Type preparation report dependency-specific failures"):
    withContext:
      val root = parseClass(canonical("GenericPrivateFailures", "x"))
      val exact = ExistingUntpdClassMemberFilter.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0).fold(
        problem => fail(problem.message),
        identity
      )
      val parameter = descriptor.parameterClauses.head.head
      assertPrivateCode(
        ExistingUntpdOrdinaryMethodTypeSlotProjection.projectParameter(descriptor, parameter),
        "LOCAL_TYPE_BINDER_DEPENDENCY_UNSUPPORTED"
      )
      assertPrivateCode(
        ExistingUntpdOrdinaryMethodTypeSlotProjection.projectResult(descriptor),
        "LOCAL_TYPE_BINDER_DEPENDENCY_UNSUPPORTED"
      )
      assertPreparationCode(
        ExistingUntpdOrdinaryMethodTypeEditPreparation.prepareParameterType(descriptor, parameter, IntType),
        "SEMANTIC_TYPE_LOCAL_BINDER_DEPENDENCY_UNSUPPORTED"
      )
      assertPreparationCode(
        ExistingUntpdOrdinaryMethodTypeEditPreparation.prepareResultType(descriptor, IntType),
        "SEMANTIC_TYPE_LOCAL_BINDER_DEPENDENCY_UNSUPPORTED"
      )

  test("G2 no-op stays exact while adjacent generic dependency shapes remain unsupported"):
    withContext:
      val root = parseClass(canonical("GenericNoOp", "x"))
      val captured = publicRight(ExistingClassUntypedRewrite.capture(root))
      val noOp = publicRight(ExistingClassUntypedRewrite(captured, captured.emptyPlan))
      assert(!noOp.changed)
      assert(noOp.tree.eq(root))
      assert(noOp.directMemberIdentities.zip(captured.members).forall((actual, original) =>
        actual.sameObjectAs(original.identity)
      ))

      val rejected = Vector(
        "def keep[A](x: A): Int = 1",
        "def keep[A](x: Int): A = ???",
        "def keep[A](x: List[A]): List[A] = x",
        "def keep[A](x: A)(y: A): A = x",
        "def keep[A](using x: A): A = x",
        "def keep[A](x: => A): A = x",
        "def keep[A](x: A*): A = x.head",
        "def keep[A](x: A = ???): A = x",
        "def keep[A <: AnyVal](x: A): A = x",
        "def keep[A, B](x: A): A = x",
        "def keep[Int](x: Int): Int = x",
        "def keep[String](x: String): String = x",
        "def keep[Boolean](x: Boolean): Boolean = x",
        "def keep[AnyVal](x: AnyVal): AnyVal = x"
      )
      rejected.zipWithIndex.foreach { (definition, index) =>
        val adjacent = publicRight(ExistingClassUntypedRewrite.capture(parseClass(
          s"class Rejected$index:\n  $definition\n"
        )))
        assertPublicCode(adjacent.method(adjacent.members.head.ref), "UNSUPPORTED_STRUCTURE")
      }

  test("G2 private dependency evidence rejects null foreign swapped and forged metadata"):
    withContext:
      val root = parseClass(canonical("GenericEvidence", "x"))
      val exact = ExistingUntpdClassMemberFilter.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0).fold(
        problem => fail(problem.message),
        identity
      )
      val dependency = descriptor.localTypeDependency.getOrElse(fail("G2 dependency evidence missing"))
      val opaque = descriptor.opaqueTypeClause.getOrElse(fail("opaque Type clause missing"))
      assert(dependency.typeParameter.eq(opaque.typeParameter))
      assert(dependency.parameterType.eq(descriptor.parameterClauses.head.head.tpt))
      assert(dependency.resultType.eq(descriptor.resultType))
      assertEquals(dependency.binderName, "A")

      val foreignRoot = parseClass(canonical("ForeignEvidence", "value").replace("(x: A)", "(value: Element)").replace("[A]", "[Element]").replace(": A =", ": Element ="))
      val foreignExact = ExistingUntpdClassMemberFilter.capture(foreignRoot).fold(
        problem => fail(problem.message),
        identity
      )
      val foreign = ExistingUntpdOrdinaryMethodDescriptor.capture(foreignExact, 0).fold(
        problem => fail(problem.message),
        identity
      ).localTypeDependency.getOrElse(fail("foreign G2 dependency evidence missing"))

      val tampered = Vector(
        descriptor.copy(opaqueTypeClause = Some(null)),
        descriptor.copy(localTypeDependency = null),
        descriptor.copy(localTypeDependency = Some(null)),
        descriptor.copy(localTypeDependency = None),
        descriptor.copy(localTypeDependency = Some(foreign)),
        descriptor.copy(localTypeDependency = Some(dependency.copy(parameterType = dependency.resultType))),
        descriptor.copy(localTypeDependency = Some(dependency.copy(resultType = dependency.parameterType))),
        descriptor.copy(localTypeDependency = Some(dependency.copy(typeParameter = foreign.typeParameter))),
        descriptor.copy(localTypeDependency = Some(dependency.copy(binderName = "Element")))
      )
      tampered.foreach { value =>
        assertEquals(
          ExistingUntpdOrdinaryMethodDescriptor.validate(value).left.toOption.map(_.code),
          Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
        )
      }

  private def canonical(name: String, body: String): String =
    s"class $name:\n  def keep[A](x: A): A = $body\n  val untouched: Int = 7\n"

  private def generatedValue(label: String, literal: String): SemanticDefinition =
    semantic(SemanticDefinition.immutableValue(
      semantic(DefinitionName.fromSource(label)),
      IntType,
      TermShape.Literal(literal)
    ))

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U069ExistingGenericG2.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def semantic[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail(s"expected $expected, found success $success")

  private def assertPrivateCode[A](
      value: Either[ExistingUntpdOrdinaryMethodTypeSlotProjection.Error, A],
      expected: String
  ): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail(s"expected $expected, found success $success")

  private def assertPreparationCode[A](
      value: Either[ExistingUntpdOrdinaryMethodTypeEditPreparation.Error, A],
      expected: String
  ): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem))
      case Right(success) => fail(s"expected $expected, found success $success")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
