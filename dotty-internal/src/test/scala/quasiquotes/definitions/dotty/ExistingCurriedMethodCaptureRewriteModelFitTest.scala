package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBinder, TermBindingCategory, TermBindingInternals, TermShapeBindingView}
import quasiquotes.types.TypeNormalForm

class ExistingCurriedMethodCaptureRewriteModelFitTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodDescriptor.Descriptor

  test("test-only exact descriptor carrier retains two ordinary clauses") {
    withContext {
      val root = parseClass(
        "class ExistingCurried:\n  def combine(a: Int)(b: Int): Int = a + b\n"
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val descriptor = ExistingCurriedMethodCaptureRewriteModelFitProbe
        .assemble(captured, 0)
        .fold(message => fail(message), identity)

      assert(descriptor.captured.eq(captured))
      assertEquals(descriptor.memberIndex, 0)
      assert(descriptor.method.eq(captured.members.head.tree))
      assertEquals(descriptor.parameterClauses.map(_.size), Vector(1, 1))
      assert(descriptor.parameterClauses(0)(0).tree.eq(descriptor.method.paramss(0).head))
      assert(descriptor.parameterClauses(1)(0).tree.eq(descriptor.method.paramss(1).head))
      assert(descriptor.resultType.eq(descriptor.method.tpt))
      assert(descriptor.rhs.eq(descriptor.method.rhs))
    }
  }

  test("normal existing curried methods have stable canonical and renamed raw topology") {
    withContext {
      val fixtures = Vector(
        ("ExistingCurried", "combine", "a", "b"),
        ("RenamedCurried", "merge", "left", "right")
      )
      fixtures.foreach { (ownerName, methodName, firstName, secondName) =>
        val methodSource =
          s"def $methodName($firstName: Int)($secondName: Int): Int = $firstName + $secondName"
        val source = s"class $ownerName:\n  $methodSource\n"
        val root = parseClass(source)
        val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

        assertEquals(method.name.toString, methodName)
        assertEquals(method.mods.flags, Flags.Method)
        assert(!method.paramss.flatten.exists(_.isInstanceOf[untpd.TypeDef]))
        method.paramss match
          case List(List(first: untpd.ValDef), List(second: untpd.ValDef)) =>
            Vector(first -> firstName, second -> secondName).foreach { (parameter, expectedName) =>
              assertEquals(parameter.name.toString, expectedName)
              assertEquals(parameter.mods.flags, Flags.Param)
              assertSourceSlice(parameter, s"$expectedName: Int")
              assertSourceSlice(parameter.tpt, "Int")
              assertTypeIdent(parameter.tpt, "Int")
              assert(parameter.rhs.isEmpty)
            }
          case other => fail(s"expected two ordinary unary clauses, found $other")
        assertTypeIdent(method.tpt, "Int")
        method.rhs match
          case untpd.InfixOp(untpd.Ident(first), untpd.Ident(operator), untpd.Ident(second)) =>
            assertEquals(first.toString, firstName)
            assertEquals(operator.toString, "+")
            assertEquals(second.toString, secondName)
          case other => fail(s"expected direct infix RHS, found $other")
        assertSourceSlice(method, methodSource)
        assertSourceSlice(method.tpt, "Int")
        assertSourceSlice(method.rhs, s"$firstName + $secondName")
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        val parameters = method.paramss.flatten.collect { case value: untpd.ValDef => value }
        val capturedSites = Vector(method, method.tpt, method.rhs) ++
          parameters.toVector.flatMap(parameter => Vector(parameter, parameter.tpt))
        assert(capturedSites.forall(_.source.exists))
        assertEquals(capturedSites.map(_.source.path).distinct, Vector("U052ExistingCurried.scala"))
        assert(capturedSites.forall(_.span.exists))
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  test("current private and public admission fail curried methods while one-clause controls succeed") {
    withContext {
      val curried = parseClass(
        "class CurriedBoundary:\n  def combine(a: Int)(b: Int): Int = a + b\n"
      )
      val captured = capture(curried)
      val privateFailure = ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0).left.toOption.get
      assertEquals(privateFailure.code, "UNSUPPORTED_PARAMETER_TOPOLOGY")

      val publicCapture = ExistingClassUntypedRewrite.capture(curried).fold(
        problem => fail(problem.message),
        identity
      )
      val publicFailure = publicCapture.method(publicCapture.members.head.ref).left.toOption.get
      assertEquals(publicFailure.code, "UNSUPPORTED_STRUCTURE")
      assert(publicFailure.detail.contains("UNSUPPORTED_PARAMETER_TOPOLOGY"))

      val controls = Vector(
        "class One:\n  def f(x: Int): Int = x\n" -> Vector(1),
        "class Two:\n  def f(x: Int, y: Int): Int = x + y\n" -> Vector(2)
      )
      controls.foreach { (source, expectedSizes) =>
        val root = parseClass(source)
        val owner = capture(root)
        val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0)
          .fold(problem => fail(problem.message), identity)
        assertEquals(descriptor.parameterClauses.map(_.size), expectedSizes)
        val public = ExistingClassUntypedRewrite.capture(root).fold(
          problem => fail(problem.message),
          identity
        )
        val view = public.method(public.members.head.ref).fold(
          problem => fail(problem.message),
          identity
        )
        assertEquals(view.parameterClauses.map(_.parameters.size), expectedSizes)
      }
    }
  }

  test("all downstream private entry points stop at the same current descriptor admission gate") {
    withContext {
      val synthetic = syntheticDescriptor(
        "class DownstreamGate:\n  def combine(a: AnyVal)(b: AnyVal): AnyVal = a\n"
      )
      val first = synthetic.parameterClauses(0)(0)

      assertEquals(
        ExistingUntpdOrdinaryMethodDescriptor.validate(synthetic).left.toOption.map(_.code),
        Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
      )
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeSlotProjection.projectParameter(synthetic, first).left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeSlotProjection.projectResult(synthetic).left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
      assertEquals(
        ExistingUntpdOrdinaryMethodRhsTermProjection.projectRhs(synthetic).left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareParameterType(synthetic, first, TypeNormalForm.STypeIdent("Int"))
          .left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeEditPreparation
          .prepareResultType(synthetic, TypeNormalForm.STypeIdent("Int"))
          .left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
      var builderCalled = false
      val body = ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(synthetic) { _ =>
        builderCalled = true
        Right(TermShape.Literal("0"))
      }
      assertEquals(body.left.toOption.map(_.code), Some("INVALID_DESCRIPTOR"))
      assert(!builderCalled)
      assertEquals(
        ExistingUntpdOrdinaryMethodReconstruction
          .reconstructMethod(synthetic, Vector.empty, None, None)
          .left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
    }
  }

  test("Core persistent parameters preserve clause positions and reject foreign graphs") {
    val parameters = TermBindingInternals.persistentParameters(Vector(Vector("a"), Vector("b")))
      .fold(problem => fail(problem.message), identity)
    val first = parameters.binderAt(0, 0).fold(problem => fail(problem.message), identity)
    val second = parameters.binderAt(1, 0).fold(problem => fail(problem.message), identity)
    val firstReference = parameters.referenceAt(0, 0).fold(problem => fail(problem.message), identity)
    val secondReference = parameters.referenceAt(1, 0).fold(problem => fail(problem.message), identity)

    assertNotEquals(first, second)
    assertEquals(boundBinder(firstReference), first)
    assertEquals(boundBinder(secondReference), second)
    assertEquals(parameters.validateDefinitionBody(Vector(1, 1), firstReference), Right(firstReference))
    assertEquals(parameters.validateDefinitionBody(Vector(1, 1), secondReference), Right(secondReference))

    val foreign = TermBindingInternals.persistentParameters(Vector(Vector("a"), Vector("b")))
      .fold(problem => fail(problem.message), identity)
    val foreignReference = foreign.referenceAt(0, 0).fold(problem => fail(problem.message), identity)
    assertEquals(
      parameters.validateDefinitionBody(Vector(1, 1), foreignReference).left.toOption.map(_.code),
      Some("TERM_BINDER_SCOPE_MISMATCH")
    )

    val duplicateNames = TermBindingInternals.persistentParameters(Vector(Vector("same"), Vector("same")))
      .fold(problem => fail(problem.message), identity)
    assertNotEquals(
      duplicateNames.binderAt(0, 0).fold(problem => fail(problem.message), identity),
      duplicateNames.binderAt(1, 0).fold(problem => fail(problem.message), identity)
    )
  }

  test("parser admits duplicate cross-clause spelling and opaque Types remain exact in the test carrier") {
    withContext {
      val duplicate = syntheticDescriptor(
        "class Duplicate:\n  def f(same: Int)(same: Int): Int = same\n"
      )
      assertEquals(
        duplicate.parameterClauses.map(_.map(_.diagnosticName)),
        Vector(Vector("same"), Vector("same"))
      )

      val opaque = syntheticDescriptor(
        "class Opaque:\n  def f(value: domain.Opaque)(count: Int): Int = count\n"
      )
      assert(opaque.parameterClauses(0)(0).tpt.isInstanceOf[untpd.Select])
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeSlotProjection
          .projectParameter(opaque, opaque.parameterClauses(0)(0))
          .left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
    }
  }
  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U052ExistingCurried.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.TypeDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def capture(root: untpd.TypeDef)(using Context): ExistingUntpdClassMemberFilter.Capture =
    ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)

  private def syntheticDescriptor(source: String)(using Context): Descriptor =
    val owner = capture(parseClass(source))
    ExistingCurriedMethodCaptureRewriteModelFitProbe.assemble(owner, 0)
      .fold(message => fail(message), identity)

  private def assertTypeIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Type Ident($expected), found $other")

  private def assertSourceSlice(tree: untpd.Tree, expected: String): Unit =
    assert(tree.source.exists)
    assert(tree.span.exists)
    assertEquals(
      tree.source.content.slice(tree.span.start, tree.span.end).mkString,
      expected
    )

  private def boundBinder(shape: TermShape): TermBinder =
    val view = TermShapeBindingView.inspect(shape).fold(problem => fail(problem.message), identity)
    assertEquals(view.category, TermBindingCategory.BoundReference)
    view.boundReference.getOrElse(fail("missing bound-reference view")).binder
  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

private object ExistingCurriedMethodCaptureRewriteModelFitProbe:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}

  def assemble(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[String, Descriptor] =
    for
      owner <- Option(captured).toRight("capture required")
      member <- Option(owner.members)
        .flatMap(_.lift(memberIndex))
        .toRight(s"member index $memberIndex was not captured")
      method <- member.tree match
        case value: untpd.DefDef => Right(value)
        case _ => Left("selected member is not a method")
      clauses <- method.paramss match
        case List(List(first: untpd.ValDef), List(second: untpd.ValDef)) =>
          Right(Vector(Vector(first), Vector(second)))
        case other => Left(s"expected two ordinary unary clauses, found $other")
      parameters = clauses.map(_.map { parameter =>
        Parameter(parameter, parameter.tpt, parameter.name.toString)
      })
    yield Descriptor(
      owner,
      memberIndex,
      method,
      method.name.toString,
      parameters,
      method.tpt,
      method.rhs
    )
