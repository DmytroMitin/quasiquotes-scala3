package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape
import quasiquotes.terms.{ConstructedTerm, TermBindingInternals}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.dotty.TypeUntypedLowering

class ExistingZeroParameterMethodCaptureRewriteModelFitTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodDescriptor.Descriptor

  test("test-only carriers retain distinct zero-clause and one-empty-clause topology") {
    withContext {
      val parameterless = syntheticDescriptor("class ParameterlessCarrier:\n  def value: Int = 41\n")
      val emptyParen = syntheticDescriptor("class EmptyParenCarrier:\n  def value(): Int = 41\n")

      assertEquals(parameterless.method.paramss, Nil)
      assertEquals(parameterless.parameterClauses, Vector.empty)
      assertEquals(emptyParen.method.paramss.map(_.size), List(0))
      assertEquals(emptyParen.parameterClauses.map(_.size), Vector(0))
      Vector(parameterless, emptyParen).foreach { descriptor =>
        assert(descriptor.method.eq(descriptor.captured.members(descriptor.memberIndex).tree))
        assert(descriptor.resultType.eq(descriptor.method.tpt))
        assert(descriptor.rhs.eq(descriptor.method.rhs))
      }
    }
  }

  test("parameterless and empty-paren methods have stable canonical and renamed raw topology") {
    withContext {
      val fixtures = Vector(
        ("Parameterless", "value", false),
        ("RenamedParameterless", "answer", false),
        ("EmptyParen", "value", true),
        ("RenamedEmptyParen", "answer", true)
      )
      fixtures.foreach { (ownerName, methodName, hasEmptyParenClause) =>
        val parameterText = if hasEmptyParenClause then "()" else ""
        val methodSource = s"def $methodName$parameterText: Int = 41"
        val root = parseClass(s"class $ownerName:\n  $methodSource\n")
        val method = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

        assertEquals(method.name.toString, methodName)
        assertEquals(method.mods.flags, Flags.Method)
        if hasEmptyParenClause then assertEquals(method.paramss.map(_.size), List(0))
        else assertEquals(method.paramss, Nil)
        assert(!method.paramss.flatten.exists(_.isInstanceOf[untpd.TypeDef]))
        assertTypeIdent(method.tpt, "Int")
        method.rhs match
          case number: untpd.Number => assertEquals(number.digits, "41")
          case other => fail(s"expected Number(41), found $other")
        assertSourceSlice(method, methodSource)
        assertSourceSlice(method.tpt, "Int")
        assertSourceSlice(method.rhs, "41")
        val capturedSites = Vector(method, method.tpt, method.rhs)
        assert(capturedSites.forall(_.source.exists))
        assertEquals(capturedSites.map(_.source.path).distinct, Vector("U053ExistingZeroParameter.scala"))
        assert(capturedSites.forall(_.span.exists))
        val trees = ExistingUntpdClassMemberFilter.allTrees(root)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  test("current private and public admission reject both zero-parameter forms while controls succeed") {
    withContext {
      val rejected = Vector(
        "class ParameterlessBoundary:\n  def value: Int = 41\n",
        "class EmptyParenBoundary:\n  def value(): Int = 41\n"
      )
      rejected.foreach { source =>
        val root = parseClass(source)
        val captured = capture(root)
        val privateFailure = ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0).left.toOption.get
        assertEquals(privateFailure.code, "UNSUPPORTED_PARAMETER_TOPOLOGY")

        val publicCapture = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
        val publicFailure = publicCapture.method(publicCapture.members.head.ref).left.toOption.get
        assertEquals(publicFailure.code, "UNSUPPORTED_STRUCTURE")
        assert(publicFailure.detail.contains("UNSUPPORTED_PARAMETER_TOPOLOGY"))
      }

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
        val public = ExistingClassUntypedRewrite.capture(root).fold(problem => fail(problem.message), identity)
        val view = public.method(public.members.head.ref).fold(problem => fail(problem.message), identity)
        assertEquals(view.parameterClauses.map(_.parameters.size), expectedSizes)
      }
    }
  }

  test("all downstream private entry points stop at the same descriptor admission gate") {
    withContext {
      val descriptors = Vector(
        syntheticDescriptor("class ParameterlessGate:\n  def value: AnyVal = 41\n"),
        syntheticDescriptor("class EmptyParenGate:\n  def value(): AnyVal = 41\n")
      )
      descriptors.foreach { synthetic =>
        assertEquals(
          ExistingUntpdOrdinaryMethodDescriptor.validate(synthetic).left.toOption.map(_.code),
          Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
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
            .prepareResultType(synthetic, TypeNormalForm.STypeIdent("Int"))
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
        var builderCalled = false
        val body = ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(synthetic) { _ =>
          builderCalled = true
          Right(TermShape.Literal("42"))
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
  }

  test("Core zero-parameter scopes allocate no binders and keep free terms free") {
    val topologies = Vector(
      Vector.empty[Vector[String]] -> Vector.empty[Int],
      Vector(Vector.empty[String]) -> Vector(0)
    )
    topologies.foreach { (names, counts) =>
      val parameters = TermBindingInternals.persistentParameters(names)
        .fold(problem => fail(problem.message), identity)
      assertEquals(parameters.binderAt(0, 0).left.toOption.map(_.code), Some("TERM_BINDER_UNBOUND"))
      assertEquals(parameters.referenceAt(0, 0).left.toOption.map(_.code), Some("TERM_BINDER_UNBOUND"))

      val literal = TermShape.Literal("41")
      val free = TermShape.Identifier("helper", false)
      assertEquals(parameters.complete(literal), Right(literal))
      assertEquals(parameters.validateDefinitionBody(counts, literal), Right(literal))
      assertEquals(parameters.complete(free), Right(free))
      assertEquals(parameters.validateDefinitionBody(counts, free), Right(free))

      val foreign = TermBindingInternals.persistentParameters(Vector(Vector("x")))
        .fold(problem => fail(problem.message), identity)
      val foreignReference = foreign.referenceAt(0, 0).fold(problem => fail(problem.message), identity)
      assertEquals(
        parameters.validateDefinitionBody(counts, foreignReference).left.toOption.map(_.code),
        Some("TERM_BINDER_SCOPE_MISMATCH")
      )
    }
  }

  test("result and RHS islands remain exact independently of zero-binder semantic fit") {
    withContext {
      val fixtures = Vector(
        syntheticDescriptor("class LiteralParameterless:\n  def value: Int = 41\n"),
        syntheticDescriptor("class FreeEmptyParen:\n  def value(): Int = helper\n")
      )
      fixtures.foreach { descriptor =>
        assert(descriptor.resultType.eq(descriptor.method.tpt))
        assert(descriptor.rhs.eq(descriptor.method.rhs))
        assertEquals(descriptor.parameterClauses.flatten.size, 0)
      }
      fixtures.head.rhs match
        case number: untpd.Number => assertEquals(number.digits, "41")
        case other => fail(s"expected Number(41), found $other")
      fixtures(1).rhs match
        case untpd.Ident(name) => assertEquals(name.toString, "helper")
        case other => fail(s"expected free Ident(helper), found $other")

      val opaque = syntheticDescriptor("class OpaqueParameterless:\n  def value: domain.Opaque = helper\n")
      assert(opaque.resultType.isInstanceOf[untpd.Select])
      assert(opaque.resultType.eq(opaque.method.tpt))
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeSlotProjection.projectResult(opaque).left.toOption.map(_.code),
        Some("INVALID_DESCRIPTOR")
      )
    }
  }

  test("test-only result and body preparation reconstructs each exact source convention") {
    withContext {
      val fixtures = Vector(
        syntheticDescriptor("class PreparedParameterless:\n  def value: AnyVal = 41\n") -> List.empty[Int],
        syntheticDescriptor("class PreparedEmptyParen:\n  def value(): AnyVal = 41\n") -> List(0)
      )
      fixtures.foreach { (descriptor, expectedClauses) =>
        val prepared = ExistingZeroParameterMethodCaptureRewriteModelFitProbe
          .prepare(descriptor, TermShape.Literal("42"))
          .fold(message => fail(message), identity)
        assertEquals(prepared.method.paramss.map(_.size), expectedClauses)
        assert(!prepared.method.eq(descriptor.method))
        assert(prepared.method.tpt.eq(prepared.resultType))
        assert(prepared.method.rhs.eq(prepared.body))
        assertTypeIdent(prepared.resultType, "Int")
        prepared.body match
          case number: untpd.Number => assertEquals(number.digits, "42")
          case other => fail(s"expected Number(42), found $other")
        assert(ExistingUntpdClassMemberFilter.allTrees(prepared.method).forall(_.symbol == NoSymbol))
        assert(!ExistingUntpdClassMemberFilter.allTrees(prepared.method).exists(_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U053ExistingZeroParameter.scala", source)
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
    ExistingZeroParameterMethodCaptureRewriteModelFitProbe.assemble(owner, 0)
      .fold(message => fail(message), identity)

  private def assertTypeIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Type Ident($expected), found $other")

  private def assertSourceSlice(tree: untpd.Tree, expected: String): Unit =
    assert(tree.source.exists)
    assert(tree.span.exists)
    assertEquals(tree.source.content.slice(tree.span.start, tree.span.end).mkString, expected)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

private object ExistingZeroParameterMethodCaptureRewriteModelFitProbe:
  import ExistingUntpdOrdinaryMethodDescriptor.Descriptor

  final case class Prepared(method: untpd.DefDef, resultType: untpd.Tree, body: untpd.Tree)

  def assemble(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[String, Descriptor] =
    for
      owner <- Option(captured).toRight("capture required")
      member <- Option(owner.members).flatMap(_.lift(memberIndex))
        .toRight(s"member index $memberIndex was not captured")
      method <- member.tree match
        case value: untpd.DefDef => Right(value)
        case _ => Left("selected member is not a method")
      clauses <- method.paramss match
        case Nil => Right(Vector.empty)
        case List(Nil) => Right(Vector(Vector.empty))
        case other => Left(s"expected zero clauses or one empty clause, found $other")
    yield Descriptor(owner, memberIndex, method, method.name.toString, clauses, method.tpt, method.rhs)

  def prepare(
      descriptor: Descriptor,
      replacementBody: TermShape
  )(using Context): Either[String, Prepared] =
    for
      resultType <- prepareType(descriptor.resultType)
      body <- prepareBody(descriptor, replacementBody)
      method <- reconstruct(descriptor, resultType, body)
    yield Prepared(method, resultType, body)

  private def prepareType(oldSite: untpd.Tree)(using Context): Either[String, untpd.Tree] =
    TypeUntypedLowering.lower(TypeNormalForm.STypeIdent("Int"))
      .left.map(_.message)
      .map(_.cloneIn(oldSite.source).withSpan(oldSite.span))

  private def prepareBody(
      descriptor: Descriptor,
      replacement: TermShape
  )(using Context): Either[String, untpd.Tree] =
    val names = descriptor.parameterClauses.map(_.map(_.diagnosticName))
    val counts = names.map(_.size)
    for
      parameters <- TermBindingInternals.persistentParameters(names).left.map(_.message)
      completed <- parameters.complete(replacement).left.map(_.message)
      checked <- parameters.validateDefinitionBody(counts, completed).left.map(_.message)
      constructed <- ConstructedTerm.fromShapeInScope(checked, Vector.empty).left.map(_.message)
      lowered <- ConstructedTermUntypedBackend.lowerInScopes(constructed, Vector.empty).left.map(_.message)
      family <- ExistingUntpdSingleParameterMethodRhsRewriter.classify(lowered, "U053").left.map(_.message)
      positioned <- ExistingUntpdMethodBodyRewriteOriginAdapter
        .prepareReplacement(lowered, descriptor.rhs, family).left.map(_.message)
    yield positioned

  private def reconstruct(
      descriptor: Descriptor,
      resultType: untpd.Tree,
      body: untpd.Tree
  )(using Context): Either[String, untpd.DefDef] =
    given SourceFile = NoSource
    val clauses = descriptor.parameterClauses.map(_.map(_.tree).toList).toList
    val sourceFree = untpd.DefDef(descriptor.method.name, clauses, resultType, body)
      .withMods(descriptor.method.mods)
    val positioned = untpd.cpy.DefDef(sourceFree)(
      sourceFree.name,
      sourceFree.paramss,
      sourceFree.tpt,
      sourceFree.rhs
    ).cloneIn(descriptor.method.source).withSpan(descriptor.method.span)
    val expectedSizes = descriptor.method.paramss.map(_.size)
    Either.cond(
      !positioned.eq(descriptor.method) && positioned.paramss.map(_.size) == expectedSizes &&
        positioned.tpt.eq(resultType) && positioned.rhs.eq(body),
      positioned,
      "method reconstruction did not preserve the exact zero-parameter clause topology"
    )
