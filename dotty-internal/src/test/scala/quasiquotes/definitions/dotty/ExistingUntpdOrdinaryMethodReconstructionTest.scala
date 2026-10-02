package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingUntpdOrdinaryMethodReconstructionTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodReconstruction.*
  import ExistingUntpdOrdinaryMethodRhsEditPreparation.{PreparedBody, ReplacementScope}
  import ExistingUntpdOrdinaryMethodTypeEditPreparation.{PreparedParameterType, PreparedResultType}

  test("supports every one-parameter partial and complete prepared-field combination") {
    withContext {
      val descriptor = descriptorFor("class One:\n  def change(x: AnyVal): AnyVal = x\n")
      val parameter = prepareParameter(descriptor, 0, "Int")
      val resultType = prepareResult(descriptor, "Int")
      val body = prepareBody(descriptor, "abs")
      val cases = Vector(
        (Vector(parameter), None, None),
        (Vector.empty, Some(resultType), None),
        (Vector.empty, None, Some(body)),
        (Vector(parameter), Some(resultType), None),
        (Vector(parameter), None, Some(body)),
        (Vector.empty, Some(resultType), Some(body)),
        (Vector(parameter), Some(resultType), Some(body))
      )
      cases.foreach { case (parameters, result, rhs) =>
        val reconstructed = reconstruct(descriptor, parameters, result, rhs)
        val method = reconstructed.positionedMethod
        assert(!method.eq(descriptor.method))
        assertEquals(method.name, descriptor.method.name)
        assert(method.mods.eq(descriptor.method.mods))
        assertEquals(method.source, descriptor.method.source)
        assertEquals(method.span, descriptor.method.span)
        assertEquals(method.symbol, NoSymbol)
        assert(method.paramss.head.head.eq(parameters.headOption.fold(descriptor.parameterClauses.head.head.tree)(_.positionedParameter)))
        assert(method.tpt.eq(result.fold(descriptor.resultType)(_.positionedType)))
        assert(method.rhs.eq(rhs.fold(descriptor.rhs)(_.positionedReplacement)))
        assertEquals(validateReconstructedMethod(reconstructed, descriptor), Right(()))
      }
    }
  }

  test("two-parameter selection is wrapper-identity based and preserves descriptor order") {
    withContext {
      val descriptor = descriptorFor("class Two:\n  def combine(x: AnyVal, y: AnyVal): AnyVal = x\n")
      val first = prepareParameter(descriptor, 0, "Int")
      val second = prepareParameter(descriptor, 1, "String")
      val firstOnly = reconstruct(descriptor, Vector(first), None, None).positionedMethod
      val secondOnly = reconstruct(descriptor, Vector(second), None, None).positionedMethod
      val reversedInput = reconstruct(descriptor, Vector(second, first), None, None).positionedMethod
      assert(firstOnly.paramss.head(0).eq(first.positionedParameter))
      assert(firstOnly.paramss.head(1).eq(descriptor.parameterClauses.head(1).tree))
      assert(secondOnly.paramss.head(0).eq(descriptor.parameterClauses.head(0).tree))
      assert(secondOnly.paramss.head(1).eq(second.positionedParameter))
      assert(reversedInput.paramss.head(0).eq(first.positionedParameter))
      assert(reversedInput.paramss.head(1).eq(second.positionedParameter))
      assert(reversedInput.paramss.head(0).asInstanceOf[untpd.ValDef].tpt.eq(first.positionedType))
      assert(reversedInput.paramss.head(1).asInstanceOf[untpd.ValDef].tpt.eq(second.positionedType))
    }
  }

  test("two-parameter all-fields composition uses exact prepared children in one fresh shell") {
    withContext {
      val descriptor = descriptorFor("class Full:\n  def combine(x: AnyVal, y: AnyVal): AnyVal = x\n")
      val first = prepareParameter(descriptor, 0, "Int")
      val second = prepareParameter(descriptor, 1, "Int")
      val resultType = prepareResult(descriptor, "Int")
      val body = prepareBody(descriptor, "max")
      val reconstructed = reconstruct(descriptor, Vector(first, second), Some(resultType), Some(body))
      assert(reconstructed.descriptor.eq(descriptor))
      assertEquals(reconstructed.parameterTypes, Vector(first, second))
      assertEquals(reconstructed.editedParameters, Vector(descriptor.parameterClauses.head(0), descriptor.parameterClauses.head(1)))
      assert(reconstructed.resultType.contains(resultType))
      assert(reconstructed.body.contains(body))
      assert(reconstructed.positionedMethod.paramss.head(0).eq(first.positionedParameter))
      assert(reconstructed.positionedMethod.paramss.head(1).eq(second.positionedParameter))
      assert(reconstructed.positionedMethod.tpt.eq(resultType.positionedType))
      assert(reconstructed.positionedMethod.rhs.eq(body.positionedReplacement))
    }
  }

  test("rejects no edits duplicates nulls and preparations from another descriptor wrapper") {
    withContext {
      val root = parseClass("class Boundary:\n  def change(x: AnyVal): AnyVal = x\n")
      val captured = capture(root)
      val descriptor = descriptorForCapture(captured)
      val sameRawDescriptor = descriptorForCapture(captured)
      val parameter = prepareParameter(descriptor, 0, "Int")
      val resultType = prepareResult(descriptor, "Int")
      val body = prepareBody(descriptor, "abs")
      assertCode(reconstructMethod(descriptor, Vector.empty, None, None), "EMPTY_EDIT_SET")
      assertCode(reconstructMethod(descriptor, null, None, None), "PARAMETER_PREPARATIONS_REQUIRED")
      assertCode(reconstructMethod(descriptor, Vector(null), None, None), "PARAMETER_PREPARATION_REQUIRED")
      assertCode(reconstructMethod(descriptor, Vector(parameter, parameter), None, None), "DUPLICATE_PARAMETER_EDIT")
      assertCode(reconstructMethod(sameRawDescriptor, Vector(parameter), None, None), "FOREIGN_PARAMETER_PREPARATION")
      assertCode(reconstructMethod(sameRawDescriptor, Vector.empty, Some(resultType), None), "FOREIGN_RESULT_PREPARATION")
      assertCode(reconstructMethod(sameRawDescriptor, Vector.empty, None, Some(body)), "FOREIGN_BODY_PREPARATION")
      assertCode(reconstructMethod(null, Vector(parameter), None, None), "INVALID_DESCRIPTOR")
    }
  }

  test("rejects same-text foreign captures and swapped prepared carrier fields") {
    withContext {
      val first = descriptorFor("class Same:\n  def change(x: AnyVal): AnyVal = x\n")
      val second = descriptorFor("class Same:\n  def change(x: AnyVal): AnyVal = x\n")
      val firstParameter = prepareParameter(first, 0, "Int")
      val secondParameter = prepareParameter(second, 0, "Int")
      val firstResult = prepareResult(first, "Int")
      val secondResult = prepareResult(second, "Int")
      val firstBody = prepareBody(first, "abs")
      val secondBody = prepareBody(second, "abs")
      assertCode(reconstructMethod(first, Vector(secondParameter), None, None), "FOREIGN_PARAMETER_PREPARATION")
      assertCode(reconstructMethod(first, Vector.empty, Some(secondResult), None), "FOREIGN_RESULT_PREPARATION")
      assertCode(reconstructMethod(first, Vector.empty, None, Some(secondBody)), "FOREIGN_BODY_PREPARATION")
      assertCode(reconstructMethod(first, Vector(firstParameter.copy(descriptor = null)), None, None), "FOREIGN_PARAMETER_PREPARATION")
      assertCode(reconstructMethod(first, Vector.empty, Some(firstResult.copy(descriptor = null)), None), "FOREIGN_RESULT_PREPARATION")
      assertCode(reconstructMethod(first, Vector.empty, None, Some(firstBody.copy(descriptor = null))), "FOREIGN_BODY_PREPARATION")
      assertCode(reconstructMethod(first, Vector(firstParameter.copy(positionedType = secondParameter.positionedType)), None, None), "INVALID_PARAMETER_PREPARATION")
      assertCode(reconstructMethod(first, Vector.empty, Some(firstResult.copy(positionedType = secondResult.positionedType)), None), "INVALID_RESULT_PREPARATION")
      assertCode(reconstructMethod(first, Vector.empty, None, Some(firstBody.copy(positionedReplacement = secondBody.positionedReplacement))), "INVALID_BODY_PREPARATION")
    }
  }

  test("repeated reconstruction creates fresh shells and reuses every exact selected child") {
    withContext {
      val descriptor = descriptorFor("class Repeat:\n  def combine(x: AnyVal, y: AnyVal): AnyVal = x\n")
      val second = prepareParameter(descriptor, 1, "Int")
      val resultType = prepareResult(descriptor, "Int")
      val body = prepareBody(descriptor, "max")
      val before = snapshot(descriptor.captured.originalRoot)
      val preparedBefore = snapshotPrepared(second, resultType, body)
      val first = reconstruct(descriptor, Vector(second), Some(resultType), Some(body))
      val repeated = reconstruct(descriptor, Vector(second), Some(resultType), Some(body))
      assert(!first.positionedMethod.eq(repeated.positionedMethod))
      Vector(first.positionedMethod, repeated.positionedMethod).foreach { method =>
        assert(method.paramss.head(0).eq(descriptor.parameterClauses.head(0).tree))
        assert(method.paramss.head(1).eq(second.positionedParameter))
        assert(method.tpt.eq(resultType.positionedType))
        assert(method.rhs.eq(body.positionedReplacement))
      }
      assertSnapshotUnchanged(descriptor.captured.originalRoot, before)
      assertPreparedSnapshotsUnchanged(preparedBefore)
      assertEquals(ExistingUntpdOrdinaryMethodTypeEditPreparation.validatePreparedParameterType(second, descriptor), Right(()))
      assertEquals(ExistingUntpdOrdinaryMethodTypeEditPreparation.validatePreparedResultType(resultType, descriptor), Right(()))
      assertEquals(ExistingUntpdOrdinaryMethodRhsEditPreparation.validatePreparedBody(body, descriptor), Right(()))

      val failureOwnerBefore = snapshot(descriptor.captured.originalRoot)
      val failurePreparedBefore = snapshotPrepared(second, resultType, body)
      assertCode(reconstructMethod(descriptor, Vector(second, second), Some(resultType), Some(body)), "DUPLICATE_PARAMETER_EDIT")
      assertSnapshotUnchanged(descriptor.captured.originalRoot, failureOwnerBefore)
      assertPreparedSnapshotsUnchanged(failurePreparedBefore)
    }
  }

  test("final validator rejects substituted shells children topology and provenance") {
    withContext {
      val descriptor = descriptorFor("class Final:\n  def change(x: AnyVal): AnyVal = x\n")
      val parameter = prepareParameter(descriptor, 0, "Int")
      val resultType = prepareResult(descriptor, "Int")
      val body = prepareBody(descriptor, "abs")
      val reconstructed = reconstruct(descriptor, Vector(parameter), Some(resultType), Some(body))
      assertCode(validateReconstructedMethod(null, descriptor), "FINAL_METHOD_INVARIANT_FAILED")
      assertCode(validateReconstructedMethod(reconstructed.copy(descriptor = null), descriptor), "FINAL_METHOD_INVARIANT_FAILED")
      given SourceFile = NoSource
      val sourceFreeNoEdit = untpd
        .DefDef(
          descriptor.method.name,
          descriptor.method.paramss,
          descriptor.resultType,
          descriptor.rhs
        )
        .withMods(descriptor.method.mods)
      val positionedNoEdit = untpd.cpy.DefDef(sourceFreeNoEdit)(
        sourceFreeNoEdit.name,
        sourceFreeNoEdit.paramss,
        sourceFreeNoEdit.tpt,
        sourceFreeNoEdit.rhs
      ).cloneIn(descriptor.method.source).withSpan(descriptor.method.span)
      val forgedNoEdit = reconstructed.copy(
        parameterTypes = Vector.empty, editedParameters = Vector.empty,
        resultType = None, body = None, positionedMethod = positionedNoEdit
      )
      assertCode(validateReconstructedMethod(forgedNoEdit, descriptor), "FINAL_METHOD_INVARIANT_FAILED")
      assertCode(validateReconstructedMethod(reconstructed, null), "FINAL_METHOD_INVARIANT_FAILED")
      assertCode(validateReconstructedMethod(reconstructed.copy(positionedMethod = descriptor.method), descriptor), "FINAL_METHOD_INVARIANT_FAILED")
      val wrongChildren = untpd.cpy.DefDef(reconstructed.positionedMethod)(
        reconstructed.positionedMethod.name,
        List(List(descriptor.parameterClauses.head.head.tree)),
        descriptor.resultType,
        descriptor.rhs
      )
      assertCode(validateReconstructedMethod(reconstructed.copy(positionedMethod = wrongChildren), descriptor), "FINAL_METHOD_INVARIANT_FAILED")

      val nullParamss = untpd.cpy.DefDef(reconstructed.positionedMethod)(
        reconstructed.positionedMethod.name,
        null.asInstanceOf[List[untpd.ParamClause]],
        reconstructed.positionedMethod.tpt,
        reconstructed.positionedMethod.rhs
      )
      val nullClause = untpd.cpy.DefDef(reconstructed.positionedMethod)(
        reconstructed.positionedMethod.name,
        List(null.asInstanceOf[untpd.ParamClause]),
        reconstructed.positionedMethod.tpt,
        reconstructed.positionedMethod.rhs
      )
      val nullParameter = untpd.cpy.DefDef(reconstructed.positionedMethod)(
        reconstructed.positionedMethod.name,
        List(List(null.asInstanceOf[untpd.ValDef])),
        reconstructed.positionedMethod.tpt,
        reconstructed.positionedMethod.rhs
      )
      val nullType = untpd.cpy.DefDef(reconstructed.positionedMethod)(
        reconstructed.positionedMethod.name,
        reconstructed.positionedMethod.paramss,
        null,
        reconstructed.positionedMethod.rhs
      )
      val nullRhs = untpd.cpy.DefDef(reconstructed.positionedMethod)(
        reconstructed.positionedMethod.name,
        reconstructed.positionedMethod.paramss,
        reconstructed.positionedMethod.tpt,
        null
      )
      val nullMods = reconstructed.positionedMethod.withMods(null)
      Vector(nullParamss, nullClause, nullParameter, nullType, nullRhs, nullMods).foreach { malformed =>
        assertCode(validateReconstructedMethod(reconstructed.copy(positionedMethod = malformed), descriptor), "FINAL_METHOD_INVARIANT_FAILED")
      }
    }
  }

  test("one-parameter all-field method topology and provenance match accepted U032 oracle") {
    withContext {
      val descriptor = descriptorFor("class Parity:\n  def change(x: AnyVal): AnyVal = x\n")
      val parameter = prepareParameter(descriptor, 0, "Int")
      val resultType = prepareResult(descriptor, "Int")
      val body = prepareBody(descriptor, "abs")
      val reconstructed = reconstruct(descriptor, Vector(parameter), Some(resultType), Some(body))
      val view = ExistingUntpdSingleParameterMethodView.capture(descriptor.captured, descriptor.memberIndex).toOption.get
      val oracle = ExistingUntpdSingleParameterMethodAtomicRewriter
        .rewrite(view, TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("Int"), body.loweredReplacement.cloneIn(NoSource))
        .toOption.get
      assert(!reconstructed.positionedMethod.eq(oracle.positionedMethod))
      assertEquals(reconstructed.positionedMethod.name, oracle.positionedMethod.name)
      assert(reconstructed.positionedMethod.mods.eq(oracle.positionedMethod.mods))
      assertEquals(reconstructed.positionedMethod.source, oracle.positionedMethod.source)
      assertEquals(reconstructed.positionedMethod.span, oracle.positionedMethod.span)
      assertEquals(reconstructed.positionedMethod.paramss.size, oracle.positionedMethod.paramss.size)
      assertEquals(reconstructed.positionedMethod.paramss.head.size, oracle.positionedMethod.paramss.head.size)
      assertTreeSiteParity(reconstructed.positionedMethod.paramss.head.head, oracle.positionedMethod.paramss.head.head)
      assertTreeSiteParity(reconstructed.positionedMethod.paramss.head.head.asInstanceOf[untpd.ValDef].tpt, oracle.positionedMethod.paramss.head.head.asInstanceOf[untpd.ValDef].tpt)
      assertTreeSiteParity(reconstructed.positionedMethod.tpt, oracle.positionedMethod.tpt)
      assertTreeSiteParity(reconstructed.positionedMethod.rhs, oracle.positionedMethod.rhs)
      assert(allTrees(reconstructed.positionedMethod).forall(tree => tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]))
    }
  }

  private def reconstruct(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      parameters: Vector[PreparedParameterType],
      resultType: Option[PreparedResultType],
      body: Option[PreparedBody]
  )(using Context): ReconstructedMethod =
    reconstructMethod(descriptor, parameters, resultType, body).fold(problem => fail(problem.message), identity)

  private def prepareParameter(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      index: Int,
      primitive: String
  )(using Context): PreparedParameterType =
    ExistingUntpdOrdinaryMethodTypeEditPreparation
      .prepareParameterType(descriptor, descriptor.parameterClauses.head(index), TypeNormalForm.STypeIdent(primitive))
      .fold(problem => fail(problem.message), identity)

  private def prepareResult(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      primitive: String
  )(using Context): PreparedResultType =
    ExistingUntpdOrdinaryMethodTypeEditPreparation.prepareResultType(descriptor, TypeNormalForm.STypeIdent(primitive))
      .fold(problem => fail(problem.message), identity)

  private def prepareBody(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      operation: String
  )(using Context): PreparedBody =
    ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(descriptor) { scope =>
      val parameters = descriptor.parameterClauses.head
      if parameters.size == 1 then
        scope.reference(parameters.head).map(reference =>
          TermShape.Apply(TermShape.Select(TermShape.Identifier("Math", false), operation), List(reference))
        )
      else
        for
          first <- scope.reference(parameters(0))
          second <- scope.reference(parameters(1))
        yield TermShape.Apply(TermShape.Select(TermShape.Identifier("Math", false), operation), List(first, second))
    }.fold(problem => fail(problem.message), identity)

  private def descriptorFor(source: String, memberIndex: Int = 0)(using Context) =
    descriptorForCapture(capture(parseClass(source)), memberIndex)

  private def descriptorForCapture(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int = 0
  )(using Context): ExistingUntpdOrdinaryMethodDescriptor.Descriptor =
    ExistingUntpdOrdinaryMethodDescriptor.capture(captured, memberIndex).fold(problem => fail(problem.message), identity)

  private def capture(root: untpd.TypeDef)(using Context) =
    ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)

  private def assertCode[A](result: Either[Error, A], expected: String): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, expected, clues(problem))
      assert(problem.detail.nonEmpty)
      assert(problem.message.nonEmpty)
    case Right(value) => fail(s"expected $expected, found success $value")

  private final case class TreeSnapshot(tree: untpd.Tree, source: SourceFile, span: Span, symbol: dotty.tools.dotc.core.Symbols.Symbol)
  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    allTrees(tree).map(node => TreeSnapshot(node, node.source, node.span, node.symbol))
  private def assertSnapshotUnchanged(tree: untpd.Tree, before: Vector[TreeSnapshot])(using Context): Unit =
    val after = allTrees(tree)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { (actual, expected) =>
      assert(actual.eq(expected.tree))
      assert(actual.source.eq(expected.source))
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
    }
  private final case class PreparedSnapshot(tree: untpd.Tree, graph: Vector[TreeSnapshot])
  private def snapshotPrepared(
      parameter: PreparedParameterType,
      resultType: PreparedResultType,
      body: PreparedBody
  )(using Context): Vector[PreparedSnapshot] =
    Vector(
      parameter.loweredType,
      parameter.positionedType,
      parameter.positionedParameter,
      resultType.loweredType,
      resultType.positionedType,
      body.loweredReplacement,
      body.positionedReplacement
    ).map(tree => PreparedSnapshot(tree, snapshot(tree)))
  private def assertPreparedSnapshotsUnchanged(before: Vector[PreparedSnapshot])(using Context): Unit =
    before.foreach(value => assertSnapshotUnchanged(value.tree, value.graph))
  private def assertTreeSiteParity(actual: untpd.Tree, oracle: untpd.Tree): Unit =
    assertEquals(actual.source, oracle.source)
    assertEquals(actual.span, oracle.span)
  private def allTrees(tree: untpd.Tree)(using Context) = ExistingUntpdClassMemberFilter.allTrees(tree)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U042MethodReconstruction.scala", source)
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
    val base = new ContextBase
    run(using base.initialCtx)
