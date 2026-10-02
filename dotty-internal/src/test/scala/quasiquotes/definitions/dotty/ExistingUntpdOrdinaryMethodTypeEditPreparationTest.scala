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
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.types.TypeNormalForm

class ExistingUntpdOrdinaryMethodTypeEditPreparationTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodTypeEditPreparation.*

  test("parameter preparation retains exact descriptor evidence and returns only fresh field fragments") {
    withContext {
      val root = parseClass(
        """class ParameterPreparation:
          |  val before: Int = 1
          |  def transform(value: AnyVal): String = value.toString
          |  type After = Boolean
          |""".stripMargin
      )
      val descriptor = descriptorFor(root, 1)
      val parameter = descriptor.parameterClauses.head.head
      val before = snapshot(root)

      val prepared = prepareParameter(descriptor, parameter, "Int")

      assert(prepared.descriptor.eq(descriptor))
      assert(prepared.captured.eq(descriptor.captured))
      assertEquals(prepared.memberIndex, descriptor.memberIndex)
      assert(prepared.method.eq(descriptor.method))
      assert(prepared.parameter.eq(parameter))
      assert(prepared.oldParameter.eq(parameter.tree))
      assert(prepared.oldParameterType.eq(parameter.tpt))
      assertIdent(prepared.loweredType, "Int")
      assert(!prepared.loweredType.source.exists)
      assert(!prepared.loweredType.span.exists)
      assertEquals(prepared.loweredType.symbol, NoSymbol)
      assertIdent(prepared.positionedType, "Int")
      assert(!prepared.positionedType.eq(prepared.loweredType))
      assert(!prepared.positionedType.eq(parameter.tpt))
      assertEquals(prepared.positionedType.source, parameter.tpt.source)
      assertEquals(prepared.positionedType.span, parameter.tpt.span)
      assert(!prepared.positionedParameter.eq(parameter.tree))
      assert(prepared.positionedParameter.tpt.eq(prepared.positionedType))
      assertEquals(prepared.positionedParameter.name, parameter.tree.name)
      assert(prepared.positionedParameter.mods.eq(parameter.tree.mods))
      assert(prepared.positionedParameter.rhs.isEmpty)
      assertEquals(prepared.positionedParameter.source, parameter.tree.source)
      assertEquals(prepared.positionedParameter.span, parameter.tree.span)
      assertEquals(validatePreparedParameterType(prepared, descriptor), Right(()))
      assert(
        allTrees(prepared.positionedParameter).forall(tree =>
          !tree.isInstanceOf[untpd.DefDef] &&
            !tree.isInstanceOf[untpd.Template] &&
            !tree.isInstanceOf[untpd.TypeDef]
        )
      )
      assertSnapshotUnchanged(root, before)
    }
  }

  test("two-parameter preparation selects exact wrapper positions zero and one") {
    withContext {
      val root = parseClass(
        "class PairPreparation:\n  def combine(left: AnyVal, right: List[Int]): Boolean = true\n"
      )
      val descriptor = descriptorFor(root, 0)
      val parameters = descriptor.parameterClauses.head
      val before = snapshot(root)

      val first = prepareParameter(descriptor, parameters(0), "String")
      val second = prepareParameter(descriptor, parameters(1), "Boolean")

      assert(first.parameter.eq(parameters(0)))
      assert(first.oldParameter.eq(parameters(0).tree))
      assert(first.oldParameterType.eq(parameters(0).tpt))
      assertIdent(first.positionedType, "String")
      assert(second.parameter.eq(parameters(1)))
      assert(second.oldParameter.eq(parameters(1).tree))
      assert(second.oldParameterType.eq(parameters(1).tpt))
      assertIdent(second.positionedType, "Boolean")
      assert(!first.positionedType.eq(second.positionedType))
      assertEquals(validatePreparedParameterType(first, descriptor), Right(()))
      assertEquals(validatePreparedParameterType(second, descriptor), Right(()))
      assertSnapshotUnchanged(root, before)
    }
  }

  test("result preparation covers one and two parameter descriptors without reconstructing a method or owner") {
    withContext {
      val fixtures = Vector(
        ("class OneResult:\n  def convert(value: Int): AnyVal = value\n", "String"),
        ("class TwoResult:\n  def combine(left: Int, right: String): AnyVal = left\n", "Boolean")
      )
      fixtures.foreach { case (source, replacement) =>
        val root = parseClass(source)
        val descriptor = descriptorFor(root, 0)
        val before = snapshot(root)
        val prepared = prepareResult(descriptor, replacement)

        assert(prepared.descriptor.eq(descriptor))
        assert(prepared.captured.eq(descriptor.captured))
        assertEquals(prepared.memberIndex, descriptor.memberIndex)
        assert(prepared.method.eq(descriptor.method))
        assert(prepared.oldResultType.eq(descriptor.resultType))
        assertIdent(prepared.loweredType, replacement)
        assert(!prepared.loweredType.source.exists)
        assert(!prepared.loweredType.span.exists)
        assertIdent(prepared.positionedType, replacement)
        assert(!prepared.positionedType.eq(prepared.loweredType))
        assert(!prepared.positionedType.eq(descriptor.resultType))
        assertEquals(prepared.positionedType.source, descriptor.resultType.source)
        assertEquals(prepared.positionedType.span, descriptor.resultType.span)
        assertEquals(validatePreparedResultType(prepared, descriptor), Right(()))
        assert(
          allTrees(prepared.positionedType).forall(tree =>
            !tree.isInstanceOf[untpd.DefDef] &&
              !tree.isInstanceOf[untpd.Template] &&
              !tree.isInstanceOf[untpd.TypeDef]
          )
        )
        assertSnapshotUnchanged(root, before)
      }
    }
  }

  test("repeated parameter and result preparations allocate fresh lowered and positioned fragments") {
    withContext {
      val descriptor = descriptorFor(
        parseClass("class Repeat:\n  def change(left: AnyVal, right: AnyVal): AnyVal = left\n"),
        0
      )
      val parameter = descriptor.parameterClauses.head(1)
      val firstParameter = prepareParameter(descriptor, parameter, "Int")
      val secondParameter = prepareParameter(descriptor, parameter, "Int")
      val firstResult = prepareResult(descriptor, "Int")
      val secondResult = prepareResult(descriptor, "Int")

      assert(!firstParameter.loweredType.eq(secondParameter.loweredType))
      assert(!firstParameter.positionedType.eq(secondParameter.positionedType))
      assert(!firstParameter.positionedParameter.eq(secondParameter.positionedParameter))
      assert(!firstResult.loweredType.eq(secondResult.loweredType))
      assert(!firstResult.positionedType.eq(secondResult.positionedType))
      assert(!firstParameter.loweredType.eq(firstResult.loweredType))
      assert(!firstParameter.positionedType.eq(firstResult.positionedType))
    }
  }

  test("strict descriptor-wrapper authority rejects foreign same-raw and same-spelling parameters") {
    withContext {
      val root = parseClass("class Local:\n  def same(value: Int): Int = value\n")
      val firstDescriptor = descriptorFor(root, 0)
      val secondDescriptor = descriptorForCapture(firstDescriptor.captured, 0)
      val firstParameter = firstDescriptor.parameterClauses.head.head
      val secondParameter = secondDescriptor.parameterClauses.head.head
      assert(firstParameter.tree.eq(secondParameter.tree))
      assert(firstParameter.tpt.eq(secondParameter.tpt))
      assert(!firstParameter.eq(secondParameter))
      assertCode(
        prepareParameterType(firstDescriptor, secondParameter, TypeNormalForm.STypeIdent("Int")),
        "PARAMETER_NOT_CAPTURED"
      )
      val prepared = prepareParameter(firstDescriptor, firstParameter, "Int")
      assertCode(
        validatePreparedParameterType(prepared, secondDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      val result = prepareResult(firstDescriptor, "Int")
      assertCode(
        validatePreparedResultType(result, secondDescriptor),
        "FINAL_RESULT_PREPARATION_INVARIANT_FAILED"
      )

      val foreignDescriptor = descriptorFor(
        parseClass("class Local:\n  def same(value: Int): Int = value\n"),
        0
      )
      assertCode(
        prepareParameterType(firstDescriptor, foreignDescriptor.parameterClauses.head.head, TypeNormalForm.STypeIdent("Int")),
        "PARAMETER_NOT_CAPTURED"
      )
    }
  }

  test("forged parameter wrappers and unsupported semantic Type families fail closed") {
    withContext {
      val root = parseClass("class Boundary:\n  def update(value: AnyVal): AnyVal = value\n")
      val descriptor = descriptorFor(root, 0)
      val before = snapshot(root)
      val parameter = descriptor.parameterClauses.head.head
      val copied = parameter.copy(diagnosticName = parameter.diagnosticName)
      assert(!copied.eq(parameter))
      assertCode(
        prepareParameterType(descriptor, copied, TypeNormalForm.STypeIdent("Int")),
        "PARAMETER_NOT_CAPTURED"
      )
      assertCode(prepareParameterType(descriptor, null, TypeNormalForm.STypeIdent("Int")), "PARAMETER_REQUIRED")
      assertCode(prepareParameterType(descriptor, parameter, null), "SEMANTIC_TYPE_REQUIRED")
      assertCode(prepareResultType(descriptor, null), "SEMANTIC_TYPE_REQUIRED")
      Vector[TypeNormalForm](
        TypeNormalForm.STypeIdent("AnyVal"),
        TypeNormalForm.STypeIdent(null)
      ).foreach { replacement =>
        assertCode(prepareParameterType(descriptor, parameter, replacement), "SEMANTIC_TYPE_IDENTIFIER_UNSUPPORTED")
        assertCode(prepareResultType(descriptor, replacement), "SEMANTIC_TYPE_IDENTIFIER_UNSUPPORTED")
      }
      Vector[TypeNormalForm](
        TypeNormalForm.STypeResolved(null),
        TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(TypeNormalForm.STypeIdent("Int"))),
        TypeNormalForm.STypeTuple(List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"))),
        TypeNormalForm.STypeFunction(List(TypeNormalForm.STypeIdent("Int")), TypeNormalForm.STypeIdent("String"))
      ).foreach { replacement =>
        assertCode(prepareParameterType(descriptor, parameter, replacement), "SEMANTIC_TYPE_FAMILY_UNSUPPORTED")
        assertCode(prepareResultType(descriptor, replacement), "SEMANTIC_TYPE_FAMILY_UNSUPPORTED")
      }
      assertCode(prepareResultType(null, TypeNormalForm.STypeIdent("Int")), "INVALID_DESCRIPTOR")
      assertSnapshotUnchanged(root, before)
    }
  }

  test("final validators reject null contamination aliases and carrier-field swaps") {
    withContext {
      val firstDescriptor = descriptorFor(
        parseClass("class First:\n  def change(value: AnyVal): AnyVal = value\n"),
        0
      )
      val secondDescriptor = descriptorFor(
        parseClass("class Second:\n  def change(value: AnyVal): AnyVal = value\n"),
        0
      )
      val first = prepareParameter(firstDescriptor, firstDescriptor.parameterClauses.head.head, "Int")
      val second = prepareParameter(secondDescriptor, secondDescriptor.parameterClauses.head.head, "Int")
      val firstResult = prepareResult(firstDescriptor, "Int")
      val secondResult = prepareResult(secondDescriptor, "Int")

      assertCode(validatePreparedParameterType(null, firstDescriptor), "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED")
      assertCode(validatePreparedResultType(null, firstDescriptor), "FINAL_RESULT_PREPARATION_INVARIANT_FAILED")
      assertCode(
        validatePreparedParameterType(first.copy(positionedType = second.positionedType), firstDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedParameterType(first.copy(positionedParameter = second.positionedParameter), firstDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedParameterType(first.copy(loweredType = first.positionedType), firstDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedParameterType(first.copy(oldParameterType = first.positionedType), firstDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedResultType(firstResult.copy(positionedType = secondResult.positionedType), firstDescriptor),
        "FINAL_RESULT_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedResultType(firstResult.copy(loweredType = firstResult.positionedType), firstDescriptor),
        "FINAL_RESULT_PREPARATION_INVARIANT_FAILED"
      )

      val symbol = newSymbol(NoSymbol, typeName("Contaminated"), EmptyFlags, NoType)
      val contaminated = untpd.Ident(typeName("Int")).withType(symbol.typeRef)
      assertCode(
        validatePreparedParameterType(first.copy(loweredType = contaminated), firstDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertCode(
        validatePreparedResultType(firstResult.copy(positionedType = contaminated), firstDescriptor),
        "FINAL_RESULT_PREPARATION_INVARIANT_FAILED"
      )
    }
  }

  test("prepared fragments match accepted full rewrite topology and transformation sites") {
    withContext {
      val oneRoot = parseClass("class OneParity:\n  def change(value: AnyVal): AnyVal = value\n")
      val oneCapture = capture(oneRoot)
      val oneDescriptor = descriptorForCapture(oneCapture, 0)
      val oneView = ExistingUntpdSingleParameterMethodView.capture(oneCapture, 0).toOption.get
      val preparedParameter = prepareParameter(oneDescriptor, oneDescriptor.parameterClauses.head.head, "Int")
      val preparedResult = prepareResult(oneDescriptor, "String")
      val fullParameter = ExistingUntpdSingleParameterMethodParameterTypeRewriter
        .rewrite(oneView, TypeNormalForm.STypeIdent("Int")).toOption.get
      val fullResult = ExistingUntpdSingleParameterMethodResultTypeRewriter
        .rewrite(oneView, TypeNormalForm.STypeIdent("String")).toOption.get
      assertSameTypeContract(preparedParameter.loweredType, fullParameter.loweredParameterType, "Int")
      assertSamePositionedTypeContract(preparedParameter.positionedType, fullParameter.positionedParameterType, oneDescriptor.parameterClauses.head.head.tpt)
      assertEquals(preparedParameter.positionedParameter.name, fullParameter.positionedParameter.name)
      assert(preparedParameter.positionedParameter.mods.eq(fullParameter.positionedParameter.mods))
      assertEquals(preparedParameter.positionedParameter.source, fullParameter.positionedParameter.source)
      assertEquals(preparedParameter.positionedParameter.span, fullParameter.positionedParameter.span)
      assertSameTypeContract(preparedResult.loweredType, fullResult.loweredResultType, "String")
      assertSamePositionedTypeContract(preparedResult.positionedType, fullResult.positionedResultType, oneDescriptor.resultType)

      val twoRoot = parseClass("class TwoParity:\n  def change(left: AnyVal, right: AnyVal): AnyVal = left\n")
      val twoCapture = capture(twoRoot)
      val twoDescriptor = descriptorForCapture(twoCapture, 0)
      val twoView = ExistingUntpdTwoParameterMethodView.capture(twoCapture, 0).toOption.get
      twoDescriptor.parameterClauses.head.indices.foreach { index =>
        val prepared = prepareParameter(twoDescriptor, twoDescriptor.parameterClauses.head(index), "Boolean")
        val full = ExistingUntpdTwoParameterMethodParameterTypeRewriter
          .rewrite(twoView, index, TypeNormalForm.STypeIdent("Boolean")).toOption.get
        assertSameTypeContract(prepared.loweredType, full.loweredParameterType, "Boolean")
        assertSamePositionedTypeContract(prepared.positionedType, full.positionedParameterType, twoDescriptor.parameterClauses.head(index).tpt)
        assertEquals(prepared.positionedParameter.name, full.positionedParameter.name)
        assert(prepared.positionedParameter.mods.eq(full.positionedParameter.mods))
      }
      val preparedTwoResult = prepareResult(twoDescriptor, "Int")
      val fullTwoResult = ExistingUntpdTwoParameterMethodResultTypeRewriter
        .rewrite(twoView, TypeNormalForm.STypeIdent("Int")).toOption.get
      assertSameTypeContract(preparedTwoResult.loweredType, fullTwoResult.loweredResultType, "Int")
      assertSamePositionedTypeContract(preparedTwoResult.positionedType, fullTwoResult.positionedResultType, twoDescriptor.resultType)
    }
  }

  private def prepareParameter(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      parameter: ExistingUntpdOrdinaryMethodDescriptor.Parameter,
      primitive: String
  )(using Context): PreparedParameterType =
    prepareParameterType(descriptor, parameter, TypeNormalForm.STypeIdent(primitive))
      .fold(problem => fail(problem.message), identity)

  private def prepareResult(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      primitive: String
  )(using Context): PreparedResultType =
    prepareResultType(descriptor, TypeNormalForm.STypeIdent(primitive))
      .fold(problem => fail(problem.message), identity)

  private def assertSameTypeContract(
      prepared: untpd.Tree,
      accepted: untpd.Tree,
      expectedName: String
  ): Unit =
    assertIdent(prepared, expectedName)
    assertIdent(accepted, expectedName)
    assert(!prepared.eq(accepted))
    assert(!prepared.source.exists && !prepared.span.exists)
    assert(!accepted.source.exists && !accepted.span.exists)

  private def assertSamePositionedTypeContract(
      prepared: untpd.Tree,
      accepted: untpd.Tree,
      oldSite: untpd.Tree
  ): Unit =
    assert(!prepared.eq(accepted))
    assertEquals(prepared.source, oldSite.source)
    assertEquals(prepared.span, oldSite.span)
    assertEquals(accepted.source, oldSite.source)
    assertEquals(accepted.span, oldSite.span)

  private def assertIdent(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${other.getClass.getSimpleName}")

  private def assertCode[A](result: Either[Error, A], expected: String): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, expected)
      assert(problem.detail.nonEmpty)
      assert(problem.message.nonEmpty)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def descriptorFor(
      root: untpd.TypeDef,
      memberIndex: Int
  )(using Context): ExistingUntpdOrdinaryMethodDescriptor.Descriptor =
    descriptorForCapture(capture(root), memberIndex)

  private def descriptorForCapture(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): ExistingUntpdOrdinaryMethodDescriptor.Descriptor =
    ExistingUntpdOrdinaryMethodDescriptor.capture(captured, memberIndex)
      .fold(problem => fail(problem.message), identity)

  private def capture(
      root: untpd.TypeDef
  )(using Context): ExistingUntpdClassMemberFilter.Capture =
    ExistingUntpdClassMemberFilter.capture(root)
      .fold(problem => fail(problem.message), identity)

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: SourceFile,
      span: Span,
      symbol: dotty.tools.dotc.core.Symbols.Symbol,
      modifiers: Option[untpd.Modifiers]
  )

  private def snapshot(root: untpd.TypeDef)(using Context): Vector[TreeSnapshot] =
    allTrees(root).map { tree =>
      val modifiers = tree match
        case definition: untpd.DefTree => Option(definition.mods)
        case _ => None
      TreeSnapshot(tree, tree.source, tree.span, tree.symbol, modifiers)
    }

  private def assertSnapshotUnchanged(
      root: untpd.TypeDef,
      before: Vector[TreeSnapshot]
  )(using Context): Unit =
    val after = allTrees(root)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { case (tree, original) =>
      assert(tree.eq(original.tree))
      assert(tree.source.eq(original.source))
      assertEquals(tree.span, original.span)
      assertEquals(tree.symbol, original.symbol)
      tree match
        case definition: untpd.DefTree =>
          assert(original.modifiers.exists(definition.mods.eq))
        case _ => assertEquals(original.modifiers, None)
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U040TypeEditPreparation.scala", source)
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
