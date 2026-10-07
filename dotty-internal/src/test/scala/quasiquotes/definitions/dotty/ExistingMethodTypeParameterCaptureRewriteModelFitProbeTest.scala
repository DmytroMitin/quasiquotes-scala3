package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.{ConstructedTerm, TermBindingInternals}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.dotty.TypeUntypedLowering

class ExistingMethodTypeParameterCaptureRewriteModelFitProbeTest extends munit.FunSuite:
  import ExistingMethodTypeParameterCaptureRewriteModelFitProbe.*

  test("parallel carriers retain TypeDef and term ValDef as distinct exact topology") {
    withContext {
      Vector(
        carrier("class Phantom:\n  def keep[A](x: Int): Int = x\n"),
        carrier("class Dependent:\n  def id[A](x: A): A = x\n")
      ).foreach { exact =>
        assert(exact.method.eq(exact.captured.members(exact.memberIndex).tree))
        assertEquals(exact.method.paramss.map(_.size), List(1, 1))
        assert(exact.method.paramss.head.head.eq(exact.typeParameter))
        assert(exact.method.paramss(1).head.eq(exact.termParameter))
        assert(exact.typeParameter.rhs.eq(exact.typeBounds))
        assert(exact.termParameter.tpt.eq(exact.parameterType))
        assert(exact.method.tpt.eq(exact.resultType))
        assert(exact.method.rhs.eq(exact.rhs))
        assertEquals(exact.typeParameter.mods.flags, Flags.Param)
        assertEquals(exact.rawTermClauseOrdinal, 1)
        assertEquals(exact.termModelClauseOrdinal, 0)
        assert(exact.typeParameter.source.eq(exact.method.source))
        assert(exact.termParameter.source.eq(exact.method.source))
        Vector[untpd.Tree](exact.method, exact.typeParameter, exact.typeBounds,
          exact.termParameter, exact.parameterType, exact.resultType, exact.rhs).foreach { tree =>
          assert(tree.source.exists)
          assert(tree.span.exists)
        }
        val trees = ExistingUntpdClassMemberFilter.allTrees(exact.method)
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

        val synthetic = syntheticTermDescriptor(exact)
        assertEquals(synthetic.parameterClauses.size, 1)
        assert(synthetic.parameterClauses.head.head.tree.eq(exact.termParameter))
        assertEquals(
          ExistingUntpdOrdinaryMethodDescriptor.validate(synthetic)
            .left.toOption.map(_.code),
          Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
        )
        assertEquals(
          ExistingUntpdOrdinaryMethodTypeSlotProjection.projectResult(synthetic)
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
      }
    }
  }

  test("term scope is term-only: x is bound, helper is free, and foreign references fail closed") {
    val scope = TermBindingInternals
      .persistentParameters(Vector(Vector("x")))
      .fold(problem => fail(problem.message), identity)
    val x = scope.referenceAt(0, 0).fold(problem => fail(problem.message), identity)
    val completed = scope.complete(x).fold(problem => fail(problem.message), identity)
    assertEquals(scope.validateDefinitionBody(Vector(1), completed), Right(completed))

    val helper = TermShape.Identifier("helper", false)
    assertEquals(scope.validateDefinitionBody(Vector(1), helper), Right(helper))

    val foreign = TermBindingInternals
      .persistentParameters(Vector(Vector("x")))
      .fold(problem => fail(problem.message), identity)
      .referenceAt(0, 0)
      .fold(problem => fail(problem.message), identity)
    assertEquals(
      scope.validateDefinitionBody(Vector(1), foreign).left.toOption.map(_.code),
      Some("TERM_BINDER_SCOPE_MISMATCH")
    )
  }

  test("G1 Type slots project while G2 method-local A is semantically unsupported") {
    withContext {
      val phantom = carrier("class PhantomTypes:\n  def keep[A](x: Int): Int = x\n")
      assertEquals(projectTermParameterType(phantom), Right(TypeNormalForm.STypeIdent("Int")))
      assertEquals(projectResultType(phantom), Right(TypeNormalForm.STypeIdent("Int")))

      val dependent = carrier("class DependentTypes:\n  def id[A](x: A): A = x\n")
      assertEquals(
        projectTermParameterType(dependent).left.toOption,
        Some("METHOD_TYPE_BINDER_MODEL_REQUIRED")
      )
      assertEquals(
        projectResultType(dependent).left.toOption,
        Some("METHOD_TYPE_BINDER_MODEL_REQUIRED")
      )
      assert(dependent.parameterType.eq(dependent.termParameter.tpt))
      assert(dependent.resultType.eq(dependent.method.tpt))

      val nongeneric = ExistingUntpdClassMemberFilter
        .capture(parseClass("class UnresolvedA:\n  def f(x: A): A = x\n"))
        .fold(problem => fail(problem.message), identity)
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor
        .capture(nongeneric, 0).fold(problem => fail(problem.message), identity)
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeSlotProjection
          .projectParameter(descriptor, descriptor.parameterClauses.head.head)
          .left.toOption.map(_.code),
        Some("UNSUPPORTED_IDENTIFIER")
      )
      assertEquals(
        ExistingUntpdOrdinaryMethodTypeSlotProjection
          .projectResult(descriptor).left.toOption.map(_.code),
        Some("UNSUPPORTED_IDENTIFIER")
      )
    }
  }

  test("G1 rewrites admitted term/result/body slots and G2 preserves dependent slots opaquely") {
    withContext {
      val phantom = carrier(
        "class PreparedPhantom:\n  def keep[A](x: AnyVal): AnyVal = x\n  val untouched: Int = 7\n"
      )
      val preparedPhantom = prepare(phantom, rewriteTypes = true)
        .fold(message => fail(message), identity)
      assertPrepared(phantom, preparedPhantom, expectFreshTermParameter = true)
      preparedPhantom.method.paramss(1).head.asInstanceOf[untpd.ValDef].tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected prepared Int parameter Type, found " + other)
      preparedPhantom.method.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected prepared Int result Type, found " + other)

      val dependent = carrier(
        "class PreparedDependent:\n  def id[A](x: A): A = ???\n  val untouched: Int = 9\n"
      )
      val preparedDependent = prepare(dependent, rewriteTypes = false)
        .fold(message => fail(message), identity)
      assertPrepared(dependent, preparedDependent, expectFreshTermParameter = false)
      assert(preparedDependent.method.paramss(1).head.eq(dependent.termParameter))
      assert(preparedDependent.method.paramss(1).head.asInstanceOf[untpd.ValDef].tpt
        .eq(dependent.parameterType))
      assert(preparedDependent.method.tpt.eq(dependent.resultType))
    }
  }

  private def assertPrepared(
      original: ExactCarrier,
      prepared: Prepared,
      expectFreshTermParameter: Boolean
  )(using Context): Unit =
    assert(!prepared.method.eq(original.method))
    assertEquals(prepared.method.paramss.map(_.size), List(1, 1))
    assert(prepared.method.paramss.head.head.eq(original.typeParameter))
    assert(prepared.method.paramss.head.head.asInstanceOf[untpd.TypeDef].rhs
      .eq(original.typeBounds))
    if expectFreshTermParameter then
      assert(!prepared.method.paramss(1).head.eq(original.termParameter))
    else assert(prepared.method.paramss(1).head.eq(original.termParameter))
    assert(prepared.method.mods.eq(original.method.mods))
    assertEquals(prepared.method.source, original.method.source)
    assertEquals(prepared.method.span, original.method.span)
    prepared.method.rhs match
      case untpd.Ident(name) => assertEquals(name.toString, original.termParameter.name.toString)
      case other => fail("expected prepared term-binder RHS, found " + other)
    val trees = ExistingUntpdClassMemberFilter.allTrees(prepared.method)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def carrier(source: String)(using Context): ExactCarrier =
    val captured = ExistingUntpdClassMemberFilter
      .capture(parseClass(source)).fold(problem => fail(problem.message), identity)
    ExistingMethodTypeParameterCaptureRewriteModelFitProbe
      .assemble(captured, 0).fold(message => fail(message), identity)

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U055ExistingGenericProbe.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

private[dotty] object ExistingMethodTypeParameterCaptureRewriteModelFitProbe:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}

  final case class ExactCarrier(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      typeParameter: untpd.TypeDef,
      typeBounds: untpd.Tree,
      termParameter: untpd.ValDef,
      parameterType: untpd.Tree,
      resultType: untpd.Tree,
      rhs: untpd.Tree,
      rawTermClauseOrdinal: Int,
      termModelClauseOrdinal: Int
  )

  final case class Prepared(method: untpd.DefDef, body: untpd.Tree)

  def assemble(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[String, ExactCarrier] =
    for
      member <- captured.members.lift(memberIndex).toRight("member index was not captured")
      method <- member.tree match
        case value: untpd.DefDef => Right(value)
        case other => Left("selected member is not a method: " + other.getClass.getSimpleName)
      _ <- Either.cond(method.paramss.map(_.size) == List(1, 1), (),
        "expected one Type clause followed by one unary term clause")
      typeParameter <- method.paramss.head.head match
        case value: untpd.TypeDef => Right(value)
        case other => Left("first clause is not TypeDef: " + other.getClass.getSimpleName)
      termParameter <- method.paramss(1).head match
        case value: untpd.ValDef => Right(value)
        case other => Left("second clause is not ValDef: " + other.getClass.getSimpleName)
    yield ExactCarrier(
      captured,
      memberIndex,
      method,
      typeParameter,
      typeParameter.rhs,
      termParameter,
      termParameter.tpt,
      method.tpt,
      method.rhs,
      rawTermClauseOrdinal = 1,
      termModelClauseOrdinal = 0
    )

  def syntheticTermDescriptor(carrier: ExactCarrier): Descriptor =
    Descriptor(
      carrier.captured,
      carrier.memberIndex,
      carrier.method,
      carrier.method.name.toString,
      Vector(Vector(Parameter(
        carrier.termParameter,
        carrier.parameterType,
        carrier.termParameter.name.toString
      ))),
      carrier.resultType,
      carrier.rhs
    )

  def projectTermParameterType(carrier: ExactCarrier): Either[String, TypeNormalForm] =
    projectType(carrier.parameterType, carrier.typeParameter)

  def projectResultType(carrier: ExactCarrier): Either[String, TypeNormalForm] =
    projectType(carrier.resultType, carrier.typeParameter)

  def prepare(carrier: ExactCarrier, rewriteTypes: Boolean)(using Context): Either[String, Prepared] =
    for
      parameter <-
        if rewriteTypes then prepareParameter(carrier.termParameter)
        else Right(carrier.termParameter)
      result <-
        if rewriteTypes then prepareType(carrier.resultType)
        else Right(carrier.resultType)
      body <- prepareBody(carrier)
      method <- reconstruct(carrier, parameter, result, body)
    yield Prepared(method, body)

  private def projectType(
      tree: untpd.Tree,
      typeParameter: untpd.TypeDef
  ): Either[String, TypeNormalForm] = tree match
    case untpd.Ident(name) if name.toString == typeParameter.name.toString =>
      Left("METHOD_TYPE_BINDER_MODEL_REQUIRED")
    case untpd.Ident(name)
        if name.isTypeName && Set("Int", "String", "Boolean", "AnyVal")(name.toString) =>
      Right(TypeNormalForm.STypeIdent(name.toString))
    case other => Left("unsupported exact Type topology " + other.getClass.getSimpleName)

  private def prepareParameter(original: untpd.ValDef)(using Context): Either[String, untpd.ValDef] =
    for
      lowered <- lowerInt()
      positionedType = lowered.cloneIn(original.tpt.source).withSpan(original.tpt.span)
      positioned <- positionParameter(original, positionedType)
    yield positioned

  private def prepareType(oldSite: untpd.Tree)(using Context): Either[String, untpd.Tree] =
    lowerInt().map(_.cloneIn(oldSite.source).withSpan(oldSite.span))

  private def lowerInt()(using Context): Either[String, untpd.Tree] =
    TypeUntypedLowering.lower(TypeNormalForm.STypeIdent("Int")).left.map(_.message)

  private def positionParameter(
      original: untpd.ValDef,
      positionedType: untpd.Tree
  )(using Context): Either[String, untpd.ValDef] =
    given SourceFile = NoSource
    val sourceFree = untpd.ValDef(original.name, positionedType, untpd.EmptyTree)
      .withMods(original.mods)
    val positioned = untpd.cpy.ValDef(sourceFree)(
      sourceFree.name,
      positionedType,
      sourceFree.rhs
    ).cloneIn(original.source).withSpan(original.span)
    Either.cond(
      positioned.mods.eq(original.mods) && positioned.tpt.eq(positionedType),
      positioned,
      "parameter preparation lost modifiers or origin"
    )

  private def prepareBody(carrier: ExactCarrier)(using Context): Either[String, untpd.Tree] =
    val names = Vector(Vector(carrier.termParameter.name.toString))
    for
      scope <- TermBindingInternals.persistentParameters(names).left.map(_.message)
      reference <- scope.referenceAt(0, 0).left.map(_.message)
      completed <- scope.complete(reference).left.map(_.message)
      checked <- scope.validateDefinitionBody(Vector(1), completed).left.map(_.message)
      binding <- checked match
        case TermShape.BoundReference(id, _) => Right(id -> names.head.head)
        case _ => Left("expected bound reference")
      constructed <- ConstructedTerm.fromShapeInScope(checked, Vector(binding._1))
        .left.map(_.message)
      lowered <- ConstructedTermUntypedBackend
        .lowerInScopes(constructed, Vector(binding)).left.map(_.message)
      family <- ExistingUntpdSingleParameterMethodRhsRewriter
        .classify(lowered, "U055").left.map(_.message)
      positioned <- ExistingUntpdMethodBodyRewriteOriginAdapter
        .prepareReplacement(lowered, carrier.rhs, family).left.map(_.message)
    yield positioned

  private def reconstruct(
      carrier: ExactCarrier,
      parameter: untpd.ValDef,
      resultType: untpd.Tree,
      body: untpd.Tree
  )(using Context): Either[String, untpd.DefDef] =
    given SourceFile = NoSource
    val clauses = carrier.method.paramss.updated(1, List(parameter))
    val sourceFree = untpd.DefDef(carrier.method.name, clauses, resultType, body)
      .withMods(carrier.method.mods)
    val positioned = untpd.cpy.DefDef(sourceFree)(
      sourceFree.name,
      sourceFree.paramss,
      sourceFree.tpt,
      sourceFree.rhs
    ).cloneIn(carrier.method.source).withSpan(carrier.method.span)
    Either.cond(
      positioned.paramss.head.head.eq(carrier.typeParameter) &&
        positioned.paramss.head.head.asInstanceOf[untpd.TypeDef].rhs.eq(carrier.typeBounds) &&
        positioned.paramss(1).head.eq(parameter) &&
        positioned.tpt.eq(resultType) && positioned.rhs.eq(body),
      positioned,
      "method reconstruction lost exact generic clause topology"
    )
