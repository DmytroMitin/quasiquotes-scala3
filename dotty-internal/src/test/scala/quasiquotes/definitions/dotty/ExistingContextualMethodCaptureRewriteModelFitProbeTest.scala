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
import quasiquotes.terms.{
  ConstructedTerm,
  TermBinder,
  TermBindingCategory,
  TermBindingInternals,
  TermShapeBindingView
}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.dotty.TypeUntypedLowering

class ExistingContextualMethodCaptureRewriteModelFitProbeTest extends munit.FunSuite:
  import ExistingContextualMethodCaptureRewriteModelFitProbe.{ClauseKind, ExactCarrier}

  test("exact carriers retain clause kinds, raw identities, Type islands, and RHS binder positions") {
    withContext {
      val contextual = carrier(
        "class ContextualCarrier:\n  def value(using x: Int): Int = x\n"
      )
      val mixed = carrier(
        "class MixedCarrier:\n  def combine(x: Int)(using y: Int): Int = x + y\n"
      )

      assertEquals(contextual.clauseKinds, Vector(ClauseKind.Contextual))
      assertEquals(mixed.clauseKinds, Vector(ClauseKind.Ordinary, ClauseKind.Contextual))
      assertEquals(contextual.descriptor.parameterClauses.map(_.size), Vector(1))
      assertEquals(mixed.descriptor.parameterClauses.map(_.size), Vector(1, 1))

      Vector(contextual, mixed).foreach { exact =>
        val descriptor = exact.descriptor
        assert(descriptor.method.eq(descriptor.captured.members.head.tree))
        assert(descriptor.resultType.eq(descriptor.method.tpt))
        assert(descriptor.rhs.eq(descriptor.method.rhs))
        descriptor.parameterClauses.zip(descriptor.method.paramss).foreach {
          case (parameters, rawClause) =>
            parameters.zip(rawClause).foreach { case (parameter, raw) =>
              assert(parameter.tree.eq(raw))
              assert(parameter.tpt.eq(raw.asInstanceOf[untpd.ValDef].tpt))
            }
        }
        val projectedTypes =
          ExistingContextualMethodCaptureRewriteModelFitProbe.projectTypes(exact)
            .fold(message => fail(message), identity)
        assertEquals(
          projectedTypes.flatten,
          Vector.fill(descriptor.parameterClauses.flatten.size)(
            TypeNormalForm.STypeIdent("Int")
          )
        )
        assertEquals(
          ExistingContextualMethodCaptureRewriteModelFitProbe
            .projectResult(exact)
            .fold(message => fail(message), identity),
          TypeNormalForm.STypeIdent("Int")
        )
        val projectedRhs =
          ExistingContextualMethodCaptureRewriteModelFitProbe.projectRhs(exact)
            .fold(message => fail(message), identity)
        val references = boundReferences(projectedRhs)
        assertEquals(
          references.map(_._1),
          descriptor.parameterClauses.flatten.map(_.diagnosticName)
        )
      }

      val free = carrier(
        "class FreeContextualBody:\n  def value(using x: Int): Int = helper\n"
      )
      assertEquals(
        ExistingContextualMethodCaptureRewriteModelFitProbe.projectRhs(free),
        Right(TermShape.Identifier("helper", false))
      )

      val opaque = carrier(
        "class OpaqueContextual:\n  def value(using x: domain.Opaque): Int = 1\n"
      )
      val opaqueType = opaque.descriptor.parameterClauses.head.head.tpt
      assert(opaqueType.isInstanceOf[untpd.Select])
      assert(opaqueType.eq(
        opaque.descriptor.method.paramss.head.head
          .asInstanceOf[untpd.ValDef].tpt
      ))
      assert(ExistingContextualMethodCaptureRewriteModelFitProbe.projectTypes(opaque).isLeft)
    }
  }

  test("all production downstream entry points remain closed at descriptor validation") {
    withContext {
      Vector(
        carrier("class ContextualGate:\n  def value(using x: AnyVal): AnyVal = x\n"),
        carrier("class MixedGate:\n  def combine(x: AnyVal)(using y: AnyVal): AnyVal = x\n")
      ).foreach { exact =>
        val descriptor = exact.descriptor
        val first = descriptor.parameterClauses.head.head
        assertEquals(
          ExistingUntpdOrdinaryMethodDescriptor
            .validate(descriptor)
            .left.toOption.map(_.code),
          Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
        )
        assertEquals(
          ExistingUntpdOrdinaryMethodTypeSlotProjection
            .projectParameter(descriptor, first)
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
        assertEquals(
          ExistingUntpdOrdinaryMethodTypeSlotProjection
            .projectResult(descriptor)
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
        assertEquals(
          ExistingUntpdOrdinaryMethodRhsTermProjection
            .projectRhs(descriptor)
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
        assertEquals(
          ExistingUntpdOrdinaryMethodTypeEditPreparation
            .prepareParameterType(descriptor, first, TypeNormalForm.STypeIdent("Int"))
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
        assertEquals(
          ExistingUntpdOrdinaryMethodTypeEditPreparation
            .prepareResultType(descriptor, TypeNormalForm.STypeIdent("Int"))
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
        var builderCalled = false
        val body = ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(descriptor) { _ =>
          builderCalled = true
          Right(TermShape.Literal("0"))
        }
        assertEquals(body.left.toOption.map(_.code), Some("INVALID_DESCRIPTOR"))
        assert(!builderCalled)
        assertEquals(
          ExistingUntpdOrdinaryMethodReconstruction
            .reconstructMethod(descriptor, Vector.empty, None, None)
            .left.toOption.map(_.code),
          Some("INVALID_DESCRIPTOR")
        )
      }
    }
  }

  test("Core scopes preserve ordinary and contextual ordinals and remain fail-closed") {
    val names = Vector(Vector("x"), Vector("y"))
    val parameters = TermBindingInternals
      .persistentParameters(names)
      .fold(problem => fail(problem.message), identity)
    val ordinary = parameters.referenceAt(0, 0).fold(problem => fail(problem.message), identity)
    val contextual =
      parameters.referenceAt(1, 0).fold(problem => fail(problem.message), identity)
    assertNotEquals(boundBinder(ordinary), boundBinder(contextual))
    val mixed = TermShape.Infix(ordinary, "+", contextual)
    val completedMixed =
      parameters.complete(mixed).fold(problem => fail(problem.message), identity)
    assertEquals(
      parameters.validateDefinitionBody(Vector(1, 1), completedMixed),
      Right(completedMixed)
    )
    val free = TermShape.Identifier("helper", false)
    assertEquals(parameters.validateDefinitionBody(Vector(1, 1), free), Right(free))
    val selected = TermShape.Select(TermShape.Identifier("obj", false), "helper")
    assertEquals(parameters.validateDefinitionBody(Vector(1, 1), selected), Right(selected))

    val foreign = TermBindingInternals
      .persistentParameters(names)
      .fold(problem => fail(problem.message), identity)
    val foreignReference =
      foreign.referenceAt(1, 0).fold(problem => fail(problem.message), identity)
    assertEquals(
      parameters
        .validateDefinitionBody(Vector(1, 1), foreignReference)
        .left.toOption.map(_.code),
      Some("TERM_BINDER_SCOPE_MISMATCH")
    )
  }

  test("test-only preparation and reconstruction preserve contextual flags and clause order") {
    withContext {
      val contextual = carrier(
        "class PreparedContextual:\n  def value(using x: AnyVal): AnyVal = x\n  val untouched: Int = 7\n"
      )
      val contextualPrepared = ExistingContextualMethodCaptureRewriteModelFitProbe
        .prepare(contextual, Set(0 -> 0), Vector(0 -> 0))
        .fold(message => fail(message), identity)
      assertPrepared(
        contextual,
        contextualPrepared,
        Vector(ClauseKind.Contextual),
        Set(0 -> 0)
      )

      val mixed = carrier(
        "class PreparedMixed:\n  def combine(x: AnyVal)(using y: AnyVal): AnyVal = x\n  val untouched: Int = 9\n"
      )
      val mixedPrepared = ExistingContextualMethodCaptureRewriteModelFitProbe
        .prepare(mixed, Set(1 -> 0), Vector(0 -> 0, 1 -> 0))
        .fold(message => fail(message), identity)
      assertPrepared(
        mixed,
        mixedPrepared,
        Vector(ClauseKind.Ordinary, ClauseKind.Contextual),
        Set(1 -> 0)
      )
      assert(mixedPrepared.method.paramss(0).head.eq(mixed.descriptor.method.paramss(0).head))
      assert(!mixedPrepared.method.paramss(1).head.eq(mixed.descriptor.method.paramss(1).head))
    }
  }

  private def assertPrepared(
      original: ExactCarrier,
      prepared: ExistingContextualMethodCaptureRewriteModelFitProbe.Prepared,
      expectedKinds: Vector[ClauseKind],
      edited: Set[(Int, Int)]
  )(using Context): Unit =
    assert(!prepared.method.eq(original.descriptor.method))
    assertEquals(
      prepared.method.paramss.map(_.size),
      original.descriptor.method.paramss.map(_.size)
    )
    assertEquals(
      ExistingContextualMethodCaptureRewriteModelFitProbe.clauseKinds(prepared.method),
      expectedKinds
    )
    prepared.method.paramss.zipWithIndex.foreach { case (clause, clauseIndex) =>
      clause.zipWithIndex.foreach { case (raw, parameterIndex) =>
        val parameter = raw.asInstanceOf[untpd.ValDef]
        val old =
          original.descriptor.method.paramss(clauseIndex)(parameterIndex)
            .asInstanceOf[untpd.ValDef]
        assertEquals(parameter.mods.flags, old.mods.flags)
        if edited(clauseIndex -> parameterIndex) then assert(!parameter.eq(old))
        else assert(parameter.eq(old))
      }
    }
    assert(prepared.method.mods.eq(original.descriptor.method.mods))
    assertEquals(prepared.method.source, original.descriptor.method.source)
    assertEquals(prepared.method.span, original.descriptor.method.span)
    prepared.method.tpt match
      case untpd.Ident(name) => assertEquals(name.toString, "Int")
      case other => fail("expected prepared Int result Type, found " + other)
    val trees = ExistingUntpdClassMemberFilter.allTrees(prepared.method)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def carrier(source: String)(using Context): ExactCarrier =
    val owner = ExistingUntpdClassMemberFilter
      .capture(parseClass(source))
      .fold(problem => fail(problem.message), identity)
    ExistingContextualMethodCaptureRewriteModelFitProbe
      .assemble(owner, 0)
      .fold(message => fail(message), identity)

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U054ExistingContextualProbe.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def boundReferences(shape: TermShape): Vector[(String, TermBinder)] = shape match
    case reference: TermShape.BoundReference =>
      Vector(reference.displayName -> boundBinder(reference))
    case TermShape.Infix(left, _, right) => boundReferences(left) ++ boundReferences(right)
    case other => fail("expected bound-reference topology, found " + other)

  private def boundBinder(shape: TermShape): TermBinder =
    val view = TermShapeBindingView
      .inspect(shape)
      .fold(problem => fail(problem.message), identity)
    assertEquals(view.category, TermBindingCategory.BoundReference)
    view.boundReference.getOrElse(fail("missing bound-reference view")).binder

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

private object ExistingContextualMethodCaptureRewriteModelFitProbe:
  import ExistingUntpdOrdinaryMethodDescriptor.{Descriptor, Parameter}

  enum ClauseKind:
    case Ordinary, Contextual

  final case class ExactCarrier(
      descriptor: Descriptor,
      clauseKinds: Vector[ClauseKind]
  )

  final case class Prepared(method: untpd.DefDef, resultType: untpd.Tree, body: untpd.Tree)

  def assemble(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[String, ExactCarrier] =
    for
      member <- captured.members.lift(memberIndex).toRight("member index was not captured")
      method <- member.tree match
        case value: untpd.DefDef => Right(value)
        case _ => Left("selected member is not a method")
      rawClauses <- readClauses(method)
      kinds <- classifyClauses(rawClauses)
      parameters = rawClauses.map(_.map(parameter =>
        Parameter(parameter, parameter.tpt, parameter.name.toString)
      ))
      descriptor = Descriptor(
        captured,
        memberIndex,
        method,
        method.name.toString,
        parameters,
        method.tpt,
        method.rhs
      )
    yield ExactCarrier(descriptor, kinds)

  def clauseKinds(method: untpd.DefDef): Vector[ClauseKind] =
    classifyClauses(readClauses(method).fold(message =>
      throw new IllegalArgumentException(message), identity
    )).fold(message => throw new IllegalArgumentException(message), identity)

  def projectTypes(
      carrier: ExactCarrier
  ): Either[String, Vector[Vector[TypeNormalForm]]] =
    carrier.descriptor.parameterClauses.foldLeft[
      Either[String, Vector[Vector[TypeNormalForm]]]
    ](Right(Vector.empty)) { case (clauses, clause) =>
      for
        collected <- clauses
        projected <- clause.foldLeft[Either[String, Vector[TypeNormalForm]]](
          Right(Vector.empty)
        ) { case (parameters, parameter) =>
          for
            accepted <- parameters
            semantic <- projectType(parameter.tpt)
          yield accepted :+ semantic
        }
      yield collected :+ projected
    }

  def projectResult(carrier: ExactCarrier): Either[String, TypeNormalForm] =
    projectType(carrier.descriptor.resultType)

  def projectRhs(carrier: ExactCarrier): Either[String, TermShape] =
    val names = carrier.descriptor.parameterClauses.map(_.map(_.diagnosticName))
    val positions = names.zipWithIndex.flatMap { case (clause, clauseIndex) =>
      clause.zipWithIndex.map { case (name, parameterIndex) =>
        name -> (clauseIndex -> parameterIndex)
      }
    }.toMap
    for
      scope <- TermBindingInternals.persistentParameters(names).left.map(_.message)
      projected <- decodeRhs(carrier.descriptor.rhs, positions, scope)
      completed <- scope.complete(projected).left.map(_.message)
      checked <- scope.validateDefinitionBody(names.map(_.size), completed).left.map(_.message)
    yield checked

  def prepare(
      carrier: ExactCarrier,
      editedPositions: Set[(Int, Int)],
      bodyReferences: Vector[(Int, Int)]
  )(using Context): Either[String, Prepared] =
    val descriptor = carrier.descriptor
    for
      clauses <- descriptor.parameterClauses.zipWithIndex.foldLeft[
        Either[String, Vector[Vector[untpd.ValDef]]]
      ](Right(Vector.empty)) { case (collectedClauses, (clause, clauseIndex)) =>
        for
          collected <- collectedClauses
          parameters <- clause.zipWithIndex.foldLeft[
            Either[String, Vector[untpd.ValDef]]
          ](Right(Vector.empty)) { case (collectedParameters, (parameter, parameterIndex)) =>
            for
              accepted <- collectedParameters
              next <-
                if editedPositions(clauseIndex -> parameterIndex) then
                  prepareParameter(parameter)
                else Right(parameter.tree)
            yield accepted :+ next
          }
        yield collected :+ parameters
      }
      resultType <- prepareType(descriptor.resultType)
      body <- prepareBody(descriptor, bodyReferences)
      method <- reconstruct(descriptor, clauses.map(_.toList).toList, resultType, body)
    yield Prepared(method, resultType, body)

  private def readClauses(method: untpd.DefDef): Either[String, Vector[Vector[untpd.ValDef]]] =
    method.paramss.zipWithIndex.foldLeft[
      Either[String, Vector[Vector[untpd.ValDef]]]
    ](Right(Vector.empty)) { case (clauses, (clause, clauseIndex)) =>
      for
        collected <- clauses
        parameters <- clause.zipWithIndex.foldLeft[
          Either[String, Vector[untpd.ValDef]]
        ](Right(Vector.empty)) { case (values, (raw, parameterIndex)) =>
          for
            accepted <- values
            parameter <- raw match
              case value: untpd.ValDef => Right(value)
              case _ => Left("non-ValDef parameter at " + clauseIndex + "/" + parameterIndex)
          yield accepted :+ parameter
        }
      yield collected :+ parameters
    }

  private def classifyClauses(
      clauses: Vector[Vector[untpd.ValDef]]
  ): Either[String, Vector[ClauseKind]] =
    clauses.zipWithIndex.foldLeft[Either[String, Vector[ClauseKind]]](
      Right(Vector.empty)
    ) { case (kinds, (clause, index)) =>
      val contextual = clause.map(_.mods.is(Flags.Given))
      if contextual.nonEmpty && contextual.forall(identity) then
        kinds.map(_ :+ ClauseKind.Contextual)
      else if contextual.nonEmpty && contextual.forall(value => !value) then
        kinds.map(_ :+ ClauseKind.Ordinary)
      else Left("clause has empty or mixed contextual flags at index " + index)
    }

  private def projectType(tree: untpd.Tree): Either[String, TypeNormalForm] = tree match
    case untpd.Ident(name)
        if name.isTypeName && Set("Int", "String", "Boolean", "AnyVal")(name.toString) =>
      Right(TypeNormalForm.STypeIdent(name.toString))
    case other => Left("unsupported exact Type topology " + other.getClass.getSimpleName)

  private def decodeRhs(
      tree: untpd.Tree,
      positions: Map[String, (Int, Int)],
      scope: TermBindingInternals.PersistentParameters
  ): Either[String, TermShape] = tree match
    case untpd.Ident(name) =>
      positions.get(name.toString) match
        case Some((clause, parameter)) =>
          scope.referenceAt(clause, parameter).left.map(_.message)
        case None => Right(TermShape.Identifier(name.toString, false))
    case untpd.InfixOp(left, untpd.Ident(operator), right) =>
      for
        projectedLeft <- decodeRhs(left, positions, scope)
        projectedRight <- decodeRhs(right, positions, scope)
      yield TermShape.Infix(projectedLeft, operator.toString, projectedRight)
    case number: untpd.Number => Right(TermShape.Literal(number.digits))
    case other => Left("unsupported exact RHS topology " + other.getClass.getSimpleName)

  private def prepareParameter(parameter: Parameter)(using Context): Either[String, untpd.ValDef] =
    for
      lowered <- lowerInt()
      positionedType = lowered.cloneIn(parameter.tpt.source).withSpan(parameter.tpt.span)
      positioned <- positionParameter(parameter.tree, positionedType)
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
    val sourceFree =
      untpd.ValDef(original.name, positionedType, untpd.EmptyTree).withMods(original.mods)
    val positioned = untpd.cpy.ValDef(sourceFree)(
      sourceFree.name,
      positionedType,
      sourceFree.rhs
    ).cloneIn(original.source).withSpan(original.span)
    Either.cond(
      !positioned.eq(original) &&
        positioned.mods.eq(original.mods) &&
        positioned.tpt.eq(positionedType),
      positioned,
      "parameter preparation lost modifiers or origin"
    )

  private def prepareBody(
      descriptor: Descriptor,
      positions: Vector[(Int, Int)]
  )(using Context): Either[String, untpd.Tree] =
    val names = descriptor.parameterClauses.map(_.map(_.diagnosticName))
    for
      scope <- TermBindingInternals.persistentParameters(names).left.map(_.message)
      references <- positions.foldLeft[Either[String, Vector[TermShape]]](
        Right(Vector.empty)
      ) { case (collected, (clause, parameter)) =>
        for
          accepted <- collected
          reference <- scope.referenceAt(clause, parameter).left.map(_.message)
        yield accepted :+ reference
      }
      shape <- references match
        case Vector(reference) => Right(reference)
        case Vector(first, second) =>
          Right(TermShape.Apply(
            TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
            List(first, second)
          ))
        case _ => Left("unsupported body reference count")
      completed <- scope.complete(shape).left.map(_.message)
      checked <- scope.validateDefinitionBody(names.map(_.size), completed).left.map(_.message)
      bindings <- references.zip(positions).foldLeft[
        Either[String, Vector[(BinderId, String)]]
      ](Right(Vector.empty)) { case (collected, (reference, (clause, parameter))) =>
        for
          accepted <- collected
          binding <- reference match
            case TermShape.BoundReference(id, _) => Right(id -> names(clause)(parameter))
            case _ => Left("expected bound reference")
        yield accepted :+ binding
      }
      constructed <- ConstructedTerm
        .fromShapeInScope(checked, bindings.map(_._1))
        .left.map(_.message)
      lowered <- ConstructedTermUntypedBackend
        .lowerInScopes(constructed, bindings)
        .left.map(_.message)
      family <- ExistingUntpdSingleParameterMethodRhsRewriter
        .classify(lowered, "U054")
        .left.map(_.message)
      positioned <- ExistingUntpdMethodBodyRewriteOriginAdapter
        .prepareReplacement(lowered, descriptor.rhs, family)
        .left.map(_.message)
    yield positioned

  private def reconstruct(
      descriptor: Descriptor,
      clauses: List[List[untpd.ValDef]],
      resultType: untpd.Tree,
      body: untpd.Tree
  )(using Context): Either[String, untpd.DefDef] =
    given SourceFile = NoSource
    val sourceFree =
      untpd.DefDef(descriptor.method.name, clauses, resultType, body)
        .withMods(descriptor.method.mods)
    val positioned = untpd.cpy.DefDef(sourceFree)(
      sourceFree.name,
      sourceFree.paramss,
      sourceFree.tpt,
      sourceFree.rhs
    ).cloneIn(descriptor.method.source).withSpan(descriptor.method.span)
    Either.cond(
      !positioned.eq(descriptor.method) &&
        positioned.paramss.map(_.size) == descriptor.method.paramss.map(_.size) &&
        positioned.tpt.eq(resultType) &&
        positioned.rhs.eq(body),
      positioned,
      "method reconstruction lost exact clause topology"
    )
