package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.SemanticDefinition
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBinder, TermBindingInternals, TermShapeBindingView}
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import quasiquotes.types.TypeNormalForm

/** Public exact-version algebra for inspecting and atomically rewriting one existing class. */
object ExistingClassUntypedRewrite:
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final class Capture private[dotty] (
      private[dotty] val exact: ExistingUntpdClassMemberFilter.Capture,
      private val memberValues: Vector[MemberView],
      private val capturedContext: Context
  ):
    def classIdentity: ExactIdentity = new ExactIdentity(exact.originalRoot)
    def members: Vector[MemberView] = memberValues

    def member(index: Int): Either[Failure, MemberView] =
      memberValues.lift(index).toRight(
        failure("SELECTION_FAILED", s"direct-member index $index is outside this capture.")
      )

    def method(member: MemberRef): Either[Failure, MethodView] =
      given Context = capturedContext
      for
        selected <- validateMemberRef(exact, member)
        descriptor <- ExistingUntpdOrdinaryMethodDescriptor
          .capture(exact, selected.index)
          .left.map(mapDescriptor)
        scope <- ParameterScopeState.create(descriptor)
        methodRef = new MethodRef(
          exact,
          selected.index,
          descriptor.method,
          scope
        )
        parameters = descriptor.parameterClauses.zipWithIndex.map { (clause, clauseIndex) =>
          new ParameterClauseView(
            ParameterClauseKind.Ordinary,
            clause.zipWithIndex.map { (parameter, parameterIndex) =>
              val ref = new ParameterRef(
                methodRef,
                clauseIndex,
                parameterIndex,
                parameter.tree,
                parameter.tpt
              )
              new ParameterView(
                ref,
                clauseIndex,
                parameterIndex,
                parameter.diagnosticName,
                new ExactIdentity(parameter.tree),
                new TypeSlotView(
                  new ExactIdentity(parameter.tpt),
                  ExistingUntpdOrdinaryMethodTypeSlotProjection
                    .projectParameter(descriptor, parameter)
                    .left.map(mapTypeProjection)
                )
              )
            }
          )
        }
        bodySemantic = ExistingUntpdOrdinaryMethodRhsTermProjection
          .projectRhs(descriptor)
          .left.map(mapTermProjection)
          .flatMap(projection => scope.importProjection(projection))
      yield new MethodView(
        methodRef,
        selected,
        descriptor.diagnosticMethodName,
        parameters,
        new TypeSlotView(
          new ExactIdentity(descriptor.resultType),
          ExistingUntpdOrdinaryMethodTypeSlotProjection
            .projectResult(descriptor)
            .left.map(mapTypeProjection)
        ),
        new TermSlotView(new ExactIdentity(descriptor.rhs), bodySemantic),
        new ExistingMethodParameterScope(scope)
      )

    def emptyPlan: EditPlan = EditPlan.empty(exact)

  final class MemberRef private[dotty] (
      private[dotty] val captureIdentity: ExistingUntpdClassMemberFilter.Capture,
      private[dotty] val memberIndex: Int,
      private[dotty] val treeIdentity: untpd.Tree
  ) derives CanEqual

  final class MethodRef private[dotty] (
      private[dotty] val captureIdentity: ExistingUntpdClassMemberFilter.Capture,
      private[dotty] val memberIndex: Int,
      private[dotty] val methodIdentity: untpd.DefDef,
      private[dotty] val scopeIdentity: ParameterScopeState
  ) derives CanEqual

  final class ParameterRef private[dotty] (
      private[dotty] val methodIdentity: MethodRef,
      private[dotty] val clauseOrdinal: Int,
      private[dotty] val parameterOrdinal: Int,
      private[dotty] val parameterIdentity: untpd.ValDef,
      private[dotty] val typeIdentity: untpd.Tree
  ) derives CanEqual

  final class ExactIdentity private[dotty] (
      private val underlying: AnyRef
  ) derives CanEqual:
    def sameObjectAs(other: ExactIdentity): Boolean =
      other != null && underlying != null && (underlying eq other.underlying)

    override def toString: String = "ExactIdentity(<opaque>)"

  final class MemberKind private (val code: String) derives CanEqual:
    override def equals(other: Any): Boolean = other match
      case that: MemberKind => code == that.code
      case _ => false
    override def hashCode(): Int = code.hashCode
    override def toString: String = s"MemberKind($code)"

  object MemberKind:
    val Method: MemberKind = new MemberKind("METHOD")
    val Value: MemberKind = new MemberKind("VALUE")
    val TypeMember: MemberKind = new MemberKind("TYPE_MEMBER")
    private[dotty] val Other: MemberKind = new MemberKind("OTHER")

  final class ParameterClauseKind private (val code: String) derives CanEqual:
    override def equals(other: Any): Boolean = other match
      case that: ParameterClauseKind => code == that.code
      case _ => false
    override def hashCode(): Int = code.hashCode
    override def toString: String = s"ParameterClauseKind($code)"

  object ParameterClauseKind:
    val Ordinary: ParameterClauseKind = new ParameterClauseKind("ORDINARY")

  final class MemberView private[dotty] (
      private val refValue: MemberRef,
      private val indexValue: Int,
      private val kindValue: MemberKind,
      private val diagnosticNameValue: Option[String],
      private val identityValue: ExactIdentity
  ):
    def ref: MemberRef = refValue
    def index: Int = indexValue
    def kind: MemberKind = kindValue
    def diagnosticName: Option[String] = diagnosticNameValue
    def identity: ExactIdentity = identityValue

  final class MethodView private[dotty] (
      private val refValue: MethodRef,
      private val memberValue: MemberView,
      private val diagnosticNameValue: String,
      private val parameterClausesValue: Vector[ParameterClauseView],
      private val resultTypeValue: TypeSlotView,
      private val bodyValue: TermSlotView,
      private val parameterScopeValue: ExistingMethodParameterScope
  ):
    def ref: MethodRef = refValue
    def member: MemberRef = memberValue.ref
    def diagnosticName: String = diagnosticNameValue
    def parameterClauses: Vector[ParameterClauseView] = parameterClausesValue
    def resultType: TypeSlotView = resultTypeValue
    def body: TermSlotView = bodyValue
    def parameterScope: ExistingMethodParameterScope = parameterScopeValue

  final class ParameterClauseView private[dotty] (
      private val kindValue: ParameterClauseKind,
      private val parametersValue: Vector[ParameterView]
  ):
    def kind: ParameterClauseKind = kindValue
    def parameters: Vector[ParameterView] = parametersValue

  final class ParameterView private[dotty] (
      private val refValue: ParameterRef,
      private val clauseIndexValue: Int,
      private val parameterIndexValue: Int,
      private val diagnosticNameValue: String,
      private val identityValue: ExactIdentity,
      private val declaredTypeValue: TypeSlotView
  ):
    def ref: ParameterRef = refValue
    def clauseIndex: Int = clauseIndexValue
    def parameterIndex: Int = parameterIndexValue
    def diagnosticName: String = diagnosticNameValue
    def identity: ExactIdentity = identityValue
    def declaredType: TypeSlotView = declaredTypeValue

  final class TypeSlotView private[dotty] (
      private val identityValue: ExactIdentity,
      private val semanticValue: Either[Failure, TypeNormalForm]
  ):
    def identity: ExactIdentity = identityValue
    def semantic: Either[Failure, TypeNormalForm] = semanticValue

  final class TermSlotView private[dotty] (
      private val identityValue: ExactIdentity,
      private val semanticValue: Either[Failure, TermShape]
  ):
    def identity: ExactIdentity = identityValue
    def semantic: Either[Failure, TermShape] = semanticValue

  final class ExistingMethodParameterScope private[dotty] (
      private val state: ParameterScopeState
  ):
    def binder(parameter: ParameterRef): Either[Failure, TermBinder] =
      state.binder(parameter)

    def reference(parameter: ParameterRef): Either[Failure, TermShape] =
      state.reference(parameter)

  final class EditPlan private[dotty] (
      private[dotty] val captureIdentity: ExistingUntpdClassMemberFilter.Capture,
      private[dotty] val omissions: Vector[MemberRef],
      private[dotty] val parameterTypes: Vector[ParameterTypeIntent],
      private[dotty] val resultTypes: Vector[ResultTypeIntent],
      private[dotty] val bodies: Vector[BodyIntent],
      private[dotty] val appends: Vector[AppendIntent]
  ):
    def isEmpty: Boolean = editCount == 0
    def editCount: Int =
      omissions.size + parameterTypes.size + resultTypes.size + bodies.size + appends.size

    def omit(member: MemberRef): Either[Failure, EditPlan] =
      for
        selected <- validateMemberRef(captureIdentity, member)
        _ <- Either.cond(
          !omissions.exists(sameMember(_, selected)),
          (),
          conflict(s"direct member ${selected.index} is already omitted.")
        )
        _ <- Either.cond(
          !hasFieldEdit(selected.index),
          (),
          conflict(s"direct member ${selected.index} cannot be omitted and field-edited.")
        )
      yield copy(omissions = omissions :+ selected.ref)

    def replaceParameterType(
        parameter: ParameterRef,
        replacement: TypeNormalForm
    ): Either[Failure, EditPlan] =
      for
        selected <- validateParameterRef(captureIdentity, parameter)
        semantic <- Option(replacement).toRight(
          failure("SEMANTIC_FRAGMENT_INVALID", "the replacement parameter Type must be present.")
        )
        _ <- Either.cond(
          !isOmitted(selected.methodIdentity.memberIndex),
          (),
          conflict(s"direct member ${selected.methodIdentity.memberIndex} is already omitted.")
        )
        _ <- Either.cond(
          !parameterTypes.exists(intent => sameParameter(intent.parameter, selected)),
          (),
          conflict("the selected parameter Type is already edited.")
        )
      yield copy(parameterTypes = parameterTypes :+ ParameterTypeIntent(selected, semantic))

    def replaceResultType(
        method: MethodRef,
        replacement: TypeNormalForm
    ): Either[Failure, EditPlan] =
      for
        selected <- validateMethodRef(captureIdentity, method)
        semantic <- Option(replacement).toRight(
          failure("SEMANTIC_FRAGMENT_INVALID", "the replacement result Type must be present.")
        )
        _ <- Either.cond(
          !isOmitted(selected.memberIndex),
          (),
          conflict(s"direct member ${selected.memberIndex} is already omitted.")
        )
        _ <- Either.cond(
          !resultTypes.exists(intent => sameMethod(intent.method, selected)),
          (),
          conflict("the selected method result Type is already edited.")
        )
      yield copy(resultTypes = resultTypes :+ ResultTypeIntent(selected, semantic))

    def replaceBody(
        method: MethodRef,
        replacement: TermShape
    ): Either[Failure, EditPlan] =
      for
        selected <- validateMethodRef(captureIdentity, method)
        semantic <- Option(replacement).toRight(
          failure("SEMANTIC_FRAGMENT_INVALID", "the replacement body must be present.")
        )
        _ <- selected.scopeIdentity.validate(semantic)
        _ <- Either.cond(
          !isOmitted(selected.memberIndex),
          (),
          conflict(s"direct member ${selected.memberIndex} is already omitted.")
        )
        _ <- Either.cond(
          !bodies.exists(intent => sameMethod(intent.method, selected)),
          (),
          conflict("the selected method body is already edited.")
        )
      yield copy(bodies = bodies :+ BodyIntent(selected, semantic))

    def append(
        definition: SemanticDefinition,
        virtualSourceName: String
    ): Either[Failure, EditPlan] =
      for
        semantic <- Option(definition).toRight(
          failure("SEMANTIC_FRAGMENT_INVALID", "the appended semantic Definition must be present.")
        )
        path <- Option(virtualSourceName).toRight(
          failure("SEMANTIC_FRAGMENT_INVALID", "the generated virtual source name must be present.")
        )
        _ <- GeneratedOriginFragmentSupport.validateVirtualSourceName(path)
          .left.map(problem => failure("SEMANTIC_FRAGMENT_INVALID", problem.message))
      yield copy(appends = appends :+ AppendIntent(semantic, path))

    private def copy(
        omissions: Vector[MemberRef] = omissions,
        parameterTypes: Vector[ParameterTypeIntent] = parameterTypes,
        resultTypes: Vector[ResultTypeIntent] = resultTypes,
        bodies: Vector[BodyIntent] = bodies,
        appends: Vector[AppendIntent] = appends
    ): EditPlan =
      new EditPlan(captureIdentity, omissions, parameterTypes, resultTypes, bodies, appends)

    private def isOmitted(memberIndex: Int): Boolean =
      omissions.exists(_.memberIndex == memberIndex)

    private def hasFieldEdit(memberIndex: Int): Boolean =
      parameterTypes.exists(_.parameter.methodIdentity.memberIndex == memberIndex) ||
        resultTypes.exists(_.method.memberIndex == memberIndex) ||
        bodies.exists(_.method.memberIndex == memberIndex)

  private object EditPlan:
    def empty(capture: ExistingUntpdClassMemberFilter.Capture): EditPlan =
      new EditPlan(capture, Vector.empty, Vector.empty, Vector.empty, Vector.empty, Vector.empty)

  final class Result private[dotty] (
      private val treeValue: untpd.TypeDef,
      private val changedValue: Boolean,
      private val directMemberIdentitiesValue: Vector[ExactIdentity]
  ):
    def tree: untpd.TypeDef = treeValue
    def changed: Boolean = changedValue
    def directMemberIdentities: Vector[ExactIdentity] = directMemberIdentitiesValue

  def capture(existingClass: untpd.Tree)(using Context): Either[Failure, Capture] =
    Option(existingClass).toRight(
      failure("MISSING_INPUT", "the existing class tree must be present.")
    ).flatMap { tree =>
      ExistingUntpdClassMemberFilter.capture(tree)
        .left.map(problem => failure("CAPTURE_FAILED", privateDetail("U023", problem.code, problem.detail)))
        .map { exact =>
          val members = exact.members.map { member =>
            val ref = new MemberRef(exact, member.index, member.tree)
            new MemberView(
              ref,
              member.index,
              memberKind(member.tree),
              diagnosticName(member.tree),
              new ExactIdentity(member.tree)
            )
          }
          new Capture(exact, members, summon[Context])
        }
    }

  def apply(
      capture: Capture,
      plan: EditPlan
  )(using Context): Either[Failure, Result] =
    for
      selectedCapture <- Option(capture).toRight(
        failure("MISSING_INPUT", "the Capture must be present.")
      )
      selectedPlan <- Option(plan).toRight(
        failure("MISSING_INPUT", "the EditPlan must be present.")
      )
      _ <- Either.cond(
        selectedPlan.captureIdentity.eq(selectedCapture.exact),
        (),
        failure("SELECTION_FAILED", "the EditPlan belongs to a different Capture.")
      )
      methodEdits <- prepareMethods(selectedCapture.exact, selectedPlan)
      generated <- prepareGenerated(selectedPlan.appends)
      transaction <- ExistingUntpdClassOwnerTransaction
        .apply(
          selectedCapture.exact,
          selectedPlan.omissions.map(_.memberIndex),
          methodEdits,
          generated
        )
        .left.map(mapOwnerTransaction)
    yield new Result(
      transaction.root,
      transaction.changed,
      transaction.finalMembers.map(member => new ExactIdentity(member))
    )

  private[dotty] final case class ParameterTypeIntent(
      parameter: ParameterRef,
      replacement: TypeNormalForm
  )
  private[dotty] final case class ResultTypeIntent(
      method: MethodRef,
      replacement: TypeNormalForm
  )
  private[dotty] final case class BodyIntent(method: MethodRef, replacement: TermShape)
  private[dotty] final case class AppendIntent(
      definition: SemanticDefinition,
      virtualSourceName: String
  )

  private[dotty] final class ParameterScopeState(
      val captureIdentity: ExistingUntpdClassMemberFilter.Capture,
      val memberIndex: Int,
      val methodIdentity: untpd.DefDef,
      val parameters: TermBindingInternals.PersistentParameters,
      val parameterTrees: Vector[Vector[untpd.ValDef]],
      val binders: Vector[Vector[TermBinder]]
  ):
    private val expectedCounts = parameterTrees.map(_.size)

    def binder(parameter: ParameterRef): Either[Failure, TermBinder] =
      validateLocal(parameter).flatMap { selected =>
        parameters.binderAt(selected.clauseOrdinal, selected.parameterOrdinal)
          .left.map(problem => internal("Core binder lookup", problem.message))
      }

    def reference(parameter: ParameterRef): Either[Failure, TermShape] =
      validateLocal(parameter).flatMap { selected =>
        parameters.referenceAt(selected.clauseOrdinal, selected.parameterOrdinal)
          .left.map(problem => internal("Core parameter reference", problem.message))
      }

    def validate(shape: TermShape): Either[Failure, TermShape] =
      parameters.complete(shape)
        .flatMap(parameters.validateDefinitionBody(expectedCounts, _))
        .left.map { problem =>
          failure(
            "SEMANTIC_FRAGMENT_INVALID",
            s"the replacement body does not belong to this method parameter scope: ${problem.message}"
          )
        }

    def importProjection(
        projection: ExistingUntpdOrdinaryMethodRhsTermProjection.Projection
    ): Either[Failure, TermShape] =
      translateReferences(
        projection.shape,
        projection.parameterBinders,
        (clauseIndex, parameterIndex) =>
          parameters.referenceAt(clauseIndex, parameterIndex)
            .left.map(problem => internal("Core projected parameter reference", problem.message))
      ).flatMap(validate)

    def adaptForPreparation(
        shape: TermShape,
        descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
        target: ExistingUntpdOrdinaryMethodRhsEditPreparation.ReplacementScope
    ): Either[ExistingUntpdOrdinaryMethodRhsEditPreparation.Error, TermShape] =
      translateReferences(
        shape,
        binders,
        (clauseIndex, parameterIndex) =>
          descriptor.parameterClauses.lift(clauseIndex)
            .flatMap(_.lift(parameterIndex))
            .toRight(failure(
              "INTERNAL_INVARIANT_FAILED",
              "the public parameter topology no longer matches the U037 descriptor."
            ))
            .flatMap(parameter => target.reference(parameter).left.map(problem =>
              failure("SEMANTIC_FRAGMENT_INVALID", privateDetail("U041", problem.code, problem.detail))
            ))
      ) match
        case Right(value) => Right(value)
        case Left(problem) => Left(
          ExistingUntpdOrdinaryMethodRhsEditPreparation.Error(problem.code, problem.detail)
        )

    private def validateLocal(parameter: ParameterRef): Either[Failure, ParameterRef] =
      Option(parameter).toRight(
        failure("SELECTION_FAILED", "the ParameterRef must be present.")
      ).flatMap { selected =>
        val method = selected.methodIdentity
        val exact = parameterTrees.lift(selected.clauseOrdinal)
          .flatMap(_.lift(selected.parameterOrdinal))
        Either.cond(
          method != null && method.scopeIdentity.eq(this) &&
            method.captureIdentity.eq(captureIdentity) &&
            method.memberIndex == memberIndex &&
            method.methodIdentity.eq(methodIdentity) &&
            exact.exists(_.eq(selected.parameterIdentity)),
          selected,
          failure("SELECTION_FAILED", "the ParameterRef does not belong to this method parameter scope.")
        )
      }

  private object ParameterScopeState:
    def create(
        descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor
    ): Either[Failure, ParameterScopeState] =
      val names = descriptor.parameterClauses.map(_.map(_.tree.name.toString))
      for
        parameters <- TermBindingInternals.persistentParameters(names)
          .left.map(problem => internal("Core method parameter scope", problem.message))
        binders <- collectBinders(parameters, names)
      yield new ParameterScopeState(
        descriptor.captured,
        descriptor.memberIndex,
        descriptor.method,
        parameters,
        descriptor.parameterClauses.map(_.map(_.tree)),
        binders
      )

    private def collectBinders(
        parameters: TermBindingInternals.PersistentParameters,
        names: Vector[Vector[String]]
    ): Either[Failure, Vector[Vector[TermBinder]]] =
      names.zipWithIndex.foldLeft[Either[Failure, Vector[Vector[TermBinder]]]](Right(Vector.empty)) {
        case (clausesResult, (clause, clauseIndex)) =>
          for
            clauses <- clausesResult
            binders <- clause.indices.foldLeft[Either[Failure, Vector[TermBinder]]](Right(Vector.empty)) {
              case (bindersResult, parameterIndex) =>
                for
                  values <- bindersResult
                  binder <- parameters.binderAt(clauseIndex, parameterIndex)
                    .left.map(problem => internal("Core method parameter binder", problem.message))
                yield values :+ binder
            }
          yield clauses :+ binders
      }

  private def prepareMethods(
      capture: ExistingUntpdClassMemberFilter.Capture,
      plan: EditPlan
  )(using Context): Either[Failure, Vector[ExistingUntpdOrdinaryMethodReconstruction.ReconstructedMethod]] =
    val indices = (
      plan.parameterTypes.map(_.parameter.methodIdentity.memberIndex) ++
        plan.resultTypes.map(_.method.memberIndex) ++
        plan.bodies.map(_.method.memberIndex)
    ).distinct.sorted
    indices.foldLeft[Either[Failure, Vector[ExistingUntpdOrdinaryMethodReconstruction.ReconstructedMethod]]](Right(Vector.empty)) {
      case (editsResult, memberIndex) =>
        for
          edits <- editsResult
          descriptor <- ExistingUntpdOrdinaryMethodDescriptor.capture(capture, memberIndex)
            .left.map(mapDescriptor)
          parameterPreparations <- prepareParameterTypes(descriptor, plan.parameterTypes.filter(
            _.parameter.methodIdentity.memberIndex == memberIndex
          ))
          resultPreparation <- plan.resultTypes.find(_.method.memberIndex == memberIndex) match
            case None => Right(None)
            case Some(intent) =>
              ExistingUntpdOrdinaryMethodTypeEditPreparation
                .prepareResultType(descriptor, intent.replacement)
                .left.map(mapTypePreparation)
                .map(Some(_))
          bodyPreparation <- plan.bodies.find(_.method.memberIndex == memberIndex) match
            case None => Right(None)
            case Some(intent) =>
              ExistingUntpdOrdinaryMethodRhsEditPreparation
                .prepareBody(descriptor) { target =>
                  intent.method.scopeIdentity.adaptForPreparation(
                    intent.replacement,
                    descriptor,
                    target
                  )
                }
                .left.map(mapBodyPreparation)
                .map(Some(_))
          reconstructed <- ExistingUntpdOrdinaryMethodReconstruction
            .reconstructMethod(
              descriptor,
              parameterPreparations,
              resultPreparation,
              bodyPreparation
            )
            .left.map(problem => failure(
              "RECONSTRUCTION_FAILED",
              privateDetail("U042", problem.code, problem.detail)
            ))
        yield edits :+ reconstructed
    }

  private def prepareParameterTypes(
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      intents: Vector[ParameterTypeIntent]
  )(using Context): Either[Failure, Vector[ExistingUntpdOrdinaryMethodTypeEditPreparation.PreparedParameterType]] =
    intents.foldLeft[Either[Failure, Vector[ExistingUntpdOrdinaryMethodTypeEditPreparation.PreparedParameterType]]](Right(Vector.empty)) {
      case (preparedResult, intent) =>
        for
          prepared <- preparedResult
          parameter <- descriptor.parameterClauses.lift(intent.parameter.clauseOrdinal)
            .flatMap(_.lift(intent.parameter.parameterOrdinal))
            .filter(value =>
              value.tree.eq(intent.parameter.parameterIdentity) &&
                value.tpt.eq(intent.parameter.typeIdentity)
            )
            .toRight(failure(
              "SELECTION_FAILED",
              "the ParameterRef no longer matches the exact U037 parameter topology."
            ))
          next <- ExistingUntpdOrdinaryMethodTypeEditPreparation
            .prepareParameterType(descriptor, parameter, intent.replacement)
            .left.map(mapTypePreparation)
        yield prepared :+ next
    }

  private def prepareGenerated(
      appends: Vector[AppendIntent]
  )(using Context): Either[Failure, Vector[ExistingUntpdClassOwnerTransaction.PreparedGeneratedMember]] =
    appends.foldLeft[Either[Failure, Vector[ExistingUntpdClassOwnerTransaction.PreparedGeneratedMember]]](Right(Vector.empty)) {
      case (preparedResult, intent) =>
        for
          prepared <- preparedResult
          next <- ExistingUntpdClassOwnerTransaction
            .prepareGenerated(intent.definition, intent.virtualSourceName)
            .left.map(mapGeneratedPreparation)
        yield prepared :+ next
    }

  private def validateMemberRef(
      capture: ExistingUntpdClassMemberFilter.Capture,
      member: MemberRef
  ): Either[Failure, MemberView] =
    Option(member).toRight(
      failure("SELECTION_FAILED", "the MemberRef must be present.")
    ).flatMap { selected =>
      capture.members.lift(selected.memberIndex) match
        case Some(exactMember)
            if selected.captureIdentity.eq(capture) && exactMember.tree.eq(selected.treeIdentity) =>
          Right(new MemberView(
            selected,
            selected.memberIndex,
            memberKind(exactMember.tree),
            diagnosticName(exactMember.tree),
            new ExactIdentity(exactMember.tree)
          ))
        case _ => Left(failure(
          "SELECTION_FAILED",
          "the MemberRef does not belong to the exact captured direct-member topology."
        ))
    }

  private def validateMethodRef(
      capture: ExistingUntpdClassMemberFilter.Capture,
      method: MethodRef
  ): Either[Failure, MethodRef] =
    Option(method).toRight(
      failure("SELECTION_FAILED", "the MethodRef must be present.")
    ).flatMap { selected =>
      val member = capture.members.lift(selected.memberIndex)
      Either.cond(
        selected.captureIdentity.eq(capture) &&
          member.exists(_.tree.eq(selected.methodIdentity)) &&
          selected.scopeIdentity != null &&
          selected.scopeIdentity.captureIdentity.eq(capture) &&
          selected.scopeIdentity.memberIndex == selected.memberIndex &&
          selected.scopeIdentity.methodIdentity.eq(selected.methodIdentity),
        selected,
        failure("SELECTION_FAILED", "the MethodRef does not belong to this Capture.")
      )
    }

  private def validateParameterRef(
      capture: ExistingUntpdClassMemberFilter.Capture,
      parameter: ParameterRef
  ): Either[Failure, ParameterRef] =
    Option(parameter).toRight(
      failure("SELECTION_FAILED", "the ParameterRef must be present.")
    ).flatMap { selected =>
      validateMethodRef(capture, selected.methodIdentity).flatMap { method =>
        method.scopeIdentity.parameterTrees.lift(selected.clauseOrdinal)
          .flatMap(_.lift(selected.parameterOrdinal)) match
          case Some(tree) if tree.eq(selected.parameterIdentity) => Right(selected)
          case _ => Left(failure(
            "SELECTION_FAILED",
            "the ParameterRef does not belong to the exact captured method topology."
          ))
      }
    }

  private def translateReferences[E](
      shape: TermShape,
      sourceBinders: Vector[Vector[TermBinder]],
      targetReference: (Int, Int) => Either[E, TermShape]
  ): Either[E | Failure, TermShape] =
    def loop(current: TermShape): Either[E | Failure, TermShape] =
      current match
        case _: TermShape.BoundReference =>
          TermShapeBindingView.inspect(current)
            .left.map(problem => failure("SEMANTIC_FRAGMENT_INVALID", problem.message))
            .flatMap { view =>
              view.boundReference match
                case None => Left(internal("Core binder inspection", "a bound reference had no binder view."))
                case Some(reference) =>
                  val positions = sourceBinders.zipWithIndex.flatMap { (clause, clauseIndex) =>
                    clause.zipWithIndex.collect {
                      case (binder, parameterIndex) if binder == reference.binder =>
                        clauseIndex -> parameterIndex
                    }
                  }
                  positions match
                    case Vector((clauseIndex, parameterIndex)) =>
                      targetReference(clauseIndex, parameterIndex)
                    case _ => Left(failure(
                      "SEMANTIC_FRAGMENT_INVALID",
                      "a bound reference does not identify exactly one captured method parameter."
                    ))
            }
        case value: TermShape.Identifier => Right(value)
        case value: TermShape.Literal => Right(value)
        case TermShape.Select(qualifier, name) =>
          loop(qualifier).map(TermShape.Select(_, name))
        case TermShape.Apply(function, arguments) =>
          for
            translatedFunction <- loop(function)
            translatedArguments <- translateList(arguments, loop)
          yield TermShape.Apply(translatedFunction, translatedArguments)
        case TermShape.Infix(left, operator, right) =>
          for
            translatedLeft <- loop(left)
            translatedRight <- loop(right)
          yield TermShape.Infix(translatedLeft, operator, translatedRight)
        case TermShape.Unary(operator, operand) =>
          loop(operand).map(TermShape.Unary(operator, _))
        case TermShape.Tuple(elements) =>
          translateList(elements, loop).map(TermShape.Tuple(_))
        case TermShape.If(condition, thenBranch, elseBranch) =>
          for
            translatedCondition <- loop(condition)
            translatedThen <- loop(thenBranch)
            translatedElse <- loop(elseBranch)
          yield TermShape.If(translatedCondition, translatedThen, translatedElse)
        case TermShape.Parenthesized(expression) =>
          loop(expression).map(TermShape.Parenthesized(_))
        case other => Right(other)
    loop(shape)

  private def translateList[E](
      values: List[TermShape],
      translate: TermShape => Either[E, TermShape]
  ): Either[E, List[TermShape]] =
    values.foldRight[Either[E, List[TermShape]]](Right(Nil)) { (value, result) =>
      for
        translated <- translate(value)
        rest <- result
      yield translated :: rest
    }

  private def sameMember(left: MemberRef, right: MemberView): Boolean =
    left.memberIndex == right.index && left.treeIdentity.eq(right.ref.treeIdentity)

  private def sameMethod(left: MethodRef, right: MethodRef): Boolean =
    left.captureIdentity.eq(right.captureIdentity) && left.memberIndex == right.memberIndex &&
      left.methodIdentity.eq(right.methodIdentity)

  private def sameParameter(left: ParameterRef, right: ParameterRef): Boolean =
    sameMethod(left.methodIdentity, right.methodIdentity) &&
      left.clauseOrdinal == right.clauseOrdinal &&
      left.parameterOrdinal == right.parameterOrdinal &&
      left.parameterIdentity.eq(right.parameterIdentity)

  private def memberKind(tree: untpd.Tree): MemberKind = tree match
    case _: untpd.DefDef => MemberKind.Method
    case _: untpd.ValDef => MemberKind.Value
    case _: untpd.TypeDef => MemberKind.TypeMember
    case _ => MemberKind.Other

  private def diagnosticName(tree: untpd.Tree): Option[String] = tree match
    case member: untpd.MemberDef => Option(member.name).map(_.toString)
    case _ => None

  private def mapDescriptor(
      problem: ExistingUntpdOrdinaryMethodDescriptor.Error
  ): Failure =
    val code = problem.code match
      case "UNSUPPORTED_PARAMETER_TOPOLOGY" | "CONTEXTUAL_PARAMETER_UNSUPPORTED" =>
        "UNSUPPORTED_STRUCTURE"
      case "CAPTURE_REQUIRED" | "CAPTURE_INVARIANT_FAILED" |
          "MEMBER_INDEX_NOT_CAPTURED" | "SELECTED_MEMBER_NOT_METHOD" =>
        "SELECTION_FAILED"
      case "MALFORMED_OWNER_GRAPH" | "SYMBOL_BEARING_OWNER_GRAPH" |
          "TYPED_SPLICE_OWNER_GRAPH" => "CAPTURE_FAILED"
      case _ => "INTERNAL_INVARIANT_FAILED"
    failure(code, privateDetail("U037", problem.code, problem.detail))

  private def mapTypeProjection(
      problem: ExistingUntpdOrdinaryMethodTypeSlotProjection.Error
  ): Failure =
    val code =
      if problem.code.contains("INTERNAL") then "INTERNAL_INVARIANT_FAILED"
      else "UNSUPPORTED_STRUCTURE"
    failure(code, privateDetail("U038", problem.code, problem.detail))

  private def mapTermProjection(
      problem: ExistingUntpdOrdinaryMethodRhsTermProjection.Error
  ): Failure =
    val code =
      if problem.code.contains("FINAL") || problem.code.contains("INVARIANT") then
        "INTERNAL_INVARIANT_FAILED"
      else "UNSUPPORTED_STRUCTURE"
    failure(code, privateDetail("U039", problem.code, problem.detail))

  private def mapTypePreparation(
      problem: ExistingUntpdOrdinaryMethodTypeEditPreparation.Error
  ): Failure =
    val code = problem.code match
      case value if value.startsWith("SEMANTIC_TYPE_") && value.endsWith("UNSUPPORTED") =>
        "UNSUPPORTED_STRUCTURE"
      case value if value.startsWith("SEMANTIC_") => "SEMANTIC_FRAGMENT_INVALID"
      case value if value.startsWith("TYPE_LOWERING") || value.startsWith("LOWERED_") =>
        "LOWERING_FAILED"
      case value if value.contains("ORIGIN") || value.startsWith("OLD_") => "ORIGIN_FAILED"
      case value if value.startsWith("INVALID_DESCRIPTOR") || value.startsWith("PARAMETER_") =>
        "SELECTION_FAILED"
      case value if value.startsWith("FINAL_") => "INTERNAL_INVARIANT_FAILED"
      case _ => "INTERNAL_INVARIANT_FAILED"
    failure(code, privateDetail("U040", problem.code, problem.detail))

  private def mapBodyPreparation(
      problem: ExistingUntpdOrdinaryMethodRhsEditPreparation.Error
  ): Failure =
    val code = problem.code match
      case "FOREIGN_BINDER_GRAPH" | "FREE_IDENTIFIER_PARAMETER_CAPTURE" |
          "PLACEHOLDER_IDENTIFIER_UNSUPPORTED" | "SEMANTIC_IDENTIFIER_INVALID" |
          "SEMANTIC_REPLACEMENT_REQUIRED" => "SEMANTIC_FRAGMENT_INVALID"
      case "SEMANTIC_FRAGMENT_INVALID" => "SEMANTIC_FRAGMENT_INVALID"
      case value if value.startsWith("SEMANTIC_") => "UNSUPPORTED_STRUCTURE"
      case value if value.startsWith("SOURCE_FREE_") || value == "TERM_CONSTRUCTION_FAILED" =>
        "LOWERING_FAILED"
      case value if value.contains("ORIGIN") || value.startsWith("OLD_RHS") => "ORIGIN_FAILED"
      case "INVALID_DESCRIPTOR" | "PARAMETER_REQUIRED" | "PARAMETER_NOT_CAPTURED" |
          "PARAMETER_OCCURRENCE_NOT_UNIQUE" => "SELECTION_FAILED"
      case value if value.startsWith("FINAL_") => "INTERNAL_INVARIANT_FAILED"
      case _ => "INTERNAL_INVARIANT_FAILED"
    failure(code, privateDetail("U041", problem.code, problem.detail))

  private def mapGeneratedPreparation(
      problem: ExistingUntpdClassOwnerTransaction.Error
  ): Failure =
    val detail = privateDetail("U044", problem.code, problem.detail)
    val code =
      if problem.detail.contains("INVALID_VIRTUAL_SOURCE") then "SEMANTIC_FRAGMENT_INVALID"
      else if problem.detail.contains("GENERATED_ORIGIN_FAILED") then "ORIGIN_FAILED"
      else if problem.detail.contains("EXACT_LOWERING_FAILED") then "LOWERING_FAILED"
      else if problem.code == "INVALID_GENERATED_PREPARATION" then "INTERNAL_INVARIANT_FAILED"
      else "LOWERING_FAILED"
    failure(code, detail)

  private def mapOwnerTransaction(
      problem: ExistingUntpdClassOwnerTransaction.Error
  ): Failure =
    val code =
      if problem.code == "FINAL_TRANSACTION_INVARIANT_FAILED" then "INTERNAL_INVARIANT_FAILED"
      else "RECONSTRUCTION_FAILED"
    failure(code, privateDetail("U044", problem.code, problem.detail))

  private def conflict(detail: String): Failure = failure("EDIT_CONFLICT", detail)

  private def internal(stage: String, detail: String): Failure =
    failure("INTERNAL_INVARIANT_FAILED", s"$stage failed: $detail")

  private def privateDetail(stage: String, code: String, detail: String): String =
    s"$stage $code: $detail"

  private def failure(code: String, detail: String): Failure = Failure(code, detail)
