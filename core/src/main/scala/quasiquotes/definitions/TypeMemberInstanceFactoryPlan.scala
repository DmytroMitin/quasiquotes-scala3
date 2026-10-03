package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.util.control.NonFatal

/** Compiler-free semantic plan for the exact AUXify-082 abstract-Type-member factory. */
private[quasiquotes] object TypeMemberInstanceFactoryPlan:
  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  enum DeclarationRole derives CanEqual:
    case FirstTypeParameter, SecondTypeParameter

  enum ExternalTypeRole derives CanEqual:
    case TargetConstructor

  enum MemberRole derives CanEqual:
    case ResultAliasMember

  final case class RoleTopology(
      targetArgumentRole: DeclarationRole,
      resultRefinementBaseRole: ExternalTypeRole,
      resultAliasRhsRole: DeclarationRole,
      anonymousParentRole: ExternalTypeRole,
      anonymousAliasRhsRole: DeclarationRole,
      anonymousAliasMemberRole: MemberRole
  ) derives CanEqual

  final case class RoleSnapshot(
      factorySourceName: String,
      firstTypeParameterSourceName: String,
      secondTypeParameterSourceName: String,
      targetConstructorSourceName: String,
      memberSourceName: String,
      topology: RoleTopology
  ) derives CanEqual

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String
  ) derives CanEqual

  final class Plan private[definitions] (
      val factoryDisplayName: String,
      val firstTypeParameter: TypeParameter,
      val secondTypeParameter: TypeParameter,
      val resultType: Refinement,
      val resultTarget: Applied,
      val resultAlias: ScopedTypeAlias,
      val anonymousParentTarget: Applied,
      val anonymousAlias: ScopedTypeAlias,
      val roleSnapshot: RoleSnapshot
  )

  private var nextBinderValue = 0

  def create(
      factoryDisplayName: String,
      firstTypeParameterDisplayName: String,
      secondTypeParameterDisplayName: String,
      targetTypeConstructorDisplayName: String,
      memberDisplayName: String
  ): Either[ModelError, Plan] =
    for
      _ <- legalName(factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
      _ <- legalName(
        firstTypeParameterDisplayName,
        "FIRST_TYPE_PARAMETER_NAME_INVALID",
        "first Type parameter"
      )
      _ <- legalName(
        secondTypeParameterDisplayName,
        "SECOND_TYPE_PARAMETER_NAME_INVALID",
        "second Type parameter"
      )
      _ <- legalName(
        targetTypeConstructorDisplayName,
        "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
        "target Type constructor"
      )
      _ <- legalName(memberDisplayName, "MEMBER_NAME_INVALID", "concrete Type member")
      _ <- require(
        firstTypeParameterDisplayName != secondTypeParameterDisplayName,
        "TYPE_PARAMETER_NAMES_MUST_BE_DISTINCT",
        "the two declarations in one method Type-parameter clause must have distinct names."
      )
      _ <- require(
        targetTypeConstructorDisplayName != firstTypeParameterDisplayName &&
          targetTypeConstructorDisplayName != secondTypeParameterDisplayName,
        "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE",
        "the unary target constructor must remain distinct from both local non-higher-kinded Type parameters."
      )
      _ <- require(
        memberDisplayName != secondTypeParameterDisplayName,
        "MEMBER_SECOND_TYPE_PARAMETER_ROLE_COLLAPSE",
        "a member named like the second Type parameter makes its alias RHS cyclic."
      )
      binders <- allocateBinders()
      first = TypeParameter(binders(0), firstTypeParameterDisplayName)
      second = TypeParameter(binders(1), secondTypeParameterDisplayName)
      target = Applied(
        SourceName(targetTypeConstructorDisplayName),
        Vector(TypeParameterReference(first.binderId, first.displayName))
      )
      resultAlias = ScopedTypeAlias(
        memberDisplayName,
        TypeParameterReference(second.binderId, second.displayName)
      )
      anonymousAlias = ScopedTypeAlias(
        memberDisplayName,
        TypeParameterReference(second.binderId, second.displayName)
      )
      snapshot = RoleSnapshot(
        factoryDisplayName,
        firstTypeParameterDisplayName,
        secondTypeParameterDisplayName,
        targetTypeConstructorDisplayName,
        memberDisplayName,
        canonicalTopology
      )
    yield new Plan(
      factoryDisplayName,
      first,
      second,
      Refinement(target, Vector(resultAlias)),
      target,
      resultAlias,
      target,
      anonymousAlias,
      snapshot
    )

  /** Revalidates every retained semantic edge without exposing raw BinderId inputs. */
  def validate(plan: Plan): Either[ModelError, RoleSnapshot] =
    Option(plan)
      .toRight(error("PLAN_MISSING", "the abstract-Type-member factory plan must be present."))
      .flatMap(validatePresent)

  private def validatePresent(plan: Plan): Either[ModelError, RoleSnapshot] =
    try
      for
        _ <- require(
          plan.firstTypeParameter != null &&
            plan.secondTypeParameter != null &&
            plan.resultType != null &&
            plan.resultTarget != null &&
            plan.resultAlias != null &&
            plan.anonymousParentTarget != null &&
            plan.anonymousAlias != null &&
            plan.roleSnapshot != null,
          "PLAN_GRAPH_INVALID",
          "every declaration and retained abstract-Type-member factory edge must be present."
        )
        _ <- legalName(plan.factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
        _ <- legalName(
          plan.firstTypeParameter.displayName,
          "FIRST_TYPE_PARAMETER_NAME_INVALID",
          "first Type parameter"
        )
        _ <- legalName(
          plan.secondTypeParameter.displayName,
          "SECOND_TYPE_PARAMETER_NAME_INVALID",
          "second Type parameter"
        )
        _ <- require(
          plan.firstTypeParameter.binderId != null &&
            plan.secondTypeParameter.binderId != null &&
            plan.firstTypeParameter.binderId != plan.secondTypeParameter.binderId,
          "PLAN_GRAPH_INVALID",
          "the two Type declarations must retain distinct non-null identities."
        )
        _ <- require(
          plan.firstTypeParameter.displayName != plan.secondTypeParameter.displayName,
          "PLAN_GRAPH_INVALID",
          "the two Type declarations must retain distinct source names."
        )
        resultConstructor <- targetConstructor(plan.resultTarget, plan.firstTypeParameter)
        _ <- validateResultType(plan)
        _ <- validateAlias(plan.resultAlias, plan.secondTypeParameter)
        parentConstructor <- targetConstructor(
          plan.anonymousParentTarget,
          plan.firstTypeParameter
        )
        _ <- require(
          parentConstructor == resultConstructor &&
            plan.anonymousParentTarget == plan.resultTarget,
          "PLAN_GRAPH_INVALID",
          "the anonymous parent must retain the exact result target role."
        )
        _ <- validateAlias(plan.anonymousAlias, plan.secondTypeParameter)
        _ <- require(
          !plan.resultAlias.eq(plan.anonymousAlias) &&
            plan.anonymousAlias.memberName == plan.resultAlias.memberName,
          "PLAN_GRAPH_INVALID",
          "the result and anonymous aliases must remain distinct coherent semantic edges."
        )
        _ <- require(
          resultConstructor != plan.firstTypeParameter.displayName &&
            resultConstructor != plan.secondTypeParameter.displayName &&
            plan.resultAlias.memberName != plan.secondTypeParameter.displayName,
          "PLAN_GRAPH_INVALID",
          "the validated lexical roles must remain non-collapsing."
        )
        expected = RoleSnapshot(
          plan.factoryDisplayName,
          plan.firstTypeParameter.displayName,
          plan.secondTypeParameter.displayName,
          resultConstructor,
          plan.resultAlias.memberName,
          canonicalTopology
        )
        _ <- require(
          plan.roleSnapshot == expected,
          "PLAN_GRAPH_INVALID",
          "the stored role snapshot must exactly describe the retained semantic graph."
        )
      yield expected
    catch case NonFatal(_) =>
      Left(error("PLAN_GRAPH_INVALID", "the abstract-Type-member factory graph is malformed."))

  private def validateResultType(plan: Plan): Either[ModelError, Unit] =
    plan.resultType match
      case Refinement(base, members)
          if base == plan.resultTarget &&
            members != null &&
            members.size == 1 &&
            members.head != null &&
            members.head.eq(plan.resultAlias) =>
        Right(())
      case _ =>
        Left(
          error(
            "PLAN_GRAPH_INVALID",
            "the result must retain exactly one concrete alias edge over the exact target."
          )
        )

  private def validateAlias(
      alias: ScopedTypeAlias,
      declaration: TypeParameter
  ): Either[ModelError, Unit] =
    for
      _ <- legalName(alias.memberName, "MEMBER_NAME_INVALID", "concrete Type member")
      _ <- require(
        exactTypeReference(alias.rhs, declaration),
        "PLAN_GRAPH_INVALID",
        "each concrete alias RHS must reference the exact second Type declaration."
      )
    yield ()

  private def targetConstructor(
      target: Applied,
      declaration: TypeParameter
  ): Either[ModelError, String] =
    target match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference))
          if exactTypeReference(reference, declaration) =>
        legalName(
          constructor,
          "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
          "target Type constructor"
        ).map(_ => constructor)
      case _ =>
        Left(
          error(
            "PLAN_GRAPH_INVALID",
            "each target must be one direct unary source-named application of the exact first Type declaration."
          )
        )

  private def exactTypeReference(value: ScopedType, declaration: TypeParameter): Boolean =
    value match
      case TypeParameterReference(binderId, displayName) =>
        binderId != null &&
          binderId == declaration.binderId &&
          displayName == declaration.displayName
      case _ => false

  private def canonicalTopology: RoleTopology =
    RoleTopology(
      targetArgumentRole = DeclarationRole.FirstTypeParameter,
      resultRefinementBaseRole = ExternalTypeRole.TargetConstructor,
      resultAliasRhsRole = DeclarationRole.SecondTypeParameter,
      anonymousParentRole = ExternalTypeRole.TargetConstructor,
      anonymousAliasRhsRole = DeclarationRole.SecondTypeParameter,
      anonymousAliasMemberRole = MemberRole.ResultAliasMember
    )

  private def allocateBinders(): Either[ModelError, Vector[BinderId]] = synchronized:
    if nextBinderValue > Int.MaxValue - 2 then
      Left(
        error(
          "BINDER_IDENTITY_EXHAUSTED",
          "the graph-local abstract-Type-member factory BinderId space is exhausted."
        )
      )
    else
      val allocated = Vector.tabulate(2)(offset => BinderId(nextBinderValue + offset))
      nextBinderValue += 2
      Right(allocated)

  private def legalName(
      value: String,
      code: String,
      role: String
  ): Either[ModelError, Unit] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem => error(code, s"the $role must be one legal Scala source name: ${problem.detail}"))
      .map(_ => ())

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[ModelError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): ModelError =
    ModelError(code, detail)
