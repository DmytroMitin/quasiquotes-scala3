package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.util.control.NonFatal

/** Compiler-free semantic plan for the exact AUXify-087 curried-method factory. */
private[quasiquotes] object CurriedMethodInstanceFactoryPlan:
  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  enum ParameterMode derives CanEqual:
    case ByValue

  enum DeclarationRole derives CanEqual:
    case TypeParameter, StrictCarrier, FirstNestedParameter, SecondNestedParameter

  enum ExternalTypeRole derives CanEqual:
    case TargetConstructor

  enum MemberRole derives CanEqual:
    case OverrideMethod

  final case class RoleTopology(
      carrierOuterArgumentRole: DeclarationRole,
      carrierInnerArgumentRole: DeclarationRole,
      carrierResultRole: DeclarationRole,
      targetArgumentRole: DeclarationRole,
      resultTargetRole: ExternalTypeRole,
      parentTargetRole: ExternalTypeRole,
      firstParameterTypeRole: DeclarationRole,
      secondParameterTypeRole: DeclarationRole,
      overrideResultRole: DeclarationRole,
      bodyCalleeRole: DeclarationRole,
      bodyInnerArgumentRole: DeclarationRole,
      bodyOuterArgumentRole: DeclarationRole,
      generatedMemberRole: MemberRole
  ) derives CanEqual

  final case class RoleSnapshot(
      factorySourceName: String,
      typeParameterSourceName: String,
      carrierSourceName: String,
      targetConstructorSourceName: String,
      memberSourceName: String,
      firstParameterSourceName: String,
      secondParameterSourceName: String,
      topology: RoleTopology
  ) derives CanEqual

  final case class TypeParameter(binderId: BinderId, displayName: String) derives CanEqual

  final case class NestedUnaryFunctionType(
      firstArgument: TypeParameterReference,
      secondArgument: TypeParameterReference,
      result: TypeParameterReference
  ) derives CanEqual

  final case class StrictCarrier(
      binderId: BinderId,
      displayName: String,
      mode: ParameterMode,
      parameterType: NestedUnaryFunctionType
  ) derives CanEqual

  final case class NestedParameter(
      binderId: BinderId,
      displayName: String,
      mode: ParameterMode,
      parameterType: TypeParameterReference
  ) derives CanEqual

  final case class TermReference(binderId: BinderId) derives CanEqual

  final case class CurriedApplyBody(
      callee: TermReference,
      firstArgument: TermReference,
      secondArgument: TermReference
  ) derives CanEqual

  final case class MethodOverride(
      memberDisplayName: String,
      resultType: TypeParameterReference,
      body: CurriedApplyBody
  ) derives CanEqual

  final class Plan private[definitions] (
      val factoryDisplayName: String,
      val typeParameter: TypeParameter,
      val strictCarrier: StrictCarrier,
      val resultTarget: Applied,
      val anonymousParentTarget: Applied,
      val firstParameter: NestedParameter,
      val secondParameter: NestedParameter,
      val methodOverride: MethodOverride,
      val roleSnapshot: RoleSnapshot
  )

  private var nextBinderValue = 0

  def create(
      factoryDisplayName: String,
      typeParameterDisplayName: String,
      carrierDisplayName: String,
      targetTypeConstructorDisplayName: String,
      memberDisplayName: String,
      firstParameterDisplayName: String,
      secondParameterDisplayName: String
  ): Either[ModelError, Plan] =
    for
      _ <- legalName(factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
      _ <- legalName(typeParameterDisplayName, "TYPE_PARAMETER_NAME_INVALID", "Type parameter")
      _ <- legalName(carrierDisplayName, "CARRIER_NAME_INVALID", "strict carrier")
      _ <- legalName(targetTypeConstructorDisplayName, "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID", "target Type constructor")
      _ <- legalName(memberDisplayName, "MEMBER_NAME_INVALID", "override member")
      _ <- legalName(firstParameterDisplayName, "FIRST_PARAMETER_NAME_INVALID", "first nested parameter")
      _ <- legalName(secondParameterDisplayName, "SECOND_PARAMETER_NAME_INVALID", "second nested parameter")
      _ <- lexicalRoles(
        typeParameterDisplayName,
        carrierDisplayName,
        targetTypeConstructorDisplayName,
        memberDisplayName,
        firstParameterDisplayName,
        secondParameterDisplayName
      )
      binders <- allocateBinders()
      typeDeclaration = TypeParameter(binders(0), typeParameterDisplayName)
      typeReference = TypeParameterReference(typeDeclaration.binderId, typeDeclaration.displayName)
      carrier = StrictCarrier(
        binders(1),
        carrierDisplayName,
        ParameterMode.ByValue,
        NestedUnaryFunctionType(typeReference, typeReference, typeReference)
      )
      first = NestedParameter(binders(2), firstParameterDisplayName, ParameterMode.ByValue, typeReference)
      second = NestedParameter(binders(3), secondParameterDisplayName, ParameterMode.ByValue, typeReference)
      target = Applied(SourceName(targetTypeConstructorDisplayName), Vector(typeReference))
      member = MethodOverride(
        memberDisplayName,
        typeReference,
        CurriedApplyBody(
          TermReference(carrier.binderId),
          TermReference(first.binderId),
          TermReference(second.binderId)
        )
      )
      snapshot = RoleSnapshot(
        factoryDisplayName,
        typeParameterDisplayName,
        carrierDisplayName,
        targetTypeConstructorDisplayName,
        memberDisplayName,
        firstParameterDisplayName,
        secondParameterDisplayName,
        canonicalTopology
      )
    yield new Plan(
      factoryDisplayName,
      typeDeclaration,
      carrier,
      target,
      target,
      first,
      second,
      member,
      snapshot
    )

  def validate(plan: Plan): Either[ModelError, RoleSnapshot] =
    Option(plan)
      .toRight(error("PLAN_MISSING", "the curried-method factory plan must be present."))
      .flatMap(validatePresent)

  private def validatePresent(plan: Plan): Either[ModelError, RoleSnapshot] =
    try
      for
        _ <- require(
          plan.typeParameter != null && plan.strictCarrier != null &&
            plan.resultTarget != null && plan.anonymousParentTarget != null &&
            plan.firstParameter != null && plan.secondParameter != null &&
            plan.methodOverride != null && plan.roleSnapshot != null,
          "PLAN_GRAPH_INVALID",
          "every curried-method factory declaration and semantic edge must be present."
        )
        _ <- legalName(plan.factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
        _ <- legalName(plan.typeParameter.displayName, "TYPE_PARAMETER_NAME_INVALID", "Type parameter")
        _ <- legalName(plan.strictCarrier.displayName, "CARRIER_NAME_INVALID", "strict carrier")
        _ <- legalName(plan.firstParameter.displayName, "FIRST_PARAMETER_NAME_INVALID", "first nested parameter")
        _ <- legalName(plan.secondParameter.displayName, "SECOND_PARAMETER_NAME_INVALID", "second nested parameter")
        _ <- requireDistinctBinders(plan)
        _ <- validateCarrier(plan)
        resultConstructor <- targetConstructor(plan.resultTarget, plan.typeParameter)
        parentConstructor <- targetConstructor(plan.anonymousParentTarget, plan.typeParameter)
        _ <- require(
          resultConstructor == parentConstructor && plan.resultTarget == plan.anonymousParentTarget,
          "PLAN_GRAPH_INVALID",
          "the result and anonymous parent must retain the same exact unary target role."
        )
        _ <- legalName(resultConstructor, "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID", "target Type constructor")
        _ <- legalName(plan.methodOverride.memberDisplayName, "MEMBER_NAME_INVALID", "override member")
        _ <- validateNestedParameter(plan.firstParameter, plan.typeParameter)
        _ <- validateNestedParameter(plan.secondParameter, plan.typeParameter)
        _ <- require(
          exactTypeReference(plan.methodOverride.resultType, plan.typeParameter) &&
            plan.methodOverride.body != null &&
            plan.methodOverride.body.callee != null &&
            plan.methodOverride.body.firstArgument != null &&
            plan.methodOverride.body.secondArgument != null &&
            plan.methodOverride.body.callee.binderId == plan.strictCarrier.binderId &&
            plan.methodOverride.body.firstArgument.binderId == plan.firstParameter.binderId &&
            plan.methodOverride.body.secondArgument.binderId == plan.secondParameter.binderId,
          "PLAN_GRAPH_INVALID",
          "the override result and nested application must reference the exact retained declarations in order."
        )
        _ <- lexicalRoles(
          plan.typeParameter.displayName,
          plan.strictCarrier.displayName,
          resultConstructor,
          plan.methodOverride.memberDisplayName,
          plan.firstParameter.displayName,
          plan.secondParameter.displayName
        ).left.map(_ => error("PLAN_GRAPH_INVALID", "the validated lexical roles must remain non-collapsing."))
        expected = RoleSnapshot(
          plan.factoryDisplayName,
          plan.typeParameter.displayName,
          plan.strictCarrier.displayName,
          resultConstructor,
          plan.methodOverride.memberDisplayName,
          plan.firstParameter.displayName,
          plan.secondParameter.displayName,
          canonicalTopology
        )
        _ <- require(plan.roleSnapshot == expected, "PLAN_GRAPH_INVALID", "the stored role snapshot must exactly describe the retained semantic graph.")
      yield expected
    catch case NonFatal(_) => Left(error("PLAN_GRAPH_INVALID", "the curried-method factory semantic graph is malformed."))

  private def validateCarrier(plan: Plan): Either[ModelError, Unit] =
    val valid = plan.strictCarrier.mode == ParameterMode.ByValue && plan.strictCarrier.parameterType != null &&
      exactTypeReference(plan.strictCarrier.parameterType.firstArgument, plan.typeParameter) &&
      exactTypeReference(plan.strictCarrier.parameterType.secondArgument, plan.typeParameter) &&
      exactTypeReference(plan.strictCarrier.parameterType.result, plan.typeParameter)
    require(valid, "PLAN_GRAPH_INVALID", "the strict carrier must retain exactly A => (A => A) over the exact Type declaration.")

  private def validateNestedParameter(parameter: NestedParameter, declaration: TypeParameter): Either[ModelError, Unit] =
    require(
      parameter.mode == ParameterMode.ByValue && exactTypeReference(parameter.parameterType, declaration),
      "PLAN_GRAPH_INVALID",
      "each nested parameter must remain strict and reference the exact Type declaration."
    )

  private def requireDistinctBinders(plan: Plan): Either[ModelError, Unit] =
    val ids = Vector(plan.typeParameter.binderId, plan.strictCarrier.binderId, plan.firstParameter.binderId, plan.secondParameter.binderId)
    require(ids.forall(_ != null) && ids.distinct.size == 4, "PLAN_GRAPH_INVALID", "the four declarations must retain distinct non-null identities.")

  private def targetConstructor(target: Applied, declaration: TypeParameter): Either[ModelError, String] =
    target match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) if exactTypeReference(reference, declaration) => Right(constructor)
      case _ => Left(error("PLAN_GRAPH_INVALID", "each target must be one direct unary source-named application of the exact Type declaration."))

  private def exactTypeReference(reference: TypeParameterReference, declaration: TypeParameter): Boolean =
    reference != null && reference.binderId != null && reference.binderId == declaration.binderId && reference.displayName == declaration.displayName

  private def lexicalRoles(tpe: String, carrier: String, target: String, member: String, first: String, second: String): Either[ModelError, Unit] =
    for
      _ <- require(target != tpe, "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE", "the target Type constructor must remain distinct from the local non-higher-kinded Type parameter.")
      _ <- require(carrier != member, "CARRIER_MEMBER_ROLE_COLLAPSE", "the override member would capture the carrier reference as recursive self-reference.")
      _ <- require(carrier != first, "CARRIER_FIRST_PARAMETER_ROLE_COLLAPSE", "the first nested parameter would shadow the outer carrier.")
      _ <- require(carrier != second, "CARRIER_SECOND_PARAMETER_ROLE_COLLAPSE", "the second nested parameter would shadow the outer carrier.")
      _ <- require(first != second, "NESTED_PARAMETER_NAMES_MUST_BE_DISTINCT", "parameters in separate clauses of one method must still have distinct names.")
    yield ()

  private def canonicalTopology: RoleTopology =
    RoleTopology(
      DeclarationRole.TypeParameter,
      DeclarationRole.TypeParameter,
      DeclarationRole.TypeParameter,
      DeclarationRole.TypeParameter,
      ExternalTypeRole.TargetConstructor,
      ExternalTypeRole.TargetConstructor,
      DeclarationRole.TypeParameter,
      DeclarationRole.TypeParameter,
      DeclarationRole.TypeParameter,
      DeclarationRole.StrictCarrier,
      DeclarationRole.FirstNestedParameter,
      DeclarationRole.SecondNestedParameter,
      MemberRole.OverrideMethod
    )

  private def allocateBinders(): Either[ModelError, Vector[BinderId]] = synchronized:
    if nextBinderValue > Int.MaxValue - 4 then Left(error("BINDER_IDENTITY_EXHAUSTED", "the graph-local curried-method factory BinderId space is exhausted."))
    else
      val allocated = Vector.tabulate(4)(offset => BinderId(nextBinderValue + offset))
      nextBinderValue += 4
      Right(allocated)

  private def legalName(value: String, code: String, role: String): Either[ModelError, Unit] =
    DefinitionName.fromSource(value).left.map(problem => error(code, s"the $role must be one legal Scala source name: ${problem.detail}")).map(_ => ())

  private def require(condition: Boolean, code: String, detail: String): Either[ModelError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): ModelError = ModelError(code, detail)
