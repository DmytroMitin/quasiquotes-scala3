package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

/** Compiler-free semantic carrier for the exact AUXify-041 instance factory. */
private[quasiquotes] object InstanceFactoryPlan:
  enum ParameterMode derives CanEqual:
    case ByValue, ByName

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String
  ) derives CanEqual

  final case class ValueType(
      reference: TypeParameterReference
  ) derives CanEqual

  final case class BinaryFunctionType(
      firstArgument: TypeParameterReference,
      secondArgument: TypeParameterReference,
      result: TypeParameterReference
  ) derives CanEqual

  final case class ByNameCarrier(
      binderId: BinderId,
      displayName: String,
      mode: ParameterMode,
      valueType: ValueType
  ) derives CanEqual

  final case class BinaryFunctionCarrier(
      binderId: BinderId,
      displayName: String,
      mode: ParameterMode,
      functionType: BinaryFunctionType
  ) derives CanEqual

  final case class NestedParameter(
      binderId: BinderId,
      displayName: String,
      parameterType: TypeParameterReference
  ) derives CanEqual

  final case class TermReference(binderId: BinderId) derives CanEqual

  final case class EmptyOverride(
      memberDisplayName: String,
      body: TermReference
  ) derives CanEqual

  final case class CombineBody(
      callee: TermReference,
      arguments: Vector[TermReference]
  ) derives CanEqual

  final case class CombineOverride(
      memberDisplayName: String,
      firstParameter: NestedParameter,
      secondParameter: NestedParameter,
      resultType: TypeParameterReference,
      body: CombineBody
  ) derives CanEqual

  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final class Plan private[definitions] (
      val factoryDisplayName: String,
      val typeParameter: TypeParameter,
      val emptyValue: ByNameCarrier,
      val combineFunction: BinaryFunctionCarrier,
      val targetType: Applied,
      val emptyOverride: EmptyOverride,
      val combineOverride: CombineOverride
  )

  def create(
      factoryDisplayName: String,
      typeParameter: TypeParameter,
      emptyValue: ByNameCarrier,
      combineFunction: BinaryFunctionCarrier,
      targetType: ScopedType,
      emptyOverride: EmptyOverride,
      combineOverride: CombineOverride
  ): Either[ModelError, Plan] =
    for
      _ <- present(typeParameter, "TYPE_PARAMETER_MISSING", "the Type parameter")
      _ <- present(emptyValue, "EMPTY_VALUE_CARRIER_MISSING", "the by-name carrier")
      _ <- present(
        combineFunction,
        "COMBINE_FUNCTION_CARRIER_MISSING",
        "the binary-function carrier"
      )
      _ <- present(targetType, "TARGET_TYPE_MISSING", "the target Type")
      _ <- present(emptyOverride, "EMPTY_OVERRIDE_MISSING", "the empty override")
      _ <- present(combineOverride, "COMBINE_OVERRIDE_MISSING", "the combine override")
      _ <- present(
        combineOverride.firstParameter,
        "COMBINE_FIRST_PARAMETER_MISSING",
        "the first combine parameter"
      )
      _ <- present(
        combineOverride.secondParameter,
        "COMBINE_SECOND_PARAMETER_MISSING",
        "the second combine parameter"
      )
      _ <- present(combineOverride.body, "COMBINE_BODY_MISSING", "the combine body")
      _ <- present(emptyOverride.body, "EMPTY_BODY_MISSING", "the empty body reference")
      _ <- present(combineOverride.body.callee, "COMBINE_CALLEE_MISSING", "the combine callee")
      _ <- require(
        combineOverride.body.arguments != null &&
          combineOverride.body.arguments.forall(_ != null),
        "COMBINE_ARGUMENTS_MISSING",
        "the combine argument references must be present."
      )
      _ <- requireDistinctBinders(
        Vector(
          typeParameter.binderId,
          emptyValue.binderId,
          combineFunction.binderId,
          combineOverride.firstParameter.binderId,
          combineOverride.secondParameter.binderId
        )
      )
      _ <- legalName(factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
      _ <- legalName(
        typeParameter.displayName,
        "TYPE_PARAMETER_NAME_INVALID",
        "Type parameter"
      )
      _ <- legalName(
        emptyValue.displayName,
        "EMPTY_VALUE_CARRIER_NAME_INVALID",
        "by-name carrier"
      )
      _ <- legalName(
        combineFunction.displayName,
        "COMBINE_FUNCTION_CARRIER_NAME_INVALID",
        "binary-function carrier"
      )
      _ <- legalName(
        emptyOverride.memberDisplayName,
        "EMPTY_OVERRIDE_NAME_INVALID",
        "empty override"
      )
      _ <- legalName(
        combineOverride.memberDisplayName,
        "COMBINE_OVERRIDE_NAME_INVALID",
        "combine override"
      )
      _ <- legalName(
        combineOverride.firstParameter.displayName,
        "COMBINE_FIRST_PARAMETER_NAME_INVALID",
        "first combine parameter"
      )
      _ <- legalName(
        combineOverride.secondParameter.displayName,
        "COMBINE_SECOND_PARAMETER_NAME_INVALID",
        "second combine parameter"
      )
      _ <- require(
        emptyValue.mode == ParameterMode.ByName,
        "EMPTY_VALUE_MODE_MISMATCH",
        "the empty-value carrier must use by-name parameter mode."
      )
      _ <- present(
        emptyValue.valueType,
        "EMPTY_VALUE_TYPE_MISSING",
        "the empty-value carrier Type"
      )
      _ <- validateReference(
        emptyValue.valueType.reference,
        typeParameter,
        "EMPTY_VALUE_TYPE_BINDER_MISMATCH"
      )
      _ <- require(
        combineFunction.mode == ParameterMode.ByValue,
        "COMBINE_FUNCTION_MODE_MISMATCH",
        "the binary-function carrier must use ordinary by-value parameter mode."
      )
      _ <- present(
        combineFunction.functionType,
        "COMBINE_FUNCTION_TYPE_MISSING",
        "the binary-function Type"
      )
      _ <- validateBinaryFunction(combineFunction.functionType, typeParameter)
      validatedTarget <- validateTarget(targetType, typeParameter)
      _ <- validateReference(
        combineOverride.firstParameter.parameterType,
        typeParameter,
        "COMBINE_PARAMETER_TYPE_BINDER_MISMATCH"
      )
      _ <- validateReference(
        combineOverride.secondParameter.parameterType,
        typeParameter,
        "COMBINE_PARAMETER_TYPE_BINDER_MISMATCH"
      )
      _ <- validateReference(
        combineOverride.resultType,
        typeParameter,
        "COMBINE_RESULT_TYPE_BINDER_MISMATCH"
      )
      _ <- require(
        emptyOverride.body.binderId == emptyValue.binderId,
        "EMPTY_BODY_BINDER_MISMATCH",
        "the empty body must reference the exact outer by-name carrier binder."
      )
      _ <- require(
        combineOverride.body.callee.binderId == combineFunction.binderId,
        "COMBINE_BODY_CALLEE_BINDER_MISMATCH",
        "the combine callee must reference the exact outer binary-function carrier binder."
      )
      _ <- require(
        combineOverride.body.arguments.map(_.binderId) == Vector(
          combineOverride.firstParameter.binderId,
          combineOverride.secondParameter.binderId
        ),
        "COMBINE_BODY_ARGUMENT_BINDER_MISMATCH",
        "the combine arguments must reference the first and second nested binders once, in order."
      )
    yield new Plan(
      factoryDisplayName,
      typeParameter,
      emptyValue,
      combineFunction,
      validatedTarget,
      emptyOverride,
      combineOverride
    )

  private def requireDistinctBinders(
      binders: Vector[BinderId]
  ): Either[ModelError, Unit] =
    require(
      binders.forall(_ != null) && binders.distinct.size == 5,
      "DECLARATION_BINDERS_MUST_BE_DISTINCT",
      "the Type, two outer Term, and two nested Term declaration roles require five distinct BinderIds."
    )

  private def validateBinaryFunction(
      value: BinaryFunctionType,
      declaration: TypeParameter
  ): Either[ModelError, Unit] =
    for
      _ <- validateReference(
        value.firstArgument,
        declaration,
        "COMBINE_FUNCTION_TYPE_BINDER_MISMATCH"
      )
      _ <- validateReference(
        value.secondArgument,
        declaration,
        "COMBINE_FUNCTION_TYPE_BINDER_MISMATCH"
      )
      _ <- validateReference(
        value.result,
        declaration,
        "COMBINE_FUNCTION_TYPE_BINDER_MISMATCH"
      )
    yield ()

  private def validateTarget(
      value: ScopedType,
      declaration: TypeParameter
  ): Either[ModelError, Applied] =
    value match
      case applied @ Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        for
          _ <- legalName(
            constructor,
            "TARGET_TYPE_CONSTRUCTOR_INVALID",
            "target Type constructor"
          )
          _ <- validateReference(
            reference,
            declaration,
            "TARGET_TYPE_BINDER_MISMATCH"
          )
        yield applied
      case Applied(SourceName(constructor), Vector(_)) =>
        legalName(
          constructor,
          "TARGET_TYPE_CONSTRUCTOR_INVALID",
          "target Type constructor"
        ).flatMap(_ =>
          Left(
            error(
              "TARGET_TYPE_BINDER_MISMATCH",
              "the unary target argument must be the exact factory Type-binder reference."
            )
          )
        )
      case _ =>
        Left(
          error(
            "TARGET_TYPE_UNSUPPORTED",
            "the target must be one source-named unary application to the factory Type binder."
          )
        )

  private def validateReference(
      reference: TypeParameterReference,
      declaration: TypeParameter,
      code: String
  ): Either[ModelError, Unit] =
    require(
      reference != null &&
        reference.binderId == declaration.binderId &&
        reference.displayName == declaration.displayName,
      code,
      "the Type reference must retain the exact factory Type binder and display spelling."
    )

  private def present[A](
      value: A,
      code: String,
      role: String
  ): Either[ModelError, Unit] =
    require(value != null, code, s"$role must be present.")

  private def legalName(
      value: String,
      code: String,
      role: String
  ): Either[ModelError, Unit] =
    require(
      value != null && DefinitionName.fromSource(value).isRight,
      code,
      s"the $role must be one legal Scala source name."
    )

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[ModelError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): ModelError =
    ModelError(code, detail)
