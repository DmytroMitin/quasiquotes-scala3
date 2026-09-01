package quasiquotes.phase150

import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.BinderId

/** Test-only compiler-free semantic carrier for the exact AUXify-039 alias. */
private[quasiquotes] object AuxTypeAliasSemanticPlanProbe:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  sealed trait TypeNode derives CanEqual
  object TypeNode:
    final case class SourceName(value: String) extends TypeNode
    final case class BinderReference(binderId: BinderId, displayName: String)
        extends TypeNode
    final case class Applied(
        constructor: SourceName,
        arguments: Vector[BinderReference]
    ) extends TypeNode
    final case class Refinement(base: Applied, members: Vector[TypeAlias])
        extends TypeNode

  final case class TypeAlias(memberName: String, rhs: TypeNode) derives CanEqual

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String,
      upperBound: TypeNode.SourceName
  ) derives CanEqual

  final class Plan private[AuxTypeAliasSemanticPlanProbe] (
      val aliasName: String,
      val parameters: Vector[TypeParameter],
      val rhs: TypeNode.Refinement,
      val appliedBase: TypeNode.Applied,
      val refinementMember: TypeAlias,
      val outputReference: TypeNode.BinderReference
  ):
    val argumentBinderPositions: Vector[Int] =
      val positions = parameters.map(_.binderId).zipWithIndex.toMap
      appliedBase.arguments.map(argument => positions(argument.binderId))

  def create(
      aliasName: String,
      parameters: Vector[TypeParameter],
      rhs: TypeNode
  ): Either[Error, Plan] =
    import TypeNode.*
    for
      _ <- validName(aliasName, "ALIAS_NAME_INVALID", "alias")
      declarations <- validateParameters(parameters)
      refinement <- rhs match
        case value: Refinement => Right(value)
        case _ => Left(error("RHS_REFINEMENT_REQUIRED", "the alias RHS must be one refinement."))
      _ <- Either.cond(
        refinement.members.size == 1,
        (),
        error("REFINEMENT_MEMBER_COUNT_UNSUPPORTED", "the RHS must contain exactly one refinement alias.")
      )
      member = refinement.members.head
      _ <- validName(member.memberName, "REFINEMENT_MEMBER_NAME_INVALID", "refinement member")
      applied = refinement.base
      _ <- validName(applied.constructor.value, "TYPE_CONSTRUCTOR_NAME_INVALID", "type constructor")
      _ <- Either.cond(
        applied.arguments.size == 2,
        (),
        error("APPLIED_ARGUMENT_ARITY_UNSUPPORTED", "the target constructor must receive exactly two Type arguments.")
      )
      _ <- validateReferences(applied.arguments, declarations.take(2), "APPLIED_ARGUMENT")
      output <- member.rhs match
        case value: BinderReference => Right(value)
        case _ => Left(error("REFINEMENT_RHS_BINDER_REQUIRED", "the refinement RHS must reference the added output Type binder."))
      _ <- validateReference(output, declarations(2), "OUTPUT_REFERENCE")
    yield new Plan(aliasName, declarations, refinement, applied, member, output)

  private def validateParameters(
      parameters: Vector[TypeParameter]
  ): Either[Error, Vector[TypeParameter]] =
    if parameters == null || parameters.size != 3 || parameters.exists(_ == null)
    then Left(error("TYPE_PARAMETER_ARITY_UNSUPPORTED", "the alias requires exactly three present Type parameters."))
    else if parameters.exists(_.binderId == null) || parameters.map(_.binderId).distinct.size != 3
    then Left(error("TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT", "all three Type binders must be present and distinct."))
    else if parameters.map(_.displayName).distinct.size != 3
    then Left(error("TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT", "all three Type-parameter display names must be distinct."))
    else
      parameters.zipWithIndex.foldLeft(Right(()): Either[Error, Unit]) {
        case (result, (parameter, index)) =>
          result.flatMap(_ =>
            for
              _ <- validName(parameter.displayName, "TYPE_PARAMETER_DISPLAY_NAME_INVALID", s"Type parameter ${index + 1}")
              _ <- Option(parameter.upperBound)
                .toRight(error("TYPE_PARAMETER_UPPER_BOUND_REQUIRED", s"Type parameter ${index + 1} must have one source-named upper bound."))
              _ <- validName(parameter.upperBound.value, "TYPE_PARAMETER_UPPER_BOUND_INVALID", s"Type parameter ${index + 1} upper bound")
            yield ()
          )
      }.map(_ => parameters)

  private def validateReferences(
      actual: Vector[TypeNode.BinderReference],
      expected: Vector[TypeParameter],
      prefix: String
  ): Either[Error, Unit] =
    actual.zip(expected).zipWithIndex.foldLeft(Right(()): Either[Error, Unit]) {
      case (result, ((reference, declaration), index)) =>
        result.flatMap(_ => validateReference(reference, declaration, s"${prefix}_${index + 1}"))
    }

  private def validateReference(
      reference: TypeNode.BinderReference,
      declaration: TypeParameter,
      prefix: String
  ): Either[Error, Unit] =
    if reference == null || reference.binderId != declaration.binderId
    then Left(error(s"${prefix}_BINDER_MISMATCH", "the Type reference must use the exact declared binder identity."))
    else if reference.displayName != declaration.displayName
    then Left(error(s"${prefix}_DISPLAY_NAME_MISMATCH", "the Type reference display spelling must match its declaration."))
    else Right(())

  private def validName(value: String, code: String, role: String): Either[Error, Unit] =
    Option(value)
      .toRight(error(code, s"the $role name must be present."))
      .flatMap(name => DefinitionName.fromSource(name).left.map(problem => error(code, problem.message)).map(_ => ()))

  private def error(code: String, detail: String): Error = Error(code, detail)
