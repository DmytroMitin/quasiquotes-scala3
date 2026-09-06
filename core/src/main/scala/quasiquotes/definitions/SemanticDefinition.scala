package quasiquotes.definitions

import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBinder, TermBindingFailure, TermBindingInternals, TermShapeBindingView}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

import scala.util.control.NonFatal

/** Stable public failure returned by semantic Definition operations. */
final case class DefinitionSemanticError(code: String, detail: String) derives CanEqual:
  def message: String = s"$code: $detail"

/** Non-exhaustive semantic category for a public Definition value. */
final class DefinitionKind private (val code: String) derives CanEqual:
  override def equals(other: Any): Boolean =
    other match
      case that: DefinitionKind => code == that.code
      case _ => false

  override def hashCode(): Int = code.hashCode
  override def toString: String = s"DefinitionKind($code)"

object DefinitionKind:
  val Value: DefinitionKind = new DefinitionKind("value")
  val Method: DefinitionKind = new DefinitionKind("method")
  val TypeMember: DefinitionKind = new DefinitionKind("type-member")

/** Normalized Definition modifiers. V1 publicly constructs only the empty set. */
final class DefinitionModifiers private () derives CanEqual:
  override def equals(other: Any): Boolean = other.isInstanceOf[DefinitionModifiers]
  override def hashCode(): Int = 0
  override def toString: String = "DefinitionModifiers.empty"

object DefinitionModifiers:
  val empty: DefinitionModifiers = new DefinitionModifiers()

/** One validated ordinary method parameter. */
final case class DefinitionParameter(
    name: DefinitionName,
    declaredType: TypeNormalForm
) derives CanEqual

/** Non-exhaustive semantic kind for a Definition parameter clause. */
final class DefinitionParameterClauseKind private (val code: String) derives CanEqual:
  override def equals(other: Any): Boolean =
    other match
      case that: DefinitionParameterClauseKind => code == that.code
      case _ => false

  override def hashCode(): Int = code.hashCode
  override def toString: String = s"DefinitionParameterClauseKind($code)"

object DefinitionParameterClauseKind:
  val Ordinary: DefinitionParameterClauseKind =
    new DefinitionParameterClauseKind("ordinary")

/** A validated, scalable Definition parameter clause value. */
final class DefinitionParameterClause private (
    private val kindValue: DefinitionParameterClauseKind,
    private val parametersValue: Vector[DefinitionParameter]
) derives CanEqual:
  def kind: DefinitionParameterClauseKind = kindValue
  def parameters: Vector[DefinitionParameter] = parametersValue

  override def equals(other: Any): Boolean =
    other match
      case that: DefinitionParameterClause =>
        kindValue == that.kindValue && parametersValue == that.parametersValue
      case _ => false

  override def hashCode(): Int = (kindValue, parametersValue).hashCode
  override def toString: String =
    s"DefinitionParameterClause(${kindValue.code}, ${parametersValue.size} parameters)"

object DefinitionParameterClause:
  def ordinary(
      parameters: Vector[DefinitionParameter]
  ): Either[DefinitionSemanticError, DefinitionParameterClause] =
    DefinitionSemanticInternals.validateClauseParameters(parameters).map { validated =>
      new DefinitionParameterClause(DefinitionParameterClauseKind.Ordinary, validated)
    }

/** Persistent opaque co-reference view for a completed method's parameters. */
final class DefinitionParameterScope private[quasiquotes] (
    private val parametersIdentity: AnyRef
):
  private def parameters: TermBindingInternals.PersistentParameters =
    parametersIdentity.asInstanceOf[TermBindingInternals.PersistentParameters]

  def binder(
      clauseIndex: Int,
      parameterIndex: Int
  ): Either[DefinitionSemanticError, TermBinder] =
    parameters.binderAt(clauseIndex, parameterIndex)
      .left.map(DefinitionSemanticInternals.termFailure)

  def reference(
      clauseIndex: Int,
      parameterIndex: Int
  ): Either[DefinitionSemanticError, TermShape] =
    parameters.referenceAt(clauseIndex, parameterIndex)
      .left.map(DefinitionSemanticInternals.termFailure)

  private[definitions] def validateDefinitionBody(
      expectedParameterCounts: Vector[Int],
      body: TermShape
  ): Either[TermBindingFailure, TermShape] =
    parameters.validateDefinitionBody(expectedParameterCounts, body)

private[definitions] object DefinitionParameterScope:
  def apply(parameters: TermBindingInternals.PersistentParameters): DefinitionParameterScope =
    new DefinitionParameterScope(parameters.asInstanceOf[AnyRef])

final class ValueDefinitionView private[quasiquotes] (
    private val declaredTypeValue: TypeNormalForm,
    private val bodyValue: Option[TermShape]
):
  def declaredType: TypeNormalForm = declaredTypeValue
  def body: Option[TermShape] = bodyValue

final class MethodDefinitionView private[quasiquotes] (
    private val parameterClausesValue: Vector[DefinitionParameterClause],
    private val parameterScopeValue: DefinitionParameterScope,
    private val resultTypeValue: TypeNormalForm,
    private val bodyValue: Option[TermShape]
):
  def parameterClauses: Vector[DefinitionParameterClause] = parameterClausesValue
  def parameterScope: DefinitionParameterScope = parameterScopeValue
  def resultType: TypeNormalForm = resultTypeValue
  def body: Option[TermShape] = bodyValue

final class TypeDefinitionView private[quasiquotes] (
    private val aliasedTypeValue: Option[TypeNormalForm]
):
  def aliasedType: Option[TypeNormalForm] = aliasedTypeValue

/** Immutable, non-exhaustive public semantic Definition value. */
final class SemanticDefinition private[quasiquotes] (
    private val kindValue: DefinitionKind,
    private val nameValue: DefinitionName,
    private val modifiersValue: DefinitionModifiers,
    private val storageIdentity: AnyRef
) derives CanEqual:
  private def storage: SemanticDefinition.Storage =
    storageIdentity.asInstanceOf[SemanticDefinition.Storage]

  def kind: DefinitionKind = kindValue
  def name: DefinitionName = nameValue
  def modifiers: DefinitionModifiers = modifiersValue
  def asValue: Option[ValueDefinitionView] = storage.valueView
  def asMethod: Option[MethodDefinitionView] = storage.methodView
  def asType: Option[TypeDefinitionView] = storage.typeView

  private lazy val equalityKey: Any =
    (kindValue, nameValue, modifiersValue, storage.semanticKey)

  override def equals(other: Any): Boolean =
    other match
      case that: SemanticDefinition => equalityKey == that.equalityKey
      case _ => false

  override def hashCode(): Int = equalityKey.hashCode
  override def toString: String =
    s"SemanticDefinition(${kindValue.code}, ${nameValue.source})"

object SemanticDefinition:
  private sealed trait Storage:
    def valueView: Option[ValueDefinitionView] = None
    def methodView: Option[MethodDefinitionView] = None
    def typeView: Option[TypeDefinitionView] = None
    def semanticKey: Any

  private final class ValueStorage(
      declaredType: TypeNormalForm,
      rhs: TermShape
  ) extends Storage:
    override def valueView: Option[ValueDefinitionView] =
      Some(new ValueDefinitionView(declaredType, Some(rhs)))
    def semanticKey: Any = ("value", declaredType, rhs)

  private final class MethodStorage(
      clauses: Vector[DefinitionParameterClause],
      scope: DefinitionParameterScope,
      resultType: TypeNormalForm,
      body: TermShape,
      normalizedBody: TermShape
  ) extends Storage:
    override def methodView: Option[MethodDefinitionView] =
      Some(new MethodDefinitionView(clauses, scope, resultType, Some(body)))

    def semanticKey: Any =
      (
        "method",
        clauses.map(clause =>
          clause.kind.code -> clause.parameters.map(_.declaredType)
        ),
        resultType,
        normalizedBody
      )

  private final class TypeStorage(rhs: TypeNormalForm) extends Storage:
    override def typeView: Option[TypeDefinitionView] =
      Some(new TypeDefinitionView(Some(rhs)))
    def semanticKey: Any = ("type-member", rhs)

  def immutableValue(
      name: DefinitionName,
      declaredType: TypeNormalForm,
      rhs: TermShape,
      modifiers: DefinitionModifiers = DefinitionModifiers.empty
  ): Either[DefinitionSemanticError, SemanticDefinition] =
    for
      presentName <- DefinitionSemanticInternals.presentName(name)
      presentModifiers <- DefinitionSemanticInternals.presentModifiers(modifiers)
      validType <- DefinitionSemanticInternals.validType(declaredType, "value declared type")
      validRhs <- DefinitionSemanticInternals.validStandaloneTerm(rhs, "value right-hand side")
    yield new SemanticDefinition(
      DefinitionKind.Value,
      presentName,
      presentModifiers,
      new ValueStorage(validType, validRhs).asInstanceOf[AnyRef]
    )

  def concreteMethod(
      name: DefinitionName,
      parameterClauses: Vector[DefinitionParameterClause],
      resultType: TypeNormalForm,
      modifiers: DefinitionModifiers = DefinitionModifiers.empty
  )(
      body: DefinitionParameterScope =>
        Either[DefinitionSemanticError, TermShape]
  ): Either[DefinitionSemanticError, SemanticDefinition] =
    for
      presentName <- DefinitionSemanticInternals.presentName(name)
      presentModifiers <- DefinitionSemanticInternals.presentModifiers(modifiers)
      clauses <- DefinitionSemanticInternals.admittedMethodClauses(parameterClauses)
      validResultType <- DefinitionSemanticInternals.validType(resultType, "method result type")
      parameters <- TermBindingInternals
        .persistentParameters(clauses.map(_.parameters.map(_.name.source)))
        .left.map(DefinitionSemanticInternals.termFailure)
      scope = DefinitionParameterScope(parameters)
      returnedBody <- DefinitionSemanticInternals.invokeBody(body, scope, parameters)
      completedBody <- parameters.complete(returnedBody)
        .left.map(DefinitionSemanticInternals.termFailure)
      normalizedBody = parameters.alphaNormalize(completedBody)
    yield new SemanticDefinition(
      DefinitionKind.Method,
      presentName,
      presentModifiers,
      new MethodStorage(
        clauses,
        scope,
        validResultType,
        completedBody,
        normalizedBody
      ).asInstanceOf[AnyRef]
    )

  def typeAlias(
      name: DefinitionName,
      rhs: TypeNormalForm,
      modifiers: DefinitionModifiers = DefinitionModifiers.empty
  ): Either[DefinitionSemanticError, SemanticDefinition] =
    for
      presentName <- DefinitionSemanticInternals.presentName(name)
      presentModifiers <- DefinitionSemanticInternals.presentModifiers(modifiers)
      validRhs <- DefinitionSemanticInternals.validType(rhs, "type alias right-hand side")
    yield new SemanticDefinition(
      DefinitionKind.TypeMember,
      presentName,
      presentModifiers,
      new TypeStorage(validRhs).asInstanceOf[AnyRef]
    )

private[definitions] object DefinitionSemanticInternals:
  def presentName(value: DefinitionName): Either[DefinitionSemanticError, DefinitionName] =
    Option(value).toRight(missing("the definition name must be present."))

  def presentModifiers(
      value: DefinitionModifiers
  ): Either[DefinitionSemanticError, DefinitionModifiers] =
    Option(value).toRight(missing("definition modifiers must be present."))

  def validType(
      value: TypeNormalForm,
      label: String
  ): Either[DefinitionSemanticError, TypeNormalForm] =
    try
      Option(value).toRight(missing(s"the $label must be present.")).flatMap { present =>
        TypeTemplate.validateConstructed(present)
          .left.map(error => invalidType(s"invalid $label: ${error.message}"))
          .map(_ => present)
      }
    catch
      case NonFatal(error) =>
        Left(invalidType(s"invalid $label: ${error.getClass.getSimpleName}."))

  def validateClauseParameters(
      parameters: Vector[DefinitionParameter]
  ): Either[DefinitionSemanticError, Vector[DefinitionParameter]] =
    Option(parameters)
      .toRight(missing("ordinary Definition parameters must be present."))
      .flatMap { present =>
        if present.isEmpty then
          Left(invalidParameter("an ordinary Definition clause must contain a parameter."))
        else
          present.zipWithIndex
            .foldLeft[Either[DefinitionSemanticError, Vector[DefinitionParameter]]](
              Right(Vector.empty)
            ) { case (result, (parameter, index)) =>
              for
                collected <- result
                current <- Option(parameter).toRight(
                  invalidParameter(s"parameter $index must be present.")
                )
                _ <- Option(current.name).toRight(
                  invalidParameter(s"parameter $index must have a validated name.")
                )
                _ <- validType(current.declaredType, s"parameter $index declared type")
                  .left.map(error => invalidParameter(error.detail))
              yield collected :+ current
            }
            .flatMap { validated =>
              val duplicate = validated.groupBy(_.name.source).collectFirst {
                case (source, occurrences) if occurrences.size > 1 => source
              }
              duplicate
                .map(source =>
                  Left(invalidParameter(s"duplicate parameter name `$source`."))
                )
                .getOrElse(Right(validated))
            }
      }

  def admittedMethodClauses(
      clauses: Vector[DefinitionParameterClause]
  ): Either[DefinitionSemanticError, Vector[DefinitionParameterClause]] =
    Option(clauses).toRight(missing("method parameter clauses must be present.")).flatMap {
      case Vector() => Right(Vector.empty)
      case Vector(clause)
          if clause != null &&
            clause.kind == DefinitionParameterClauseKind.Ordinary &&
            (clause.parameters.size == 1 || clause.parameters.size == 2) =>
        validateClauseParameters(clause.parameters).map(_ => Vector(clause))
      case Vector(null) =>
        Left(invalidParameter("the method parameter clause must be present."))
      case other =>
        Left(
          unsupported(
            s"concrete methods currently admit zero clauses or one ordinary clause with one or two parameters; found ${other.size} clauses."
          )
        )
    }

  def validStandaloneTerm(
      value: TermShape,
      label: String
  ): Either[DefinitionSemanticError, TermShape] =
    Option(value).toRight(missing(s"the $label must be present.")).flatMap { present =>
      TermShapeBindingView.inspect(present)
        .left.map(termFailure)
        .map(_ => present)
    }

  def invokeBody(
      callback: DefinitionParameterScope => Either[DefinitionSemanticError, TermShape],
      scope: DefinitionParameterScope,
      parameters: TermBindingInternals.PersistentParameters
  ): Either[DefinitionSemanticError, TermShape] =
    if callback == null then Left(missing("the method body callback must be present."))
    else
      try
        val returned = TermBindingInternals.withPersistentParameters(parameters) {
          callback(scope)
        }
        Option(returned)
          .toRight(invalidBody("the method body callback returned no result."))
          .flatMap(identity)
          .flatMap(body =>
            Option(body).toRight(invalidBody("the method body callback returned no body."))
          )
      catch
        case NonFatal(error) =>
          Left(invalidBody(s"the method body callback failed: ${error.getClass.getSimpleName}."))

  def termFailure(error: TermBindingFailure): DefinitionSemanticError =
    error.code match
      case "TERM_BINDER_SCOPE_MISMATCH" =>
        failure("DEFINITION_SEMANTIC_SCOPE_MISMATCH", error.detail)
      case "TERM_BINDER_UNBOUND" =>
        failure("DEFINITION_SEMANTIC_UNBOUND_PARAMETER", error.detail)
      case "TERM_BINDING_UNSUPPORTED" =>
        unsupported(error.detail)
      case "TERM_BINDER_COLLISION" | "TERM_BINDING_INTERNAL_INVARIANT" =>
        failure("DEFINITION_SEMANTIC_INTERNAL_INVARIANT", error.detail)
      case _ => invalidBody(error.detail)

  private def failure(code: String, detail: String): DefinitionSemanticError =
    DefinitionSemanticError(code, detail)

  private def missing(detail: String): DefinitionSemanticError =
    failure("DEFINITION_SEMANTIC_MISSING", detail)

  private def invalidType(detail: String): DefinitionSemanticError =
    failure("DEFINITION_SEMANTIC_INVALID_TYPE", detail)

  private def invalidParameter(detail: String): DefinitionSemanticError =
    failure("DEFINITION_SEMANTIC_INVALID_PARAMETER", detail)

  private def invalidBody(detail: String): DefinitionSemanticError =
    failure("DEFINITION_SEMANTIC_INVALID_BODY", detail)

  private def unsupported(detail: String): DefinitionSemanticError =
    failure("DEFINITION_SEMANTIC_UNSUPPORTED", detail)
