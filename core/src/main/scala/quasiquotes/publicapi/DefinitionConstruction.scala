package quasiquotes.publicapi

/** Read-only result for exactly one bounded contextual method. */
final class DefinitionResultView private (
    val name: String,
    val typeParameterName: String,
    val contextualParameterName: String,
    val contextualParameterType: CompletedType,
    val resultType: CompletedType,
    val body: CompletedTerm
) derives CanEqual:
  def kindCode: String = "method"

  override def equals(other: Any): Boolean =
    other match
      case that: DefinitionResultView =>
        name == that.name &&
          typeParameterName == that.typeParameterName &&
          contextualParameterName == that.contextualParameterName &&
          contextualParameterType == that.contextualParameterType &&
          resultType == that.resultType &&
          body == that.body
      case _ => false

  override def hashCode: Int =
    (
      name,
      typeParameterName,
      contextualParameterName,
      contextualParameterType,
      resultType,
      body
    ).hashCode

  override def toString: String =
    s"def $name[$typeParameterName](using $contextualParameterName: ${contextualParameterType.source}): ${resultType.source} = ${body.source}"

private[publicapi] object DefinitionResultView:
  def create(
      name: String,
      typeParameterName: String,
      contextualParameterName: String,
      contextualParameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): DefinitionResultView =
    new DefinitionResultView(
      name,
      typeParameterName,
      contextualParameterName,
      contextualParameterType,
      resultType,
      body
    )

object DefinitionConstruction:
  def contextualMethod(
      name: String,
      typeParameterName: String,
      contextualParameterName: String,
      contextualParameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): Either[PublicFailure, DefinitionResultView] =
    for
      methodName <- validateName(name, FailureAnchor.MethodName)
      parameterName <- validateName(typeParameterName, FailureAnchor.TypeParameter)
      contextualName <- validateName(
        contextualParameterName,
        FailureAnchor.ContextualParameterName
      )
      contextualType <- requirePresent(
        contextualParameterType,
        FailureAnchor.ContextualParameterType,
        "The contextual parameter type must be present."
      )
      completedResultType <- requirePresent(
        resultType,
        FailureAnchor.ResultType,
        "The result type must be present."
      )
      completedBody <- requirePresent(
        body,
        FailureAnchor.Body,
        "The method body must be present."
      )
      _ <- validateBoundType(
        contextualType,
        parameterName,
        FailureAnchor.ContextualParameterType
      )
      _ <- validateBoundType(
        completedResultType,
        parameterName,
        FailureAnchor.ResultType
      )
      _ <- Either.cond(
        completedBody.referenceName == contextualName,
        (),
        PublicFailure.invalidContextualMethodContract(
          s"The body must reference contextual parameter `$contextualName`.",
          FailureAnchor.Body
        )
      )
    yield DefinitionResultView.create(
      methodName,
      parameterName,
      contextualName,
      contextualType,
      completedResultType,
      completedBody
    )

  private def validateName(
      value: String,
      anchor: FailureAnchor
  ): Either[PublicFailure, String] =
    Either.cond(
      value != null && PublicIdentifier.isValid(value),
      value,
      PublicFailure.invalidName(String.valueOf(value), anchor)
    )

  private def requirePresent[A](
      value: A,
      anchor: FailureAnchor,
      detail: String
  ): Either[PublicFailure, A] =
    Either.cond(
      value != null,
      value,
      PublicFailure.invalidContextualMethodContract(detail, anchor)
    )

  private def validateBoundType(
      value: CompletedType,
      declared: String,
      anchor: FailureAnchor
  ): Either[PublicFailure, Unit] =
    CompletedType.firstUndeclared(value, declared) match
      case Some(name) =>
        Left(PublicFailure.undeclaredTypeParameter(name, anchor))
      case None => Right(())
