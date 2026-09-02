package quasiquotes.publicapi

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

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

/** Read-only result for one bounded ordinary single-parameter method. */
final class SingleParameterMethodResultView private (
    val name: String,
    val parameterName: String,
    val parameterType: CompletedType,
    val resultType: CompletedType,
    val body: CompletedTerm
) derives CanEqual:
  def kindCode: String = "single-parameter-method"

  def source: String =
    s"def $name($parameterName: ${parameterType.source}): ${resultType.source} = ${body.source}"

  override def equals(other: Any): Boolean =
    other match
      case that: SingleParameterMethodResultView =>
        name == that.name &&
          parameterName == that.parameterName &&
          parameterType == that.parameterType &&
          resultType == that.resultType &&
          body == that.body
      case _ => false

  override def hashCode: Int =
    (name, parameterName, parameterType, resultType, body).hashCode

  override def toString: String = source

private[publicapi] object SingleParameterMethodResultView:
  def create(
      name: String,
      parameterName: String,
      parameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): SingleParameterMethodResultView =
    new SingleParameterMethodResultView(
      name,
      parameterName,
      parameterType,
      resultType,
      body
    )

/** Read-only result for one bounded ordinary exact-two-parameter method. */
final class TwoParameterMethodResultView private (
    val name: String,
    val firstParameterName: String,
    val firstParameterType: CompletedType,
    val secondParameterName: String,
    val secondParameterType: CompletedType,
    val resultType: CompletedType,
    val body: CompletedTerm
) derives CanEqual:
  def kindCode: String = "two-parameter-method"

  def source: String =
    s"def $name($firstParameterName: ${firstParameterType.source}, $secondParameterName: ${secondParameterType.source}): ${resultType.source} = ${body.source}"

  override def equals(other: Any): Boolean =
    other match
      case that: TwoParameterMethodResultView =>
        name == that.name &&
          firstParameterName == that.firstParameterName &&
          firstParameterType == that.firstParameterType &&
          secondParameterName == that.secondParameterName &&
          secondParameterType == that.secondParameterType &&
          resultType == that.resultType &&
          body == that.body
      case _ => false

  override def hashCode: Int =
    (
      name,
      firstParameterName,
      firstParameterType,
      secondParameterName,
      secondParameterType,
      resultType,
      body
    ).hashCode

  override def toString: String = source

private[publicapi] object TwoParameterMethodResultView:
  def create(
      name: String,
      firstParameterName: String,
      firstParameterType: CompletedType,
      secondParameterName: String,
      secondParameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): TwoParameterMethodResultView =
    new TwoParameterMethodResultView(
      name,
      firstParameterName,
      firstParameterType,
      secondParameterName,
      secondParameterType,
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

  def singleParameterMethod(
      name: String,
      parameterName: String,
      parameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): Either[PublicFailure, SingleParameterMethodResultView] =
    constructSingleParameterMethod(
      name,
      parameterName,
      parameterType,
      resultType,
      body
    ).map(_ =>
      SingleParameterMethodResultView.create(
        name,
        parameterName,
        parameterType,
        resultType,
        body
      )
    )

  private[publicapi] def constructSingleParameterMethod(
      name: String,
      parameterName: String,
      parameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): Either[PublicFailure, ConstructedDefinition.SingleParameterDef] =
    for
      methodName <- validateName(name, FailureAnchor.MethodName)
      parameter <- validateName(parameterName, FailureAnchor.ParameterName)
      completedParameterType <- singleParameterDefinitionType(
        parameterType,
        FailureAnchor.ParameterType
      )
      completedResultType <- singleParameterDefinitionType(
        resultType,
        FailureAnchor.ResultType
      )
      _ <- Either.cond(
        completedResultType == completedParameterType,
        (),
        PublicFailure.invalidSingleParameterMethodContract(
          "A parameter-reference body requires the result type to equal the parameter type.",
          FailureAnchor.ResultType
        )
      )
      completedBody <- requireSingleParameterBody(body, parameter)
      internalMethodName <- DefinitionName
        .plain(methodName)
        .left
        .map(error =>
          PublicFailure.invalidSingleParameterMethodContract(
            error.message,
            FailureAnchor.MethodName
          )
        )
      internalParameterName <- DefinitionName
        .plain(parameter)
        .left
        .map(error =>
          PublicFailure.invalidSingleParameterMethodContract(
            error.message,
            FailureAnchor.ParameterName
          )
        )
      binderId = BinderId(0)
      internalBody <- ConstructedTerm
        .fromShapeInScope(
          TermShape.BoundReference(binderId, completedBody.referenceName),
          binderId
        )
        .left
        .map(error => PublicFailure.internalInvariant(error.message))
      constructed <- ConstructedDefinition
        .singleParameterDef(
          internalMethodName,
          binderId,
          internalParameterName,
          completedParameterType,
          completedResultType,
          internalBody
        )
        .left
        .map(error => PublicFailure.internalInvariant(error.message))
    yield constructed

  def twoParameterMethod(
      name: String,
      firstParameterName: String,
      firstParameterType: CompletedType,
      secondParameterName: String,
      secondParameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): Either[PublicFailure, TwoParameterMethodResultView] =
    constructTwoParameterMethod(
      name,
      firstParameterName,
      firstParameterType,
      secondParameterName,
      secondParameterType,
      resultType,
      body
    ).map(_ =>
      TwoParameterMethodResultView.create(
        name,
        firstParameterName,
        firstParameterType,
        secondParameterName,
        secondParameterType,
        resultType,
        body
      )
    )

  private[publicapi] def constructTwoParameterMethod(
      name: String,
      firstParameterName: String,
      firstParameterType: CompletedType,
      secondParameterName: String,
      secondParameterType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm
  ): Either[PublicFailure, ConstructedDefinition.TwoParameterDef] =
    for
      methodName <- validateName(name, FailureAnchor.MethodName)
      firstName <- validateName(firstParameterName, FailureAnchor.ParameterName)
      secondName <- validateName(secondParameterName, FailureAnchor.ParameterName)
      _ <- Either.cond(
        firstName != secondName,
        (),
        PublicFailure.invalidTwoParameterMethodContract(
          "The two ordinary parameter names must be distinct.",
          FailureAnchor.ParameterName
        )
      )
      completedFirstType <- definitionType(
        firstParameterType,
        FailureAnchor.ParameterType,
        PublicFailure.invalidTwoParameterMethodContract
      )
      completedSecondType <- definitionType(
        secondParameterType,
        FailureAnchor.ParameterType,
        PublicFailure.invalidTwoParameterMethodContract
      )
      completedResultType <- definitionType(
        resultType,
        FailureAnchor.ResultType,
        PublicFailure.invalidTwoParameterMethodContract
      )
      completedBody <- requireTwoParameterBody(body, firstName, secondName)
      selectedFirst = completedBody.referenceName == firstName
      selectedType =
        if selectedFirst then completedFirstType else completedSecondType
      _ <- Either.cond(
        completedResultType == selectedType,
        (),
        PublicFailure.invalidTwoParameterMethodContract(
          "A parameter-reference body requires the result type to equal the selected parameter type.",
          FailureAnchor.ResultType
        )
      )
      internalMethodName <- DefinitionName
        .plain(methodName)
        .left
        .map(error =>
          PublicFailure.invalidTwoParameterMethodContract(
            error.message,
            FailureAnchor.MethodName
          )
        )
      internalFirstName <- DefinitionName
        .plain(firstName)
        .left
        .map(error =>
          PublicFailure.invalidTwoParameterMethodContract(
            error.message,
            FailureAnchor.ParameterName
          )
        )
      internalSecondName <- DefinitionName
        .plain(secondName)
        .left
        .map(error =>
          PublicFailure.invalidTwoParameterMethodContract(
            error.message,
            FailureAnchor.ParameterName
          )
        )
      firstBinderId = BinderId(0)
      secondBinderId = BinderId(1)
      selectedBinderId = if selectedFirst then firstBinderId else secondBinderId
      internalBody <- ConstructedTerm
        .fromShapeInScope(
          TermShape.BoundReference(selectedBinderId, completedBody.referenceName),
          Vector(firstBinderId, secondBinderId)
        )
        .left
        .map(error => PublicFailure.internalInvariant(error.message))
      constructed <- ConstructedDefinition
        .twoParameterDef(
          internalMethodName,
          firstBinderId,
          internalFirstName,
          completedFirstType,
          secondBinderId,
          internalSecondName,
          completedSecondType,
          completedResultType,
          internalBody
        )
        .left
        .map(error => PublicFailure.internalInvariant(error.message))
    yield constructed

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

  private def requireSingleParameterBody(
      body: CompletedTerm,
      parameterName: String
  ): Either[PublicFailure, CompletedTerm] =
    if body == null then
      Left(
        PublicFailure.invalidSingleParameterMethodContract(
          "The method body must be present.",
          FailureAnchor.Body
        )
      )
    else if !body.isDefinitionParameterReference then
      Left(
        PublicFailure.invalidSingleParameterMethodContract(
          "Use CompletedTerm.definitionParameterReference(...) for a bound method-parameter body.",
          FailureAnchor.Body
        )
      )
    else if body.referenceName != parameterName then
      Left(
        PublicFailure.invalidSingleParameterMethodContract(
          s"The body parameter reference `${body.referenceName}` must match declared parameter `$parameterName`.",
          FailureAnchor.Body
        )
      )
    else Right(body)

  private def requireTwoParameterBody(
      body: CompletedTerm,
      firstParameterName: String,
      secondParameterName: String
  ): Either[PublicFailure, CompletedTerm] =
    if body == null then
      Left(
        PublicFailure.invalidTwoParameterMethodContract(
          "The method body must be present.",
          FailureAnchor.Body
        )
      )
    else if !body.isDefinitionParameterReference then
      Left(
        PublicFailure.invalidTwoParameterMethodContract(
          "Use CompletedTerm.definitionParameterReference(...) for a bound method-parameter body.",
          FailureAnchor.Body
        )
      )
    else if
      body.referenceName != firstParameterName &&
      body.referenceName != secondParameterName
    then
      Left(
        PublicFailure.invalidTwoParameterMethodContract(
          s"The body parameter reference `${body.referenceName}` must match declared parameter `$firstParameterName` or `$secondParameterName`.",
          FailureAnchor.Body
        )
      )
    else Right(body)

  private def definitionType(
      value: CompletedType,
      anchor: FailureAnchor,
      invalidContract: (String, FailureAnchor) => PublicFailure
  ): Either[PublicFailure, TypeNormalForm] =
    val converted =
      if value == null then
        Left(
          invalidContract(
            "The definition type must be present.",
            anchor
          )
        )
      else
        value.kindCode match
          case "named" =>
            Right(TypeNormalForm.STypeIdent(value.name.get))
          case "applied" =>
            for
              constructor <- definitionType(
                value.constructor.get,
                anchor,
                invalidContract
              )
              arguments <- collectDefinitionTypes(
                value.arguments,
                anchor,
                invalidContract
              )
            yield TypeNormalForm.STypeApply(constructor, arguments.toList)
          case "type-parameter" =>
            Left(
              invalidContract(
                "Ordinary parameter methods do not declare type parameters.",
                anchor
              )
            )
          case other =>
            Left(
              invalidContract(
                s"Unsupported completed definition type kind `$other`.",
                anchor
              )
            )

    converted.flatMap { normalForm =>
      TypeTemplate
        .validateConstructed(normalForm)
        .left
        .map(error =>
          invalidContract(
            error.message,
            anchor
          )
        )
        .map(_ => normalForm)
    }

  private def singleParameterDefinitionType(
      value: CompletedType,
      anchor: FailureAnchor
  ): Either[PublicFailure, TypeNormalForm] =
    convertSingleParameterDefinitionType(value, anchor).flatMap { normalForm =>
      TypeTemplate
        .validateConstructed(normalForm)
        .left
        .map(error =>
          PublicFailure.invalidSingleParameterMethodContract(
            error.message,
            anchor
          )
        )
        .map(_ => normalForm)
    }

  private def convertSingleParameterDefinitionType(
      value: CompletedType,
      anchor: FailureAnchor
  ): Either[PublicFailure, TypeNormalForm] =
    if value == null then
      Left(
        PublicFailure.invalidSingleParameterMethodContract(
          "The definition type must be present.",
          anchor
        )
      )
    else
      value.kindCode match
        case "named" =>
          Right(TypeNormalForm.STypeIdent(value.name.get))
        case "applied" =>
          for
            constructor <- convertSingleParameterDefinitionType(
              value.constructor.get,
              anchor
            )
            arguments <- collectSingleParameterDefinitionTypes(
              value.arguments,
              anchor
            )
          yield TypeNormalForm.STypeApply(constructor, arguments.toList)
        case "type-parameter" =>
          Left(
            PublicFailure.invalidSingleParameterMethodContract(
              "Ordinary parameter methods do not declare type parameters.",
              anchor
            )
          )
        case other =>
          Left(
            PublicFailure.invalidSingleParameterMethodContract(
              s"Unsupported completed definition type kind `$other`.",
              anchor
            )
          )

  private def collectSingleParameterDefinitionTypes(
      values: Vector[CompletedType],
      anchor: FailureAnchor
  ): Either[PublicFailure, Vector[TypeNormalForm]] =
    values.foldLeft[Either[PublicFailure, Vector[TypeNormalForm]]](
      Right(Vector.empty)
    ) { (result, value) =>
      for
        completed <- result
        next <- convertSingleParameterDefinitionType(value, anchor)
      yield completed :+ next
    }

  private def collectDefinitionTypes(
      values: Vector[CompletedType],
      anchor: FailureAnchor,
      invalidContract: (String, FailureAnchor) => PublicFailure
  ): Either[PublicFailure, Vector[TypeNormalForm]] =
    values.foldLeft[Either[PublicFailure, Vector[TypeNormalForm]]](
      Right(Vector.empty)
    ) { (result, value) =>
      for
        completed <- result
        next <- definitionType(value, anchor, invalidContract)
      yield completed :+ next
    }
