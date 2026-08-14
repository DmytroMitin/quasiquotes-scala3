package quasiquotes.definitions

import quasiquotes.parser.BinderId
import quasiquotes.terms.{ConstructedTerm, TermConstructionError}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[quasiquotes] sealed trait ConstructedDefinition derives CanEqual:
  def name: DefinitionName
  def render: String

private[quasiquotes] object ConstructedDefinition:
  final class ParameterlessDef private[ConstructedDefinition] (
      val name: DefinitionName,
      val resultType: TypeNormalForm,
      val body: ConstructedTerm
  ) extends ConstructedDefinition:
    def render: String =
      s"ConstructedParameterlessDef(name=${name.render}, resultType=${resultType.render}, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: ParameterlessDef =>
          name == that.name &&
            resultType == that.resultType &&
            body == that.body
        case _ => false

    override def hashCode: Int =
      ("ParameterlessDef", name, resultType, body).hashCode

    override def toString: String = render

  final class SingleParameterDef private[ConstructedDefinition] (
      val name: DefinitionName,
      val parameterBinderId: BinderId,
      val parameterName: DefinitionName,
      val parameterType: TypeNormalForm,
      val resultType: TypeNormalForm,
      val body: ConstructedTerm
  ) extends ConstructedDefinition:
    private lazy val semanticBody =
      ConstructedTerm.semanticInScope(body, parameterBinderId)

    def render: String =
      s"ConstructedSingleParameterDef(name=${name.render}, parameter=${parameterName.render}: ${parameterType.render}, resultType=${resultType.render}, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: SingleParameterDef =>
          name == that.name &&
            parameterType == that.parameterType &&
            resultType == that.resultType &&
            semanticBody == that.semanticBody
        case _ => false

    override def hashCode: Int =
      (
        "SingleParameterDef",
        name,
        parameterType,
        resultType,
        semanticBody
      ).hashCode

    override def toString: String = render

  final class TwoParameterDef private[ConstructedDefinition] (
      val name: DefinitionName,
      val firstParameterBinderId: BinderId,
      val firstParameterName: DefinitionName,
      val firstParameterType: TypeNormalForm,
      val secondParameterBinderId: BinderId,
      val secondParameterName: DefinitionName,
      val secondParameterType: TypeNormalForm,
      val resultType: TypeNormalForm,
      val body: ConstructedTerm
  ) extends ConstructedDefinition:
    private lazy val semanticBody =
      ConstructedTerm.semanticInScope(
        body,
        Vector(firstParameterBinderId, secondParameterBinderId)
      )

    def render: String =
      s"ConstructedTwoParameterDef(name=${name.render}, firstParameter=${firstParameterName.render}: ${firstParameterType.render}, secondParameter=${secondParameterName.render}: ${secondParameterType.render}, resultType=${resultType.render}, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: TwoParameterDef =>
          name == that.name &&
            firstParameterType == that.firstParameterType &&
            secondParameterType == that.secondParameterType &&
            resultType == that.resultType &&
            semanticBody == that.semanticBody
        case _ => false

    override def hashCode: Int =
      (
        "TwoParameterDef",
        name,
        firstParameterType,
        secondParameterType,
        resultType,
        semanticBody
      ).hashCode

    override def toString: String = render

  final class ImmutableVal private[ConstructedDefinition] (
      val name: DefinitionName,
      val declaredType: TypeNormalForm,
      val rhs: ConstructedTerm
  ) extends ConstructedDefinition:
    def render: String =
      s"ConstructedImmutableVal(name=${name.render}, declaredType=${declaredType.render}, rhs=${rhs.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: ImmutableVal =>
          name == that.name &&
            declaredType == that.declaredType &&
            rhs == that.rhs
        case _ => false

    override def hashCode: Int =
      ("ImmutableVal", name, declaredType, rhs).hashCode

    override def toString: String = render

  def parameterlessDef(
      name: DefinitionName,
      resultType: TypeNormalForm,
      body: ConstructedTerm
  ): Either[DefinitionConstructionError, ParameterlessDef] =
    validateType(resultType).map(_ =>
      new ParameterlessDef(name, resultType, body)
    )

  def singleParameterDef(
      name: DefinitionName,
      parameterBinderId: BinderId,
      parameterName: DefinitionName,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      body: ConstructedTerm
  ): Either[DefinitionConstructionError, SingleParameterDef] =
    for
      _ <- validateType(parameterType)
      _ <- validateType(resultType)
      _ <- ConstructedTerm
        .validateInScope(body, parameterBinderId)
        .left
        .map(error =>
          DefinitionConstructionError.InvalidConstructedDefinitionBody(
            error.message
          )
        )
    yield
      new SingleParameterDef(
        name,
        parameterBinderId,
        parameterName,
        parameterType,
        resultType,
        body
      )

  def twoParameterDef(
      name: DefinitionName,
      firstParameterBinderId: BinderId,
      firstParameterName: DefinitionName,
      firstParameterType: TypeNormalForm,
      secondParameterBinderId: BinderId,
      secondParameterName: DefinitionName,
      secondParameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      body: ConstructedTerm
  ): Either[DefinitionConstructionError, TwoParameterDef] =
    for
      _ <- validateTwoParameterList(
        firstParameterBinderId,
        firstParameterName,
        secondParameterBinderId,
        secondParameterName
      )
      _ <- validateType(firstParameterType)
      _ <- validateType(secondParameterType)
      _ <- validateType(resultType)
      _ <- ConstructedTerm
        .validateInScope(
          body,
          Vector(firstParameterBinderId, secondParameterBinderId)
        )
        .left
        .map(error =>
          DefinitionConstructionError.InvalidConstructedDefinitionBody(
            error.message
          )
        )
    yield
      new TwoParameterDef(
        name,
        firstParameterBinderId,
        firstParameterName,
        firstParameterType,
        secondParameterBinderId,
        secondParameterName,
        secondParameterType,
        resultType,
        body
      )

  def immutableVal(
      name: DefinitionName,
      declaredType: TypeNormalForm,
      rhs: ConstructedTerm
  ): Either[DefinitionConstructionError, ImmutableVal] =
    validateType(declaredType).map(_ =>
      new ImmutableVal(name, declaredType, rhs)
    )

  def fromShape(
      shape: DefinitionShape
  ): Either[DefinitionConstructionError, ConstructedDefinition] =
    shape match
      case method: DefinitionShape.ParameterlessDef =>
        for
          resultType <- normalFormFromShape(method.resultType)
          body <- constructedTermFromShape(method.body)
          result <- parameterlessDef(method.name, resultType, body)
            .left
            .map(error =>
              DefinitionConstructionError.UnsupportedParsedDefinitionType(
                error.message
              )
            )
        yield result
      case method: DefinitionShape.SingleParameterDef =>
        for
          parameterType <- normalFormFromShape(method.parameterType)
          resultType <- normalFormFromShape(method.resultType)
          body <- constructedTermFromShapeInScope(
            method.body,
            method.parameterBinderId
          )
          result <- singleParameterDef(
            method.name,
            method.parameterBinderId,
            method.parameterName,
            parameterType,
            resultType,
            body
          ).left
            .map(error =>
              DefinitionConstructionError.CompletedDefinitionFactoryFailure(
                error.message
              )
            )
        yield result
      case method: DefinitionShape.TwoParameterDef =>
        val binders = Vector(
          method.firstParameterBinderId,
          method.secondParameterBinderId
        )
        for
          firstParameterType <- normalFormFromShape(method.firstParameterType)
          secondParameterType <- normalFormFromShape(method.secondParameterType)
          resultType <- normalFormFromShape(method.resultType)
          body <- constructedTermFromShapeInScope(method.body, binders)
          result <- twoParameterDef(
            method.name,
            method.firstParameterBinderId,
            method.firstParameterName,
            firstParameterType,
            method.secondParameterBinderId,
            method.secondParameterName,
            secondParameterType,
            resultType,
            body
          ).left
            .map(error =>
              DefinitionConstructionError.CompletedDefinitionFactoryFailure(
                error.message
              )
            )
        yield result
      case value: DefinitionShape.ImmutableVal =>
        for
          declaredType <- normalFormFromShape(value.declaredType)
          rhs <- constructedTermFromShape(value.rhs)
          result <- immutableVal(value.name, declaredType, rhs)
            .left
            .map(error =>
              DefinitionConstructionError.UnsupportedParsedDefinitionType(
                error.message
              )
            )
        yield result

  private def validateType(
      normalForm: TypeNormalForm
  ): Either[DefinitionConstructionError, Unit] =
    TypeTemplate
      .validateConstructed(normalForm)
      .left
      .map(error =>
        DefinitionConstructionError.InvalidConstructedDefinitionType(
          error.message
        )
      )

  private def normalFormFromShape(
      shape: quasiquotes.parser.TypeShape
  ): Either[DefinitionConstructionError, TypeNormalForm] =
    TypeNormalForm
      .fromShape(shape)
      .left
      .map(error =>
        DefinitionConstructionError.UnsupportedParsedDefinitionType(
          error.message
        )
      )

  private def constructedTermFromShape(
      shape: quasiquotes.parser.TermShape
  ): Either[DefinitionConstructionError, ConstructedTerm] =
    ConstructedTerm
      .fromShape(shape)
      .left
      .map(error =>
        DefinitionConstructionError.UnsupportedParsedDefinitionBody(
          error.message
        )
      )

  private def constructedTermFromShapeInScope(
      shape: quasiquotes.parser.TermShape,
      binderId: BinderId
  ): Either[DefinitionConstructionError, ConstructedTerm] =
    ConstructedTerm
      .fromShapeInScope(shape, binderId)
      .left
      .map(error =>
        DefinitionConstructionError.UnsupportedParsedDefinitionBody(
          error.message
        )
      )

  private def constructedTermFromShapeInScope(
      shape: quasiquotes.parser.TermShape,
      binderIds: Vector[BinderId]
  ): Either[DefinitionConstructionError, ConstructedTerm] =
    ConstructedTerm
      .fromShapeInScope(shape, binderIds)
      .left
      .map(error =>
        DefinitionConstructionError.UnsupportedParsedDefinitionBody(
          error.message
        )
      )

  private def validateTwoParameterList(
      firstBinderId: BinderId,
      firstName: DefinitionName,
      secondBinderId: BinderId,
      secondName: DefinitionName
  ): Either[DefinitionConstructionError, Unit] =
    if firstBinderId == secondBinderId then
      Left(
        DefinitionConstructionError.InvalidTwoParameterList(
          "parameter binder identities must be distinct"
        )
      )
    else if firstName == secondName then
      Left(
        DefinitionConstructionError.InvalidTwoParameterList(
          "declared parameter names must be distinct"
        )
      )
    else Right(())
