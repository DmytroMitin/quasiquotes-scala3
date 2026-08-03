package quasiquotes.definitions

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
