package quasiquotes.definitions

import quasiquotes.terms.{ConstructedTerm, TermTemplate}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[quasiquotes] sealed trait DefinitionTemplate derives CanEqual:
  def name: DefinitionName
  def requiredTermBindings: Vector[String]
  def requiredTypeBindings: Vector[String]
  def complete(
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[DefinitionConstructionError, ConstructedDefinition]
  def render: String

private[quasiquotes] object DefinitionTemplate:
  final class ParameterlessDef private[DefinitionTemplate] (
      val name: DefinitionName,
      val resultType: TypeTemplate,
      val body: TermTemplate
  ) extends DefinitionTemplate:
    def requiredTermBindings: Vector[String] =
      body.requiredTermBindings

    def requiredTypeBindings: Vector[String] =
      unionRequiredTypes(resultType, body)

    def complete(
        termBindings: Map[String, ConstructedTerm],
        typeBindings: Map[String, TypeNormalForm]
    ): Either[DefinitionConstructionError, ConstructedDefinition] =
      completeDefinition(
        resultType,
        body,
        termBindings,
        typeBindings,
        completedType =>
          completedBody =>
            ConstructedDefinition.parameterlessDef(
              name,
              completedType,
              completedBody
            )
      )

    def render: String =
      s"DefinitionTemplate.ParameterlessDef(name=${name.render}, resultType=$resultType, body=${body.render})"

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

  final class ImmutableVal private[DefinitionTemplate] (
      val name: DefinitionName,
      val declaredType: TypeTemplate,
      val rhs: TermTemplate
  ) extends DefinitionTemplate:
    def requiredTermBindings: Vector[String] =
      rhs.requiredTermBindings

    def requiredTypeBindings: Vector[String] =
      unionRequiredTypes(declaredType, rhs)

    def complete(
        termBindings: Map[String, ConstructedTerm],
        typeBindings: Map[String, TypeNormalForm]
    ): Either[DefinitionConstructionError, ConstructedDefinition] =
      completeDefinition(
        declaredType,
        rhs,
        termBindings,
        typeBindings,
        completedType =>
          completedBody =>
            ConstructedDefinition.immutableVal(
              name,
              completedType,
              completedBody
            )
      )

    def render: String =
      s"DefinitionTemplate.ImmutableVal(name=${name.render}, declaredType=$declaredType, rhs=${rhs.render})"

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
      resultType: TypeTemplate,
      body: TermTemplate
  ): Either[DefinitionConstructionError, ParameterlessDef] =
    validateTypeTemplate(resultType).map(_ =>
      new ParameterlessDef(name, resultType, body)
    )

  def immutableVal(
      name: DefinitionName,
      declaredType: TypeTemplate,
      rhs: TermTemplate
  ): Either[DefinitionConstructionError, ImmutableVal] =
    validateTypeTemplate(declaredType).map(_ =>
      new ImmutableVal(name, declaredType, rhs)
    )

  private def completeDefinition(
      definitionType: TypeTemplate,
      body: TermTemplate,
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm],
      create: TypeNormalForm =>
        ConstructedTerm =>
          Either[DefinitionConstructionError, ConstructedDefinition]
  ): Either[DefinitionConstructionError, ConstructedDefinition] =
    val requiredTerms = body.requiredTermBindings
    val requiredTypes = unionRequiredTypes(definitionType, body)
    for
      _ <- validateBindingSets(
        requiredTerms,
        requiredTypes,
        termBindings,
        typeBindings
      )
      _ <- validateTypeBindings(requiredTypes, typeBindings)
      completedType <- TypeTemplate
        .construct(definitionType, typeBindings)
        .left
        .map(error =>
          DefinitionConstructionError.DefinitionTypeConstructionFailure(
            error.message
          )
        )
      _ <- TypeTemplate
        .validateConstructed(completedType)
        .left
        .map(error =>
          DefinitionConstructionError.DefinitionTypeConstructionFailure(
            error.message
          )
        )
      bodyTypeNames = body.requiredTypeBindings.toSet
      bodyTypeBindings = typeBindings.view
        .filterKeys(bodyTypeNames)
        .toMap
      completedBody <- body
        .complete(termBindings, bodyTypeBindings)
        .left
        .map(error =>
          DefinitionConstructionError.BodyConstructionFailure(error.message)
        )
      result <- create(completedType)(completedBody)
        .left
        .map(error =>
          DefinitionConstructionError.CompletedDefinitionFactoryFailure(
            error.message
          )
        )
    yield result

  private def unionRequiredTypes(
      definitionType: TypeTemplate,
      body: TermTemplate
  ): Vector[String] =
    (TypeTemplate.requiredBindings(definitionType) ++
      body.requiredTypeBindings).distinct

  private def validateBindingSets(
      requiredTerms: Vector[String],
      requiredTypes: Vector[String],
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[DefinitionConstructionError, Unit] =
    requiredTerms
      .find(!termBindings.contains(_))
      .map(name =>
        Left(DefinitionConstructionError.MissingTermBinding(name))
      )
      .orElse(
        firstExtra(termBindings.keySet, requiredTerms.toSet).map(name =>
          Left(DefinitionConstructionError.UnexpectedTermBinding(name))
        )
      )
      .orElse(
        requiredTypes
          .find(!typeBindings.contains(_))
          .map(name =>
            Left(DefinitionConstructionError.MissingTypeBinding(name))
          )
      )
      .orElse(
        firstExtra(typeBindings.keySet, requiredTypes.toSet).map(name =>
          Left(DefinitionConstructionError.UnexpectedTypeBinding(name))
        )
      )
      .getOrElse(Right(()))

  private def validateTypeBindings(
      requiredTypes: Vector[String],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[DefinitionConstructionError, Unit] =
    requiredTypes.foldLeft[Either[DefinitionConstructionError, Unit]](
      Right(())
    ) { (result, name) =>
      result.flatMap { _ =>
        TypeTemplate
          .validateConstructed(typeBindings(name))
          .left
          .map(error =>
            DefinitionConstructionError.InvalidTypeBinding(
              name,
              error.message
            )
          )
      }
    }

  private def validateTypeTemplate(
      template: TypeTemplate
  ): Either[DefinitionConstructionError, Unit] =
    TypeTemplate
      .validateTemplate(template)
      .left
      .map(error =>
        DefinitionConstructionError.InvalidDefinitionTypeTemplate(
          error.message
        )
      )

  private def firstExtra(
      supplied: Set[String],
      required: Set[String]
  ): Option[String] =
    (supplied -- required).toVector.sorted.headOption
