package quasiquotes.definitions

import quasiquotes.parser.BinderId
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

  final class SingleParameterDef private[DefinitionTemplate] (
      val name: DefinitionName,
      val parameterBinderId: BinderId,
      val parameterName: DefinitionName,
      val parameterType: TypeTemplate,
      val resultType: TypeTemplate,
      val body: TermTemplate
  ) extends DefinitionTemplate:
    def requiredTermBindings: Vector[String] =
      body.requiredTermBindings

    def requiredTypeBindings: Vector[String] =
      unionRequiredTypes(Vector(parameterType, resultType), body)

    def complete(
        termBindings: Map[String, ConstructedTerm],
        typeBindings: Map[String, TypeNormalForm]
    ): Either[DefinitionConstructionError, ConstructedDefinition] =
      val requiredTerms = body.requiredTermBindings
      val requiredTypes = requiredTypeBindings
      for
        _ <- validateBindingSets(
          requiredTerms,
          requiredTypes,
          termBindings,
          typeBindings
        )
        _ <- validateTypeBindings(requiredTypes, typeBindings)
        completedParameterType <- completeType(parameterType, typeBindings)
        completedResultType <- completeType(resultType, typeBindings)
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
        result <- ConstructedDefinition
          .singleParameterDef(
            name,
            parameterBinderId,
            parameterName,
            completedParameterType,
            completedResultType,
            completedBody
          )
          .left
          .map(error =>
            DefinitionConstructionError.CompletedDefinitionFactoryFailure(
              error.message
            )
          )
      yield result

    def render: String =
      s"DefinitionTemplate.SingleParameterDef(name=${name.render}, parameter=${parameterName.render}: $parameterType, resultType=$resultType, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: SingleParameterDef =>
          name == that.name &&
            parameterType == that.parameterType &&
            resultType == that.resultType &&
            body == that.body
        case _ => false

    override def hashCode: Int =
      (
        "SingleParameterDef",
        name,
        parameterType,
        resultType,
        body
      ).hashCode

    override def toString: String = render

  final class TwoParameterDef private[DefinitionTemplate] (
      val name: DefinitionName,
      val firstParameterBinderId: BinderId,
      val firstParameterName: DefinitionName,
      val firstParameterType: TypeTemplate,
      val secondParameterBinderId: BinderId,
      val secondParameterName: DefinitionName,
      val secondParameterType: TypeTemplate,
      val resultType: TypeTemplate,
      val body: TermTemplate
  ) extends DefinitionTemplate:
    def requiredTermBindings: Vector[String] =
      body.requiredTermBindings

    def requiredTypeBindings: Vector[String] =
      unionRequiredTypes(
        Vector(firstParameterType, secondParameterType, resultType),
        body
      )

    def complete(
        termBindings: Map[String, ConstructedTerm],
        typeBindings: Map[String, TypeNormalForm]
    ): Either[DefinitionConstructionError, ConstructedDefinition] =
      val requiredTerms = body.requiredTermBindings
      val requiredTypes = requiredTypeBindings
      for
        _ <- validateBindingSets(
          requiredTerms,
          requiredTypes,
          termBindings,
          typeBindings
        )
        _ <- validateTypeBindings(requiredTypes, typeBindings)
        completedFirstParameterType <- completeType(
          firstParameterType,
          typeBindings
        )
        completedSecondParameterType <- completeType(
          secondParameterType,
          typeBindings
        )
        completedResultType <- completeType(resultType, typeBindings)
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
        result <- ConstructedDefinition
          .twoParameterDef(
            name,
            firstParameterBinderId,
            firstParameterName,
            completedFirstParameterType,
            secondParameterBinderId,
            secondParameterName,
            completedSecondParameterType,
            completedResultType,
            completedBody
          )
          .left
          .map(error =>
            DefinitionConstructionError.CompletedDefinitionFactoryFailure(
              error.message
            )
          )
      yield result

    def render: String =
      s"DefinitionTemplate.TwoParameterDef(name=${name.render}, firstParameter=${firstParameterName.render}: $firstParameterType, secondParameter=${secondParameterName.render}: $secondParameterType, resultType=$resultType, body=${body.render})"

    override def equals(other: Any): Boolean =
      other match
        case that: TwoParameterDef =>
          name == that.name &&
            firstParameterType == that.firstParameterType &&
            secondParameterType == that.secondParameterType &&
            resultType == that.resultType &&
            body == that.body
        case _ => false

    override def hashCode: Int =
      (
        "TwoParameterDef",
        name,
        firstParameterType,
        secondParameterType,
        resultType,
        body
      ).hashCode

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

  def singleParameterDef(
      name: DefinitionName,
      parameterBinderId: BinderId,
      parameterName: DefinitionName,
      parameterType: TypeTemplate,
      resultType: TypeTemplate,
      body: TermTemplate
  ): Either[DefinitionConstructionError, SingleParameterDef] =
    for
      _ <- validateTypeTemplate(parameterType)
      _ <- validateTypeTemplate(resultType)
      _ <- TermTemplate
        .validateInScope(body, parameterBinderId)
        .left
        .map(error =>
          DefinitionConstructionError.InvalidDefinitionBodyTemplate(
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
      firstParameterType: TypeTemplate,
      secondParameterBinderId: BinderId,
      secondParameterName: DefinitionName,
      secondParameterType: TypeTemplate,
      resultType: TypeTemplate,
      body: TermTemplate
  ): Either[DefinitionConstructionError, TwoParameterDef] =
    for
      _ <- validateTwoParameterList(
        firstParameterBinderId,
        firstParameterName,
        secondParameterBinderId,
        secondParameterName
      )
      _ <- validateTypeTemplate(firstParameterType)
      _ <- validateTypeTemplate(secondParameterType)
      _ <- validateTypeTemplate(resultType)
      _ <- TermTemplate
        .validateInScope(
          body,
          Vector(firstParameterBinderId, secondParameterBinderId)
        )
        .left
        .map(error =>
          DefinitionConstructionError.InvalidDefinitionBodyTemplate(
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
    unionRequiredTypes(Vector(definitionType), body)

  private def unionRequiredTypes(
      definitionTypes: Vector[TypeTemplate],
      body: TermTemplate
  ): Vector[String] =
    (definitionTypes.flatMap(TypeTemplate.requiredBindings) ++
      body.requiredTypeBindings).distinct

  private def completeType(
      template: TypeTemplate,
      typeBindings: Map[String, TypeNormalForm]
  ): Either[DefinitionConstructionError, TypeNormalForm] =
    for
      completed <- TypeTemplate
        .construct(template, typeBindings)
        .left
        .map(error =>
          DefinitionConstructionError.DefinitionTypeConstructionFailure(
            error.message
          )
        )
      _ <- TypeTemplate
        .validateConstructed(completed)
        .left
        .map(error =>
          DefinitionConstructionError.DefinitionTypeConstructionFailure(
            error.message
          )
        )
    yield completed

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
