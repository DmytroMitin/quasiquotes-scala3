package quasiquotes.neutral

import _root_.quasiquotes.publicapi.{
  CompletedType,
  DefinitionConstruction,
  DefinitionResultView
}

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for the bounded legacy contextual-method view. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaContextualMethodAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      definition: DefinitionResultView
  ): Either[Error, Defn.Def] =
    if definition == null then Left(missing)
    else revalidate(definition).flatMap(authorValidated)

  private def revalidate(
      definition: DefinitionResultView
  ): Either[Error, DefinitionResultView] =
    try
      DefinitionConstruction
        .contextualMethod(
          definition.name,
          definition.typeParameterName,
          definition.contextualParameterName,
          definition.contextualParameterType,
          definition.resultType,
          definition.body
        )
        .left
        .map(_ => validationUnsupported)
    catch case NonFatal(_) => Left(validationUnsupported)

  private def authorValidated(
      definition: DefinitionResultView
  ): Either[Error, Defn.Def] =
    for
      contextualType <- authorType(
        definition.contextualParameterType,
        definition.typeParameterName
      )
      resultType <- authorType(definition.resultType, definition.typeParameterName)
      _ <- requireRepresentableBody(definition)
      authored <- construct(definition, contextualType, resultType)
      _ <- requireFresh(authored)
      _ <- requireExactRoundTrip(authored, definition)
    yield authored

  private def authorType(
      value: CompletedType,
      declaredTypeParameter: String
  ): Either[Error, Type] =
    try
      if value == null then Left(typeUnsupported)
      else
        value.kindCode match
          case "type-parameter" =>
            value.name match
              case Some(name)
                  if name == declaredTypeParameter &&
                    value.constructor.isEmpty &&
                    value.arguments.isEmpty =>
                Right(Type.Name(name))
              case _ => Left(typeUnsupported)
          case "named" =>
            value.name match
              case Some(name)
                  if name != declaredTypeParameter &&
                    value.constructor.isEmpty &&
                    value.arguments.isEmpty =>
                Right(Type.Name(name))
              case _ => Left(typeUnsupported)
          case "applied" =>
            (value.name, value.constructor, value.arguments) match
              case (None, Some(constructor), arguments)
                  if constructor != null &&
                    constructor.kindCode == "named" &&
                    arguments != null &&
                    arguments.nonEmpty &&
                    arguments.forall(_ != null) =>
                for
                  authoredConstructor <- authorType(constructor, declaredTypeParameter)
                  authoredArguments <- traverseTypes(arguments, declaredTypeParameter)
                yield Type.Apply(
                  authoredConstructor,
                  Type.ArgClause(authoredArguments.toList)
                )
              case _ => Left(typeUnsupported)
          case _ => Left(typeUnsupported)
    catch case NonFatal(_) => Left(typeUnsupported)

  private def traverseTypes(
      values: Vector[CompletedType],
      declaredTypeParameter: String
  ): Either[Error, Vector[Type]] =
    values.foldLeft(Right(Vector.empty): Either[Error, Vector[Type]]) {
      (authored, value) =>
        for
          completed <- authored
          next <- authorType(value, declaredTypeParameter)
        yield completed :+ next
    }

  private def requireRepresentableBody(
      definition: DefinitionResultView
  ): Either[Error, Unit] =
    try
      Either.cond(
        definition.body != null &&
          definition.body.kindCode == "reference" &&
          definition.body.referenceName == definition.contextualParameterName,
        (),
        bodyUnsupported
      )
    catch case NonFatal(_) => Left(bodyUnsupported)

  private def construct(
      definition: DefinitionResultView,
      contextualType: Type,
      resultType: Type
  ): Either[Error, Defn.Def] =
    try
      val typeParameter = Type.Param(
        Nil,
        Type.Name(definition.typeParameterName),
        Type.ParamClause(Nil),
        Type.Bounds.empty
      )
      val contextualParameter = Term.Param(
        Nil,
        Term.Name(definition.contextualParameterName),
        Some(contextualType),
        None
      )
      Right(
        Defn.Def(
          Nil,
          Term.Name(definition.name),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(typeParameter)),
              List(
                Term.ParamClause(
                  List(contextualParameter),
                  Some(Mod.Using())
                )
              )
            )
          ),
          Some(resultType),
          Term.Name(definition.contextualParameterName)
        )
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireFresh(authored: Defn.Def): Either[Error, Unit] =
    try
      Either.cond(
        allTrees(authored).forall(_.pos == Position.None),
        (),
        constructionFailed
      )
    catch case NonFatal(_) => Left(constructionFailed)

  private def requireExactRoundTrip(
      authored: Defn.Def,
      expected: DefinitionResultView
  ): Either[Error, Unit] =
    try
      ScalametaContextualMethodProjection.project(authored) match
        case Right(ProjectedContextualMethod(projected, None)) =>
          Either.cond(projected == expected, (), roundTripFailed)
        case _ => Left(roundTripFailed)
    catch case NonFatal(_) => Left(roundTripFailed)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)

  private def missing: Error =
    error(
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_MISSING",
      "the contextual-method definition view must be present."
    )

  private def validationUnsupported: Error =
    error(
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_VALIDATION_UNSUPPORTED",
      "Core DefinitionConstruction.contextualMethod rejected the input."
    )

  private def typeUnsupported: Error =
    error(
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_TYPE_UNSUPPORTED",
      "a contextual or result Type is outside the exact structural reverse intersection."
    )

  private def bodyUnsupported: Error =
    error(
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_BODY_UNSUPPORTED",
      "the body must remain an ordinary reference to the contextual parameter."
    )

  private def constructionFailed: Error =
    error(
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_CONSTRUCTION_FAILED",
      "the exact direct fresh Scalameta contextual method could not be constructed."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_ROUNDTRIP_FAILED",
      "the authored contextual method did not reproject exactly without provenance."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
