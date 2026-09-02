package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.Type

/** Direct structural authoring for the unresolved N002 Type normal-form family. */
@nowarn("cat=deprecation")
object ScalametaTypeNormalFormAuthoring:
  /** Stable bounded failure for semantic-value-to-Scalameta Type authoring. */
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(normalForm: TypeNormalForm): Either[Error, Type] =
    Option(normalForm)
      .toRight(error("NEUTRAL_TYPE_AUTHORING_MISSING", "the Type normal form must be present."))
      .flatMap(authorPresent)

  private def authorPresent(normalForm: TypeNormalForm): Either[Error, Type] =
    for
      shape <- toN002Shape(normalForm)
      validated <- TypeNormalForm
        .fromShape(shape)
        .left
        .map(problem => error("NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED", problem.message))
      authored <- authorValidated(validated)
    yield authored

  private def toN002Shape(normalForm: TypeNormalForm): Either[Error, TypeShape] =
    Option(normalForm)
      .toRight(
        error(
          "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED",
          "the Type normal form contains a missing recursive child."
        )
      )
      .flatMap {
        case STypeIdent(name) =>
          Right(TypeShape.Identifier(name))
        case _: STypeResolved =>
          Left(
            error(
              "NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED",
              "STypeResolved is outside the unresolved N002 authoring family."
            )
          )
        case STypeApply(constructor, arguments) =>
          for
            authoredConstructor <- toN002Shape(constructor)
            authoredArguments <- traverse(arguments)(toN002Shape)
          yield TypeShape.Apply(authoredConstructor, authoredArguments)
        case STypeTuple(elements) =>
          traverse(elements)(toN002Shape).map(TypeShape.Tuple(_))
        case STypeFunction(arguments, result) =>
          for
            authoredArguments <- traverse(arguments)(toN002Shape)
            authoredResult <- toN002Shape(result)
          yield TypeShape.Function(authoredArguments, authoredResult)
      }

  private def authorValidated(normalForm: TypeNormalForm): Either[Error, Type] =
    normalForm match
      case STypeIdent(name) =>
        Right(Type.Name(name))
      case _: STypeResolved =>
        Left(
          error(
            "NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED",
            "STypeResolved is outside the unresolved N002 authoring family."
          )
        )
      case STypeApply(constructor, arguments) =>
        for
          authoredConstructor <- authorValidated(constructor)
          authoredArguments <- traverse(arguments)(authorValidated)
        yield Type.Apply(authoredConstructor, authoredArguments)
      case STypeTuple(elements) =>
        traverse(elements)(authorValidated).map(Type.Tuple(_))
      case STypeFunction(arguments, result) =>
        for
          authoredArguments <- traverse(arguments)(authorValidated)
          authoredResult <- authorValidated(result)
        yield Type.Function(authoredArguments, authoredResult)

  private def traverse[A, B](
      values: List[A]
  )(transform: A => Either[Error, B]): Either[Error, List[B]] =
    Option(values)
      .toRight(
        error(
          "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED",
          "the Type normal form contains a missing recursive list."
        )
      )
      .flatMap(
        _.foldRight(Right(Nil): Either[Error, List[B]]) { (value, accumulated) =>
          for
            head <- transform(value)
            tail <- accumulated
          yield head :: tail
        }
      )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
