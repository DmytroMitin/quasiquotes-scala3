package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*

/** Compiler-free projection for the bounded Core Type normal-form family. */
@nowarn("cat=deprecation")
object ScalametaTypeNormalFormProjection:
  def project(
      sourceType: Type
  ): Either[NeutralProjectionError, ProjectedTypeNormalForm] =
    Option(sourceType)
      .toRight(error("NEUTRAL_TYPE_MISSING", "the Scalameta type must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      sourceType: Type
  ): Either[NeutralProjectionError, ProjectedTypeNormalForm] =
    for
      shape <- projectShape(sourceType)
      normalForm <- TypeNormalForm
        .fromShape(shape)
        .left
        .map(problem => error("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", problem.message))
    yield ProjectedTypeNormalForm(normalForm, truthfulSpan(sourceType))

  private def projectShape(
      sourceType: Type
  ): Either[NeutralProjectionError, TypeShape] =
    sourceType match
      case name: Type.Name =>
        Right(TypeShape.Identifier(name.value))
      case applied: Type.Apply =>
        for
          constructor <- projectShape(applied.tpe)
          arguments <- traverse(applied.args)(projectShape)
        yield TypeShape.Apply(constructor, arguments)
      case tuple: Type.Tuple =>
        traverse(tuple.args)(projectShape).map(TypeShape.Tuple(_))
      case function: Type.Function =>
        for
          arguments <- traverse(function.params)(projectShape)
          result <- projectShape(function.res)
        yield TypeShape.Function(arguments, result)
      case selected: Type.Select =>
        selectedQualifier(selected.qual)
          .map(TypeShape.Select(_, selected.name.value))
      case other =>
        Left(structureError(other.productPrefix))

  private def selectedQualifier(
      qualifier: Term
  ): Either[NeutralProjectionError, TypeShape] =
    qualifier match
      case name: Term.Name => Right(TypeShape.Identifier(name.value))
      case selected: Term.Select =>
        selectedQualifier(selected.qual)
          .map(TypeShape.Select(_, selected.name.value))
      case other => Left(structureError(other.productPrefix))

  private def traverse[A, B](
      values: List[A]
  )(projectValue: A => Either[NeutralProjectionError, B]): Either[NeutralProjectionError, List[B]] =
    values.foldRight(Right(Nil): Either[NeutralProjectionError, List[B]]) { (value, rest) =>
      for
        head <- projectValue(value)
        tail <- rest
      yield head :: tail
    }

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def structureError(nodeKind: String): NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
      s"unsupported Scalameta type node: $nodeKind."
    )

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
