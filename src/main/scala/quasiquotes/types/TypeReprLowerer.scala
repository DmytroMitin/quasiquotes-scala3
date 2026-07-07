package quasiquotes.types

import quasiquotes.parser.TypeShape
import scala.quoted.*

object TypeReprLowerer:
  def lower(shape: TypeShape)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    shape match
      case TypeShape.Identifier("Int") => Right(TypeRepr.of[Int])
      case TypeShape.Identifier("String") => Right(TypeRepr.of[String])
      case TypeShape.Identifier("Boolean") => Right(TypeRepr.of[Boolean])
      case TypeShape.Identifier("AnyVal") => Right(TypeRepr.of[AnyVal])
      case TypeShape.Parenthesized(typeShape) => lower(typeShape)
      case TypeShape.Apply(TypeShape.Identifier("List"), argument :: Nil) =>
        lowerList(argument)
      case TypeShape.Apply(TypeShape.Identifier("Option"), argument :: Nil) =>
        lowerOption(argument)
      case TypeShape.Tuple(first :: second :: Nil) =>
        lowerTuple(first, second)
      case TypeShape.Function(argument :: Nil, result) =>
        lowerFunction(argument, result)
      case TypeShape.Select(_, _) =>
        Left(TypeQuasiquoteError("Selected type syntax is not supported for Phase 13 TypeRepr lowering; `scala.Int` vs `Int` remains an explicit TODO."))
      case unsupported =>
        Left(TypeQuasiquoteError(s"Unsupported type shape for Phase 13 TypeRepr lowering: ${unsupported.render}"))

  private def lowerList(argument: TypeShape)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    argument match
      case TypeShape.Identifier("Int") => Right(TypeRepr.of[List[Int]])
      case TypeShape.Identifier("String") => Right(TypeRepr.of[List[String]])
      case TypeShape.Identifier("Boolean") => Right(TypeRepr.of[List[Boolean]])
      case other => unsupportedApplied("List", other)

  private def lowerOption(argument: TypeShape)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    argument match
      case TypeShape.Identifier("Int") => Right(TypeRepr.of[Option[Int]])
      case TypeShape.Identifier("String") => Right(TypeRepr.of[Option[String]])
      case TypeShape.Identifier("Boolean") => Right(TypeRepr.of[Option[Boolean]])
      case other => unsupportedApplied("Option", other)

  private def lowerTuple(first: TypeShape, second: TypeShape)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    (first, second) match
      case (TypeShape.Identifier("Int"), TypeShape.Identifier("String")) => Right(TypeRepr.of[(Int, String)])
      case (TypeShape.Identifier("String"), TypeShape.Identifier("Int")) => Right(TypeRepr.of[(String, Int)])
      case (TypeShape.Identifier("Int"), TypeShape.Identifier("Int")) => Right(TypeRepr.of[(Int, Int)])
      case (TypeShape.Identifier("String"), TypeShape.Identifier("String")) => Right(TypeRepr.of[(String, String)])
      case _ => Left(TypeQuasiquoteError(s"Unsupported tuple type shape for Phase 13 TypeRepr lowering: ${TypeShape.Tuple(List(first, second)).render}"))

  private def lowerFunction(argument: TypeShape, result: TypeShape)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    (argument, result) match
      case (TypeShape.Identifier("Int"), TypeShape.Identifier("String")) => Right(TypeRepr.of[Int => String])
      case (TypeShape.Identifier("Int"), TypeShape.Identifier("Int")) => Right(TypeRepr.of[Int => Int])
      case (TypeShape.Identifier("String"), TypeShape.Identifier("Int")) => Right(TypeRepr.of[String => Int])
      case _ => Left(TypeQuasiquoteError(s"Unsupported function type shape for Phase 13 TypeRepr lowering: ${TypeShape.Function(List(argument), result).render}"))

  private def unsupportedApplied(constructor: String, argument: TypeShape): Either[TypeQuasiquoteError, Nothing] =
    Left(TypeQuasiquoteError(s"Unsupported type shape for Phase 13 TypeRepr lowering: ${TypeShape.Apply(TypeShape.Identifier(constructor), List(argument)).render}"))
