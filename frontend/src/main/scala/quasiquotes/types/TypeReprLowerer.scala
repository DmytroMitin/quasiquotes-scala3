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
      case TypeShape.Tuple(first :: second :: third :: Nil) =>
        lowerTuple3(first, second, third)
      case TypeShape.Function(argument :: Nil, result) =>
        lowerFunction(argument, result)
      case TypeShape.Function(first :: second :: Nil, result) =>
        lowerFunction2(first, second, result)
      case TypeShape.Select(_, _) =>
        Left(TypeQuasiquoteError("Selected type syntax is not supported for Phase 13 TypeRepr lowering; `scala.Int` vs `Int` remains an explicit TODO."))
      case unsupported =>
        Left(TypeQuasiquoteError(s"Unsupported type shape for Phase 13 TypeRepr lowering: ${unsupported.render}"))

  def lowerNormalForm(normalForm: TypeNormalForm)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    normalForm match
      case TypeNormalForm.STypeIdent("Int") => Right(TypeRepr.of[Int])
      case TypeNormalForm.STypeIdent("String") => Right(TypeRepr.of[String])
      case TypeNormalForm.STypeIdent("Boolean") => Right(TypeRepr.of[Boolean])
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), argument :: Nil) =>
        lowerNormalFormList(argument)
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), argument :: Nil) =>
        lowerNormalFormOption(argument)
      case TypeNormalForm.STypeTuple(first :: second :: Nil) =>
        lowerNormalFormTuple(first, second)
      case TypeNormalForm.STypeTuple(first :: second :: third :: Nil) =>
        lowerNormalFormTuple3(first, second, third)
      case TypeNormalForm.STypeFunction(argument :: Nil, result) =>
        lowerNormalFormFunction(argument, result)
      case TypeNormalForm.STypeFunction(first :: second :: Nil, result) =>
        lowerNormalFormFunction2(first, second, result)
      case unsupported =>
        unsupportedNormalForm(unsupported)

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

  private def lowerTuple3(
      first: TypeShape,
      second: TypeShape,
      third: TypeShape
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*
    for
      firstRepr <- lower(first)
      secondRepr <- lower(second)
      thirdRepr <- lower(third)
    yield
      firstRepr.asType match
        case '[a] =>
          secondRepr.asType match
            case '[b] =>
              thirdRepr.asType match
                case '[c] => TypeRepr.of[(a, b, c)]

  private def lowerFunction2(
      first: TypeShape,
      second: TypeShape,
      result: TypeShape
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*
    for
      firstRepr <- lower(first)
      secondRepr <- lower(second)
      resultRepr <- lower(result)
    yield
      firstRepr.asType match
        case '[a] =>
          secondRepr.asType match
            case '[b] =>
              resultRepr.asType match
                case '[r] => TypeRepr.of[(a, b) => r]

  private def unsupportedApplied(constructor: String, argument: TypeShape): Either[TypeQuasiquoteError, Nothing] =
    Left(TypeQuasiquoteError(s"Unsupported type shape for Phase 13 TypeRepr lowering: ${TypeShape.Apply(TypeShape.Identifier(constructor), List(argument)).render}"))

  private def lowerNormalFormList(argument: TypeNormalForm)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    argument match
      case TypeNormalForm.STypeIdent("Int") => Right(TypeRepr.of[List[Int]])
      case TypeNormalForm.STypeIdent("String") => Right(TypeRepr.of[List[String]])
      case TypeNormalForm.STypeIdent("Boolean") => Right(TypeRepr.of[List[Boolean]])
      case _ => unsupportedNormalForm(TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(argument)))

  private def lowerNormalFormOption(argument: TypeNormalForm)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    argument match
      case TypeNormalForm.STypeIdent("Int") => Right(TypeRepr.of[Option[Int]])
      case TypeNormalForm.STypeIdent("String") => Right(TypeRepr.of[Option[String]])
      case TypeNormalForm.STypeIdent("Boolean") => Right(TypeRepr.of[Option[Boolean]])
      case _ => unsupportedNormalForm(TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(argument)))

  private def lowerNormalFormTuple(first: TypeNormalForm, second: TypeNormalForm)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    (first, second) match
      case (TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String")) => Right(TypeRepr.of[(Int, String)])
      case (TypeNormalForm.STypeIdent("String"), TypeNormalForm.STypeIdent("Int")) => Right(TypeRepr.of[(String, Int)])
      case (TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("Int")) => Right(TypeRepr.of[(Int, Int)])
      case (TypeNormalForm.STypeIdent("String"), TypeNormalForm.STypeIdent("String")) => Right(TypeRepr.of[(String, String)])
      case _ => unsupportedNormalForm(TypeNormalForm.STypeTuple(List(first, second)))

  private def lowerNormalFormFunction(argument: TypeNormalForm, result: TypeNormalForm)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    (argument, result) match
      case (TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String")) => Right(TypeRepr.of[Int => String])
      case (TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("Int")) => Right(TypeRepr.of[Int => Int])
      case (TypeNormalForm.STypeIdent("String"), TypeNormalForm.STypeIdent("Int")) => Right(TypeRepr.of[String => Int])
      case _ => unsupportedNormalForm(TypeNormalForm.STypeFunction(List(argument), result))

  private def lowerNormalFormTuple3(
      first: TypeNormalForm,
      second: TypeNormalForm,
      third: TypeNormalForm
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*
    for
      firstRepr <- lowerNormalForm(first)
      secondRepr <- lowerNormalForm(second)
      thirdRepr <- lowerNormalForm(third)
    yield
      firstRepr.asType match
        case '[a] =>
          secondRepr.asType match
            case '[b] =>
              thirdRepr.asType match
                case '[c] => TypeRepr.of[(a, b, c)]

  private def lowerNormalFormFunction2(
      first: TypeNormalForm,
      second: TypeNormalForm,
      result: TypeNormalForm
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*
    for
      firstRepr <- lowerNormalForm(first)
      secondRepr <- lowerNormalForm(second)
      resultRepr <- lowerNormalForm(result)
    yield
      firstRepr.asType match
        case '[a] =>
          secondRepr.asType match
            case '[b] =>
              resultRepr.asType match
                case '[r] => TypeRepr.of[(a, b) => r]

  private def unsupportedNormalForm(normalForm: TypeNormalForm): Either[TypeQuasiquoteError, Nothing] =
    Left(TypeQuasiquoteError(s"Cannot lower unsupported constructed type normal form to TypeRepr: ${ConstructedType.renderSource(normalForm)}"))
