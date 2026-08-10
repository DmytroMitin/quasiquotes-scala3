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
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        lowerAppliedShape(name, arguments)
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
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), arguments)
          if AppliedTypeConstructorPolicy
            .forConstruction(name, arguments.size)
            .isDefined =>
        lowerAppliedNormalForm(name, arguments)
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

  private def lowerAppliedShape(
      name: String,
      arguments: List[TypeShape]
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    collect(arguments.map(lower)).flatMap(lowerAppliedReprs(name, _))

  private def lowerAppliedNormalForm(
      name: String,
      arguments: List[TypeNormalForm]
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    collect(arguments.map(lowerNormalForm)).flatMap(lowerAppliedReprs(name, _))

  private def lowerAppliedReprs(using q: Quotes)(
      name: String,
      arguments: List[q.reflect.TypeRepr]
  ): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    import q.reflect.*

    (name, arguments) match
      case ("List", argument :: Nil) =>
        argument.asType match
          case '[a] => Right(TypeRepr.of[List[a]])
      case ("Option", argument :: Nil) =>
        argument.asType match
          case '[a] => Right(TypeRepr.of[Option[a]])
      case ("Either", first :: second :: Nil) =>
        first.asType match
          case '[a] =>
            second.asType match
              case '[b] => Right(TypeRepr.of[Either[a, b]])
      case _ =>
        Left(
          TypeQuasiquoteError(
            s"Unsupported fixed applied-type constructor/arity for Phase 67 TypeRepr lowering: $name/${arguments.size}"
          )
        )

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

  private def collect[A](
      values: List[Either[TypeQuasiquoteError, A]]
  ): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) {
      (value, accumulated) =>
        for
          head <- value
          tail <- accumulated
        yield head :: tail
    }
