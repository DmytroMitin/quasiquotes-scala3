package quasiquotes.types

import quasiquotes.parser.TypeShape

sealed trait TypeNormalForm derives CanEqual:
  final def render: String = TypeNormalForm.render(this)

object TypeNormalForm:
  final case class STypeIdent(name: String) extends TypeNormalForm
  final case class STypeApply(constructor: TypeNormalForm, arguments: List[TypeNormalForm]) extends TypeNormalForm
  final case class STypeTuple(elements: List[TypeNormalForm]) extends TypeNormalForm
  final case class STypeFunction(arguments: List[TypeNormalForm], result: TypeNormalForm) extends TypeNormalForm

  def fromShape(shape: TypeShape): Either[TypeQuasiquoteError, TypeNormalForm] =
    shape match
      case TypeShape.Identifier(name) => normalizeIdentifier(name)
      case TypeShape.Parenthesized(typeShape) => fromShape(typeShape)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        collect(arguments.map(fromShape))
          .map(argumentForms => STypeApply(STypeIdent(name), argumentForms))
      case TypeShape.Apply(constructor, arguments) =>
        Left(TypeQuasiquoteError(s"Unsupported type shape for Phase 15 structural normal form: ${TypeShape.Apply(constructor, arguments).render}"))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShape)).map(STypeTuple(_))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(s"Unsupported tuple type shape for Phase 15 structural normal form: ${TypeShape.Tuple(elements).render}"))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentForms <- collect(arguments.map(fromShape))
          resultForm <- fromShape(result)
        yield STypeFunction(argumentForms, resultForm)
      case TypeShape.Function(arguments, result) =>
        Left(TypeQuasiquoteError(s"Unsupported function type shape for Phase 15 structural normal form: ${TypeShape.Function(arguments, result).render}"))
      case TypeShape.Select(_, _) =>
        Left(TypeQuasiquoteError("Selected type syntax is not supported for Phase 15 structural normal form; `scala.Int` vs `Int` remains an explicit TODO."))
      case unsupported =>
        Left(TypeQuasiquoteError(s"Unsupported type shape for Phase 15 structural normal form: ${unsupported.render}"))

  def render(normalForm: TypeNormalForm): String =
    normalForm match
      case STypeIdent(name) => s"STypeIdent($name)"
      case STypeApply(constructor, arguments) =>
        s"STypeApply(${render(constructor)}, [${arguments.map(render).mkString(", ")}])"
      case STypeTuple(elements) =>
        s"STypeTuple([${elements.map(render).mkString(", ")}])"
      case STypeFunction(arguments, result) =>
        s"STypeFunction([${arguments.map(render).mkString(", ")}], ${render(result)})"

  private def normalizeIdentifier(name: String): Either[TypeQuasiquoteError, TypeNormalForm] =
    name match
      case "Int" | "String" | "Boolean" | "AnyVal" => Right(STypeIdent(name))
      case other => Left(TypeQuasiquoteError(s"Unsupported type identifier for Phase 15 structural normal form: $other"))

  private def collect[A](values: List[Either[TypeQuasiquoteError, A]]): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
