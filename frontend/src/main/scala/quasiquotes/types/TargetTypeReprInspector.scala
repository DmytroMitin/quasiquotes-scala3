package quasiquotes.types

import scala.quoted.*

object TargetTypeReprInspector:
  private[quasiquotes] final case class Inspection[T](
      normalForm: TypeNormalForm,
      originalsByPath: Map[Vector[Int], T]
  )

  def inspect(using q: Quotes)(target: q.reflect.TypeRepr): Either[TypeQuasiquoteError, TypeNormalForm] =
    inspectWithOrigins(target).map(_.normalForm)

  private[quasiquotes] def inspectWithOrigins(using q: Quotes)(
      target: q.reflect.TypeRepr
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    inspectAt(target, Vector.empty)

  private def inspectAt(using q: Quotes)(
      target: q.reflect.TypeRepr,
      path: Vector[Int]
  ): Either[TypeQuasiquoteError, Inspection[q.reflect.TypeRepr]] =
    import q.reflect.*

    target match
      case TypeRef(_, "Int") => scalar(target, path, TypeNormalForm.STypeIdent("Int"))
      case TypeRef(_, "String") => scalar(target, path, TypeNormalForm.STypeIdent("String"))
      case TypeRef(_, "Boolean") => scalar(target, path, TypeNormalForm.STypeIdent("Boolean"))
      case AppliedType(TypeRef(_, name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        collect(arguments.zipWithIndex.map((argument, index) => inspectAt(argument, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), children.map(_.normalForm)), children))
      case AppliedType(TypeRef(_, "Tuple2"), first :: second :: Nil) =>
        collect(List(first, second).zipWithIndex.map((argument, index) => inspectAt(argument, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeTuple(children.map(_.normalForm)), children))
      case AppliedType(TypeRef(_, "Tuple3"), first :: second :: third :: Nil) =>
        collect(List(first, second, third).zipWithIndex.map((argument, index) => inspectAt(argument, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeTuple(children.map(_.normalForm)), children))
      case AppliedType(TypeRef(_, "Function1"), argument :: result :: Nil) =>
        collect(List(argument, result).zipWithIndex.map((child, index) => inspectAt(child, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeFunction(List(children.head.normalForm), children(1).normalForm), children))
      case AppliedType(TypeRef(_, "Function2"), first :: second :: result :: Nil) =>
        collect(List(first, second, result).zipWithIndex.map((child, index) => inspectAt(child, path :+ index)))
          .map(children => composite(target, path, TypeNormalForm.STypeFunction(children.take(2).map(_.normalForm), children(2).normalForm), children))
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTargetType))

  private def scalar[T](target: T, path: Vector[Int], normalForm: TypeNormalForm): Either[TypeQuasiquoteError, Inspection[T]] =
    Right(Inspection(normalForm, Map(path -> target)))

  private def composite[T](
      target: T,
      path: Vector[Int],
      normalForm: TypeNormalForm,
      children: List[Inspection[T]]
  ): Inspection[T] =
    Inspection(
      normalForm,
      children.foldLeft(Map(path -> target))((origins, child) => origins ++ child.originalsByPath)
    )

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
