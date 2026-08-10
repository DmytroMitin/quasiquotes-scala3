package quasiquotes.types

import scala.quoted.*

object TargetTypeReprInspector:
  def inspect(using q: Quotes)(target: q.reflect.TypeRepr): Either[TypeQuasiquoteError, TypeNormalForm] =
    import q.reflect.*

    target match
      case TypeRef(_, "Int") => Right(TypeNormalForm.STypeIdent("Int"))
      case TypeRef(_, "String") => Right(TypeNormalForm.STypeIdent("String"))
      case TypeRef(_, "Boolean") => Right(TypeNormalForm.STypeIdent("Boolean"))
      case AppliedType(TypeRef(_, name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        collect(arguments.map(inspect))
          .map(argumentForms =>
            TypeNormalForm.STypeApply(
              TypeNormalForm.STypeIdent(name),
              argumentForms
            )
          )
      case AppliedType(TypeRef(_, "Tuple2"), first :: second :: Nil) =>
        for
          firstForm <- inspect(first)
          secondForm <- inspect(second)
        yield TypeNormalForm.STypeTuple(List(firstForm, secondForm))
      case AppliedType(TypeRef(_, "Tuple3"), first :: second :: third :: Nil) =>
        for
          firstForm <- inspect(first)
          secondForm <- inspect(second)
          thirdForm <- inspect(third)
        yield TypeNormalForm.STypeTuple(List(firstForm, secondForm, thirdForm))
      case AppliedType(TypeRef(_, "Function1"), argument :: result :: Nil) =>
        for
          argumentForm <- inspect(argument)
          resultForm <- inspect(result)
        yield TypeNormalForm.STypeFunction(List(argumentForm), resultForm)
      case AppliedType(TypeRef(_, "Function2"), first :: second :: result :: Nil) =>
        for
          firstForm <- inspect(first)
          secondForm <- inspect(second)
          resultForm <- inspect(result)
        yield TypeNormalForm.STypeFunction(List(firstForm, secondForm), resultForm)
      case other =>
        Left(TypeQuasiquoteError(s"Unsupported target TypeRepr shape for Phase 17 normal-form inspection: ${other.show}"))

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
