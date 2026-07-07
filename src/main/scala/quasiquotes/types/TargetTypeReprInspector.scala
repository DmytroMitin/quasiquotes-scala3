package quasiquotes.types

import scala.quoted.*

object TargetTypeReprInspector:
  def inspect(using q: Quotes)(target: q.reflect.TypeRepr): Either[TypeQuasiquoteError, TypeNormalForm] =
    import q.reflect.*

    target match
      case TypeRef(_, "Int") => Right(TypeNormalForm.STypeIdent("Int"))
      case TypeRef(_, "String") => Right(TypeNormalForm.STypeIdent("String"))
      case TypeRef(_, "Boolean") => Right(TypeNormalForm.STypeIdent("Boolean"))
      case AppliedType(TypeRef(_, "List"), argument :: Nil) =>
        inspect(argument).map(argumentForm => TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(argumentForm)))
      case AppliedType(TypeRef(_, "Option"), argument :: Nil) =>
        inspect(argument).map(argumentForm => TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(argumentForm)))
      case AppliedType(TypeRef(_, "Tuple2"), first :: second :: Nil) =>
        for
          firstForm <- inspect(first)
          secondForm <- inspect(second)
        yield TypeNormalForm.STypeTuple(List(firstForm, secondForm))
      case AppliedType(TypeRef(_, "Function1"), argument :: result :: Nil) =>
        for
          argumentForm <- inspect(argument)
          resultForm <- inspect(result)
        yield TypeNormalForm.STypeFunction(List(argumentForm), resultForm)
      case other =>
        Left(TypeQuasiquoteError(s"Unsupported target TypeRepr shape for Phase 17 normal-form inspection: ${other.show}"))
