package quasiquotes.construct

import scala.quoted.*
import scala.util.control.NonFatal

import quasiquotes.types.*

object TypedTermConstruct:
  def ascribe(
      using q: Quotes
  )(
      term: q.reflect.Term,
      constructedType: ConstructedType
  ): Either[TypeQuasiquoteError, q.reflect.Term] =
    for
      typeRepr <- constructedType.toTypeRepr
      typed <- ascribeTypeRepr(term, typeRepr)
    yield typed

  def ascribeNormalForm(
      using q: Quotes
  )(
      term: q.reflect.Term,
      normalForm: TypeNormalForm
  ): Either[TypeQuasiquoteError, q.reflect.Term] =
    ascribe(term, ConstructedType(normalForm))

  def ascribeTemplate(
      using q: Quotes
  )(
      term: q.reflect.Term,
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[TypeQuasiquoteError, q.reflect.Term] =
    for
      constructedType <- QuasiTypeConstruct.fromTemplate(templateSource, bindings*)
      typed <- ascribe(term, constructedType)
    yield typed

  private def ascribeTypeRepr(
      using q: Quotes
  )(
      term: q.reflect.Term,
      typeRepr: q.reflect.TypeRepr
  ): Either[TypeQuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    try Right(Typed(term, Inferred(typeRepr)))
    catch
      case NonFatal(error) =>
        Left(TypeQuasiquoteError(s"Could not construct typed/ascribed term from constructed TypeRepr: ${error.getMessage.nn}"))
