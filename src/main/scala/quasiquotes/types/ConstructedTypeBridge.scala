package quasiquotes.types

import scala.quoted.*

object ConstructedTypeBridge:
  def withType[A](
      constructed: ConstructedType
  )(
      body: [t] => Type[t] ?=> A
  )(using Quotes): Either[TypeQuasiquoteError, A] =
    constructed.toTypeRepr.map { typeRepr =>
      typeRepr.asType match
        case '[t] => body[t]
    }

  def withNormalFormType[A](
      normalForm: TypeNormalForm
  )(
      body: [t] => Type[t] ?=> A
  )(using Quotes): Either[TypeQuasiquoteError, A] =
    withType(ConstructedType(normalForm))(body)

  def withTemplateType[A](
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  )(
      body: [t] => Type[t] ?=> A
  )(using Quotes): Either[TypeQuasiquoteError, A] =
    QuasiTypequotes.tqr(templateSource, bindings*).flatMap(withType(_)(body))
