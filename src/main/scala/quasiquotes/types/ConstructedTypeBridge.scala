package quasiquotes.types

import scala.quoted.*

object ConstructedTypeBridge:
  /** Core bridge for an already constructed type. Dependent Type evidence stays inside `body`. */
  def withType[A](
      constructed: ConstructedType
  )(
      body: [t] => Type[t] ?=> A
  )(using Quotes): Either[TypeQuasiquoteError, A] =
    constructed.toTypeRepr.map { typeRepr =>
      typeRepr.asType match
        case '[t] => body[t]
    }

  /** Convenience bridge for callers that already have a normal form. */
  def withNormalFormType[A](
      normalForm: TypeNormalForm
  )(
      body: [t] => Type[t] ?=> A
  )(using Quotes): Either[TypeQuasiquoteError, A] =
    withType(ConstructedType(normalForm))(body)

  /** End-to-end convenience that constructs a type before entering the scoped bridge. */
  def withTemplateType[A](
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  )(
      body: [t] => Type[t] ?=> A
  )(using Quotes): Either[TypeQuasiquoteError, A] =
    QuasiTypequotes.tqr(templateSource, bindings*).flatMap(withType(_)(body))
