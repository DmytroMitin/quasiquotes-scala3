package quasiquotes.types

import quasiquotes.parser.*
import scala.quoted.*

final case class QuasiTypeRepr(
    source: String,
    shape: TypeShape,
    renderedTypeRepr: String
)

object QuasiTypeRepr:
  def fromSource(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypeRepr] =
    TinyTypeParser.parse(source).left.map(error => TypeQuasiquoteError(error.summary)).flatMap { parsed =>
      fromShape(source, parsed.shape)
    }

  private[types] def fromShape(source: String, shape: TypeShape)(using Quotes): Either[TypeQuasiquoteError, QuasiTypeRepr] =
    import quotes.reflect.*
    TypeReprLowerer.lower(shape).map(repr => QuasiTypeRepr(source, shape, repr.show))
