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
    import quotes.reflect.*
    TinyTypeParser.parse(source).left.map(error => TypeQuasiquoteError(error.summary)).flatMap { parsed =>
      TypeReprLowerer.lower(parsed.shape).map(repr => QuasiTypeRepr(source, parsed.shape, repr.show))
    }
