package quasiquotes.types

import scala.quoted.*

final case class QuasiTypePattern(
    source: String,
    expected: QuasiTypeRepr
):
  def matchTypeRepr(using q: Quotes)(target: q.reflect.TypeRepr): Boolean =
    import q.reflect.*
    target.show == expected.renderedTypeRepr

object QuasiTypePattern:
  def repr(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    QuasiTypeRepr.fromSource(source).map(expected => QuasiTypePattern(source, expected))

  def reprOrThrow(source: String)(using Quotes): QuasiTypePattern =
    repr(source).fold(throw _, identity)
