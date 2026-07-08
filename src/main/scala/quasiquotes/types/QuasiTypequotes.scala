package quasiquotes.types

import scala.quoted.Quotes

object QuasiTypequotes:
  def tqq(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    QuasiTypePattern.repr(source)

  def tqr(
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[TypeQuasiquoteError, ConstructedType] =
    QuasiTypeConstruct.fromTemplate(templateSource, bindings*)
