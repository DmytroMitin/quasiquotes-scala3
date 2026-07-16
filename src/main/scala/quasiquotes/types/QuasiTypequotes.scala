package quasiquotes.types

import scala.quoted.Quotes

object QuasiTypequotes:
  /** Recommended research-facing pattern convenience; this is a function, not an interpolator. */
  def tqq(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    QuasiTypePattern.pattern(source)

  /** Recommended research-facing construction convenience; this is a function, not an interpolator. */
  def tqr(
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[TypeQuasiquoteError, ConstructedType] =
    QuasiTypeConstruct.fromTemplate(templateSource, bindings*)
