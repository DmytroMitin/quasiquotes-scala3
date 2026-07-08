package quasiquotes.types

import scala.quoted.Quotes

object QuasiTypequotes:
  def tqq(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    QuasiTypePattern.repr(source)
