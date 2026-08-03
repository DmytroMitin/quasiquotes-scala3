package quasiquotes.types

import scala.quoted.*

/** Quotes-owned lowering syntax for the compiler-free `ConstructedType`. */
extension (constructed: ConstructedType)
  def toTypeRepr(
      using Quotes
  ): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    TypeReprLowerer.lowerNormalForm(constructed.normalForm)
