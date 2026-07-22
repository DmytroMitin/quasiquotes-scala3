package quasiquotes.construct

import quasiquotes.types.ConstructedType

/** Explicit marker for inserting an already constructed type into a supported `qr` type position. */
final class QuasiTypeSplice private[construct] (private[construct] val constructedType: ConstructedType)

object QuasiTypeSplices:
  def typeSplice(constructedType: ConstructedType): QuasiTypeSplice =
    new QuasiTypeSplice(constructedType)
