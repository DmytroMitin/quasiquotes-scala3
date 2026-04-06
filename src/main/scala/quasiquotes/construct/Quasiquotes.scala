package quasiquotes.construct

import scala.quoted.Quotes

object Quasiquotes:
  extension (sc: StringContext)
    def qr(using q: Quotes)(args: q.reflect.Term*): q.reflect.Term =
      QuasiquoteBuilder.build(sc.parts, args) match
        case Right(term) => term
        case Left(error) => q.reflect.report.errorAndAbort(error.message)
