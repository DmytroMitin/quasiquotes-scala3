package quasiquotes.scalameta

import scala.quoted.Quotes

import quasiquotes.construct.QuasiTypeSplice

/** Explicit opt-in construction syntax. The ordinary Quasiquotes host remains
  * the current-Dotty default.
  */
object ScalametaQuasiquotes:
  extension (context: StringContext)
    def qr(using q: Quotes)(
        arguments: (q.reflect.Term | QuasiTypeSplice)*
    ): q.reflect.Term =
      TermFrontend.build(context.parts, arguments) match
        case Right(result) => result.term
        case Left(failure) => q.reflect.report.errorAndAbort(failure.message)

    def tqr(using q: Quotes)(arguments: q.reflect.TypeRepr*): q.reflect.TypeRepr =
      TypeFrontend.build(context.parts, arguments) match
        case Right(result) => result.typeRepr
        case Left(failure) =>
          q.reflect.report.errorAndAbort(
            s"Invalid Scalameta tqr type template: ${failure.message}"
          )
