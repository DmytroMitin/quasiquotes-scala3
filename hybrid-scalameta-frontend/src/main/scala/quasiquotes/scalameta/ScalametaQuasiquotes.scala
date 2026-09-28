package quasiquotes.scalameta

import scala.quoted.Quotes
import scala.annotation.targetName

import quasiquotes.construct.{
  QuasiTypeSplice,
  SelectedMemberName,
  TermSequenceQuasiquote,
  TermSequenceSplice
}
import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend
import quasiquotes.types.RuntimeTypeApplication

/** Explicit opt-in construction syntax. The ordinary Quasiquotes host remains
  * the current-Dotty default.
  */
object ScalametaQuasiquotes:
  extension (context: StringContext)
    def qr(using q: Quotes)(
        arguments: (q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName)*
    ): q.reflect.Term =
      TermFrontend.build(context.parts, arguments) match
        case Right(result) => result.term
        case Left(failure) => q.reflect.report.errorAndAbort(failure.message)

    /** Direct reflected parity with the standard sequence-Term `qr` route.
      * Scalar opt-in construction remains Scalameta-primary.
      */
    @targetName("qrWithTermSequence")
    def qr(using q: Quotes)(
        arguments: (q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName |
          TermSequenceSplice[q.reflect.Term])*
    ): q.reflect.Term =
      TermSequenceQuasiquote.buildOrAbort(context.parts, arguments)

    def tqr(using q: Quotes)(arguments: q.reflect.TypeRepr*): q.reflect.TypeRepr =
      TypeFrontend.build(context.parts, arguments) match
        case Right(result) => result.typeRepr
        case Left(failure) =>
          q.reflect.report.errorAndAbort(
            s"Invalid Scalameta tqr type template: ${failure.message}"
          )

    /** Direct reflected parity with the standard runtime-sequence `tqr` route.
      * Scalar opt-in construction remains Scalameta-primary.
      */
    def tqr(using q: Quotes)(
        constructor: q.reflect.TypeRepr,
        arguments: Seq[q.reflect.TypeRepr],
        additionalArgumentSequences: Seq[q.reflect.TypeRepr]*
    ): q.reflect.TypeRepr =
      val prefix = "Invalid tqr type template:"
      if context == null then
        q.reflect.report.errorAndAbort(s"$prefix StringContext must not be null.")
      val parts = context.parts
      if parts == null || parts.isEmpty then
        q.reflect.report.errorAndAbort(s"$prefix StringContext must contain at least one part.")
      RuntimeTypeApplication
        .build(using q)(parts, constructor, arguments, additionalArgumentSequences*)
        .fold(
          detail => q.reflect.report.errorAndAbort(s"$prefix $detail"),
          identity
        )

    def dqr(using q: Quotes)(arguments: q.reflect.TypeRepr*): q.reflect.DefDef =
      ScalametaDefinitionFrontend.build(using q)(context.parts, arguments) match
        case Right(definition) => definition
        case Left(failure) =>
          q.reflect.report.errorAndAbort(
            s"Invalid Scalameta dqr definition template: ${failure.message}"
          )
