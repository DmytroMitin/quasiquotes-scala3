package quasiquotes.construct

import scala.quoted.Quotes

object Quasiquotes:
  extension (sc: StringContext)
    def qr(using q: Quotes)(
        args: (q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName)*
    ): q.reflect.Term =
      QuasiquoteBuilder.buildLocated(sc.parts, args) match
        case Right(term) => term
        case Left(failure) => QuasiquoteDiagnosticReporter.abort(failure, args)

    def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef =
      PublicDefinitionQuasiquote.build(sc, args)
