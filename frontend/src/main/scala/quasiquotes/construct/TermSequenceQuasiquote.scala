package quasiquotes.construct

import scala.quoted.Quotes

/** Shared direct-reflection route for sequence-Term construction facades. */
private[quasiquotes] object TermSequenceQuasiquote:
  def buildOrAbort(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[
        q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName |
          TermSequenceSplice[q.reflect.Term]
      ]
  ): q.reflect.Term =
    QuasiquoteBuilder.buildLocated(parts, arguments) match
      case Right(term) => term
      case Left(failure) => QuasiquoteDiagnosticReporter.abort(failure, arguments)
