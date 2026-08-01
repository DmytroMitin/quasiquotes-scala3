package quasiquotes.construct

import scala.quoted.Quotes

import quasiquotes.definitions.*

private[quasiquotes] object DefinitionQuasiquoteMacroCaller:
  def constructOrAbort(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[DefinitionQuasiquoteArgument],
      argumentTerms: Seq[q.reflect.Term]
  ): DefinitionQuasiquoteResult =
    if arguments.size != argumentTerms.size then
      DefinitionQuasiquoteMacroDiagnosticReporter.abortCallerInvariant(
        arguments.size,
        argumentTerms.size
      )
    else
      import DefinitionQuasiquotes.*

      StringContext(parts*).dqr(arguments*) match
        case Right(result) => result
        case Left(failure) =>
          DefinitionQuasiquoteMacroDiagnosticReporter.abort(
            failure,
            argumentTerms
          )
