package quasiquotes.construct

import scala.quoted.Quotes

import quasiquotes.definitions.DefinitionQuasiquoteError
import quasiquotes.source.LocatedDiagnostic

private[quasiquotes] object DefinitionQuasiquoteMacroDiagnosticReporter:
  def abort(using q: Quotes)(
      failure: LocatedDiagnostic[DefinitionQuasiquoteError],
      argumentTerms: Seq[q.reflect.Term]
  ): Nothing =
    val position = positionFor(failure, argumentTerms)
    q.reflect.report.errorAndAbort(failure.diagnostic.message, position)

  def abortCallerInvariant(using q: Quotes)(
      arguments: Int,
      argumentTerms: Int
  ): Nothing =
    q.reflect.report.errorAndAbort(
      s"Definition quasiquote macro caller invariant failed: received $arguments compiler-free arguments and $argumentTerms macro argument anchors.",
      q.reflect.Position.ofMacroExpansion
    )

  private[construct] def positionFor(using q: Quotes)(
      failure: LocatedDiagnostic[DefinitionQuasiquoteError],
      argumentTerms: Seq[q.reflect.Term]
  ): q.reflect.Position =
    val anchor = DefinitionQuasiquoteMacroAnchorSelector.select(failure.location)
    anchor match
      case MacroDiagnosticAnchor.DefinitionInterpolationArgument(index) =>
        MacroArgumentPositionResolver.resolve(index, argumentTerms)
      case _ =>
        q.reflect.Position.ofMacroExpansion
