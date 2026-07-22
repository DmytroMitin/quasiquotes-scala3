package quasiquotes.construct

import scala.quoted.Quotes

private[construct] object MacroDiagnosticPositionResolver:
  def resolve(using q: Quotes)(
      anchor: MacroDiagnosticAnchor,
      arguments: Seq[q.reflect.Term | QuasiTypeSplice]
  ): q.reflect.Position =
    import q.reflect.*

    val fallback = Position.ofMacroExpansion
    anchor match
      case MacroDiagnosticAnchor.TermInterpolationArgument(index) =>
        arguments.lift(index) match
          case Some(_: QuasiTypeSplice) => fallback
          case Some(argument) =>
            val position = argument.asInstanceOf[Term].pos
            if position.start >= 0 && position.end >= position.start then position else fallback
          case None => fallback
      case MacroDiagnosticAnchor.MacroExpansion =>
        fallback

private[construct] object QuasiquoteDiagnosticReporter:
  def abort(using q: Quotes)(
      failure: QuasiquoteBuildFailure,
      arguments: Seq[q.reflect.Term | QuasiTypeSplice]
  ): Nothing =
    val anchor = MacroDiagnosticAnchorSelector.select(failure.location)
    val position = MacroDiagnosticPositionResolver.resolve(anchor, arguments)
    q.reflect.report.errorAndAbort(failure.error.message, position)
