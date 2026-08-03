package quasiquotes.construct

import scala.quoted.Quotes

private[construct] object MacroArgumentPositionResolver:
  def resolve(using q: Quotes)(
      index: Int,
      arguments: Seq[q.reflect.Term]
  ): q.reflect.Position =
    import q.reflect.*

    val fallback = Position.ofMacroExpansion
    arguments.lift(index) match
      case Some(argument) =>
        val position = argument.pos
        if isUsableBounds(position.start, position.end) then position else fallback
      case None => fallback

  private[construct] def isUsableBounds(start: Int, end: Int): Boolean =
    start >= 0 && end >= start

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
            MacroArgumentPositionResolver.resolve(
              index = 0,
              arguments = Seq(argument.asInstanceOf[Term])
            )
          case None => fallback
      case MacroDiagnosticAnchor.DefinitionInterpolationArgument(_) =>
        fallback
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
