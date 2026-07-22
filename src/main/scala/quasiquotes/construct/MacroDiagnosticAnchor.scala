package quasiquotes.construct

import quasiquotes.source.*

private[construct] enum MacroDiagnosticAnchor derives CanEqual:
  case TermInterpolationArgument(index: Int)
  case MacroExpansion

private[construct] object MacroDiagnosticAnchorSelector:
  def select(location: Option[DiagnosticLocation]): MacroDiagnosticAnchor =
    location.toVector.flatMap(_.origins).distinct match
      case Vector(SourceOrigin.InterpolationArgument(_, index, InterpolationCategory.TermSplice)) =>
        MacroDiagnosticAnchor.TermInterpolationArgument(index)
      case _ =>
        MacroDiagnosticAnchor.MacroExpansion
