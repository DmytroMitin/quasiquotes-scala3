package quasiquotes.construct

import quasiquotes.source.*

private[construct] enum MacroDiagnosticAnchor derives CanEqual:
  case TermInterpolationArgument(index: Int)
  case DefinitionInterpolationArgument(index: Int)
  case MacroExpansion

private[construct] object MacroDiagnosticAnchorSelector:
  def select(location: Option[DiagnosticLocation]): MacroDiagnosticAnchor =
    location.toVector.flatMap(_.origins).distinct match
      case Vector(SourceOrigin.InterpolationArgument(_, index, InterpolationCategory.TermSplice)) =>
        MacroDiagnosticAnchor.TermInterpolationArgument(index)
      case _ =>
        MacroDiagnosticAnchor.MacroExpansion

private[quasiquotes] object DefinitionQuasiquoteMacroAnchorSelector:
  def select(location: Option[DiagnosticLocation]): MacroDiagnosticAnchor =
    location match
      case Some(value) if value.precision == DiagnosticPrecision.ExactOccurrence =>
        val origins = value.origins.distinct
        val definitionArguments = origins.collect {
          case SourceOrigin.InterpolationArgument(_, index, category)
              if isDefinitionCategory(category) =>
            index
        }
        if
          origins.size == 1 &&
            definitionArguments.size == 1
        then
          MacroDiagnosticAnchor.DefinitionInterpolationArgument(
            definitionArguments.head
          )
        else MacroDiagnosticAnchor.MacroExpansion
      case _ =>
        MacroDiagnosticAnchor.MacroExpansion

  private def isDefinitionCategory(
      category: InterpolationCategory
  ): Boolean =
    category match
      case InterpolationCategory.DefinitionTypeSplice => true
      case InterpolationCategory.DefinitionBodyTermSplice => true
      case InterpolationCategory.DefinitionBodyTypeSplice => true
      case _ => false
