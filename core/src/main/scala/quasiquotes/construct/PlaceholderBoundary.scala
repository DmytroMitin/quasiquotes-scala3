package quasiquotes.construct

/** Compiler-free placeholder categories shared with the frontend index. */
private[construct] enum PlaceholderCategory derives CanEqual:
  case TermSplice
  case ConstructedTypeSplice
  case SelectedMemberNameSplice

  def label: String =
    this match
      case TermSplice => "Term splice"
      case ConstructedTypeSplice => "Constructed-type splice"
      case SelectedMemberNameSplice => "Selected-member name splice"

/** Compiler-free placeholder positions used by neutral construction errors. */
private[construct] enum PlaceholderPosition derives CanEqual:
  case Term
  case ExpressionAscriptionType
  case SelectedMemberName
  case UnsupportedType(context: String)
  case UnsupportedTerm(context: String)

  def invalidPhrase: String =
    this match
      case Term => "in term position"
      case ExpressionAscriptionType =>
        "as the complete type of an expression ascription"
      case SelectedMemberName => "as the name of an explicit receiver selection"
      case UnsupportedType(context) => s"inside $context"
      case UnsupportedTerm(context) => s"inside $context"
