package quasiquotes.terms.parser

private[quasiquotes] enum TermTemplateHoleCategory derives CanEqual:
  case Term
  case Type

private[quasiquotes] final case class CategorizedHoleOccurrence(
    name: String,
    category: TermTemplateHoleCategory
) derives CanEqual
