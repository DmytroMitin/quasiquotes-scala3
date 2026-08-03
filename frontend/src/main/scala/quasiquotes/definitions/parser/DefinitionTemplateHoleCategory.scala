package quasiquotes.definitions.parser

private[quasiquotes] enum DefinitionTemplateHoleCategory derives CanEqual:
  case DefinitionType
  case BodyTerm
  case BodyType

private[quasiquotes] final case class CategorizedDefinitionHoleOccurrence(
    name: String,
    category: DefinitionTemplateHoleCategory
) derives CanEqual
