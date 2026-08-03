package quasiquotes.definitions.parser

private[quasiquotes] sealed trait DefinitionTemplateSourceAdapterError
    derives CanEqual:
  def message: String

private[quasiquotes] object DefinitionTemplateSourceAdapterError:
  final case class InvalidHoleName(index: Int, name: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Categorized definition hole descriptor $index has invalid name `$name`: expected a nonempty ASCII identifier."

  final case class InvalidDollarSyntax(text: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Invalid or unexplained definition dollar syntax `$text`: expected `$$name` with a narrow ASCII identifier."

  final case class OccurrenceCountMismatch(expected: Int, actual: Int)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Categorized definition hole occurrence count mismatch: source contains $expected holes but the plan contains $actual descriptors."

  final case class OccurrenceNameMismatch(
      index: Int,
      sourceName: String,
      plannedName: String
  ) extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Categorized definition hole descriptor $index names `$$$plannedName`, but source occurrence $index is `$$$sourceName`."

  final case class CategoryComponentMismatch(
      index: Int,
      name: String,
      category: DefinitionTemplateHoleCategory,
      detail: String
  ) extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Definition hole occurrence $index `$$$name` with category $category is not valid in its parsed component: $detail"

  final case class DefinitionParserFailure(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Definition-template parser failure: $detail"

  final case class UnsupportedDefinitionVariant(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Unsupported definition-template variant: $detail"

  final case class InvalidDefinitionName(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Invalid fixed definition name: $detail"

  final case class InvalidDefinitionTypeTemplate(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Invalid definition type template: $detail"

  final case class InvalidDefinitionBodyTemplate(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Invalid definition body template: $detail"

  final case class UnknownGeneratedMarker(name: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Generated definition marker `$name` is not owned by the categorized source plan."

  final case class IncompleteDefinitionSourceMap(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Incomplete categorized definition source map: $detail"

  final case class InvalidLocatedDefinitionTemplate(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Invalid located definition template: $detail"

  final case class DefinitionTemplateFactoryFailure(detail: String)
      extends DefinitionTemplateSourceAdapterError:
    def message: String =
      s"Validated definition-template construction failed: $detail"
