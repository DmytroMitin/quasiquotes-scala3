package quasiquotes.terms.parser

private[quasiquotes] sealed trait TermTemplateSourceAdapterError
    derives CanEqual:
  def message: String

private[quasiquotes] object TermTemplateSourceAdapterError:
  final case class OccurrenceCountMismatch(expected: Int, actual: Int)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Categorized hole occurrence count mismatch: source contains $expected holes but the plan contains $actual descriptors."

  final case class OccurrenceNameMismatch(
      index: Int,
      sourceName: String,
      plannedName: String
  ) extends TermTemplateSourceAdapterError:
    def message: String =
      s"Categorized hole descriptor $index names `$$$plannedName`, but source occurrence $index is `$$$sourceName`."

  final case class InvalidHoleName(index: Int, name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Categorized hole descriptor $index has invalid name `$name`: expected a nonempty ASCII identifier."

  final case class InvalidDollarSyntax(text: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Invalid or unexplained dollar syntax `$text`: expected `$$name` with a narrow ASCII identifier."

  final case class ParserFailure(detail: String)
      extends TermTemplateSourceAdapterError:
    def message: String = s"Term-template parser failure: $detail"

  final case class UnsupportedTermShape(detail: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Unsupported term-template shape: $detail"

  final case class TermMarkerInInvalidPosition(name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Term hole `$$$name` must occupy a complete term identifier position."

  final case class TypeMarkerOutsideAscription(name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Type hole `$$$name` must occur inside a supported expression-ascription type."

  final case class TermMarkerInsideType(name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Term hole `$$$name` is not valid inside expression-ascription type syntax."

  final case class TypeMarkerInTermPosition(name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Type hole `$$$name` is not valid in term syntax."

  final case class UnknownGeneratedMarker(name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Generated marker `$name` is not owned by the categorized source plan."

  final case class DuplicateGeneratedIdentity(name: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Generated identifier `$name` is reused across term and type categories."

  final case class UnsupportedTypeTemplateShape(detail: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Unsupported expression-ascription type template: $detail"

  final case class SidecarOrderMismatch(detail: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Typed sidecar extraction order mismatch: $detail"

  final case class InvalidSourceMetadata(detail: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Invalid categorized term-template source metadata: $detail"

  final case class DownstreamConstructionFailure(detail: String)
      extends TermTemplateSourceAdapterError:
    def message: String =
      s"Validated term-template construction failed: $detail"
