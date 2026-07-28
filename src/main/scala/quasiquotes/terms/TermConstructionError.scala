package quasiquotes.terms

private[quasiquotes] sealed trait TermConstructionError derives CanEqual:
  def message: String

private[quasiquotes] object TermConstructionError:
  final case class UnsupportedTermShape() extends TermConstructionError:
    def message: String =
      "Unsupported term shape: the term contains syntax outside the bounded compiler-free fragment."

  final case class UnsupportedUnaryOperator(operator: String) extends TermConstructionError:
    def message: String =
      s"Unsupported unary operator `$operator`: expected one of +, -, !, or ~."

  final case class InvalidTupleArity(arity: Int) extends TermConstructionError:
    def message: String =
      s"Invalid tuple arity $arity: expected an arity from 2 through 22."

  final case class InvalidTermHoleName(name: String) extends TermConstructionError:
    def message: String =
      s"Invalid term-hole name `$name`: expected a nonempty ASCII identifier."

  final case class DuplicateGeneratedIdentifier(name: String) extends TermConstructionError:
    def message: String =
      s"Generated identifier `$name` is reused across term and type hole categories."

  final case class UnownedGeneratedMarker(name: String, identifierOrdinal: Int)
      extends TermConstructionError:
    def message: String =
      s"Identifier ordinal $identifierOrdinal contains unowned generated marker `$name`."

  final case class UnknownTermOccurrence(name: String, identifierOrdinal: Int)
      extends TermConstructionError:
    def message: String =
      s"Term-hole occurrence `$name` at identifier ordinal $identifierOrdinal is not registered."

  final case class DuplicateTermOccurrenceAddress(identifierOrdinal: Int)
      extends TermConstructionError:
    def message: String =
      s"Identifier ordinal $identifierOrdinal is assigned to more than one term-hole occurrence."

  final case class MissingTermOccurrence(name: String) extends TermConstructionError:
    def message: String =
      s"Registered term hole `$name` has no owned identifier occurrence."

  final case class TermOccurrenceCategoryMismatch(
      name: String,
      identifierOrdinal: Int
  ) extends TermConstructionError:
    def message: String =
      s"Identifier ordinal $identifierOrdinal uses type-hole transport for term occurrence `$name`."

  final case class InvalidTermHolePosition(name: String) extends TermConstructionError:
    def message: String =
      s"Term hole `$name` must occupy a complete identifier term position."

  final case class TypedSidecarCountMismatch(expected: Int, actual: Int)
      extends TermConstructionError:
    def message: String =
      s"Typed sidecar count mismatch: expected $expected entries but received $actual."

  final case class TypedSidecarRenderingMismatch(
      typedOrdinal: Int,
      expected: String,
      actual: String
  ) extends TermConstructionError:
    def message: String =
      s"Typed sidecar rendering mismatch at typed ordinal $typedOrdinal: expected `$expected` but found `$actual`."

  final case class InvalidTypeTemplateSidecar(typedOrdinal: Int, detail: String)
      extends TermConstructionError:
    def message: String =
      s"Invalid type-template sidecar at typed ordinal $typedOrdinal: $detail"

  final case class MissingTermBinding(name: String) extends TermConstructionError:
    def message: String = s"Missing term binding `$name`."

  final case class ExtraTermBinding(name: String) extends TermConstructionError:
    def message: String = s"Extra term binding `$name`."

  final case class MissingTypeBinding(name: String) extends TermConstructionError:
    def message: String = s"Missing type binding `$name`."

  final case class ExtraTypeBinding(name: String) extends TermConstructionError:
    def message: String = s"Extra type binding `$name`."

  final case class TypeBindingConstructionFailure(name: String, detail: String)
      extends TermConstructionError:
    def message: String =
      s"Type binding `$name` is outside the admitted compiler-free type subset: $detail"

  final case class IncompleteBoundTerm(name: String) extends TermConstructionError:
    def message: String =
      s"Term binding `$name` is not a validated completed term."

  final case class CompletionInvariantFailure(detail: String) extends TermConstructionError:
    def message: String = s"Term completion invariant failed: $detail"

  final case class InvalidLocatedTemplateMetadata(detail: String)
      extends TermConstructionError:
    def message: String = s"Invalid located term-template metadata: $detail"
