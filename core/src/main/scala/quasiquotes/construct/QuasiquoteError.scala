package quasiquotes.construct

import quasiquotes.parser.ParseError

sealed trait QuasiquoteError derives CanEqual:
  def message: String

object QuasiquoteError:
  final case class ParseFailure(parseError: ParseError) extends QuasiquoteError:
    def message: String = s"${parseError.kind}: ${parseError.summary}"

  final case class HoleCountMismatch(expected: Int, actual: Int) extends QuasiquoteError:
    def message: String =
      s"Hole count mismatch: expected $expected interpolated holes but received $actual"

  final case class MissingPlaceholder(index: Int) extends QuasiquoteError:
    def message: String = s"Missing term hole for placeholder __hole$index"

  final case class UnknownPlaceholder(name: String) extends QuasiquoteError:
    def message: String = s"Unknown categorized quasiquote placeholder `$name`."

  final case class PlaceholderCategoryMismatch(
      name: String,
      actual: PlaceholderCategory,
      position: PlaceholderPosition
  ) extends QuasiquoteError:
    def message: String =
      s"${actual.label} `$name` is not valid ${position.invalidPhrase}."

  final case class UnsupportedPlaceholderPosition(
      name: String,
      actual: PlaceholderCategory,
      position: PlaceholderPosition
  ) extends QuasiquoteError:
    def message: String =
      s"${actual.label} `$name` is not supported ${position.invalidPhrase}; only the complete type of an expression ascription is supported."

  final case class TypeSpliceLoweringFailure(detail: String) extends QuasiquoteError:
    def message: String = detail

  final case class InvalidPlaceholderName(name: String) extends QuasiquoteError:
    def message: String = s"Invalid placeholder identifier: $name"

  final case class UnsupportedTree(nodeKind: String, detail: String) extends QuasiquoteError:
    def message: String =
      if nodeKind == "Lambda1" || nodeKind == "Lambda1Splice" then detail
      else s"Unsupported parsed tree shape: $nodeKind ($detail)"

  final case class UnsupportedLiteral(detail: String) extends QuasiquoteError:
    def message: String = s"Unsupported literal: $detail"

  final case class UnresolvedIdentifier(name: String) extends QuasiquoteError:
    def message: String = s"Could not resolve identifier '$name' in the current macro scope"

  final case class InvalidConstructorName(name: String, detail: String) extends QuasiquoteError:
    def message: String = s"Unsupported constructor name '$name': $detail"

  final case class UnresolvedConstructor(name: String, detail: String) extends QuasiquoteError:
    def message: String = s"Could not resolve fully-qualified constructor class '$name': $detail"

  final case class UnsupportedConstructorApplication(name: String, detail: String) extends QuasiquoteError:
    def message: String = s"Could not apply constructor '$name': $detail"

  final case class UnsupportedSelection(qualifierType: String, name: String, detail: String) extends QuasiquoteError:
    def message: String =
      s"Could not select '$name' from qualifier type $qualifierType: $detail"

  final case class UnsupportedSelectedMemberNamePosition(context: String)
      extends QuasiquoteError:
    def message: String =
      s"Selected-member name hole is not supported $context; only the name field of an explicit receiver selection is supported."

  final case class MissingOrInaccessibleSelectedMember(name: String)
      extends QuasiquoteError:
    def message: String =
      s"Selected member '$name' is missing or inaccessible on the explicit receiver."

  final case class NonUniqueSelectedMember(name: String) extends QuasiquoteError:
    def message: String =
      s"Selected member '$name' is not unique on the explicit receiver."

  final case class SelectedMemberLoweringFailure(name: String)
      extends QuasiquoteError:
    def message: String =
      s"Selected member '$name' could not be lowered on the explicit receiver."

  final case class UnsupportedApplication(detail: String) extends QuasiquoteError:
    def message: String = s"Could not apply parsed function term: $detail"
