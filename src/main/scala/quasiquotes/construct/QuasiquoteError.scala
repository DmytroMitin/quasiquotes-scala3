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

  final case class InvalidPlaceholderName(name: String) extends QuasiquoteError:
    def message: String = s"Invalid placeholder identifier: $name"

  final case class UnsupportedTree(nodeKind: String, detail: String) extends QuasiquoteError:
    def message: String = s"Unsupported parsed tree shape: $nodeKind ($detail)"

  final case class UnsupportedLiteral(detail: String) extends QuasiquoteError:
    def message: String = s"Unsupported literal: $detail"

  final case class UnresolvedIdentifier(name: String) extends QuasiquoteError:
    def message: String = s"Could not resolve identifier '$name' in the current macro scope"

  final case class UnsupportedSelection(qualifierType: String, name: String, detail: String) extends QuasiquoteError:
    def message: String =
      s"Could not select '$name' from qualifier type $qualifierType: $detail"

  final case class UnsupportedApplication(detail: String) extends QuasiquoteError:
    def message: String = s"Could not apply parsed function term: $detail"
