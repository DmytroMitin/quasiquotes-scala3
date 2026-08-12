package quasiquotes.matching

import quasiquotes.parser.ParseError

sealed trait PatternError derives CanEqual:
  def message: String

object PatternError:
  final case class ParseFailure(parseError: ParseError) extends PatternError:
    def message: String = s"${parseError.kind}: ${parseError.summary}"

  final case class UnsupportedPatternShape(nodeKind: String, detail: String) extends PatternError:
    def message: String =
      if nodeKind == "Lambda1" then detail
      else s"Unsupported pattern tree shape: $nodeKind ($detail)"

  final case class InvalidHoleName(name: String) extends PatternError:
    def message: String = s"Invalid pattern hole name: $name"

  final case class NoHolesInInterpolator() extends PatternError:
    def message: String = "Interpolator-based qq patterns are not implemented in this POC; use QuasiPattern.term(\"...\")"
