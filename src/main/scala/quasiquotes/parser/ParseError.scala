package quasiquotes.parser

enum ParseErrorKind derives CanEqual:
  case SyntaxError
  case TrailingInput

final case class ParseError(
    source: String,
    kind: ParseErrorKind,
    messages: List[String]
) extends RuntimeException(messages.mkString("; ")):
  def summary: String = messages.mkString("; ")

object ParseError:
  def syntax(source: String, messages: List[String]): ParseError =
    ParseError(source, ParseErrorKind.SyntaxError, messages)

  def trailing(source: String, offset: Int, trailingSnippet: String, tokenDescription: String): ParseError =
    val snippet =
      if trailingSnippet.nonEmpty then s"'$trailingSnippet'"
      else s"token $tokenDescription"
    ParseError(
      source,
      ParseErrorKind.TrailingInput,
      List(s"Trailing input after parsed expression at offset $offset: $snippet")
    )
