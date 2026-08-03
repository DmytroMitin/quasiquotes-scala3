package quasiquotes.parser

import quasiquotes.source.SourceSpan

enum ParseErrorKind derives CanEqual:
  case SyntaxError
  case TrailingInput

final case class ParseDiagnostic(message: String, generatedSpan: Option[SourceSpan]) derives CanEqual

final case class ParseError(
    source: String,
    kind: ParseErrorKind,
    messages: List[String],
    diagnostics: List[ParseDiagnostic] = Nil
) extends RuntimeException(messages.mkString("; ")):
  def summary: String = messages.mkString("; ")

object ParseError:
  def syntax(source: String, messages: List[String]): ParseError =
    ParseError(
      source,
      ParseErrorKind.SyntaxError,
      messages,
      messages.map(message => ParseDiagnostic(message, None))
    )

  def trailing(source: String, offset: Int, trailingSnippet: String, tokenDescription: String): ParseError =
    val snippet =
      if trailingSnippet.nonEmpty then s"'$trailingSnippet'"
      else s"token $tokenDescription"
    val message = s"Trailing input after parsed expression at offset $offset: $snippet"
    val generatedSpan = Option.when(0 <= offset && offset <= source.length)(SourceSpan(offset, source.length))
    ParseError(
      source,
      ParseErrorKind.TrailingInput,
      List(message),
      List(ParseDiagnostic(message, generatedSpan))
    )
