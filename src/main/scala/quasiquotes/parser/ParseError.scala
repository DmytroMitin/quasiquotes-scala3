package quasiquotes.parser

final case class ParseError(source: String, messages: List[String])
    extends RuntimeException(messages.mkString("; "))
