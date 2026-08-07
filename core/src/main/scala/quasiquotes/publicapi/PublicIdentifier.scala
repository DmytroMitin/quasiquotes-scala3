package quasiquotes.publicapi

private[publicapi] object PublicIdentifier:
  private val Scala3Keywords = Set(
    "abstract", "as", "case", "catch", "class", "def", "derives", "do",
    "else", "end", "enum", "export", "extends", "extension", "false",
    "final", "finally", "for", "forSome", "given", "if", "implicit",
    "import", "infix", "inline", "lazy", "macro", "match", "new", "null",
    "object", "opaque", "open", "override", "package", "private",
    "protected", "return", "sealed", "super", "then", "this", "throw",
    "trait", "transparent", "true", "try", "type", "using", "val", "var",
    "while", "with", "yield"
  )

  def isValid(value: String): Boolean =
    value.nonEmpty &&
      value != "_" &&
      !Scala3Keywords(value) &&
      isAsciiLetterOrUnderscore(value.head) &&
      value.tail.forall(isAsciiLetterOrDigitOrUnderscore)

  private def isAsciiLetterOrUnderscore(value: Char): Boolean =
    value == '_' || ('A' <= value && value <= 'Z') ||
      ('a' <= value && value <= 'z')

  private def isAsciiLetterOrDigitOrUnderscore(value: Char): Boolean =
    isAsciiLetterOrUnderscore(value) || ('0' <= value && value <= '9')
