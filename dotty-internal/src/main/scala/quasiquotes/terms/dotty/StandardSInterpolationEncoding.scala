package quasiquotes.terms.dotty

import quasiquotes.parser.TermShape

private[dotty] object StandardSInterpolationEncoding:
  final case class EncodedPart(source: String, rawLiteralValue: String)

  private val PlainIdentifier = "[A-Za-z_][A-Za-z0-9_]*".r
  private val Keywords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "given",
    "if",
    "implicit",
    "import",
    "inline",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "opaque",
    "open",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "transparent",
    "true",
    "try",
    "type",
    "using",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  def encodePart(value: String): EncodedPart =
    val source = new StringBuilder
    val rawLiteral = new StringBuilder

    def appendEscape(value: String): Unit =
      source.append(value)
      rawLiteral.append(value)

    value.foreach {
      case '$' =>
        source.append("$$")
        rawLiteral.append('$')
      case '\\' => appendEscape("\\\\")
      case '"' => appendEscape("\\\"")
      case '\n' => appendEscape("\\n")
      case '\r' => appendEscape("\\r")
      case '\t' => appendEscape("\\t")
      case '\b' => appendEscape("\\b")
      case '\f' => appendEscape("\\f")
      case char if char < ' ' || char == '\u007f' =>
        appendEscape(f"\\u${char.toInt}%04x")
      case char =>
        source.append(char)
        rawLiteral.append(char)
    }
    EncodedPart(source.toString, rawLiteral.toString)

  def isPlainIdentifier(name: String): Boolean =
    name match
      case PlainIdentifier() => true
      case _ => false

  def isKeyword(name: String): Boolean =
    Keywords(name)

  def isDirectArgument(shape: TermShape): Boolean =
    shape match
      case TermShape.Identifier(name, _)
          if isPlainIdentifier(name) && !isKeyword(name) =>
        true
      case _ =>
        false
