package quasiquotes.definitions

private[quasiquotes] enum DefinitionNameSpelling derives CanEqual:
  case Plain
  case BacktickedKeyword

private[quasiquotes] sealed trait DefinitionName derives CanEqual:
  def decoded: String
  def source: String
  def spelling: DefinitionNameSpelling
  final def render: String = DefinitionName.render(this)

private[quasiquotes] object DefinitionName:
  private final case class ValidatedName(
      decoded: String,
      source: String,
      spelling: DefinitionNameSpelling
  ) extends DefinitionName

  private val Scala3Keywords: Set[String] = Set(
    "abstract",
    "as",
    "case",
    "catch",
    "class",
    "def",
    "derives",
    "do",
    "else",
    "end",
    "enum",
    "export",
    "extends",
    "extension",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "given",
    "if",
    "implicit",
    "import",
    "infix",
    "inline",
    "lazy",
    "macro",
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

  def plain(source: String): Either[DefinitionError, DefinitionName] =
    if isPlainIdentifier(source) && source != "_" && !Scala3Keywords(source) then
      Right(ValidatedName(source, source, DefinitionNameSpelling.Plain))
    else Left(DefinitionError.InvalidPlainName(source))

  def backticked(source: String): Either[DefinitionError, DefinitionName] =
    val hasOnePair =
      source.length >= 3 &&
        source.head == '`' &&
        source.last == '`' &&
        !source.substring(1, source.length - 1).contains('`') &&
        !source.contains('\r') &&
        !source.contains('\n')
    val decoded = if hasOnePair then source.substring(1, source.length - 1) else ""

    if hasOnePair && decoded.nonEmpty && Scala3Keywords(decoded) then
      Right(ValidatedName(decoded, source, DefinitionNameSpelling.BacktickedKeyword))
    else Left(DefinitionError.InvalidBacktickedName(source))

  def fromSource(source: String): Either[DefinitionError, DefinitionName] =
    if source.contains('`') then backticked(source) else plain(source)

  def render(name: DefinitionName): String =
    name.spelling match
      case DefinitionNameSpelling.Plain => s"PlainName(${name.source})"
      case DefinitionNameSpelling.BacktickedKeyword => s"BacktickedKeywordName(${name.source})"

  private def isPlainIdentifier(source: String): Boolean =
    source.nonEmpty &&
      isAsciiLetterOrUnderscore(source.head) &&
      source.tail.forall(isAsciiLetterOrDigitOrUnderscore)

  private def isAsciiLetterOrUnderscore(char: Char): Boolean =
    char == '_' || ('A' <= char && char <= 'Z') || ('a' <= char && char <= 'z')

  private def isAsciiLetterOrDigitOrUnderscore(char: Char): Boolean =
    isAsciiLetterOrUnderscore(char) || ('0' <= char && char <= '9')
