package quasiquotes.definitions

private[quasiquotes] enum DefinitionNameSpelling derives CanEqual:
  case Plain
  case BacktickedKeyword

final class DefinitionName private (
    private[quasiquotes] val decoded: String,
    val source: String,
    private[quasiquotes] val spelling: DefinitionNameSpelling
) derives CanEqual:
  private[quasiquotes] def render: String = DefinitionName.render(this)

  override def equals(other: Any): Boolean =
    other match
      case that: DefinitionName =>
        decoded == that.decoded &&
          source == that.source &&
          spelling == that.spelling
      case _ => false

  override def hashCode(): Int = (decoded, source, spelling).hashCode
  override def toString: String = render

object DefinitionName:

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

  private[quasiquotes] def plain(source: String): Either[DefinitionError, DefinitionName] =
    if source != null && isPlainIdentifier(source) && source != "_" && !Scala3Keywords(source) then
      Right(new DefinitionName(source, source, DefinitionNameSpelling.Plain))
    else Left(DefinitionError.InvalidPlainName(source))

  private[quasiquotes] def backticked(
      source: String
  ): Either[DefinitionError, DefinitionName] =
    val hasOnePair =
      source != null &&
        source.length >= 3 &&
        source.head == '`' &&
        source.last == '`' &&
        !source.substring(1, source.length - 1).contains('`') &&
        !source.contains('\r') &&
        !source.contains('\n')
    val decoded = if hasOnePair then source.substring(1, source.length - 1) else ""

    if hasOnePair && decoded.nonEmpty && Scala3Keywords(decoded) then
      Right(new DefinitionName(decoded, source, DefinitionNameSpelling.BacktickedKeyword))
    else Left(DefinitionError.InvalidBacktickedName(source))

  def fromSource(source: String): Either[DefinitionSemanticError, DefinitionName] =
    if source == null then
      Left(
        DefinitionSemanticError(
          "DEFINITION_SEMANTIC_MISSING",
          "the definition name source must be present."
        )
      )
    else
      fromSourceInternal(source).left.map(error =>
        DefinitionSemanticError("DEFINITION_SEMANTIC_INVALID_NAME", error.message)
      )

  private[quasiquotes] def fromSourceInternal(
      source: String
  ): Either[DefinitionError, DefinitionName] =
    if source != null && source.contains('`') then backticked(source) else plain(source)

  private[quasiquotes] def render(name: DefinitionName): String =
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
