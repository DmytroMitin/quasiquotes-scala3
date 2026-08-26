package quasiquotes.construct

/** A validated decoded source-level name for one explicit receiver selection.
  *
  * This value carries no compiler, tree, symbol, encoding, or lexical-spelling
  * identity. It is intentionally not a general source-name abstraction.
  */
final class SelectedMemberName private (val decoded: String):
  override def equals(other: Any): Boolean =
    other match
      case that: SelectedMemberName => decoded == that.decoded
      case _ => false

  override def hashCode: Int = decoded.hashCode

  override def toString: String = s"SelectedMemberName($decoded)"

object SelectedMemberName:
  /** Stable recoverable failure from the conservative decoded-name grammar. */
  final case class ValidationFailure(code: String, message: String) derives CanEqual

  private val Plain = "[A-Za-z_][A-Za-z0-9_]*".r
  private val Symbolic = "[!#%&*+\\-/:<=>?@\\\\^|~]+".r
  private val Spaced =
    "[A-Za-z_][A-Za-z0-9_]*( [A-Za-z_][A-Za-z0-9_]*)+".r

  /** Validates a decoded member name without consulting a compiler or dialect. */
  def from(decoded: String): Either[ValidationFailure, SelectedMemberName] =
    validate(decoded).map(_ => new SelectedMemberName(decoded))

  private def validate(decoded: String): Either[ValidationFailure, Unit] =
    if decoded == null then invalid("null-name", "Selected-member name must not be null.")
    else if decoded.isEmpty then invalid("empty-name", "Selected-member name must not be empty.")
    else if decoded.contains('$') then invalid("encoded-name", "Selected-member name must not contain '$'.")
    else if decoded.contains('`') then invalid("lexical-escape", "Selected-member name must not contain a literal backtick.")
    else if decoded.exists(character => character < ' ' || character == 0x7f.toChar) then
      invalid("control-character", "Selected-member name must not contain line breaks or control characters.")
    else if decoded.contains('.') then invalid("dotted-name", "Selected-member name must identify one member, not a dotted path.")
    else if decoded == "<init>" || decoded == "<clinit>" then
      invalid("compiler-special-name", "Compiler-special selected-member names are not supported.")
    else if !decoded.forall(_ <= 0x7f.toChar) then
      invalid("unicode-name", "Unicode selected-member names are outside the conservative supported grammar.")
    else if Plain.matches(decoded) || Symbolic.matches(decoded) || Spaced.matches(decoded) then Right(())
    else invalid("unsupported-grammar", "Selected-member name is outside the conservative supported grammar.")

  private def invalid(code: String, message: String): Left[ValidationFailure, Nothing] =
    Left(ValidationFailure(code, message))
