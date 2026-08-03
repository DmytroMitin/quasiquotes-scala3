package quasiquotes.definitions.dotty

import quasiquotes.definitions.DefinitionError

private[quasiquotes] sealed trait RawDefinitionAdapterError derives CanEqual:
  def message: String

private[quasiquotes] object RawDefinitionAdapterError:
  case object DefinitionParseFailure extends RawDefinitionAdapterError:
    val message: String = "The source could not be parsed as one raw definition."

  final case class ExpectedExactlyOneDefinition(found: Int) extends RawDefinitionAdapterError:
    def message: String =
      s"Expected exactly one top-level definition, but found $found top-level statements."

  case object UnsupportedRawDefinitionKind extends RawDefinitionAdapterError:
    val message: String =
      "Expected a parameterless def or an immutable val with a simple identifier binder."

  case object MissingExplicitType extends RawDefinitionAdapterError:
    val message: String = "The definition must have an explicit declared type."

  case object MissingDefinitionBody extends RawDefinitionAdapterError:
    val message: String = "The definition must have an explicit body or right-hand side."

  case object UnsupportedTypeParameters extends RawDefinitionAdapterError:
    val message: String = "Type parameters are not supported by the raw definition adapter."

  case object UnsupportedParameterClauses extends RawDefinitionAdapterError:
    val message: String = "Parameter clauses are not supported by the raw definition adapter."

  case object UnsupportedDefinitionModifiers extends RawDefinitionAdapterError:
    val message: String =
      "Definition modifiers and annotations are not supported by the raw definition adapter."

  case object UnsupportedMutableValue extends RawDefinitionAdapterError:
    val message: String = "Mutable values are not supported by the raw definition adapter."

  case object UnsupportedLazyValue extends RawDefinitionAdapterError:
    val message: String = "Lazy values are not supported by the raw definition adapter."

  case object UnsupportedPatternValue extends RawDefinitionAdapterError:
    val message: String = "Pattern value definitions are not supported by the raw definition adapter."

  final case class InvalidDefinitionName(cause: DefinitionError) extends RawDefinitionAdapterError:
    def message: String = cause.message

  final case class UnsupportedDefinitionType(cause: DefinitionError) extends RawDefinitionAdapterError:
    def message: String = cause.message

  final case class UnsupportedDefinitionBody(cause: DefinitionError) extends RawDefinitionAdapterError:
    def message: String = cause.message

  final case class IndefensibleComponentSpan(component: String) extends RawDefinitionAdapterError:
    def message: String =
      s"Could not establish a defensible exact source span for the definition $component."
