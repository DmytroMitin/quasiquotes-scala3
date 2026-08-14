package quasiquotes.definitions

private[quasiquotes] sealed trait DefinitionError derives CanEqual:
  def message: String

private[quasiquotes] object DefinitionError:
  final case class InvalidPlainName(source: String) extends DefinitionError:
    def message: String =
      s"Invalid plain definition name ${quoted(source)}: expected a non-keyword ASCII identifier matching [A-Za-z_][A-Za-z0-9_]*, excluding `_`."

  final case class InvalidBacktickedName(source: String) extends DefinitionError:
    def message: String =
      s"Invalid backticked definition name ${quoted(source)}: expected exactly one backtick pair around a reserved Scala 3 keyword."

  final case class UnsupportedDefinitionType(component: String) extends DefinitionError:
    def message: String =
      s"Unsupported $component: expected the currently supported compiler-free structural type subset."

  final case class UnsupportedDefinitionBody(component: String, reason: String) extends DefinitionError:
    def message: String =
      s"Unsupported $component: $reason."

  final case class InvalidTwoParameterList(reason: String) extends DefinitionError:
    def message: String =
      s"Invalid two-parameter definition parameter list: $reason."

  final case class InvalidSourceMetadata(reason: String) extends DefinitionError:
    def message: String =
      s"Invalid definition source metadata: $reason."

  private def quoted(value: String): String =
    val escaped = value
      .replace("\\", "\\\\")
      .replace("\r", "\\r")
      .replace("\n", "\\n")
      .replace("\t", "\\t")
    s"'$escaped'"
