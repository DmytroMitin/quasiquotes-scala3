package quasiquotes.definitions

private[quasiquotes] sealed trait DefinitionConstructionError derives CanEqual:
  def message: String

private[quasiquotes] object DefinitionConstructionError:
  final case class InvalidDefinitionTypeTemplate(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Invalid definition type template: $detail"

  final case class InvalidDefinitionBodyTemplate(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Invalid definition body template: $detail"

  final case class InvalidTwoParameterList(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Invalid two-parameter definition parameter list: $detail."

  final case class MissingTermBinding(name: String)
      extends DefinitionConstructionError:
    def message: String = s"Missing definition term binding `$name`."

  final case class UnexpectedTermBinding(name: String)
      extends DefinitionConstructionError:
    def message: String = s"Unexpected definition term binding `$name`."

  final case class MissingTypeBinding(name: String)
      extends DefinitionConstructionError:
    def message: String = s"Missing definition type binding `$name`."

  final case class UnexpectedTypeBinding(name: String)
      extends DefinitionConstructionError:
    def message: String = s"Unexpected definition type binding `$name`."

  final case class InvalidTypeBinding(name: String, detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Definition type binding `$name` is outside the admitted compiler-free type subset: $detail"

  final case class DefinitionTypeConstructionFailure(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Definition type construction failed: $detail"

  final case class BodyConstructionFailure(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Definition body construction failed: $detail"

  final case class InvalidConstructedDefinitionType(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Invalid completed definition type: $detail"

  final case class InvalidConstructedDefinitionBody(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Invalid completed definition body: $detail"

  final case class CompletedDefinitionFactoryFailure(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Completed definition validation failed: $detail"

  final case class UnsupportedParsedDefinitionType(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Unsupported parsed definition type: $detail"

  final case class UnsupportedParsedDefinitionBody(detail: String)
      extends DefinitionConstructionError:
    def message: String =
      s"Unsupported parsed definition body: $detail"
