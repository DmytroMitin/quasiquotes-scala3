package quasiquotes.parser

private[quasiquotes] object LocalDefDiagnosticMessages:
  val ExactlyOne: String =
    "Source-owned local def requires exactly one local method statement followed by one result expression."
  val OrdinaryName: String =
    "Source-owned local def requires literal legal ordinary method and parameter names."
  val Modifiers: String =
    "Source-owned local def does not support modifiers or annotations."
  val TypeParameters: String =
    "Source-owned local def does not support method type parameters."
  val ParameterClause: String =
    "Source-owned local def requires exactly one ordinary value parameter in one parameter list."
  val ExplicitTypes: String =
    "Source-owned local def requires complete explicit parameter and result Types."
  val UnsupportedTypes: String =
    "Source-owned local def supports only complete reflected-Type holes or the existing fixed Int/String/Boolean Types in parameter and result positions."
  val Body: String =
    "Source-owned local def body must be exactly its own ordinary parameter reference."
  val IncompatibleResultType: String =
    "Source-owned local def parameter Type is not compatible with the declared result Type."
  val LoweringFailure: String =
    "Source-owned local def symbol or owner lowering failed."
  val OwnedDefinitionSplice: String =
    "Source-owned local def result does not support an external splice containing owned definitions."
