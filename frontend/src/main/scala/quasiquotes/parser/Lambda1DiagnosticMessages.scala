package quasiquotes.parser

private[quasiquotes] object Lambda1DiagnosticMessages:
  val ExplicitParameterType =
    "Lambda1 requires an explicit parameter type; write a parameter such as `(x: Int)`."

  val ExactlyOneParameter =
    "Lambda1 supports exactly one explicitly typed ordinary parameter; rewrite this as a one-parameter lambda."

  val NestedLambda =
    "Lambda1 bodies do not support nested lambdas; move the nested lambda outside this pattern."

  val ContextFunction =
    "Lambda1 supports ordinary `=>` functions only; replace `?=>` with an explicitly typed ordinary parameter."

  val OwnedDefinitionSplice =
    "A term spliced into a Lambda1 body must not contain local val, def, or class definitions; splice a definition-free expression instead."
