package quasiquotes.parser

private[quasiquotes] object P1BlockDiagnosticMessages:
  val LocalVal: String =
    "P1 block does not support local val or var definitions; move the binding outside the quasiquote or use only expression statements."
  val LocalDef: String =
    "P1 block does not support local def definitions; move the definition outside the quasiquote or use only expression statements."

  def UnsupportedStatement(kind: String): String =
    s"P1 block supports expression statements only; unsupported block statement: $kind."
