package quasiquotes.parser

private[quasiquotes] object P2LocalValDiagnosticMessages:
  val MissingExplicitType: String =
    "P2 local val requires one explicit supported type annotation."
  val Mutable: String =
    "P2 block does not support var; exactly one eager immutable local val is required."
  val Lazy: String =
    "P2 block does not support lazy val; exactly one eager immutable local val is required."
  val Pattern: String =
    "P2 block does not support pattern or destructuring val binders; a simple identifier is required."
  val ExactlyOne: String =
    "P2 block requires exactly one local val statement followed by one final result."
  val SecondOrNested: String =
    "Phase 116 admits only one P2 local val binder per quasiquote tree; a second or nested P2 local val is unsupported."
  val SourceBinderShadowing: String =
    "Phase 116 does not support source-binder shadowing involving a P2 local val."
  val LocalDef: String =
    "P2 block does not support local def definitions."
  val UnsupportedType: String =
    "P2 local val requires an explicit type from the admitted Type subset."
  val UnsupportedInitializer: String =
    "P2 local val initializer contains a Term child outside the admitted language."
  val UnsupportedResult: String =
    "P2 local val result contains a Term child outside the admitted language."
  val OwnedDefinitionSplice: String =
    "P2 local val does not support an external splice containing owned definitions; owner-changing splice migration is not enabled."
