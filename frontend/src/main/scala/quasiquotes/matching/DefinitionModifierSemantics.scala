package quasiquotes.matching

import scala.quoted.Quotes

private[matching] object DefinitionModifierSemantics:
  def isSemanticallyEmpty(using q: Quotes)(symbol: q.reflect.Symbol): Boolean =
    import q.reflect.*

    val semanticFlags = List(
      Flags.Private,
      Flags.PrivateLocal,
      Flags.Protected,
      Flags.Final,
      Flags.Override,
      Flags.Deferred,
      Flags.Inline,
      Flags.Transparent,
      Flags.Implicit,
      Flags.Given,
      Flags.Erased,
      Flags.Infix,
      Flags.Exported,
      Flags.JavaStatic,
      Flags.Macro
    )

    symbol.annotations.isEmpty &&
      symbol.privateWithin.isEmpty &&
      symbol.protectedWithin.isEmpty &&
      semanticFlags.forall(flag => !symbol.flags.is(flag))
