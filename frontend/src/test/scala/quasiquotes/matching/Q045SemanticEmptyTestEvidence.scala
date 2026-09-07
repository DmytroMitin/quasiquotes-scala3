package quasiquotes.matching

import scala.quoted.*

object Q045SemanticEmptyTestEvidence:
  def isEmpty(using q: Quotes)(symbol: q.reflect.Symbol): Boolean =
    DefinitionModifierSemantics.isSemanticallyEmpty(symbol)
