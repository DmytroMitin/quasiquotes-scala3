package external.consumer

import scala.quoted.*

object DqrSelectiveImportSnippet:
  inline def identity(value: Int): Int = ${ identityImpl('value) }

  private def identityImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.construct.Quasiquotes.dqr

    val intType = TypeRepr.of[Int]
    val definition = dqr"def selectiveIdentity(value: $intType): $intType = value"
    Block(
      List(definition),
      Apply(Ref(definition.symbol), List(value.asTerm))
    ).asExprOf[Int]
