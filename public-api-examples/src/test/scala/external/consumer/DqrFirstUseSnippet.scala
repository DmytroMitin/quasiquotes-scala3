package external.consumer

// snippet:dqr-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*

object DqrFirstUseSnippet:
  inline def identity(value: Int): Int = ${ identityImpl('value) }

  private def identityImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    val parameterType: q.reflect.TypeRepr = TypeRepr.of[Int]
    val resultType: q.reflect.TypeRepr = TypeRepr.of[Int]
    val definition: q.reflect.DefDef =
      dqr"def boundedIdentity(value: $parameterType): $resultType = value"

    val method = definition.symbol
    val parameters = method.paramSymss.flatten
    val exactOwnerAndBinder =
      method.owner == Symbol.spliceOwner &&
        parameters.size == 1 &&
        parameters.head.owner == method &&
        definition.rhs.exists(_.symbol == parameters.head)

    if !exactOwnerAndBinder then
      report.errorAndAbort("external dqr owner/binder proof failed")

    Block(
      List(definition),
      Apply(Ref(method), List(value.asTerm))
    ).asExprOf[Int]
// snippet:dqr-first-use:end
