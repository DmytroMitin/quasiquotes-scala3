package external.consumer

// snippet:dqr-first-use:start
import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*

object DqrFirstUseSnippet:
  inline def identity(value: Int): Int = ${ identityImpl('value) }

  inline def selectRight(left: Int, right: String): String =
    ${ selectRightImpl('left, 'right) }

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

  private def selectRightImpl(
      left: Expr[Int],
      right: Expr[String]
  )(using q: Quotes): Expr[String] =
    import q.reflect.*

    val leftType: q.reflect.TypeRepr = TypeRepr.of[Int]
    val rightType: q.reflect.TypeRepr = TypeRepr.of[String]
    val definition: q.reflect.DefDef =
      dqr"def selectRight(left: $leftType, right: $rightType): $rightType = right"

    val method = definition.symbol
    val parameters = method.paramSymss.flatten
    val exactOwnerAndBinder =
      method.owner == Symbol.spliceOwner &&
        parameters.size == 2 &&
        parameters.forall(_.owner == method) &&
        definition.rhs.exists(_.symbol == parameters(1))

    if !exactOwnerAndBinder then
      report.errorAndAbort("external exact-two dqr owner/binder proof failed")

    Block(
      List(definition),
      Apply(Ref(method), List(left.asTerm, right.asTerm))
    ).asExprOf[String]
// snippet:dqr-first-use:end
