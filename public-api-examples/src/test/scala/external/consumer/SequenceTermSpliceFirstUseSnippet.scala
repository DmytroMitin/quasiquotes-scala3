package external.consumer

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.qr
import quasiquotes.construct.TermSequenceSplices.termSplice

object SequenceTermFirstUseTargets:
  def four(first: Int, second: Int, third: Int, fourth: Int): List[Int] =
    List(first, second, third, fourth)

final class SequenceTermFirstUseBox(
    first: Int,
    second: Int,
    third: Int,
    fourth: Int
):
  def ordered: List[Int] = List(first, second, third, fourth)

object SequenceTermSpliceFirstUseSnippet:
  inline def applyArguments(inline left: Int, inline right: Int): List[Int] =
    ${ applyArgumentsImpl('left, 'right) }

  inline def constructorArguments(inline left: Int, inline right: Int): List[Int] =
    ${ constructorArgumentsImpl('left, 'right) }

  private def applyArgumentsImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    val arguments = termSplice(Seq(left.asTerm, right.asTerm))
    qr"external.consumer.SequenceTermFirstUseTargets.four(${Expr(-1).asTerm}, ..$arguments, ${Expr(99).asTerm})"
      .asExprOf[List[Int]]

  private def constructorArgumentsImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    val constructorType = TypeRepr.of[SequenceTermFirstUseBox]
    val arguments = termSplice(Seq(left.asTerm, right.asTerm))
    val created = qr"new $constructorType(${Expr(-1).asTerm}, ..$arguments, ${Expr(99).asTerm})"
    Select.unique(created, "ordered").asExprOf[List[Int]]
