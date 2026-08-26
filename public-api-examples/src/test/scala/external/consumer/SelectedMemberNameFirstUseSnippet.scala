package external.consumer

import scala.quoted.*

import quasiquotes.construct.{Quasiquotes, SelectedMemberName}

final class SelectedMemberUserTarget:
  def ordinary(value: Int): Int = value + 1
  def +(value: Int): Int = value + 2
  def `type`(value: Int): Int = value + 3
  def `safe spaced name`(value: Int): Int = value + 4

object SelectedMemberNameFirstUseSnippet:
  inline def fixed(receiver: SelectedMemberUserTarget, value: Int): Int =
    ${ fixedImpl('receiver, 'value) }

  inline def manual(receiver: SelectedMemberUserTarget, value: Int): Int =
    ${ manualImpl('receiver, 'value) }

  inline def dynamic(
      receiver: SelectedMemberUserTarget,
      decodedName: String,
      value: Int
  ): Int =
    ${ dynamicImpl('receiver, 'decodedName, 'value) }

  private def fixedImpl(
      receiver: Expr[SelectedMemberUserTarget],
      value: Expr[Int]
  )(using Quotes): Expr[Int] =
    '{ $receiver.ordinary($value) }

  private def manualImpl(
      receiver: Expr[SelectedMemberUserTarget],
      value: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    val selected = SelectedMemberName.from("ordinary").fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    Select.unique(receiver.asTerm, selected.decoded)
      .appliedTo(value.asTerm)
      .asExprOf[Int]

  private def dynamicImpl(
      receiver: Expr[SelectedMemberUserTarget],
      decodedName: Expr[String],
      value: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*

    val decoded = decodedName.valueOrAbort
    val selected = SelectedMemberName.from(decoded).fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    val receiverTerm = receiver.asTerm
    val valueTerm = value.asTerm
    qr"$receiverTerm.$selected($valueTerm)".asExprOf[Int]
