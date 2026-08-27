package external.consumer

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.construct.SelectedMemberName

final class NullarySelectedCallTarget(private val value: Int):
  def nullary(): Int = value + 1
  def nonEmpty(delta: Int): Int = value + delta
  def parameterless: Int = value + 2

object NullarySelectedCallCompositionSnippet:
  inline def capacity(value: Int): Int =
    ${ capacityImpl('value) }

  private def capacityImpl(value: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    capacityImplTerm(value.asTerm).asExprOf[Int]

  private def capacityImplTerm(using Quotes)(
      value: quotes.reflect.Term
  ): quotes.reflect.Term =
    qr"new java.lang.StringBuilder($value).capacity()"

  inline def ordinaryNullary(receiver: NullarySelectedCallTarget): Int =
    ${ ordinaryNullaryImpl('receiver) }

  private def ordinaryNullaryImpl(
      receiver: Expr[NullarySelectedCallTarget]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"${receiver.asTerm}.nullary()".asExprOf[Int]

  inline def ordinaryNonEmpty(
      receiver: NullarySelectedCallTarget,
      delta: Int
  ): Int =
    ${ ordinaryNonEmptyImpl('receiver, 'delta) }

  private def ordinaryNonEmptyImpl(
      receiver: Expr[NullarySelectedCallTarget],
      delta: Expr[Int]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"${receiver.asTerm}.nonEmpty(${delta.asTerm})".asExprOf[Int]

  inline def constructorOnly(value: Int): java.lang.StringBuilder =
    ${ constructorOnlyImpl('value) }

  private def constructorOnlyImpl(
      value: Expr[Int]
  )(using Quotes): Expr[java.lang.StringBuilder] =
    import quotes.reflect.*
    qr"new java.lang.StringBuilder(${value.asTerm})"
      .asExprOf[java.lang.StringBuilder]

  inline def fixedSelectionWithoutApply(
      receiver: NullarySelectedCallTarget
  ): Int =
    ${ fixedSelectionWithoutApplyImpl('receiver) }

  private def fixedSelectionWithoutApplyImpl(
      receiver: Expr[NullarySelectedCallTarget]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    qr"${receiver.asTerm}.parameterless".asExprOf[Int]

  inline def dynamicNullary(receiver: NullarySelectedCallTarget): Int =
    ${ dynamicNullaryImpl('receiver) }

  private def dynamicNullaryImpl(
      receiver: Expr[NullarySelectedCallTarget]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val selected = SelectedMemberName.from("nullary").fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    qr"${receiver.asTerm}.$selected()".asExprOf[Int]
