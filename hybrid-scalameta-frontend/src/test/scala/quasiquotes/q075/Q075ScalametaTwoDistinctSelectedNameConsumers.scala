package quasiquotes.q075

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.scalameta.ScalametaQuasiPattern.qq

/** Separate compilation layer for the public Q075 typed-Scalameta patterns. */
object Q075ScalametaTwoDistinctSelectedNameConsumers:
  def direct(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    target match
      case qq"$receiver.$first.$second" => Some((receiver, first, second))
      case _ => None

  def unary(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)] =
    target match
      case qq"$receiver.$first.$second($argument)" =>
        Some((receiver, first, second, argument))
      case _ => None

  def guardedEqual(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    target match
      case qq"$receiver.$first.$second" if first == second =>
        Some((receiver, first, second))
      case _ => None
