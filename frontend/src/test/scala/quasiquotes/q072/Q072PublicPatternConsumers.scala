package quasiquotes.q072

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

/** Separate compilation layer: these are genuine Scala extractor patterns
  * whose transparent selector is supplied by the test-only macro probe.
  */
object Q072PublicPatternConsumers:
  import Q072PublicPatternProbe.*

  def distinctDirect(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    target match
      case q072qq"$receiver.$first.$second" => Some((receiver, first, second))
      case _ => None

  def distinctUnary(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)] =
    target match
      case q072qq"$receiver.$first.$second($argument)" =>
        Some((receiver, first, second, argument))
      case _ => None

  def guardedEquality(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName)] =
    target match
      case q072qq"$receiver.$first.$second" if first == second =>
        Some((receiver, first, second))
      case _ => None

  def wildcardSecond(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName)] =
    target match
      case q072qq"$receiver.$first.${_}" => Some((receiver, first))
      case _ => None

  def stableConstants(using q: Quotes)(
      target: q.reflect.Term,
      expected: SelectedMemberName
  ): Option[q.reflect.Term] =
    target match
      case q072qq"$receiver.${`expected`}.${`expected`}" => Some(receiver)
      case _ => None

  def repeatedMarker(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName)] =
    target match
      case q072qq"$receiver.$same.#repeat" => Some((receiver, same))
      case _ => None

  def repeatedMarkerUnary(using q: Quotes)(
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, q.reflect.Term)] =
    target match
      case q072qq"$receiver.$same.#repeat($argument)" =>
        Some((receiver, same, argument))
      case _ => None
