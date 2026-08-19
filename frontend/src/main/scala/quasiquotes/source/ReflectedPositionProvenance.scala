package quasiquotes.source

import scala.quoted.Quotes

private[quasiquotes] object ReflectedPositionProvenance:
  def sourceCode(using q: Quotes)(
      position: q.reflect.Position
  ): Option[String] =
    withoutNoSpanBoundaryAssertion(position.sourceCode).flatten

  def usableBounds(using q: Quotes)(
      position: q.reflect.Position
  ): Option[(Int, Int)] =
    withoutNoSpanBoundaryAssertion {
      val start = position.start
      val end = position.end
      Option.when(start >= 0 && end >= start)((start, end))
    }.flatten

  private def withoutNoSpanBoundaryAssertion[A](probe: => A): Option[A] =
    try Some(probe)
    catch
      // Quotes.Position has no public span-availability predicate on the
      // supported lanes. Its optional sourceCode implementation reaches the
      // internal NoSpan start/end accessors, which assert for generated trees.
      case error: AssertionError if isNoSpanBoundaryAssertion(error) => None

  private def isNoSpanBoundaryAssertion(error: AssertionError): Boolean =
    error.getMessage match
      case "assertion failed: start of NoSpan" => true
      case "assertion failed: end of NoSpan" => true
      case _ => false
