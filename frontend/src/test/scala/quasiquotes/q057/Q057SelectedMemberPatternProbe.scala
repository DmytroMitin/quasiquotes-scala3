package quasiquotes.q057

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.*

/** Test-only candidate for an extractor whose capture tuple may contain
  * heterogeneous semantic values rather than only scalar/rank-2 Terms.
  */
final class Q057HeterogeneousTermPatternExtractor[T, Captures <: Tuple](
    extract: T => Option[Captures]
):
  def unapply(value: T): Option[Captures] = extract(value)

object Q057SelectedMemberPatternProbe:
  extension (inline context: StringContext)
    transparent inline def q057qq(using q: Quotes) =
      ${ Q057SelectedMemberPatternProbeMacro.extractor('context, 'q) }

object Q057SelectedMemberPatternProbeFactory:
  private val PlaceholderName = "__q057_selected_name_placeholder"

  def direct(using q: Quotes)(
      pattern: TermPattern,
      receiverHole: String
  ): Q057HeterogeneousTermPatternExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName)
  ] =
    new Q057HeterogeneousTermPatternExtractor(target =>
      Q057SelectedMemberPatternSidecar.matchSelected(pattern, target).flatMap {
        case (result, selected) =>
          result.bindings.get(receiverHole).map(receiver => (receiver, selected))
      }
    )

  def nullary(using q: Quotes)(
      pattern: TermPattern,
      receiverHole: String
  ): Q057HeterogeneousTermPatternExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName)
  ] = direct(pattern, receiverHole)

  def unary(using q: Quotes)(
      pattern: TermPattern,
      receiverHole: String,
      argumentHole: String
  ): Q057HeterogeneousTermPatternExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, q.reflect.Term)
  ] =
    new Q057HeterogeneousTermPatternExtractor(target =>
      Q057SelectedMemberPatternSidecar.matchSelected(pattern, target).flatMap {
        case (result, selected) =>
          for
            receiver <- result.bindings.get(receiverHole)
            argument <- result.bindings.get(argumentHole)
          yield (receiver, selected, argument)
      }
    )

  /** Mechanical E1 experiment: the existing public class has no type-level
    * restriction that prevents a heterogeneous, non-ranked capture tuple.
    * Its public name and documentation are nevertheless rank-2-specific.
    */
  def mechanicallyReuseRanked[T, Captures <: Tuple](
      extractor: Q057HeterogeneousTermPatternExtractor[T, Captures]
  ): RankedTermPatternExtractor[T, Captures] =
    new RankedTermPatternExtractor(extractor.unapply)

  def validateDecodedTargetName(decoded: String): Option[SelectedMemberName] =
    SelectedMemberName.from(decoded).toOption

  private object Q057SelectedMemberPatternSidecar:
    def matchSelected(using q: Quotes)(
        pattern: TermPattern,
        target: q.reflect.Term
    ): Option[(MatchResult[q.reflect.Term], SelectedMemberName)] =
      if target == null then None
      else
        for
          targetView <- TargetTermView.fromTerm(target).toOption
          decoded <- rootSelectedName(pattern, targetView)
          selected <- validateDecodedTargetName(decoded)
          concretePattern <- replaceRootSelectedName(pattern, decoded)
          result <- TermMatcher.matchTermRaw(concretePattern, target).toOption
        yield (result, selected)

    private def rootSelectedName[T](
        pattern: TermPattern,
        target: TargetTermView[T]
    ): Option[String] =
      (pattern, target) match
        case (
              TermPattern.Select(_, PlaceholderName),
              TargetTermView.Select(_, targetName, _)
            ) => Some(targetName)
        case (
              TermPattern.Apply(TermPattern.Select(_, PlaceholderName), _),
              TargetTermView.Apply(TargetTermView.Select(_, targetName, _), _, _)
            ) => Some(targetName)
        case _ => None

    private def replaceRootSelectedName(
        pattern: TermPattern,
        decoded: String
    ): Option[TermPattern] =
      pattern match
        case TermPattern.Select(qualifier, PlaceholderName) =>
          Some(TermPattern.Select(qualifier, decoded))
        case TermPattern.Apply(TermPattern.Select(qualifier, PlaceholderName), arguments) =>
          Some(TermPattern.Apply(TermPattern.Select(qualifier, decoded), arguments))
        case _ => None

private object Q057SelectedMemberPatternProbeMacro:
  private val ReceiverHole = "q057Receiver"
  private val ArgumentHole = "q057Argument"
  private val PlaceholderName = "__q057_selected_name_placeholder"

  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "Q057 selected-name feasibility probe requires a static StringContext.",
          context
        )

    val (source, layout) = parts match
      case List("", ".", "") =>
        (s"$$$ReceiverHole.$PlaceholderName", "direct")
      case List("", ".", "()") =>
        (s"$$$ReceiverHole.$PlaceholderName()", "nullary")
      case List("", ".", "(", ")") =>
        (s"$$$ReceiverHole.$PlaceholderName($$$ArgumentHole)", "unary")
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "Q057 probe admits only $receiver.$selectedName, $receiver.$selectedName(), or $receiver.$selectedName($argument).",
          context
        )

    val pattern = QuasiPattern.term(source).fold(
      failure =>
        quotes.reflect.report.errorAndAbort(
          s"Q057 synthesized selected-name pattern failed: ${failure.message}",
          context
        ),
      _.pattern
    )
    val patternExpression = RankedTermPatternBridge.patternExpr(pattern)

    layout match
      case "direct" =>
        '{
          Q057SelectedMemberPatternProbeFactory.direct(using $callerQuotes)(
            $patternExpression,
            ${ Expr(ReceiverHole) }
          )
        }
      case "nullary" =>
        '{
          Q057SelectedMemberPatternProbeFactory.nullary(using $callerQuotes)(
            $patternExpression,
            ${ Expr(ReceiverHole) }
          )
        }
      case _ =>
        '{
          Q057SelectedMemberPatternProbeFactory.unary(using $callerQuotes)(
            $patternExpression,
            ${ Expr(ReceiverHole) },
            ${ Expr(ArgumentHole) }
          )
        }
