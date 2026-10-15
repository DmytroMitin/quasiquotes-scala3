package quasiquotes.q072

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.{TargetTermView, TermPatternProductExtractor, TermPatternProductExtractorFactory}

/** Test-only Q072 extractor. It models the typed product which the temporary
  * Q071 standard route would have exposed, without changing public `qq`.
  *
  * `#repeat` is intentionally an experimental literal marker. Its presence in
  * StringContext parts lets the macro select equality semantics without seeing
  * or rewriting Scala pattern binders.
  */
object Q072PublicPatternProbe:
  extension (inline context: StringContext)
    transparent inline def q072qq(using q: Quotes) =
      ${ Q072PublicPatternProbeMacro.extractor('context, 'q) }

private object Q072PublicPatternProbeMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "Q072 probe requires a statically known StringContext",
          context
        )

    parts match
      case List("", ".", ".", "") =>
        '{ Q072PublicPatternProbeFactory.distinctDirect(using $callerQuotes) }
      case List("", ".", ".", "(", ")") =>
        '{ Q072PublicPatternProbeFactory.distinctUnary(using $callerQuotes) }
      case List("", ".", ".#repeat") =>
        '{ Q072PublicPatternProbeFactory.repeatDirect(using $callerQuotes) }
      case List("", ".", ".#repeat(", ")") =>
        '{ Q072PublicPatternProbeFactory.repeatUnary(using $callerQuotes) }
      case _ =>
        quotes.reflect.report.errorAndAbort(
          s"Q072 probe does not implement StringContext parts: $parts",
          context
        )

private object Q072PublicPatternProbeFactory:
  def distinctDirect(using q: Quotes): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, SelectedMemberName)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      if target == null then None
      else
        TargetTermView.fromTerm(target).toOption.flatMap {
          case TargetTermView.Select(
                TargetTermView.Select(receiver, first, _),
                second,
                _
              ) =>
            for
              firstName <- SelectedMemberName.from(first).toOption
              secondName <- SelectedMemberName.from(second).toOption
            yield (receiver.original, firstName, secondName)
          case _ => None
        }
    )

  def distinctUnary(using q: Quotes): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      if target == null then None
      else
        TargetTermView.fromTerm(target).toOption.flatMap {
          case TargetTermView.Apply(
                TargetTermView.Select(
                  TargetTermView.Select(receiver, first, _),
                  second,
                  _
                ),
                argument :: Nil,
                _
              ) =>
            for
              firstName <- SelectedMemberName.from(first).toOption
              secondName <- SelectedMemberName.from(second).toOption
            yield (receiver.original, firstName, secondName, argument.original)
          case _ => None
        }
    )

  def repeatDirect(using q: Quotes): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      distinctDirect.unapply(target).collect {
        case (receiver, first, second) if first == second => (receiver, first)
      }
    )

  def repeatUnary(using q: Quotes): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, q.reflect.Term)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      distinctUnary.unapply(target).collect {
        case (receiver, first, second, argument) if first == second =>
          (receiver, first, argument)
      }
    )

object Q072ProbeTargets:
  object distinctReceiver:
    object inner:
      val outer: Int = 42

  object repeatedReceiver:
    object same:
      val same: Int = 42

  object unaryReceiver:
    object same:
      def same(value: Int): Int = value + 1
