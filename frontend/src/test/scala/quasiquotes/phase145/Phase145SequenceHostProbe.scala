package quasiquotes.phase145

import scala.quoted.*

final case class Phase145SequenceHostEvidence(
    parts: List[String],
    argumentCount: Int,
    argumentSources: List[String]
)

final class Phase145ProbeTermSequence[+Term](val terms: Seq[Term])

object Phase145ProbeTermSequence:
  def apply[Term](terms: Seq[Term]): Phase145ProbeTermSequence[Term] =
    new Phase145ProbeTermSequence(terms)

object Phase145SequenceHostProbe:
  extension (inline context: StringContext)
    inline def sequenceHostProbe(inline arguments: Any*): Phase145SequenceHostEvidence =
      ${ inspect('context, 'arguments) }

  private def inspect(
      context: Expr[StringContext],
      arguments: Expr[Seq[Any]]
  )(using Quotes): Expr[Phase145SequenceHostEvidence] =
    import quotes.reflect.*

    val partExpressions = context match
      case '{ StringContext(${ Varargs(parts) }*) } => parts.toList
      case _ => quotes.reflect.report.errorAndAbort("Expected a statically known StringContext.", context)
    val argumentExpressions = arguments match
      case Varargs(values) => values.toList
      case _ => quotes.reflect.report.errorAndAbort("Expected statically known interpolation arguments.", arguments)

    val parts = partExpressions.map(_.valueOrAbort)
    val sources = argumentExpressions.map(_.asTerm.pos.sourceCode.getOrElse("<unavailable>"))
    '{
      Phase145SequenceHostEvidence(
        ${ Expr.ofList(parts.map(Expr(_))) },
        ${ Expr(argumentExpressions.size) },
        ${ Expr.ofList(sources.map(Expr(_))) }
      )
    }
