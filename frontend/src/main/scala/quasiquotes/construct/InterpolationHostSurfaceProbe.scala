package quasiquotes.construct

import scala.quoted.*

private[quasiquotes] final case class InterpolationHostSurfaceEvidence(
    parts: List[String],
    argumentCount: Int,
    argumentSource: List[String]
)

private[quasiquotes] object InterpolationHostSurfaceProbe:
  extension (inline context: StringContext)
    inline def hostProbe(inline arguments: Any*): InterpolationHostSurfaceEvidence =
      ${ inspect('context, 'arguments) }

  private def inspect(
      context: Expr[StringContext],
      arguments: Expr[Seq[Any]]
  )(using Quotes): Expr[InterpolationHostSurfaceEvidence] =
    import quotes.reflect.*
    val partExpressions = context match
      case '{ StringContext(${ Varargs(parts) }*) } => parts.toList
      case _ => quotes.reflect.report.errorAndAbort("Expected a statically known StringContext.", context)
    val argumentExpressions = arguments match
      case Varargs(values) => values.toList
      case _ => quotes.reflect.report.errorAndAbort("Expected statically known interpolation arguments.", arguments)

    val parts = partExpressions.map(_.valueOrAbort)
    val sources: List[String] =
      argumentExpressions.map(_.asTerm.pos.sourceCode.getOrElse("<unavailable>"))
    '{
      InterpolationHostSurfaceEvidence(
        ${ Expr.ofList(parts.map(Expr(_))) },
        ${ Expr(argumentExpressions.size) },
        ${ Expr.ofList(sources.map(Expr(_))) }
      )
    }
