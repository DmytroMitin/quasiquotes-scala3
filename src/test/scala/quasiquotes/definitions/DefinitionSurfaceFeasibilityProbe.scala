package quasiquotes.definitions

import scala.quoted.*

private[definitions] sealed trait ProbeDefinitionArgument

private[definitions] final class ProbeDefinitionTypeArgument private[definitions] (
    val value: String
) extends ProbeDefinitionArgument

private[definitions] final class ProbeBodyTermArgument private[definitions] (
    val value: String
) extends ProbeDefinitionArgument

private[definitions] final class ProbeBodyTypeArgument private[definitions] (
    val value: String
) extends ProbeDefinitionArgument

private[definitions] object ProbeDefinitionArguments:
  def definitionType(value: String): ProbeDefinitionTypeArgument =
    new ProbeDefinitionTypeArgument(value)

  def bodyTerm(value: String): ProbeBodyTermArgument =
    new ProbeBodyTermArgument(value)

  def bodyType(value: String): ProbeBodyTypeArgument =
    new ProbeBodyTypeArgument(value)

private[definitions] final case class DefinitionSurfaceProbeEvidence(
    parts: List[String],
    categories: List[String],
    argumentStarts: List[Int],
    argumentEnds: List[Int],
    partStarts: List[Int],
    partEnds: List[Int]
)

private[definitions] object DefinitionSurfaceFeasibilityProbe:
  extension (inline context: StringContext)
    inline def definitionSurfaceProbe(
        inline arguments: ProbeDefinitionArgument*
    ): DefinitionSurfaceProbeEvidence =
      ${ inspect('context, 'arguments) }

  private def inspect(
      context: Expr[StringContext],
      arguments: Expr[Seq[ProbeDefinitionArgument]]
  )(using Quotes): Expr[DefinitionSurfaceProbeEvidence] =
    val partExpressions =
      context match
        case '{ StringContext(${ Varargs(parts) }*) } => parts.toList
        case _ => quotes.reflect.report.errorAndAbort(
            "Expected a statically known StringContext.",
            context
          )
    val argumentExpressions =
      arguments match
        case Varargs(values) => values.toList
        case _ => quotes.reflect.report.errorAndAbort(
            "Expected statically known interpolation arguments.",
            arguments
          )

    val parts = partExpressions.map(_.valueOrAbort)
    val categories = argumentExpressions.map(categoryOf)
    val argumentPositions = argumentExpressions.map(positionOf)
    val partPositions = partExpressions.map(positionOf)

    '{
      DefinitionSurfaceProbeEvidence(
        ${ Expr.ofList(parts.map(Expr(_))) },
        ${ Expr.ofList(categories.map(Expr(_))) },
        ${ Expr.ofList(argumentPositions.map(value => Expr(value._1))) },
        ${ Expr.ofList(argumentPositions.map(value => Expr(value._2))) },
        ${ Expr.ofList(partPositions.map(value => Expr(value._1))) },
        ${ Expr.ofList(partPositions.map(value => Expr(value._2))) }
      )
    }

  private def categoryOf(
      argument: Expr[ProbeDefinitionArgument]
  )(using Quotes): String =
    import quotes.reflect.*

    val actual = argument.asTerm.tpe.widen
    if actual <:< TypeRepr.of[ProbeDefinitionTypeArgument] then
      "DefinitionType"
    else if actual <:< TypeRepr.of[ProbeBodyTermArgument] then
      "BodyTerm"
    else if actual <:< TypeRepr.of[ProbeBodyTypeArgument] then
      "BodyType"
    else
      report.errorAndAbort(
        s"Unexpected probe descriptor type: ${actual.show}",
        argument
      )

  private def positionOf(
      expression: Expr[Any]
  )(using Quotes): (Int, Int) =
    import quotes.reflect.*

    val position = expression.asTerm.pos
    (position.start, position.end)
